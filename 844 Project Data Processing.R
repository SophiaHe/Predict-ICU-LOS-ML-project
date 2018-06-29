library(dplyr)

# read in data
setwd("C:/Users/hyq92/Documents/R/844 Project/MIMIC III Data")
admissions <- read.csv(file = "admissions.csv", header = T)
icu_details <- read.csv(file = "icustay_details_output.csv", header = T)
services <- read.csv(file = "services.csv", header = T)

first24hr_vitals <- read.csv(file = "first24hr_vitals.csv", header = T)
first24hr_lab <-read.csv(file = "first24hr_lab_output.csv", header = T)
first24hr_gcs <-read.csv(file = "gcs_firstday.csv", header = T) 
heightweight <- read.csv(file = "heightweight_output.csv", header = T)

# functions:
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

# SUBJECT_ID is unique to a patient
# HADM_ID is unique to a patient hospital stay 
# ICUSTAY_ID is unique to a patient ICU stay

# ICU_detail_N: non-dead, non-newborn & first icu stay for 1 hospital visit:
# not-unique subject_id (b/c people can have multiple hospital visits, each resulted in a ICU stay; 
# I treated those 35 people's multiple rows/ICU stays as independent), 
# unique hadm_id & unique icustay_id
icu_details_N <- icu_details %>% filter(admission_type != "NEWBORN", hospital_expire_flag == 0, 
                                        first_icu_stay == "Y")

icu_details_N <- icu_details %>% filter(admission_type != "NEWBORN", hospital_expire_flag == 0)

# merge previous_service (if any) the patient received with current services that he/she resides 
# under for the same hospital stay
services_merged <- services %>% mutate(all_services = paste(prev_service,curr_service, by = ""))%>% 
  filter(prev_service != "") %>% 
  filter(duplicated(hadm_id) == FALSE)


# all surgical patients
all_surg <- icu_details_N %>% 
  left_join(admissions, by = "hadm_id") %>% 
  left_join(services_merged, by = "hadm_id") %>% 
  left_join(first24hr_vitals, by = "icustay_id") %>% 
  left_join(first24hr_lab, by = "icustay_id") %>% 
  left_join(first24hr_gcs, by = "icustay_id") %>% 
  left_join(heightweight, by = "icustay_id") %>% 
  filter(grepl(pattern="CSURG|NSURG|ORTHO|PSURG|SURG|TRAUM|TSURG|VSURG|OBS)", all_services)) %>% 
  filter(first_icu_stay == "Y") %>% 
  filter(los_icu > 1)


# remove .x or .y in column names
names(all_surg)<- gsub(".x","",names(all_surg),fixed = TRUE)
names(all_surg)<- gsub(".y","",names(all_surg),fixed = TRUE)
all_surg <- all_surg[, unique(colnames(all_surg))]

# Further varaible selection on all_surg:

# very right skwed distn of los_icu: use log(los_icu) as response
# hist(all_surg$los_icu)

# most of first24_vital are not NA; 

# in first24_lab's NA < 10: 
# creatinine (BLOOD creatinine),hematocrit (No NA, blood HEMATOCRIT),hemoglobin (blood HEMOGLOBIN),potassium(blood POTASSIUM)
# sodium(blood sodium),bun(blood UREA NITROGEN),

# most ICU visit patients were admitted under EMERGENCY admission type: 1079/1262
# summary(all_surg$admission_type)

# deselct: los_hospital, hospital_expire_flag, hospstay_seq, icustay_seq, row_id, deathtime, has_chartevents_data
all_surg <- all_surg %>% select(-c(los_hospital, hospital_expire_flag, hospstay_seq, icustay_seq, row_id, deathtime, has_chartevents_data))

# derived: 
# lengh of time from register to transfer to icu: intime - admittime = in days

all_surg <- all_surg %>% mutate(los_reg_trs = as.numeric((as.POSIXct(as.character(intime), "%Y-%m-%d %H-%M-%S") - 
                                                            as.POSIXct(as.character(admittime), "%Y-%m-%d %H-%M-%S"))/(60*60*24))) %>% 
  mutate(los_reg_trs = ifelse(los_reg_trs < 0, 0.81280, los_reg_trs))
# summary(all_surg$los_reg_trs) # some negatives in it, why?



# model 1: gbm with mean in vitals results & max in lab results
df1 <-  all_surg %>% 
  select(subject_id, hadm_id, icustay_id,
        los_icu, gender,age,ethnicity, admission_type, first_hosp_stay, discharge_location,
        language,religion, marital_status, diagnosis,los_reg_trs,
        heartrate_mean, sysbp_mean, diasbp_mean, meanbp_mean, resprate_mean, tempc_mean,
        spo2_mean, glucose_mean, creatinine_max, hematocrit_max, hemoglobin_max, potassium_max, 
        sodium_max, bun_max,
        mingcs, gcsmotor, gcsverbal, gcseyes, endotrachflag,  height_first, weight_first)
summary(df1)

df1_final <- df1 %>% 
  # clean age, height_first (m), weight_first: use gender-based ave height for extreme/unrelistic values: df1 %>%  group_by(gender) %>% summarize(mean_height =mean(height_first, na.rm=T))
  mutate(age = ifelse(age > 100 & gender == "F", 76.47545, ifelse(age > 100 & gender == "M", 65.81420, age)),
         height_first = ifelse(height_first <  145 & gender == "F", 160.8664, ifelse(height_first <  145 & gender == "M", 176.4041,ifelse(height_first > 220 & gender == "M", 176.4041, ifelse(height_first > 220 & gender =="F", 160.8664, height_first)))),
         height_first = ifelse(is.na(height_first) == T & gender == "F", 160.8664, ifelse(is.na(height_first) == T & gender == "M", 176.4041,height_first)),
         weight_first = ifelse(is.na(weight_first) == T & gender == "F", 74.29213, ifelse(is.na(weight_first) == T & gender == "M", 86.09042, weight_first)),
         # df1 %>%  group_by(gender) %>% summarize(mean_weight =mean(weight_first, na.rm=T))
         log_los_icu = log(los_icu),
         # Replace few NAs in all continuous variables with its corresponding median
         heartrate_mean = ifelse(is.na(heartrate_mean) == T, median(heartrate_mean,na.rm = T), heartrate_mean),
         sysbp_mean = ifelse(is.na(sysbp_mean) == T, median(sysbp_mean,na.rm = T), sysbp_mean),
         meanbp_mean = ifelse(is.na(meanbp_mean) == T, median(meanbp_mean,na.rm = T), meanbp_mean),
         resprate_mean = ifelse(is.na(resprate_mean) == T, median(resprate_mean,na.rm = T), resprate_mean),
         tempc_mean = ifelse(is.na(tempc_mean) == T, median(tempc_mean,na.rm = T), tempc_mean),
         spo2_mean = ifelse(is.na(spo2_mean) == T, median(spo2_mean,na.rm = T), spo2_mean),
         glucose_mean = ifelse(is.na(glucose_mean) == T, median(glucose_mean,na.rm = T), glucose_mean),
         creatinine_max = ifelse(is.na(creatinine_max) == T, median(creatinine_max,na.rm = T), creatinine_max),
         hematocrit_max = ifelse(is.na(hematocrit_max) == T, median(hematocrit_max,na.rm = T), hematocrit_max),
         potassium_max = ifelse(is.na(potassium_max) == T, median(potassium_max,na.rm = T), potassium_max),
         sodium_max = ifelse(is.na(sodium_max) == T, median(sodium_max,na.rm = T), sodium_max),
         bun_max = ifelse(is.na(bun_max) == T, median(bun_max,na.rm = T), bun_max),
         mingcs = ifelse(is.na(mingcs) == T, median(mingcs,na.rm = T), mingcs),
         endotrachflag = ifelse(is.na(endotrachflag) == T, median(endotrachflag,na.rm = T), endotrachflag),
         gcsmotor  = ifelse(is.na(gcsmotor ) == T, median(gcsmotor ,na.rm = T), gcsmotor )) %>% 
  select(-c(subject_id, hadm_id, icustay_id, diagnosis,los_icu,hemoglobin_max,diasbp_mean,gcseyes,gcsverbal ))

# No NA in categorical variables, so replace NA in continuous variables with 0
# df1_final[is.na(df1_final)] <- 0

hist(df1$age, breaks = 40, xlab = "Age", ylab = "Frequency", main = "Age Distribution Before Adjustment")
hist(df1_final$age, breaks = 40, xlab = "Age", ylab = "Frequency", main = "Age Distribution After Adjustment")

hist(df1$height_first, breaks = 40, xlab = "Height", ylab = "Frequency", main = "Height Distribution Before Adjustment")
hist(df1_final$height_first, breaks = 40, xlab = "Height", ylab = "Frequency", main = "Height Distribution After Adjustment")

hist(df1$weight_first, breaks = 40, xlab = "Weight", ylab = "Frequency", main = "Weight Distribution Before Adjustment")
hist(df1_final$weight_first, breaks = 40, xlab = "Weight", ylab = "Frequency", main = "Weight Distribution After Adjustment")

hist(df1$los_icu, breaks = 40, xlab = "LOS in ICU (in days)", ylab = "Frequency", main = "LOS in ICU Distribution Before Adjustment")
hist(df1_final$log_los_icu, breaks = 40, xlab = "LOS in ICU (in days)", ylab = "Frequency", main = "LOS in ICU Distribution After Adjustment")

hist(df1_final$los_reg_trs, breaks = 50, xlab = "LOS from Admission to ICU (in days)", ylab = "Frequency", main = "Distribution of LOS from Admission to ICU")
summary(df1_final$los_reg_trs)


#Sample Indexes
set.seed(1234)
indexes = sample(1:nrow(df1_final), size=0.2*nrow(df1_final))

# Split data
test1 = df1_final[indexes,]
dim(test1)  # 252 33
train1 = df1_final[-indexes,]
dim(train1) # 1010  33


# correlation for continuous variables and the response
# continous variables that are highly correlated (abs(cor) > 0.80), 
# the ones that are more correlated with the log_icu_stay are removed
library(corrplot)
cor <- df1_final %>% select(-c(gender,ethnicity,admission_type,first_hosp_stay,discharge_location,language,religion,marital_status))
corrplot(cor(cor, use = "na.or.complete"), method = "circle")
corrplot(cor(cor, use = "na.or.complete"), method = "circle")

library(corrplot)
cor1 <- df1 %>% select(-c(subject_id, hadm_id, icustay_id, diagnosis,los_icu,gender,ethnicity,admission_type,first_hosp_stay,discharge_location,language,religion,marital_status))
cor1a <- df1 %>% select(hematocrit_max,hemoglobin_max,meanbp_mean,diasbp_mean,gcseyes,gcsmotor,endotrachflag,gcsverbal)
corrplot(cor(cor1, use = "na.or.complete"), method = "circle")
corrplot(cor(cor1a, use = "na.or.complete"), method = "number")



# highly positively correlated continous variables: removed
cor(cor$hematocrit_max, cor$hemoglobin_max, use = "na.or.complete") # 0.9659842 
cor(cor$log_los_icu, cor$hemoglobin_max, use = "na.or.complete") # -0.008062029 
cor(cor$hematocrit_max, cor$log_los_icu, use = "na.or.complete") # -0.006349925 so remove hemoglobin_max  


cor(cor$diasbp_mean, cor$meanbp_mean, use = "na.or.complete") # 0.8701645
cor(cor$log_los_icu, cor$diasbp_mean, use = "na.or.complete") # 0.02624066 
cor(cor$meanbp_mean, cor$log_los_icu, use = "na.or.complete") # 0.01616528 so remove diasbp_mean  


cor(cor$gcsmotor, cor$gcseyes, use = "na.or.complete") # 0.83246
cor(cor$log_los_icu, cor$gcsmotor, use = "na.or.complete") # -0.1654611 
cor(cor$gcseyes, cor$log_los_icu, use = "na.or.complete") # -0.2255939 so remove gcseyes  


# highly negatively correlated continous variables: removed
cor(cor$gcsverbal, cor$endotrachflag, use = "na.or.complete") # -0.8808937
cor(cor$log_los_icu, cor$gcsverbal, use = "na.or.complete") # -0.2858138 
cor(cor$endotrachflag, cor$log_los_icu, use = "na.or.complete") # 0.2478353 so remove gcsverbal  


cor1 <- df1_final %>% select(-c(gender,ethnicity,admission_type,first_hosp_stay,discharge_location,language,
                                religion,marital_status,hemoglobin_max,diasbp_mean,gcseyes,gcsverbal))
corrplot(cor(cor1, use = "na.or.complete"), method = "number")



# correlation for categorical variables and the response: all have not much difference between groups for each categorical variable
boxplot(log_los_icu ~ gender, data = df1_final, ylab = "LOS in ICU")
boxplot(log_los_icu ~ ethnicity, data = df1_final, ylab = "LOS in ICU")
boxplot(log_los_icu ~ admission_type, data = df1_final, ylab = "LOS in ICU")
boxplot(log_los_icu ~ first_hosp_stay, data = df1_final, ylab = "LOS in ICU")
boxplot(log_los_icu ~ discharge_location, data = df1_final, ylab = "LOS in ICU")
boxplot(log_los_icu ~ language, data = df1_final, ylab = "LOS in ICU")
boxplot(log_los_icu ~ religion, data = df1_final, ylab = "LOS in ICU")
boxplot(log_los_icu ~ marital_status, data = df1_final, ylab = "LOS in ICU")

# not much effect of weight_first on los_icu
lm1 <- lm(log_los_icu ~ weight_first, data = df1_final)
summary(lm1)

chisq.test(df1$language, df1$religion, correct=T)$p.value
chisq.test(df1$gender, df1$ethnicity, correct=T)$p.value
chisq.test(df1$gender, df1$admission_type, correct=T)$p.value
chisq.test(df1$gender, df1$first_hosp_stay, correct=T)$p.value
chisq.test(df1$gender, df1$discharge_location, correct=T)$p.value
chisq.test(df1$gender, df1$language, correct=T)$p.value
chisq.test(df1$gender, df1$religion, correct=T)$p.value
chisq.test(df1$gender, df1$marital_status, correct=T)$p.value


