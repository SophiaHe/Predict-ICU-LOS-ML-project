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
# los_ed = edouttime - edregtime; 
# lengh of time from register to transfer to icu: intime - admittime

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
         spo2_mean, glucose_mean,
         creatinine_max, hematocrit_max, hemoglobin_max, potassium_max, sodium_max, bun_max,
         mingcs, gcsmotor, gcsverbal, gcseyes, endotrachflag,  height_first, weight_first)

df1_final3 <- df1 %>% 
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
# df1_final3[is.na(df1_final3)] <- 0

# split data
response <- "log_los_icu"

## use all other columns (except for the name) as predictors
predictors <- setdiff(names(df1_final3), c(response, "name")) 



set.seed(7)
ss <- sample(1:2,size=nrow(df1_final3),replace=TRUE,prob=c(0.8,0.2))
train4 <- df1_final3[ss==1,]
dim(train4)  # 1001   28
test4 <- df1_final3[ss==2,]
dim(test4) # 261  28


############# SVM model #######################
library(e1071)

#Regression with SVM: basic model
modelsvm = svm(log_los_icu ~ ., data = train4, type = "nu-regression", kernel = "radial")

modelsvm$coefs
#Predict using SVM regression
predYsvm = predict(modelsvm, test4)

c <- as.data.frame(test4)
mean((c$log_los_icu - predYsvm)^2) # 0.739877

# tune parameters in SVM
#Tune the SVM model
OptModelsvm=tune(svm, log_los_icu ~ ., data = train4,ranges=list(epsilon=seq(0,1,0.1), cost = 2^(2:9)), 
                 tunecontrol = tune.control(sampling = "cross",cross = 5))

#Print optimum value of parameters
print(OptModelsvm)

#Plot the perfrormance of SVM Regression model
plot(OptModelsvm)

## Select the best model out of 1100 trained models and compute MSE

#Find out the best model
BstModel=OptModelsvm$best.model

# Estimated response for training set
PredYBst1=predict(BstModel,train4)
# write.csv(PredYBst1, file = "C:/Users/hyq92/Documents/R/844 Project/MIMIC III Data/Predictions/Pred_svm2.csv")


#Predict Y of testing set using best model
PredYBst=predict(BstModel,test4)

#Calculate MSE of the best model 
c <- as.data.frame(test4)
mean((c$log_los_icu - PredYBst)^2) # 0.6274648

# write.csv(PredYBst, file = "C:/Users/hyq92/Documents/R/844 Project/MIMIC III Data/Predictions/Pred_svm.csv")
