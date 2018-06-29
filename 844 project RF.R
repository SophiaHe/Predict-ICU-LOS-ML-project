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


# Cardiac Surgery 
# Cardiac_Surg <- all_surg %>% filter(grepl("CSURG",all_services)) 


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

# split data
response <- "log_los_icu"

## use all other columns (except for the name) as predictors
predictors <- setdiff(names(df1_final3), c(response, "name")) 





###########################################################################################################
# 
set.seed(7)
ss <- sample(1:3,size=nrow(df1_final3),replace=TRUE,prob=c(0.6,0.2,0.2))
train4 <- df1_final3[ss==1,]
dim(train4)  # 751 32
test4 <- df1_final3[ss==2,]
dim(test4) # 261 32
valid4 <- df1_final3[ss==3,]
dim(valid4) # 250 32


library(h2o)
h2o.init(nthreads = -1) 
train4 <- as.h2o(train4)
test4 <- as.h2o(test4)
valid4 <- as.h2o(valid4)

DRF1 <- h2o.randomForest(x = predictors, y = response,
                        training_frame = train4,
                        validation_frame = valid4, 
                        nfolds = 5,
                        max_depth = 20,
                        nbins_cats = 1115, ## allow it to fit store ID
                        model_id = "myDRF",
                        ntrees = 100)
summary(DRF1)

############################### Tune Parameter using grid search #########################
h2o.init(nthreads = -1) 
# Construct a large Cartesian hyper-parameter space
ntrees_opts = 100
mtries_opts = seq(1,15,1)
sample_rate_opts = seq(0.3,1,0.05)


hyper_params = list( ntrees = ntrees_opts,
                     mtries = mtries_opts)


# Search a random subset of these hyper-parmameters. Max runtime 
# and max models are enforced, and the search will stop after we 
# don't improve much over the best 5 random models.
search_criteria = list(strategy = "RandomDiscrete", 
                       max_runtime_secs = 600, 
                       max_models = 10, 
                       stopping_metric = "MSE", 
                       stopping_tolerance = 0.001, 
                       stopping_rounds = 5, 
                       seed = 123456)

drf_grid <- h2o.grid("randomForest", 
                     grid_id = "mygrid",
                     x = predictors, 
                     y = response, 
                     
                     training_frame = train4,
                     validation_frame = valid4,
                     nfolds = 5,
                     
                     distribution="gaussian",
                     
                     # stop as soon as mse doesn't improve by 
                     # more than 0.1% on the validation set, 
                     # for 2 consecutive scoring events:
                     stopping_rounds = 2,
                     stopping_tolerance = 1e-3,
                     stopping_metric = "MSE",
                     
                     # how often to score (affects early stopping):
                     score_tree_interval = 100, 
                     
                     ## seed to control the sampling of the 
                     ## Cartesian hyper-parameter space:
                     seed = 123456,
                     hyper_params = hyper_params,
                     search_criteria = search_criteria)

drf_sorted_grid <- h2o.getGrid(grid_id = "mygrid", sort_by = "mse")
print(drf_sorted_grid) # 0.5902198849245378

best_model <- h2o.getModel(drf_sorted_grid@model_ids[[1]])
summary(best_model) # mean CV mse = 0.59050554
best_model@model$model_summary

prediction3 = h2o.predict(best_model, newdata=test4) %>%  as.data.frame()
mean((as.data.frame(test4[[response]]) - prediction3$predict)^2) # test mse 0.6107257 

tune1 <- rbind(train4, valid4)
tune1 <- as.h2o(tune1)
prediction4 = h2o.predict(best_model, newdata=tune1) %>%  as.data.frame()
mean((as.data.frame(tune1[[response]]) - prediction4$predict)^2) # train mse 0.2243755 


# write.csv(prediction3, file = "C:/Users/hyq92/Documents/R/844 Project/MIMIC III Data/Predictions/Pred_RF.csv")
# write.csv(prediction4, file = "C:/Users/hyq92/Documents/R/844 Project/MIMIC III Data/Predictions/Pred_RF2.csv")
