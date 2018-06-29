# I. Data Prep 
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
# summary(df1)

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

#############################################################################################################
# split data
response <- "log_los_icu"

## use all other columns (except for the name) as predictors
predictors <- setdiff(names(df1_final), c(response, "name")) 


set.seed(7)
ss <- sample(1:3,size=nrow(df1_final),replace=TRUE,prob=c(0.6,0.2,0.2))
train2 <- df1_final[ss==1,]
dim(train2)  # 751 32
test2 <- df1_final[ss==2,]
dim(test2) # 261 32
valid2 <- df1_final[ss==3,]
dim(valid2) # 250 32

# test_master <- test2

library(h2o)
h2o.init(nthreads = -1) 
train2 <- as.h2o(train2)
test2 <- as.h2o(test2)
valid2 <- as.h2o(valid2)


# II. GBM model: distribution = Auto/Gaussian

# Baseline models

# first model is a default GBM, trained on the 60% training split
## We only provide the required parameters, everything else is default
gbm <- h2o.gbm(x = predictors, y = response, training_frame = train2)

## Show a detailed model summary
gbm

# mse on valid2: 0.6175632
h2o.mse(h2o.performance(gbm, newdata = valid2))


#  second model is another default GBM, but trained on 60% of the data & cross-validated using 4 folds
## h2o.rbind makes a copy here, so it's better to use splitFrame with `ratios = c(0.8)` instead above
gbm <- h2o.gbm(x = predictors, y = response, training_frame = h2o.rbind(train2, valid2), nfolds = 4, seed = 0xDECAF)

## Show a detailed summary of the cross validation metrics
## This gives you an idea of the variance between the folds
gbm@model$cross_validation_metrics_summary

## Get the cross-validated mse by scoring the combined holdout predictions: 0.5609207
## (Instead of taking the average of the metrics across the folds)
h2o.mse(h2o.performance(gbm, xval = TRUE))


# third baseline model
gbm <- h2o.gbm(
  ## standard model parameters
  x = predictors, 
  y = response, 
  training_frame = train2, 
  validation_frame = valid2,
  
  ## more trees is better if the learning rate is small enough 
  ## here, use "more than enough" trees - we have early stopping
  ntrees = 10000,                                                            
  
  ## smaller learning rate is better (this is a good value for most datasets, but see below for annealing)
  learn_rate=0.01,                                                         
  
  ## early stopping once the validation MSE doesn't improve by at least 0.01% for 5 consecutive scoring events
  stopping_rounds = 5, stopping_tolerance = 1e-4, stopping_metric = "MSE", 
  
  ## sample 80% of rows per tree
  sample_rate = 0.8,                                                       
  
  ## sample 80% of columns per split
  col_sample_rate = 0.8,                                                   
  
  ## fix a random number generator seed for reproducibility
  seed = 1234,                                                             
  
  ## score every 10 trees to make early stopping reproducible (it depends on the scoring interval)
  score_tree_interval = 10                                                 
)

## Get the mse on the validation set: 0.5932029 0.6349809
h2o.mse(h2o.performance(gbm, valid = TRUE))

###########################################################
# insepct depth
## Depth 10 is usually plenty of depth for most datasets, but you never know
hyper_params = list( max_depth = seq(1,29,2) )
#hyper_params = list( max_depth = c(4,6,8,12,16,20) ) ##faster for larger datasets

grid <- h2o.grid(
  ## hyper parameters
  hyper_params = hyper_params,
  
  ## full Cartesian hyper-parameter search
  search_criteria = list(strategy = "Cartesian"),
  
  ## which algorithm to run
  algorithm="gbm",
  
  ## identifier for the grid, to later retrieve it
  grid_id="depth_grid",
  
  ## standard model parameters
  x = predictors, 
  y = response, 
  training_frame = train2, 
  validation_frame = valid2,
  
  ## more trees is better if the learning rate is small enough 
  ## here, use "more than enough" trees - we have early stopping
  ntrees = 10000,                                                            
  
  ## smaller learning rate is better
  ## since we have learning_rate_annealing, we can afford to start with a bigger learning rate
  learn_rate = 0.05,                                                         
  
  ## learning rate annealing: learning_rate shrinks by 1% after every tree 
  ## (use 1.00 to disable, but then lower the learning_rate)
  learn_rate_annealing = 0.99,                                               
  
  ## sample 80% of rows per tree
  sample_rate = 0.8,                                                       
  
  ## sample 80% of columns per split
  col_sample_rate = 0.8, 
  
  ## fix a random number generator seed for reproducibility
  seed = 1234,                                                             
  
  ## early stopping once the validation MSE doesn't improve by at least 0.01% for 5 consecutive scoring events
  stopping_rounds = 5,
  stopping_tolerance = 1e-4,
  stopping_metric = "MSE", 
  
  ## score every 10 trees to make early stopping reproducible (it depends on the scoring interval)
  score_tree_interval = 10                                                
)

## by default, display the grid search results sorted by increasing logloss (since this is a classification task)
grid                                                                       

## sort the grid models by decreasing MSE
sortedGrid <- h2o.getGrid("depth_grid", sort_by="mse", decreasing = F)    
sortedGrid # 0.5689053042878321 0.6318370753444695

## find the range of max_depth for the top 5 models
topDepths = sortedGrid@summary_table$max_depth[1:5]                       
minDepth = min(as.numeric(topDepths))
maxDepth = max(as.numeric(topDepths))
minDepth # 5
maxDepth # 23

########################################################
# after range of depth is determined, we do final grid serach on other parameters
hyper_params = list( 
  ## restrict the search to the range of max_depth established above
  max_depth = seq(minDepth,maxDepth,1),                                      
  
  ## search a large space of row sampling rates per tree
  sample_rate = seq(0.2,1,0.01),                                             
  
  ## search a large space of column sampling rates per split
  col_sample_rate = seq(0.2,1,0.01),                                         
  
  ## search a large space of column sampling rates per tree
  col_sample_rate_per_tree = seq(0.2,1,0.01),                                
  
  ## search a large space of how column sampling per split should change as a function of the depth of the split
  col_sample_rate_change_per_level = seq(0.9,1.1,0.01),                      
  
  ## search a large space of the number of min rows in a terminal node
  min_rows = 2^seq(0,log2(nrow(train2))-1,1),                                 
  
  ## search a large space of the number of bins for split-finding for continuous and integer columns
  nbins = 2^seq(4,10,1),                                                     
  
  ## search a large space of the number of bins for split-finding for categorical columns
  nbins_cats = 2^seq(4,12,1),                                                
  
  ## search a few minimum required relative error improvement thresholds for a split to happen
  min_split_improvement = c(0,1e-8,1e-6,1e-4),                               
  
  ## try all histogram types (QuantilesGlobal and RoundRobin are good for numeric columns with outliers)
  histogram_type = c("UniformAdaptive","QuantilesGlobal","RoundRobin")       
)

search_criteria = list(
  ## Random grid search
  strategy = "RandomDiscrete",      
  
  ## limit the runtime to 60 minutes
  max_runtime_secs = 3600,         
  
  ## build no more than 100 models
  max_models = 100,                  
  
  ## random number generator seed to make sampling of parameter combinations reproducible
  seed = 1234,                        
  
  ## early stopping once the leaderboard of the top 5 models is converged to 0.1% relative difference
  stopping_rounds = 5,                
  stopping_metric = "MSE",
  stopping_tolerance = 1e-3
)

grid <- h2o.grid(
  ## hyper parameters
  hyper_params = hyper_params,
  
  ## hyper-parameter search configuration (see above)
  search_criteria = search_criteria,
  
  ## which algorithm to run
  algorithm = "gbm",
  
  ## identifier for the grid, to later retrieve it
  grid_id = "final_grid", 
  
  ## standard model parameters
  x = predictors, 
  y = response, 
  training_frame = train2, 
  validation_frame = valid2,
  
  ## more trees is better if the learning rate is small enough
  ## use "more than enough" trees - we have early stopping
  ntrees = 10000,                                                            
  
  ## smaller learning rate is better
  ## since we have learning_rate_annealing, we can afford to start with a bigger learning rate
  learn_rate = 0.05,                                                         
  
  ## learning rate annealing: learning_rate shrinks by 1% after every tree 
  ## (use 1.00 to disable, but then lower the learning_rate)
  learn_rate_annealing = 0.99,                                               
  
  ## early stopping based on timeout (no model should take more than 1 hour - modify as needed)
  max_runtime_secs = 3600,                                                 
  
  ## early stopping once the validation MSE doesn't improve by at least 0.01% for 5 consecutive scoring events
  stopping_rounds = 5, stopping_tolerance = 1e-4, stopping_metric = "MSE", 
  
  ## score every 10 trees to make early stopping reproducible (it depends on the scoring interval)
  score_tree_interval = 10,                                                
  
  ## base random number generator seed for each model (automatically gets incremented internally for each model)
  seed = 1234                                                             
)

## Sort the grid models by MSE
sortedGrid <- h2o.getGrid("final_grid", sort_by = "mse", decreasing = F)    
sortedGrid # 0.7655612928418488

# inspect the best 5 models from the grid search explicitly, and query their validation MSE
for (i in 1:5) {
  gbm <- h2o.getModel(sortedGrid@model_ids[[i]])
  print(h2o.mse(h2o.performance(gbm, valid = TRUE)))
}

gbm <- h2o.getModel(sortedGrid@model_ids[[1]])
print(h2o.mse(h2o.performance(gbm, newdata = test2))) # 0.6079597
gbm@parameters

vi <- h2o.varimp(gbm)
vi[1:10,]

############ensemble method for top 10 CV models
prediction = NULL
k=10
for (i in 1:k) {
  gbm <- h2o.getModel(sortedGrid@model_ids[[i]])
  if (is.null(prediction)) prediction = h2o.predict(gbm, newdata=test2) %>%  as.data.frame()

  else prediction = prediction + h2o.predict(gbm, newdata=test2) %>%  as.data.frame()
}
prediction <- prediction/k
head(prediction)

mean((as.data.frame(test2[[response]]) - prediction$predict)^2) #  0.6117964 BEST RESULTS using gbm!!!!!!!!!!!!
# when using this gbm model and train on the entire data, the mean mse of 5 CV models is 0.61112386; Not good

# write.csv(prediction, file = "C:/Users/hyq92/Documents/R/844 Project/MIMIC III Data/Predictions/Pred_GBM_N.csv")

############ensemble method for top 10 CV models: use for top
tune1 <- rbind(train2, valid2)
tune1 <- as.h2o(tune1)
predictionN = NULL
k=10
for (i in 1:k) {
  gbm <- h2o.getModel(sortedGrid@model_ids[[i]])
  if (is.null(predictionN)) predictionN = h2o.predict(gbm, newdata=tune1) %>%  as.data.frame()
  
  else predictionN = predictionN + h2o.predict(gbm, newdata=tune1) %>%  as.data.frame()
}
predictionN <- predictionN/k
# head(predictionN)


# write.csv(predictionN, file = "C:/Users/hyq92/Documents/R/844 Project/MIMIC III Data/Predictions/Pred_GBM_N2.csv")



######################################################################################################
######################################################################################################
######################################################################################################
######### train gbm model with Huber distribution with correlated variables removed ##############
######################################################################################################
######################################################################################################

# remove correlated variables
# model 1: gbm with mean in vitals results & max in lab results
df1 <-  all_surg %>% 
  select(subject_id, hadm_id, icustay_id,
         los_icu, gender,age,ethnicity, admission_type, first_hosp_stay, discharge_location,
         language,religion, marital_status, diagnosis,los_reg_trs,
         heartrate_mean, sysbp_mean, diasbp_mean, meanbp_mean, resprate_mean, tempc_mean,
         spo2_mean, glucose_mean,
         creatinine_max, hematocrit_max, hemoglobin_max, potassium_max, sodium_max, bun_max,
         mingcs, gcsmotor, gcsverbal, gcseyes, endotrachflag,  height_first, weight_first)
# summary(df1)

df1_final2 <- df1 %>% 
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
predictors <- setdiff(names(df1_final2), c(response, "name")) 


set.seed(7)
ss <- sample(1:3,size=nrow(df1_final2),replace=TRUE,prob=c(0.6,0.2,0.2))
train3 <- df1_final2[ss==1,]
dim(train3)  # 751 28
test3 <- df1_final2[ss==2,]
dim(test3) # 261 28
valid3 <- df1_final2[ss==3,]
dim(valid3) # 250 28

tune1 <- rbind(train3,valid3)
tune1 <- as.h2o(tune1)

library(h2o)
h2o.init(nthreads = -1) 
train3 <- as.h2o(train3)
test3 <- as.h2o(test3)
valid3 <- as.h2o(valid3)



# third baseline model
gbm <- h2o.gbm(
  ## standard model parameters
  x = predictors, 
  y = response, 
  training_frame = train3, 
  validation_frame = valid3,
  distribution = "huber",
  
  ## more trees is better if the learning rate is small enough 
  ## here, use "more than enough" trees - we have early stopping
  ntrees = 10000,                                                            
  
  ## smaller learning rate is better (this is a good value for most datasets, but see below for annealing)
  learn_rate=0.01,                                                         
  
  ## early stopping once the validation MSE doesn't improve by at least 0.01% for 5 consecutive scoring events
  stopping_rounds = 5, stopping_tolerance = 1e-4, stopping_metric = "MSE", 
  
  ## sample 80% of rows per tree
  sample_rate = 0.8,                                                       
  
  ## sample 80% of columns per split
  col_sample_rate = 0.8,                                                   
  
  ## fix a random number generator seed for reproducibility
  seed = 1234,                                                             
  
  ## score every 10 trees to make early stopping reproducible (it depends on the scoring interval)
  score_tree_interval = 10                                                 
)

## Get the mse on the validation set: 0.5789567
h2o.mse(h2o.performance(gbm, valid = TRUE))

###########################################################
# insepct depth
## Depth 10 is usually plenty of depth for most datasets, but you never know
hyper_params = list( max_depth = seq(1,29,2) )

grid <- h2o.grid(
  ## hyper parameters
  hyper_params = hyper_params,
  
  ## full Cartesian hyper-parameter search
  search_criteria = list(strategy = "Cartesian"),
  
  ## which algorithm to run
  algorithm="gbm",
  
  ## identifier for the grid, to later retrieve it
  grid_id="depth_grid",
  
  ## standard model parameters
  x = predictors, 
  y = response, 
  training_frame = train3, 
  validation_frame = valid3,
  distribution = "huber",
  
  ## more trees is better if the learning rate is small enough 
  ## here, use "more than enough" trees - we have early stopping
  ntrees = 10000,                                                            
  
  ## smaller learning rate is better
  ## since we have learning_rate_annealing, we can afford to start with a bigger learning rate
  learn_rate = 0.05,                                                         
  
  ## learning rate annealing: learning_rate shrinks by 1% after every tree 
  ## (use 1.00 to disable, but then lower the learning_rate)
  learn_rate_annealing = 0.99,                                               
  
  ## sample 80% of rows per tree
  sample_rate = 0.8,                                                       
  
  ## sample 80% of columns per split
  col_sample_rate = 0.8, 
  
  ## fix a random number generator seed for reproducibility
  seed = 1234,                                                             
  
  ## early stopping once the validation MSE doesn't improve by at least 0.01% for 5 consecutive scoring events
  stopping_rounds = 5,
  stopping_tolerance = 1e-4,
  stopping_metric = "MSE", 
  
  ## score every 10 trees to make early stopping reproducible (it depends on the scoring interval)
  score_tree_interval = 10                                                
)

## by default, display the grid search results sorted by increasing logloss (since this is a classification task)
grid                                                                       

## sort the grid models by decreasing MSE
sortedGrid <- h2o.getGrid("depth_grid", sort_by="mse", decreasing = F)    
sortedGrid # 0.5689053042878321

## find the range of max_depth for the top 5 models
topDepths = sortedGrid@summary_table$max_depth[1:5]                       
minDepth = min(as.numeric(topDepths))
maxDepth = max(as.numeric(topDepths))
minDepth # 5
maxDepth # 23

########################################################
# after range of depth is determined, we do final grid serach on other parameters
hyper_params = list( 
  ## restrict the search to the range of max_depth established above
  max_depth = seq(minDepth,maxDepth,1),                                      
  
  ## search a large space of row sampling rates per tree
  sample_rate = seq(0.2,1,0.01),                                             
  
  ## search a large space of column sampling rates per split
  col_sample_rate = seq(0.2,1,0.01),                                         
  
  ## search a large space of column sampling rates per tree
  col_sample_rate_per_tree = seq(0.2,1,0.01),                                
  
  ## search a large space of how column sampling per split should change as a function of the depth of the split
  col_sample_rate_change_per_level = seq(0.9,1.1,0.01),                      
  
  ## search a large space of the number of min rows in a terminal node
  min_rows = 2^seq(0,log2(nrow(train3))-1,1),                                 
  
  ## search a large space of the number of bins for split-finding for continuous and integer columns
  nbins = 2^seq(4,10,1),                                                     
  
  ## search a large space of the number of bins for split-finding for categorical columns
  nbins_cats = 2^seq(4,12,1),                                                
  
  ## search a few minimum required relative error improvement thresholds for a split to happen
  min_split_improvement = c(0,1e-8,1e-6,1e-4),                               
  
  ## try all histogram types (QuantilesGlobal and RoundRobin are good for numeric columns with outliers)
  histogram_type = c("UniformAdaptive","QuantilesGlobal","RoundRobin")       
)

search_criteria = list(
  ## Random grid search
  strategy = "RandomDiscrete",      
  
  ## limit the runtime to 60 minutes
  max_runtime_secs = 3600,         
  
  ## build no more than 100 models
  max_models = 100,                  
  
  ## random number generator seed to make sampling of parameter combinations reproducible
  seed = 1234,                        
  
  ## early stopping once the leaderboard of the top 5 models is converged to 0.1% relative difference
  stopping_rounds = 5,                
  stopping_metric = "MSE",
  stopping_tolerance = 1e-3
)

grid <- h2o.grid(
  ## hyper parameters
  hyper_params = hyper_params,
  
  ## hyper-parameter search configuration (see above)
  search_criteria = search_criteria,
  
  ## which algorithm to run
  algorithm = "gbm",
  
  ## identifier for the grid, to later retrieve it
  grid_id = "final_grid3", 
  
  ## standard model parameters
  x = predictors, 
  y = response, 
  training_frame = train3, 
  validation_frame = valid3,
  distribution = "huber",
  
  ## more trees is better if the learning rate is small enough
  ## use "more than enough" trees - we have early stopping
  ntrees = 10000,                                                            
  
  ## smaller learning rate is better
  ## since we have learning_rate_annealing, we can afford to start with a bigger learning rate
  learn_rate = 0.05,                                                         
  
  ## learning rate annealing: learning_rate shrinks by 1% after every tree 
  ## (use 1.00 to disable, but then lower the learning_rate)
  learn_rate_annealing = 0.99,                                               
  
  ## early stopping based on timeout (no model should take more than 1 hour - modify as needed)
  max_runtime_secs = 3600,                                                 
  
  ## early stopping once the validation MSE doesn't improve by at least 0.01% for 5 consecutive scoring events
  stopping_rounds = 5, stopping_tolerance = 1e-4, stopping_metric = "MSE", 
  
  ## score every 10 trees to make early stopping reproducible (it depends on the scoring interval)
  score_tree_interval = 10,                                                
  
  ## base random number generator seed for each model (automatically gets incremented internally for each model)
  seed = 1234                                                             
)

## Sort the grid models by MSE
sortedGrid <- h2o.getGrid("final_grid3", sort_by = "mse", decreasing = F)    
sortedGrid # 0.7802755029957759


# inspect the best 5 models from the grid search explicitly, and query their validation MSE
for (i in 1:5) {
  gbm <- h2o.getModel(sortedGrid@model_ids[[i]])
  print(h2o.mse(h2o.performance(gbm, valid = TRUE)))
}

gbm <- h2o.getModel(sortedGrid@model_ids[[1]])
print(h2o.mse(h2o.performance(gbm, newdata = test3))) # 0.6002058 using huber distribution
gbm@parameters

vi1 <- h2o.varimp(gbm)
vi1[1:10,]

############ensemble method for top 10 CV models
prediction1 = NULL
k=10
for (i in 1:k) {
  gbm <- h2o.getModel(sortedGrid@model_ids[[i]])
  if (is.null(prediction1)) prediction1 = h2o.predict(gbm, newdata=test3) %>%  as.data.frame()
  
  else prediction1 = prediction1 + h2o.predict(gbm, newdata=test3) %>%  as.data.frame()
}
prediction1 <- prediction1/k
head(prediction1)

mean((as.data.frame(test3[[response]]) - prediction1$predict)^2) #0.596282 using huber distribution

# write.csv(prediction1, file = "C:/Users/hyq92/Documents/R/844 Project/MIMIC III Data/Predictions/Pred_GBM_Huber.csv")

############ensemble method for top 10 CV models: use for top model ensemble
predictionN1 = NULL
k=10
for (i in 1:k) {
  gbm <- h2o.getModel(sortedGrid@model_ids[[i]])
  if (is.null(predictionN1)) predictionN1 = h2o.predict(gbm, newdata=tune1) %>%  as.data.frame()
  
  else predictionN1 = predictionN1 + h2o.predict(gbm, newdata=tune1) %>%  as.data.frame()
}
predictionN1 <- predictionN1/k
head(predictionN1)


# write.csv(predictionN1, file = "C:/Users/hyq92/Documents/R/844 Project/MIMIC III Data/Predictions/Pred_GBM_Huber2.csv")

