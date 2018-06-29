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


# Check if there's any NA in df1_final3
sum(is.na(df1_final3))

# split data
response <- "log_los_icu"

## use all other columns (except for the name) as predictors
predictors <- setdiff(names(df1_final3), c(response, "name")) 

df1_final3_MM <- model.matrix( ~ . -1, df1_final3)
df1_final3_MM <- as.data.frame(df1_final3_MM)
df1_final3_MM<- df1_final3_MM[,-(which(colSums(df1_final3_MM) == 0))] # remove columns that have all zeros
df1_final3_MM <- model.matrix(~ . -1, df1_final3_MM)

df1_final3_MM_X <- df1_final3_MM[, colnames(df1_final3_MM) != "log_los_icu"]
df1_final3_MM_Y <- df1_final3_MM[, colnames(df1_final3_MM) == "log_los_icu"]


set.seed(7)
ss <- sample(1:2,size=nrow(df1_final3_MM),replace=TRUE,prob=c(0.8,0.2))
train4 <- df1_final3_MM[ss==1,]
dim(train4)  # 1001   93
test4 <- df1_final3_MM[ss==2,]
dim(test4) # 261  93

x_train4 <- train4[, colnames(train4) != "log_los_icu"]
y_train4 <- train4[, "log_los_icu"]

x_test4 <- test4[, colnames(test4) != "log_los_icu"]
y_test4 <- test4[, "log_los_icu"]


######## glmnetmodel 1 ############3
library(glmnet)

#############################################################################3
glmnet1 = glmnet(x_train4, y_train4, alpha = 0.2,  nlambda =100)
print(glmnet1)
plot(glmnet1, xvar = "lambda", label = TRUE)
plot(glmnet1, xvar = "dev", label = TRUE)

set.seed(1234)
cv_glmnet1 = cv.glmnet(x_train4, y_train4, type.measure = "mse", nfolds = 5)
cv_glmnet1$lambda.min # 0.02191665
coef(cv_glmnet1, s = "lambda.min")
plot(cv_glmnet1)

mean((y_test4 - predict(cv_glmnet1, newx = x_test4, s = "lambda.min"))^2) 
# test set prediction error: 0.643892 using alpha = 0.02191665
#############################################################################


# Fit models 
# (For plots on left):
fit.lasso <- glmnet(x_train4, y_train4, family="gaussian", alpha=1)
mean((y_test4 - predict(fit.lasso, newx = x_test4, s = 0.02))^2) # 0.6450271

fit.ridge <- glmnet(x_train4, y_train4, family="gaussian", alpha=0)
mean((y_test4 - predict(fit.ridge, newx = x_test4, s = 0.02))^2) # 0.6950486


fit.elnet <- glmnet(x_train4, y_train4, family="gaussian", alpha=.5)
mean((y_test4 - predict(fit.elnet, newx = x_test4, s = 0.02))^2) # 0.6616091


# 10-fold Cross validation for each alpha = 0, 0.1, ... , 0.9, 1.0
# (For plots on Right)
set.seed(1234)
for (i in 0:10) {
  assign(paste("fit", i, sep=""), cv.glmnet(x_train4, y_train4, type.measure="mse", nfolds = 5,
                                            alpha=i/10,family="gaussian"))
}


# Plot solution paths:
# par(mfrow=c(1,1))
par(mfrow=c(1,2))
# For plotting options, type '?plot.glmnet' in R console
plot(fit.lasso, xvar="lambda")
plot(fit10, main="LASSO")

plot(fit.ridge, xvar="lambda")
plot(fit0, main="Ridge")

plot(fit.elnet, xvar="lambda")
plot(fit5, main="Elastic Net")


# prediction MSE using lambda.1se
yhat0 <- predict(fit0, s=fit0$lambda.1se, newx=x_test4)
yhat1 <- predict(fit1, s=fit1$lambda.1se, newx=x_test4)
yhat2 <- predict(fit2, s=fit2$lambda.1se, newx=x_test4)
yhat3 <- predict(fit3, s=fit3$lambda.1se, newx=x_test4)
yhat4 <- predict(fit4, s=fit4$lambda.1se, newx=x_test4)
yhat5 <- predict(fit5, s=fit5$lambda.1se, newx=x_test4)
yhat6 <- predict(fit6, s=fit6$lambda.1se, newx=x_test4)
yhat7 <- predict(fit7, s=fit7$lambda.1se, newx=x_test4)
yhat8 <- predict(fit8, s=fit8$lambda.1se, newx=x_test4)
yhat9 <- predict(fit9, s=fit9$lambda.1se, newx=x_test4)
yhat10 <- predict(fit10, s=fit10$lambda.1se, newx=x_test4)

mse0 <- mean((y_test4 - yhat0)^2)
mse1 <- mean((y_test4 - yhat1)^2)
mse2 <- mean((y_test4 - yhat2)^2)
mse3 <- mean((y_test4 - yhat3)^2)
mse4 <- mean((y_test4 - yhat4)^2)
mse5 <- mean((y_test4 - yhat5)^2)
mse6 <- mean((y_test4 - yhat6)^2)
mse7 <- mean((y_test4 - yhat7)^2)
mse8 <- mean((y_test4 - yhat8)^2)
mse9 <- mean((y_test4 - yhat9)^2)
mse10 <- mean((y_test4 - yhat10)^2)

prediction_glmnet <- data.frame(alpha = seq(0,1,0.1), 
                                mse = c(mse0,mse1,mse2,mse3,mse4,mse5,mse6,mse7,mse8,mse9,mse10))

# mse is smallest (0.6410339) when alpha = 0.9 using predict(fit, s = fit$lambda.1se)

par(mfrow=c(1,1))
plot(fit9, xvar = "dev", label = TRUE, main = "Alpha = 0.9 using 5-fold CV")

# fit2_final <- glmnet(df1_final3_MM_X, df1_final3_MM_Y, alpha = 0.2)
# summary(fit2_final)
# par(mfrow=c(1,2))
# plot(fit2_final, xvar="lambda")
# plot(fit2, main = "XX")

# prediction MSE using lambda.min
yhat0 <- predict(fit0, s=fit0$lambda.min, newx=x_test4)
yhat1 <- predict(fit1, s=fit1$lambda.min, newx=x_test4)
yhat2 <- predict(fit2, s=fit2$lambda.min, newx=x_test4)
yhat3 <- predict(fit3, s=fit3$lambda.min, newx=x_test4)
yhat4 <- predict(fit4, s=fit4$lambda.min, newx=x_test4)
yhat5 <- predict(fit5, s=fit5$lambda.min, newx=x_test4)
yhat6 <- predict(fit6, s=fit6$lambda.min, newx=x_test4)
yhat7 <- predict(fit7, s=fit7$lambda.min, newx=x_test4)
yhat8 <- predict(fit8, s=fit8$lambda.min, newx=x_test4)
yhat9 <- predict(fit9, s=fit9$lambda.min, newx=x_test4)
yhat10 <- predict(fit10, s=fit10$lambda.min, newx=x_test4)

mse0 <- mean((y_test4 - yhat0)^2)
mse1 <- mean((y_test4 - yhat1)^2)
mse2 <- mean((y_test4 - yhat2)^2)
mse3 <- mean((y_test4 - yhat3)^2)
mse4 <- mean((y_test4 - yhat4)^2)
mse5 <- mean((y_test4 - yhat5)^2)
mse6 <- mean((y_test4 - yhat6)^2)
mse7 <- mean((y_test4 - yhat7)^2)
mse8 <- mean((y_test4 - yhat8)^2)
mse9 <- mean((y_test4 - yhat9)^2)
mse10 <- mean((y_test4 - yhat10)^2)

prediction2_glmnet <- data.frame(alpha = seq(0,1,0.1), 
                                mse = c(mse0,mse1,mse2,mse3,mse4,mse5,mse6,mse7,mse8,mse9,mse10))

# mse is smallest (0.6402778) when alpha = 1 using predict(fit, s = fit$lambda.min) BEST GLMNET MODEL!!!!!!
# mean CV error for lambda.min/final model = 0.6477814

par(mfrow=c(1,1))
plot(fit10, xvar = "dev", label = TRUE, main = "Alpha = 1 using 5-fold CV")

prediction_glmnet <- predict(fit10, s=fit10$lambda.min, newx=x_test4)
prediction_glmnet1 <- predict(fit10, s=fit10$lambda.min, newx=x_train4)

# write.csv(prediction_glmnet, file = "C:/Users/hyq92/Documents/R/844 Project/MIMIC III Data/Predictions/Pred_glmnet.csv")
# write.csv(prediction_glmnet1, file = "C:/Users/hyq92/Documents/R/844 Project/MIMIC III Data/Predictions/Pred_glmnet2.csv")

# This is a question about parsimony. The lambda.min option refers to value of ?? at the lowest CV error. 
# The error at this value of ?? is the average of the errors over the k folds and hence this estimate of the 
# error is uncertain. 

# The lambda.1se represents the value of ?? in the search that was simpler than the best 
# model (lambda.min), but which has error within 1 standard error of the best model. 
# In other words, using the value of lambda.1se as the selected value for ?? results in a model that is slightly 
# simpler than the best model but which cannot be distinguished from the best model in terms of error given 
# the uncertainty in the k-fold CV estimate of the error of the best model.
# 
# The best model that may be too complex of slightly overfitted: lambda.min
# The simplest model that has comparable error to the best model given the uncertainty: lambda.1se