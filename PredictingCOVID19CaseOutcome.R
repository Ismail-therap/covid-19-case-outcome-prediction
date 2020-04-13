library(readr)
dat1 <- read_csv("C:/Users/Hafsa/Desktop/Corona-Project-master/Kaggle data/novel-corona-virus-2019-dataset2/COVID19_line_list_data.csv")
View(dat1)

#dat2 <- read_csv("C:/Users/Hafsa/Desktop/Corona-Project-master/Kaggle data/novel-corona-virus-2019-dataset/COVID19_open_line_list.csv")
#dim(dat2)


##########################
#### Data management #####
##########################

no_outcome_yet <- "0"
'%!in%' <- function(x,y)!('%in%'(x,y))
death_dat <- dat1[dat1$death %!in% no_outcome_yet,]
recover_dat <- dat1[dat1$recovered %!in% no_outcome_yet,]

dim(death_dat)

dim(recover_dat)


dat <- rbind(recover_dat,death_dat)

dat$case_outcome <- ifelse(dat$death == 0, dat$recovered,
                           ifelse(dat$recovered == 0, dat$death,NA))

dat$dead_or_alive <- ifelse(dat$death == 0,"Alive",
                           ifelse(dat$recovered == 0,"Dead",NA))

#View(dat)

#write.csv(dat,"C:/Users/Hafsa/Desktop/Corona-Project-master/Kaggle data/novel-corona-virus-2019-dataset2/dead_or_alive.csv")

#dat <- read_csv("C:/Users/Hafsa/Desktop/Corona-Project-master/Kaggle data/novel-corona-virus-2019-dataset2/dead_or_alive.csv")


names(dat)
str(dat)

dat$symptom_onset <- as.Date(dat$symptom_onset,tryFormats = c("%m/%d/%y", "%m/%d/%Y","%m-%d-%y", "%m-%d-%Y"))
dat$hosp_visit_date <- as.Date(dat$hosp_visit_date,tryFormats = c("%m/%d/%y", "%m/%d/%Y","%m-%d-%y", "%m-%d-%Y"))
dat$exposure_start <- as.Date(dat$exposure_start,tryFormats = c("%m/%d/%y", "%m/%d/%Y","%m-%d-%y", "%m-%d-%Y"))
dat$exposure_end <- as.Date(dat$exposure_end,c("%m/%d/%y", "%m/%d/%Y","%m-%d-%y", "%m-%d-%Y"))


dat$case_outcome <- ifelse(dat$case_outcome == "1",NA,dat$case_outcome)
dat$case_outcome_date <- as.Date(dat$case_outcome,c("%m/%d/%y", "%m/%d/%Y","%m-%d-%y", "%m-%d-%Y")) 


dat$symptom_to_hospital <- as.numeric(dat$hosp_visit_date-dat$symptom_onset)
dat$hosp_visit_to_outcome <- as.numeric(dat$case_outcome_date-dat$hosp_visit_date)

dat$symptom_to_hospital <- ifelse(dat$symptom_to_hospital < 0,NA,dat$symptom_to_hospital)
dat$hosp_visit_to_outcome <- ifelse(dat$hosp_visit_to_outcome < 0,NA,dat$hosp_visit_to_outcome)

#write.csv(dat,"C:/Users/Hafsa/Desktop/Corona-Project-master/Kaggle data/novel-corona-virus-2019-dataset2/un_clean_dead_or_alive.csv")

#un_clean_dat <- read_csv("C:/Users/Hafsa/Desktop/Corona-Project-master/Kaggle data/novel-corona-virus-2019-dataset2/un_clean_dead_or_alive.csv")

#View(un_clean_dat)

#un_clean_dat$symptom_to_hospital <- as.Date(un_clean_dat$hosp_visit_date,format = "%m-%d-%y")-as.Date(un_clean_dat$symptom_onset,format = "%m-%d-%y")

#############################
#### EDA ####################
#############################

dat$dead_or_alive <- as.factor(dat$dead_or_alive)
summary(dat)


##### Data of how many location:

country <- data.frame(table(dat$dead_or_alive,dat$country))
colnames(country) <- c("Case Outcome","Country","Number of Patient")
country

prop.table(table(dat$dead_or_alive,dat$country),2)


# Total patient
sum(country$`Number of Patient`)


# Patient gender:
gen <-  table(dat$dead_or_alive,dat$gender)
prop.table(gen,2)

# Patient age:
summary(dat$age)
aggregate(dat$age,by=list(dat$dead_or_alive),FUN = median,na.rm=TRUE)
aggregate(dat$age,by=list(dat$dead_or_alive),FUN = mean,na.rm=TRUE)

# Symptoms:

symp <- table(dat$dead_or_alive,dat$symptom)
data.frame(symp)
#data.frame(prop.table(symp,2))



##############################
#### Predictive modelling ####
##############################
library(caret)
library(e1071)



dat$dependent <- ifelse(dat$dead_or_alive == "Alive",1,0)
dat$dependent <- as.factor(dat$dependent)
dat$dead_or_alive <- as.factor(dat$dead_or_alive)

dat2 <- dat[,c("dependent","dead_or_alive","gender","age","symptom_to_hospital")]
dat2 <- na.omit(dat2)

dim(dat2)

write.csv(dat2,"C:/Users/Hafsa/Desktop/Corona-Project-master/Kaggle data/novel-corona-virus-2019-dataset2/To_Azure_ML.csv")

## 80% of the sample size
smp_size <- floor(0.8 * nrow(dat2))

## set the seed to make your partition reproducible
set.seed(1000)
train_ind <- sample(seq_len(nrow(dat2)), size = smp_size)

train <- dat2[train_ind, ]
test <- dat2[-train_ind, ]
dim(train)
dim(test)

#fit <- glm(dead_or_alive ~ gender+age+symptom_to_hospital,data = dat, family = "binomial")
#summary(fit)
# 
# dim(dat)

fit <- glm(dependent ~ gender+age+symptom_to_hospital,data = train, family = "binomial")

# Use your model to make predictions, in this example newdata = training set, but replace with your test set    
pdata <- predict(fit, newdata = test[,-2], type = "response")

# use caret and compute a confusion matrix
logistic_cf <- confusionMatrix(data = as.factor(as.numeric(pdata>0.5)), reference = test$dependent)
logistic_cf

#########################
#### Random forest ######
#########################

library(randomForest)
library(mlbench)
library(caret)

# Load Dataset
# dat_rf <- dat[,c("gender","age","symptom_to_hospital","dependent")]
# dat_rf <- na.omit(dat_rf)
# dat_rf$dependent <- as.factor(dat_rf$dependent)
# #dim(dat1)
# #dim(na.omit(dat1))
# x <- dat_rf[,c("gender","age","symptom_to_hospital")]
# y <- dat_rf[,c("dependent")]
# 
# 
# # Create model with default paramters
# control <- trainControl(method="repeatedcv", number=10, repeats=3)
# seed <- 7
# metric <- "Accuracy"
# set.seed(seed)
# mtry <- sqrt(ncol(x))
# tunegrid <- expand.grid(.mtry=mtry)
# rf_default <- train(dependent~., data=dat1, method="rf", metric=metric, tuneGrid=tunegrid, trControl=control)
# print(rf_default)
# 
# 
# ##### Grid search
# control <- trainControl(method="repeatedcv", number=10, repeats=5, search="grid")
# set.seed(seed)
# tunegrid <- expand.grid(.mtry=c(1:15))
# rf_gridsearch <- train(dependent~., data=dat1, method="rf", metric=metric, tuneGrid=tunegrid, trControl=control)
# print(rf_gridsearch)
# plot(rf_gridsearch)
# 


##### Final rf model: ########

control <- trainControl(method="repeatedcv", number=10, repeats=3)
seed <- 10
metric <- "Accuracy"
set.seed(seed)
mtry <- 2
tunegrid <- expand.grid(.mtry=mtry)

rf_fit <- train(dependent~gender+age+symptom_to_hospital, data=train[,-2], method="rf", metric=metric, tuneGrid=tunegrid, trControl=control)

# predict the outcome on a test set
rf_pred <- predict(rf_fit,test[,-2])
# compare predicted outcome and true outcome
rf_cf <- confusionMatrix(rf_pred,test$dependent)


probs_rf <- predict(rf_fit, newdata=test, type='prob')


#########################


model_compare <- data.frame(Model = c('Logistic Regression',
                                      'Random Forest'),
                            Accuracy = c(logistic_cf$overall[1],
                                         rf_cf$overall[1]))

model_compare

