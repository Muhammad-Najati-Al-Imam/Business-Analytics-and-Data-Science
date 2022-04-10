#Name: Muhammad Najati Alimam
#Matriculation Number: 607859
#Email: muhammad.najati.alimam@student.hu-berlin.de OR m.najati@hotmail.com
#Note: I have tried a lot of things in my attempts, so there might be residual unused codes and libraries (not much), but the code runs fine from beginning to end.

suppressPackageStartupMessages(library(caret))
suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(tidyr))
suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(mlr))
suppressPackageStartupMessages(library(randomForest))
suppressPackageStartupMessages(library(xgboost))
suppressPackageStartupMessages(library(mice))
suppressPackageStartupMessages(library(ggplot2))
suppressPackageStartupMessages(library(eeptools))
suppressPackageStartupMessages(library(reshape2))
suppressPackageStartupMessages(library(cluster))
suppressPackageStartupMessages(library(parallel))
suppressPackageStartupMessages(library(parallelMap))
suppressPackageStartupMessages(library(doParallel))
suppressPackageStartupMessages(library(LiblineaR))
suppressPackageStartupMessages(library(randomForestSRC))
library(psych)
library(GPArotation)
library(corrplot)
library(factoextra)

setwd("D:/Dropbox/Humboldt University - Lectures & Documents/BADS/Kaggle Competition")

############### Read data
train_mother <- read.csv("BADS_WS1920_known.csv", header = TRUE, stringsAsFactors = FALSE)
test_mother <- read.csv("BADS_WS1920_unknown.csv", header = TRUE, stringsAsFactors = FALSE)


#new dataframe. test dataframe with added return column
test_mother_adjusted <- data.frame(test_mother[,], return =rep("None", nrow(test_mother)))

#combined data
all_mother <- rbind(train_mother, test_mother_adjusted)

#understand the structure
str(all_mother)
str(train_mother)

sort(table(all_mother$item_size)) #extra, just in case
length(unique(all_mother$item_size)) #we have a problem with sizes m8, but maybe you can ignore it since it's a character variable



length(unique(all_mother$brand_id))
length(unique(train_mother$brand_id))

############### Data cleaning

### how many unique item ids: this is me checking that there is a limited number of items
length(unique(all_mother$item_id)) #Too many to convert to factor

#how many brand ids
length(unique(all_mother$brand_id)) #Too many to convert to factor

### Convert all date columns to the date format
all_mother$order_date <- as.Date(all_mother$order_date)
all_mother$delivery_date <- as.Date(all_mother$delivery_date)
all_mother$user_reg_date <- as.Date(all_mother$user_reg_date)
all_mother$user_dob <- as.Date(all_mother$user_dob)

### convert title and state into factor
all_mother$user_title <- as.factor(all_mother$user_title)
all_mother$user_state <- as.factor(all_mother$user_state)


### convert return to factor
all_mother$return <- as.factor(all_mother$return)

### convert color and size to factor
all_mother$item_color <- as.factor(all_mother$item_color)
all_mother$item_size <- as.factor(all_mother$item_size)

### after checking, there seems to be no reason why titles are missing. Since 99% are Mrs, it is safe to replace them with Mrs
all_mother$user_title[all_mother$user_title  == 'not reported'] <- 'Mrs'
# table(all_mother$user_title)

#dataframe still shows the not reported factor, even though it has no entries, so we can make it re-evaluate how many levels we have
all_mother$user_title<-factor(all_mother$user_title)
table(all_mother$user_title)

# making a new dataframe to work on
adjusted <- subset(all_mother)

### New variable Age. na.omit ignores missing values
adjusted$age[!is.na(adjusted$user_dob)] <- age_calc(na.omit(adjusted$user_dob), units = "years")
adjusted$age[adjusted$age>90] <- NA
adjusted$age[adjusted$age<15] <- NA
summary(adjusted)


### New variable, checking the time between account creation and order date. Hypo: newer accounts are riskier
adjusted$account_age <- difftime(adjusted$order_date ,adjusted$user_reg_date , units = c("days"))
adjusted$account_age <- as.numeric(adjusted$account_age)
adjusted$account_age[adjusted$account_age == -1] <- 0


### New variable difference between order and delivery date
adjusted$diff_in_days <- difftime(adjusted$delivery_date ,adjusted$order_date , units = c("days"))
adjusted$diff_in_days <- as.numeric(adjusted$diff_in_days)
adjusted$diff_in_days[adjusted$diff_in_days < -1] <- NA # the minus values are errors, we can null or impute them ### Review
adjusted$diff_in_days[adjusted$diff_in_days > 123] <- NA


# adjusted$colorodds <- coloroddsclean$odd
#odds of brand
brandodds <- as.data.frame(table(adjusted$brand_id, adjusted$return))
brandodds1 <- subset(brandodds[1:136,])
brandodds2 <- subset(brandodds[137:272,])
brandodds1$Var22 <- brandodds2$Var2
brandodds1$Freq2 <- brandodds2$Freq
brandodds1$odd <- brandodds1$Freq2 - brandodds1$Freq
brandodds1$brand_id <- brandodds1$Var1
brandodds1$brand_id <- as.numeric(as.character(brandodds1$brand_id))
brandodds <- subset(brandodds1[,c(7,6)])

brandoddsclean <- left_join(adjusted, brandodds, by="brand_id") %>%
  mutate(brand_id=ifelse(is.na(odd), brand_id, odd))

adjusted$brandodds <- brandoddsclean$odd


#odds of item id
itemodds <- as.data.frame(table(adjusted$item_id, adjusted$return))
itemodds1 <- subset(itemodds[1:2048,])
itemodds2 <- subset(itemodds[2049:4096,])
itemodds1$Var22 <- itemodds2$Var2
itemodds1$Freq2 <- itemodds2$Freq
itemodds1$odd <- itemodds1$Freq2 - itemodds1$Freq
itemodds1$item_id <- itemodds1$Var1
itemodds1$item_id <- as.numeric(as.character(itemodds1$item_id))
itemodds <- subset(itemodds1[,c(7,6)])

itemoddsclean <- left_join(adjusted, itemodds, by="item_id") %>%
  mutate(item_id=ifelse(is.na(odd), item_id, odd))

adjusted$itemodds <- itemoddsclean$odd
summary(adjusted)

#### Addressing missing prices
adjusted$item_price[adjusted$item_price > 400] <- 399.95 #4 items that are 999, 2 returned, 2 not. I set them to be the next highest value
adjusted$item_price[adjusted$item_price < 0.1] <- NA #Preparing for imputation
# 

# New variable: how many orders on the same day
adjusted$order_date <- as.factor(adjusted$order_date)
numberoforders <- as.data.frame(table(adjusted$order_date))
names(numberoforders)[names(numberoforders) == 'Var1'] <- 'order_date'

numberoforders <- left_join(adjusted, numberoforders, by="order_date") %>%
  mutate(order_date=ifelse(is.na(Freq), order_date, Freq))
adjusted$date_freq <- numberoforders$Freq

### New Variable: frequency of item
freqofitem <- as.data.frame(table(adjusted$item_id))
adjusted$item_id <- as.factor(adjusted$item_id)
names(freqofitem)[names(freqofitem) == 'Var1'] <- 'item_id'
freqofitem <- left_join(adjusted, freqofitem, by="item_id") %>%
  mutate(item_id=ifelse(is.na(Freq), item_id, Freq))
adjusted$freqofitem <- freqofitem$Freq

yo <- as.data.frame(table(adjusted$brand_id))

adjusted$freqofitem[adjusted$freqofitem < 11] <- NA
adjusted$itemodds[is.na(adjusted$freqofitem)] <- NA


table(adjusted$user_title, adjusted$return)

final <- subset(adjusted[,c(8, 14:21)]) #removing duplicate price2 column
final <- mlr::createDummyFeatures(final, target="return")

##### Dataframing the cleaned and prepared testing and training data
final_train <- subset(final, final$return == 0 | final$return == 1) #prepared training data
final_train$return <- factor(final_train$return) #to delete empty levels

#test
final_test <- subset(final, final$return == "None") #prepared testing data
final_test$return <- factor(final_test$return) #to delete empty levels

final_train_clean <- subset(final_train)
final_test_clean <- subset(final_test)

######### Gradient boosting
idx<- createDataPartition(final_train_clean$return, p = 0.6, list = FALSE)
train <- final_train_clean[idx, ]
test <-  final_train_clean[-idx, ]

task <- makeClassifTask(data = train, target = "return", positive = "1")

xgb.learner <- makeLearner("classif.xgboost", predict.type = "prob",
                           par.vals = list("verbose" = 0,
                                           "early_stopping_rounds"=10)) 
xgb.learner

# Tuning parameters
xgb.parms <- makeParamSet(
  makeDiscreteParam("booster", values = "gbtree"),
  makeNumericParam("eta", lower = 0.01, upper = 0.5),
  makeNumericParam("lambda", lower = -1, upper = 20),
  makeNumericParam("lambda_bias",  lower = 0, upper = 200),
  makeNumericParam("alpha", lower = -1, upper = 10),
  makeIntegerParam("nrounds", lower=80, upper=700),
  makeIntegerParam("max_depth", lower= 1, upper= 6),
  makeNumericParam("gamma", lower = 0, upper = 10),
  makeNumericParam("colsample_bytree", lower=0, upper = 1),
  makeDiscreteParam("min_child_weight", values = 1),
  makeNumericParam("subsample", lower=0, upper=1))



tuneControl <- makeTuneControlRandom(maxit=100, tune.threshold = FALSE)


rdesc <- makeResampleDesc(method = "RepCV", rep = 5, folds=5, stratify = TRUE)

cl <- makeCluster( max(1,detectCores()-1))
registerDoParallel(cl)

parallelStartSocket(7, level = "mlr.tuneParams")
set.seed(123)

RNGkind("L'Ecuyer-CMRG")  
clusterSetRNGStream(iseed = 1234567)

xgb.tuning <- tuneParams(xgb.learner, task = task, resampling = rdesc,
                         par.set = xgb.parms, control = tuneControl, measures = mlr::auc)
parallelStop()
stopCluster(cl)

xgb.tuning$x

xgb.learner <- setHyperPars(xgb.learner, par.vals = c(xgb.tuning$x, "verbose" = 0))
xgb.learner

model_library <- list()
model_library[["xgb"]] <- mlr::train(xgb.learner, task = task)

xgbmodel <- mlr::train(xgb.learner, task = task)

pred <- sapply(model_library, predict, newdata = test, simplify=FALSE)

auc <- sapply(pred, mlr::performance, measures = mlr::auc)
auc

varImpPlot(rf.randomForest,type=1)
getFeatureImportance(xgbmodel, type = 2)

featureImportance <- list()
xgb.importance(model = xgbmodel$learner.model, feature_names = colnames(task$env$data))

getFeatureImportance(xgbmodel)
featureImportance[["xgb"]] <- unlist(getFeatureImportance(xgbmodel)$res)

maxMinStandardize <- function(x) ( (x - min(x)) / (max(x) - min(x)) ) * 100
importanceTable <- as.data.frame(sapply(featureImportance, maxMinStandardize, USE.NAMES = TRUE))
importanceTable[order(rowSums(importanceTable), decreasing = TRUE),]


pred <- sapply(model_library, predict, newdata = final_test_clean, simplify=FALSE)
pred_matrix <- sapply(pred, function(x) x$data$prob.1)
final_test_clean$predicted <- pred_matrix
submit <- data.frame("order_item_id" = test_mother$order_item_id, "return" = final_test_clean$predicted)
names(submit)[names(submit) == 'xgb'] <- 'return'
write.csv(submit, file = "muhammad-najati-alimam-predictions16.csv", row.names = FALSE)




