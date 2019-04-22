# An exercise in predicting Supreme Court case outcomes using CART and Random Forest

stevens <- read.csv("stevens.csv")

# 566 cases
str(stevens)

# split data into train/test sets
library(caTools)

set.seed(3000)
split <- sample.split(stevens$Reverse, SplitRatio = 0.7)

# create train/test sets
train <- subset(stevens, split == TRUE)
test <- subset(stevens, split == FALSE)


# build model -------------------------------------------------------------

library(rpart)
library(rpart.plot)

StevensTree <- rpart(Reverse ~ Circuit + Issue + Petitioner + Respondent + LowerCourt + Unconst, data=train, method="class", control=rpart.control(minbucket = 25))
# method="class" tells rpart to build a classification tree (binary outcome); cart can also be used for regression problems
# control=... defines minimum bucket size, i.e. limits tree so that it doesn't overfit to our training set

# plot tree
prp(StevensTree)

# predict cases in test set
PredictCART <- predict(StevensTree, newdata = test, type="class") 
# need to add type="class" for CART models: takes each test set obs. and classifies 1 or 0 - like having a threshold t = 0.5

# build confusion matrix
table(test$Reverse, PredictCART)

# PredictCART
#    0   1
# 0  41  36
# 1  22  71

accuracy <- (41 + 71) / (41 + 36 + 22 + 71) # 0.6588235

# baseline model (always reverse) = 0.547 accuracy
# logistic model = 0.665 accuracy
# The CART model is competitive with logistic regression, but MUCH more interpretable

# ROC curve
library(ROCR)

PredictROC <- predict(StevensTree, newdata = test)
PredictROC # output for each column of every test set obs.

pred <- prediction(PredictROC[,2], test$Reverse)
perf <- performance(pred, "tpr", "fpr")
plot(perf)

# random forest -----------------------------------------------------------

library(randomForest)
StevensForest <- randomForest(Reverse ~ Circuit + Issue + Petitioner + Respondent + LowerCourt + Unconst, data = train, nodesize = 25, ntree = 200)
# If outcome variable is NOT a factor, randomForest will return a warning (are you sure you want to do regression?)
# i.e. we need to make it clear to the randomForest algorithm that we are doing a classification problem (there's no additional argument to specify that using randomForest)

# convert Reverse to a factor in train/test sets
train$Reverse <- as.factor(train$Reverse)
test$Reverse <- as.factor(test$Reverse)

# create rf model
StevensForest <- randomForest(Reverse ~ Circuit + Issue + Petitioner + Respondent + LowerCourt + Unconst, data = train, nodesize = 25, ntree = 200) # no warning

# use model to make predictions on test set
set.seed(3000)
PredictForest <- predict(StevensForest, newdata = test)

# confusion matrix
table(test$Reverse, PredictForest)

# PredictForest
#    0   1
# 0  42  35
# 1  19  74

accuracy <- (42 + 74) / (42 + 35 + 19 + 74) # 0.6823529 - better than logistic model and CART


# cross-validation for cart -----------------------------------------------

library(caret)
library(e1071)

# define folds
fitControl <- trainControl(method = "cv", number = 10) # 10-fold cv

# pick possible values of complexity parameter (cp)
cartGrid <- expand.grid(.cp=(1:50*0.01)) # use cp values from 0.01 - 0.50

# train
train(Reverse ~ Circuit + Issue + Petitioner + Respondent + LowerCourt + Unconst, data = train, method = "rpart", trControl = fitControl, tuneGrid = cartGrid) 
# method = "rpart" because we want to validate parameters for cart tree
# trControl = fitControl (output of trainControl)
# tuneGrid = cartGrid (our defined cp values)

# create new CART model using cp determined by cross-validation
StevensTreeCV <- rpart(Reverse ~ Circuit + Issue + Petitioner + Respondent + LowerCourt + Unconst, method = "class", data = train, control = rpart.control(cp=0.19))
PredictCV <- predict(StevensTreeCV, newdata = test, type = "class")

# confusion matrix
table(test$Reverse, PredictCV)

accuracy <- (59 + 64) / (59 + 18 + 29 + 64) # 0.7235294 - better than random forest!
