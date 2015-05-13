# I'll predict diamonds cut and price range from other variables

# load data
require(caret)
require(ggplot2)
set.seed(111)
data(diamonds)

# Simple Splitting Based on the Outcome
in.train <- createDataPartition(diamonds$cut, p=0.80, list=FALSE)
training <- diamonds[in.train,]
testing <- diamonds[-in.train,]

# fit simple model
fitControl <- trainControl(## 10-fold CV
    method = "repeatedcv",
    number = 10,
    ## repeated ten times
    repeats = 10)

# create model
fit <- train(cut ~ clarity, data = training,
                 method = "rpart",
                 trControl = fitControl)

# predict
prediction <- predict(fit, newdata=testing[,c(3,4)], type="prob")
