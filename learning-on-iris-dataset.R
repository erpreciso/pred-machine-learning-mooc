# I'll build a basic model, and explore how NA introduction affects the prediction

# load data
data(iris)
require(caret)
set.seed(825)

# Simple Splitting Based on the Outcome
in.train <- createDataPartition(iris$Species, p=0.80, list=FALSE)
training <- iris[in.train,]
testing <- iris[-in.train,]

# introduce NAs
training[sample(dim(training)[[1]], 2),1:4] <- NA

# if I introduce NAs in the testing, prediction for that record won't be performed
# testing[sample(dim(testing)[[1]], 2),1:3] <- NA

# fit simple model
fitControl <- trainControl(## 10-fold CV
    method = "repeatedcv",
    number = 10,
    ## repeated ten times
    repeats = 10)

# create model
fit <- train(Species ~ ., data = training,
                 method = "rpart",
                 preProcess = "pca",
                 trControl = fitControl)

# predict
prediction <- predict(fit, newdata=testing, type="prob")

# assign prediction based on probability
prediction$pS <- "setosa"
prediction[prediction$setosa < 1 & prediction$virginica > prediction$versicolor, "pS"] <- "virginica"
prediction[prediction$setosa < 1 & prediction$virginica < prediction$versicolor, "pS"] <- "versicolor"

# evaluate result
confusionMatrix(prediction$pS, testing$Species)
