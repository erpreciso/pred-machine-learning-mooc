# I'll predict diamonds cut and price range from other variables

# load data
require(caret)
require(ggplot2)
require(MASS)
set.seed(111)
data(diamonds)

# Simple Splitting Based on the Outcome
in.train <- createDataPartition(diamonds$cut, p=0.80, list=FALSE)
training <- diamonds[in.train,]
testing <- diamonds[-in.train,]

# model using lda function from MASS package
lda.fit <- lda(cut ~ ., data=training)
lda.pred <- predict(lda.fit, newdata=testing)

c1 <- confusionMatrix(lda.pred$class, testing$cut)

# model using caret/lda
fit2 <- train(cut ~ ., data=training, method="lda")
predictions <- predict(fit2, newdata=testing)
c2 <- confusionMatrix(predictions, testing$cut)

paste("Accuracy: ", round(c1$overall[1], 3))
