# load libraries
require(caret)
set.seed(816)

# download data sets
setwd("/home/erpreciso/Documents/school")
# download.file("https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv", "temp", method="curl")
# download.file("https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv", "temp2", method="curl")

# import data considering also DIV/0! strings as NAs
training <- read.csv("temp", na.strings=c("NA","#DIV/0!"))
testing <- read.csv("temp2", na.strings=c("NA","#DIV/0!"))

## Preprocessing
# find predictors with zero / near zero variance
nsv <- nearZeroVar(training, saveMetrics=TRUE)
to.eliminate <- row.names(nsv[nsv$nzv,])

# variables to exclude from modeling
excl <- c("problem_id","X","user_name","raw_timestamp_part_1",
          "raw_timestamp_part_2","cvtd_timestamp","num_window")
to.eliminate <- c(to.eliminate, excl)

# remove variables
training <- training[, -which(names(training) %in% to.eliminate)]
testing <- testing[, -which(names(testing) %in% to.eliminate)]

# predictors with NA values
nas <- !is.na(colSums(training[, -which(names(training)=="classe")]))
training <- training[,nas]
testing <- testing[,nas]

# find predictors with high correlation, and remove
training.cor <- abs(cor(training[, -which(names(training)=="classe")],
                        method="pearson"))
diag(training.cor) <- 0
high.cor <- which(training.cor > .8, arr.ind=TRUE)
training <- training[, -high.cor]
testing <- testing[, -high.cor]

# create a validation subset to test different models
in.train <- createDataPartition(training$classe, p=0.8, list=FALSE)
ta <- training[in.train,]
te <- training[-in.train,]

# using cross-validation in the model
fc <- trainControl(method = "repeatedcv", number = 5, repeats = 5)

# fit simple model

lda.fit <- train(classe~.,data=ta, method="lda", trControl=fc)
confusionMatrix(predict(lda.fit, newdata=te), te$classe)

gbm.fit <- train(classe~.,data=ta, method="gbm", trControl=fc)
confusionMatrix(predict(gbm.fit, newdata=te), te$classe)
