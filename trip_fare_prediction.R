library(data.table)
library(caret)
library(randomForest)
library(data.table)
library(caret)
library(randomForest)
library(caretEnsemble)
library(moments)
library(Metrics)

dat <- fread("C:/Users/samantha.vandermerwe/Google Drive/Old Work Laptop Stuff/Documents/Assessment/Data/final.csv")
dat$Weekday <- as.factor(dat$Weekday)
dat[, TAVG := (TMAX + TMIN)/2]
dat[is.na(dat)] <- 0
dat <- dat[, -1]
dat <- dat[, .SD, .SDcols = c("Shift", "Hour", "label", "Distance", "PRCP", "TAVG", "passenger_count", "Weekday", "fare_amount", "year")]
dat <- dat[, -"Weekday"]
dat <- dat[fare_amount != 0]
dat <- dat[fare_amount > 0]
dat <- dat[Distance < 50]
dat <- dat[Distance > 0]
#apply log transform to the main predictor variable
#dat$Distance <- log(dat$Distance)
dat$Distance <- BoxCox(dat$Distance, lambda = 0.2)
skewness(x)
hist(x,
     xlim = c(min(x), max(x)),
     probability = TRUE,
     nclass = max(x) - min(x) + 1,
     col = 'lightblue',
     main = 'Right Skewed')
lines(density(x,bw=1), col = 'red', lwd = 3)



train <- dat[label == "train"]
test <- dat[label == "test"]
train <- train[, .SD, .SDcols = -c("label")]
test <- test[, .SD, .SDcols = -c("label")]

#replace na with zero
train[is.na(train)] <- 0
test[is.na(test)] <- 0

#a simple linear model
lm_mod <- train(fare_amount ~., data = train,method = 'lm')

preds <- predict(lm_mod , test)
rmse(test$fare_amount, preds)

#random forest test
rf_random <- randomForest(fare_amount ~., data = train, importance = TRUE)

preds <- predict(rf_random , test)
rmse(test$fare_amount, preds)

#######################


