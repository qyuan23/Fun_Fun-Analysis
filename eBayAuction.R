setwd("D:/Analysis")
library(readr)
ebay <- read.csv("eBayAuctions.csv")
ebay$Competitive. <- as.factor(ebay$Competitive.)
set.seed(8)
index <- sample(nrow(ebay), 0.6*nrow(ebay))
train <- ebay[index,]
valid <- ebay[-index,]


library(rpart)
library(caret)
library(gains)
## Decision Tree algorithm
tr <- rpart(Competitive. ~ ., data = train, method = "class")
pd1 <- predict(tr,valid, type="class")
confusionMatrix(as.factor(valid$Competitive.), as.factor(pd1))
pred1 <- predict(tr, valid)
gain1 <- gains(as.numeric(valid$Competitive.), pred1[,2], groups = 10)
gain1
ht <- gain1$mean.resp/mean(as.numeric(valid$Competitive.))
barplot(ht, names.arg = gain1$depth, xlab = "Percentile", ylab = "Mean Response", main = "Decile-wise lift chart")


## Boosting algorithm
bo <- boosting(Competitive. ~ ., data = train)
pd2 <- predict(bo,valid, type="class")
confusionMatrix(valid$Competitive., as.factor(pd2$class))
pred2 <- predict(bo, valid)
gain2 <- gains(as.numeric(valid$Competitive.), as.numeric(pred2$class)) 
gain2
ht <- gain2$mean.resp/mean(as.numeric(valid$Competitive.))
barplot(ht, names.arg = gain2$depth, xlab = "Percentile", ylab = "Mean Response", main = "Decile-wise lift chart")


## Bagginge algorithm
bag <- bagging(Competitive. ~ ., data = train)
pd3 <- predict(bag,valid, type="class")
confusionMatrix(valid$Competitive., as.factor(pd3$class))
pred3 <- predict(bag, valid)
gain3 <- gains(as.numeric(valid$Competitive.), as.numeric(pred3$class)) 
gain3
ht <- gain3$mean.resp/mean(as.numeric(valid$Competitive.))
barplot(ht, names.arg = gain3$depth, xlab = "Percentile", ylab = "Mean Response", main = "Decile-wise lift chart")


## Random Forest algorithm
rf <- randomForest(Competitive. ~ ., data = train, mtry = 4)
pd4 <- predict(rf,valid, type="class")
confusionMatrix(valid$Competitive., as.factor(pd4))
pred4 <- predict(rf, valid)
gain4 <- gains(as.numeric(valid$Competitive.), as.numeric(pred4), group=10) 
gain4
ht <- gain4$mean.resp/mean(as.numeric(valid$Competitive.))
barplot(ht, names.arg = gain4$depth, xlab = "Percentile", ylab = "Mean Response", main = "Decile-wise lift chart")

