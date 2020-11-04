# Import training and testing datafiles from ***Car.csv files
# With data frame names train and test
require(lattice)
require(ggplot2)



cols <- character(nrow(train))
cols[] <- "black"

cols[train$X7 == "acc"] <- "blue"
cols[train$X7 == "good"] <- "red"
cols[train$X7 == "unacc"] <- "green"
pairs(train[1:6],col=cols)

plot(jitter(train$X3) ~ jitter(train$X4), pch = 15,col=cols)

plot(jitter(as.integer(train$X1)) ~ jitter(as.integer(train$X2)), pch = 15,col=alpha(cols, 0.5))

# Install and load required packages for fancy decision tree plotting
install.packages('rpart.plot')
library(rpart)
library(rpart.plot)
library(caret)

options(digits=2)

fit1 <- rpart(X7 ~ .-ID , data=train, method="class",
              control=rpart.control(minsplit=100))
rpart.plot(fit1)
plotcp(fit1)
fit1$cptable



# Now let's make predictions
predicttrain1 <- predict(fit1, train, type = "class")
confusionMatrix(predicttrain1,train$X7)
#install.packages('e1071', dependencies=TRUE)

prediction1 <- predict(fit1, test, type = "class")
confusionMatrix(prediction1,test$X7)


#Repeat with minsplit = 10
fit2 <- rpart(X7 ~ .-ID , data=train, method="class",
              control=rpart.control(minsplit=10))
rpart.plot(fit2)
plotcp(fit2)
fit2$cptable


#Repeat with smaller training set
set.seed(1)
train_s <- train[sample(1:nrow(train), 200, replace=FALSE),]
fit3 <- rpart(X7 ~ .-ID , data=train, method="class",
              control=rpart.control(minsplit=100))
rpart.plot(fit3)
plotcp(fit3)
fit3$cptable


#Fancy stuff
new.fit <- prp(fit1,snip=TRUE)$obj

fancyRpartPlot(new.fit1)