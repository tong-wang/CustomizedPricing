Sys.setenv(LANG = "en")
require("plyr")
require("ggplot2")

#NEED TO FIRST SET R WORKING DIRECTORY TO WHERE THE FILES ARE LOCATED!!!
setwd("~/Dropbox/Consulting/Acceval")

### read the data files
data <- read.csv("Pharma Transaction Data v05.csv", header=TRUE)


### prepare data for analysis
## dependent variables
# percentage discount (the ultimate target to predict)
data$Discount <- (data$InvoicePrice - data$NetPrice) / data$InvoicePrice
# binary variable for whether there is a discount
data$isDiscount <- data$InvoicePrice > data$NetPrice
#data$nDiscount <- scale(log(data$Discount+1))[,1]


## normalizing independent variables: ContractQuantity and InvoicePrice
data$nContractQuantity <-  scale(log(data$ContractQuantity))[,1]
data$nInvoicePrice <- scale(log(data$InvoicePrice))[,1]


summary(data)


## split Test and Validation datasets
# randomly sample 70% of the data for test, the rest for validation
Proportion.Test = 0.7
N = nrow(data)
RandomIndex.Test = sample(N, as.integer(Proportion.Test*N))
dataT <- data[RandomIndex.Test, ]
dataV <- data[-RandomIndex.Test, ]


## visualize Test data
summary(dataT)
hist(dataT[dataT$isDiscount,]$Discount, breaks=100)
qplot(x=nContractQuantity, y=Discount, colour=Channel, data=dataT)

### end data preparation


###########################
### STEP 1. Classification: predict whom not to offer any discount
### Need to test multiple classification tools: logistic regression, decision tree, random forest, and SVM

## logistic regression for isDiscount
logistic1.model <- glm(isDiscount ~ nContractQuantity + nInvoicePrice + Channel, data=dataT, family=binomial())
summary(logistic1.model)

logistic1.valid <- dataV[c("RecordID", "Channel", "nContractQuantity", "nInvoicePrice", "Discount", "isDiscount")]
logistic1.valid$predP <- predict(logistic1.model, newdata=logistic1.valid, type="response")
logistic1.valid$pred1 <- logistic1.valid$predP>0.5

logistic1.table <- table(logistic1.valid$pred1, logistic1.valid$isDiscount)
logistic1.table

#qplot(x=dataV$nContractQuantity, y=logistic1.valid$predP, log="x", colour=dataV$Channel)


#logistic regression 2 (add in Territory variable)
logistic2.model <- glm(isDiscount ~ nContractQuantity + nInvoicePrice + Channel + Territory, data=dataT, family=binomial())
summary(logistic2.model)

logistic2.valid <- dataV[c("RecordID", "Channel", "Territory", "nContractQuantity", "nInvoicePrice", "Discount", "isDiscount")]
logistic2.valid$predP <- predict(logistic2.model, newdata=logistic2.valid, type="response")
logistic2.valid$pred1 <- logistic2.valid$predP>0.5

logistic2.table <- table(logistic2.valid$pred1, logistic2.valid$isDiscount)
logistic2.table

#qplot(x=dataV$nContractQuantity, y=logistic2.valid$predP, log="x", colour=dataV$Channel)

require(ROCR)
logistic2.pred <- prediction(logistic2.valid$predP, logistic2.valid$isDiscount)
# Plot ROC curve
logistic2.ROC <- performance(logistic2.pred, measure = "tpr", x.measure = "fpr")
plot(logistic2.ROC)
# Plot precision/recall curve
logistic2.precision <- performance(logistic2.pred, measure = "prec", x.measure = "rec")
plot(logistic2.precision)
# Plot accuracy as function of threshold
logistic2.accuracy <- performance(logistic2.pred, measure = "acc")
plot(logistic2.accuracy)




## decision tree
require("rpart")
tree2.model <- rpart(as.factor(isDiscount) ~ nContractQuantity + nInvoicePrice + Channel + Territory, data=dataT)
summary(tree2.model)
printcp(tree2.model)
plotcp(tree2.model)

#validation
tree2.valid <- dataV[c("RecordID", "Channel", "Territory", "nContractQuantity", "nInvoicePrice", "Discount", "isDiscount")]
tree2.valid$predP <- predict(tree2.model, newdata=tree2.valid, type="prob")[,"TRUE"]
tree2.valid$pred1 <- as.logical(predict(tree2.model, newdata=tree2.valid, type="class"))

tree2.table <- table(tree2.valid$pred1, tree2.valid$isDiscount)
tree2.table

# plot tree 
plot(tree2.model, uniform=TRUE, main="Regression Tree for Mileage ")
text(tree2.model, use.n=TRUE, all=TRUE)


require(ROCR)
tree2.pred <- prediction(tree2.valid$predP, tree2.valid$isDiscount)
# Plot ROC curve
tree2.ROC <- performance(tree2.pred, measure = "tpr", x.measure = "fpr")
plot(tree2.ROC)
# Plot precision/recall curve
tree2.precision <- performance(tree2.pred, measure = "prec", x.measure = "rec")
plot(tree2.precision)
# Plot accuracy as function of threshold
tree2.accuracy <- performance(tree2.pred, measure = "acc")
plot(tree2.accuracy)






## Random Forest prediction of isDiscount
library(randomForest)
forest1.model <- randomForest(as.factor(isDiscount) ~ nContractQuantity  + nInvoicePrice + Channel, data=dataT)
summary(forest1.model)
print(forest1.model) # view results 
importance(forest1.model) #importance of each predictor
plot(forest1.model)

#validation
forest1.valid <- dataV[c("RecordID", "Channel", "nContractQuantity", "nInvoicePrice", "Discount", "isDiscount")]
forest1.valid$predP <- predict(forest1.model, newdata=forest1.valid, type="prob")[,"TRUE"]
forest1.valid$pred1 <- as.logical(predict(forest1.model, newdata=forest1.valid, type="response"))

forest1.table <- table(forest1.valid$pred1, forest1.valid$isDiscount)
forest1.table

require(ROCR)
forest1.pred <- prediction(forest1.valid$predP, forest1.valid$isDiscount)
# Plot ROC curve
forest1.ROC <- performance(forest1.pred, measure = "tpr", x.measure = "fpr")
plot(forest1.ROC)
# Plot precision/recall curve
forest1.precision <- performance(forest1.pred, measure = "prec", x.measure = "rec")
plot(forest1.precision)
# Plot accuracy as function of threshold
forest1.accuracy <- performance(forest1.pred, measure = "acc")
plot(forest1.accuracy)





## SVM classification for isDiscount
require(e1071)

## first-time run of SVM requires optimization of parameters gamma and cost
#svm.tune <- tune.svm(as.factor(isDiscount) ~ nContractQuantity  + nInvoicePrice + Channel + Territory, data = dataT, validation.x=dataV, gamma = 2^(-1:5), cost = 10^(1:4), probability=TRUE)
#svm.model <- svm.tune$best.model

## directly use the obtained optimal parameter gamma=8, cost=10000
svm.model <- svm(as.factor(isDiscount) ~ nContractQuantity  + nInvoicePrice + Channel + Territory, data = dataT, cost = 10000, gamma = 8, probability=TRUE)
summary(svm.model)

svm.valid <- dataV[c("RecordID", "Channel", "Territory", "nContractQuantity", "nInvoicePrice", "Discount", "isDiscount")]
svm.valid$pred1 <- as.logical(predict(svm.model, newdata=svm.valid))
svm.valid$predP <- attr(predict(svm.model, newdata=svm.valid, probability=TRUE), "probabilities")[,"TRUE"]

svm.table <- table(svm.valid$pred1, svm.valid$isDiscount)
svm.table

require(ROCR)
svm.pred <- prediction(svm.valid$predP, svm.valid$isDiscount)
# Plot ROC curve
svm.ROC <- performance(svm.pred, measure = "tpr", x.measure = "fpr")
plot(svm.ROC)
# Plot precision/recall curve
svm.precision <- performance(svm.pred, measure = "prec", x.measure = "rec")
plot(svm.precision)
# Plot accuracy as function of threshold
svm.accuracy <- performance(svm.pred, measure = "acc")
plot(svm.accuracy)


plot(svm.model, data = dataV, formula = nContractQuantity ~ nInvoicePrice)





########################
# An unnecessary visualization of the probability predictions
testdata <- dataV[dataV$Channel=="PHARMACY" & dataV$Territory=="N034",][c("Channel", "Territory", "nInvoicePrice", "nContractQuantity")]
testdataP <- testdata
testdataP$nInvoicePrice <- seq(-2, 4, along.with=testdataP$nInvoicePrice)
testdataP$nContractQuantity <- 0

testdataP$svmP <- attr(predict(svm.model, newdata=testdataP, probability=TRUE), "probabilities")[,"TRUE"]
testdataP$forest1P <- predict(forest1.model, newdata=testdataP, type="prob")[,"TRUE"]
testdataP$tree2P <- predict(tree2.model, newdata=testdataP, type="prob")[,"TRUE"]
testdataP$logit2P <- predict(logistic2.model, newdata=testdataP, type="response")
testdataP$logit1P <- predict(logistic1.model, newdata=testdataP, type="response")

summary(testdataP)
plot(x=testdataP$nInvoicePrice, y=testdataP$svmP, type="l", xlim=c(-2,4), ylim=c(0,1))
lines(x=testdataP$nInvoicePrice, y=testdataP$forest1P, col="blue")
lines(x=testdataP$nInvoicePrice, y=testdataP$tree2P, col="red")
lines(x=testdataP$nInvoicePrice, y=testdataP$logit2P, col="darkgreen")
lines(x=testdataP$nInvoicePrice, y=testdataP$logit1P, col="green")


testdataQ <- testdata
testdataQ$nInvoicePrice <- 0
testdataQ$nContractQuantity <- seq(-2, 4, along.with=testdataQ$nContractQuantity)

testdataQ$svmP <- attr(predict(svm.model, newdata=testdataQ, probability=TRUE), "probabilities")[,"TRUE"]
testdataQ$forest1P <- predict(forest1.model, newdata=testdataQ, type="prob")[,"TRUE"]
testdataQ$tree2P <- predict(tree2.model, newdata=testdataQ, type="prob")[,"TRUE"]
testdataQ$logit2P <- predict(logistic2.model, newdata=testdataQ, type="response")
testdataQ$logit1P <- predict(logistic1.model, newdata=testdataQ, type="response")

summary(testdataQ)
plot(x=testdataQ$nContractQuantity, y=testdataQ$svmP, type="l", xlim=c(-2,4), ylim=c(0,1))
lines(x=testdataQ$nContractQuantity, y=testdataQ$forest1P, col="blue")
lines(x=testdataQ$nContractQuantity, y=testdataQ$tree2P, col="red")
lines(x=testdataQ$nContractQuantity, y=testdataQ$logit2P, col="darkgreen")
lines(x=testdataQ$nContractQuantity, y=testdataQ$logit1P, col="green")
########################






###########################
### STEP2. We are extending the work from the source.
## We are considering rpart as the base algorithm from base. This is considered for two reasons.
####1.Its fast to process in cmparision to SVM and other algo
####2.It provides rules which is more preferable in comparision to RandomForest,Logistic regression and SVM
####3.Accuracy seems similar for most algorithms. Thus going by Ozam-razor rule, simpler the better 
require("hydroGOF")
require("hydroGOF")
require("partykit")
require("rpart.plot")

#linear regression for Discount

linear2.model <- lm(Discount ~ nContractQuantity + nInvoicePrice + Channel , data=dataT[dataT$isDiscount,])
summary(linear2.model)

linear2.valid2 <- forest1.valid[forest1.valid$pred1==1,]
names(linear2.valid2)

linear2.valid2$pred <- predict(linear2.model, newdata=linear2.valid2)
summary(linear2.valid2)

## Calculating RMSE as the accuracy measure
RMSE_linear=rmse(linear2.valid2$pred, linear2.valid2$Discount)
plot(linear2.valid2$Discount, linear2.valid2$pred)



#rpart regression for Discount with Territory
treeDiscount.model <- rpart(Discount ~ nContractQuantity + nInvoicePrice + Channel + Territory, data=dataT[dataT$isDiscount,]
                            ,method='anova')
summary(treeDiscount.model)

treeDiscount.valid2 <- svm.valid[svm.valid$pred1==1,]
treeDiscount.valid2$pred <- predict(treeDiscount.model, newdata=treeDiscount.valid2)

summary(treeDiscount.valid2)

## Calculating RMSE as the accuracy measure
RMSE_rpart=rmse(treeDiscount.valid2$pred,treeDiscount.valid2$Discount)
plot(treeDiscount.valid2$pred,treeDiscount.valid2$Discount)


## Plotting of the Rpart

prp(treeDiscount.model)
treeDiscount_party = as.party(treeDiscount.model)
plot(treeDiscount_party,main="Plot of Decision Tree",type="simple")




#rpart regression for Discount without Territory
treeDiscount_T.model <- rpart(Discount ~ nContractQuantity + nInvoicePrice + Channel, data=dataT[dataT$isDiscount,]
                              ,method='anova', cp=0.001)
summary(treeDiscount_T.model)

treeDiscount_T.valid2 <- forest1.valid[forest1.valid$pred1==1,]
treeDiscount_T.valid2$pred <- predict(treeDiscount_T.model, newdata=treeDiscount_T.valid2)
summary(treeDiscount_T.valid2)

RMSE_rpartT=rmse(treeDiscount_T.valid2$pred,treeDiscount_T.valid2$Discount)
plot(treeDiscount_T.valid2$pred,treeDiscount_T.valid2$Discount)
## With out territory, the results seem to be better

## Plotting of the Rpart
treeDiscount_party_T = as.party(treeDiscount_T.model)
plot(treeDiscount_party_T,main="Plot of Decision Tree",type="simple")

prp(treeDiscount_T.model)



#randomForest regression for Discount
rFDiscount.model <- randomForest(Discount ~ nContractQuantity + nInvoicePrice + Channel , data=dataT[dataT$isDiscount,]
                                 ,do.trace=TRUE)
summary(rFDiscount.model)
print(rFDiscount.model) # view results 
importance(rFDiscount.model) #importance of each predictor
plot(rFDiscount.model)

rFDiscount.valid2 <- forest1.valid[forest1.valid$pred1==1,]


rFDiscount.valid2$pred <- predict(rFDiscount.model, newdata=rFDiscount.valid2)
summary(rFDiscount.valid2)

RMSE_rF=rmse(rFDiscount.valid2$pred,rFDiscount.valid2$Discount)
plot(rFDiscount.valid2$pred,rFDiscount.valid2$Discount)




#SVM regression [not working yet]
svm.tune2 <- tune.svm(Discount ~ nContractQuantity  + nInvoicePrice + Channel + Territory, data = dataT[dataT$isDiscount,], type="eps-regression", gamma = 2^(-1:5), cost = 10^(1:4))
svm.model2 <- svm.tune2$best.model
#svm.model2 <- svm(Discount ~ nContractQuantity  + nInvoicePrice + Channel + Territory, data = dataT[dataT$isDiscount,], type="eps-regression", cost = 10000, gamma = 8)
summary(svm.model2)

svm.valid2 <- svm.valid[svm.valid$pred1==1,][c("RecordID", "Channel", "Territory", "nContractQuantity", "nInvoicePrice", "Discount", "isDiscount")]
svm.valid2$pred <- predict(svm.model2, newdata=svm.valid2)
summary(svm.valid2)


plot(svm.valid2$Discount, svm.valid2$pred, xlim=c(0,1), ylim=c(0,1))
qplot(x=Discount, y=pred, colour=Channel, data=svm.valid2, xlim=c(0,1), ylim=c(0,1))





### Tabulating Results

Results=data.frame(Model_Type=c("Linear Regression","rpart With Territory","rpart Without Territory","randomForest"), RMSE = c(RMSE_linear,RMSE_rpart,RMSE_rpartT,RMSE_rF))
Results
## Rpart without Territory code seems to be best following Ozam razor's rule
