require("plyr")
require("ggplot2")

#NEED TO FIRST SET R WORKING DIRECTORY TO WHERE THE FILES ARE LOCATED!!!
#setwd("~/PATH/TO/DATA/FILE")

### read the data files

## if using the REAL dataset ##
######load("../data/data.v5.masked.Training.RData")
######load("../data/data.v5.masked.Validation.RData")

## if using the SIMULATED dataset ##
load("../data/SimulatedDataset_Training.RData")
load("../data/SimulatedDataset_Validation.RData")




###########################
### STEP 1. Classification: predict whom not to offer any discount
### Need to test multiple classification tools: logistic regression, decision tree, random forest, and SVM
### Need to consider two scenarios: (1) only use Quantity, Price, and Channel; (2) also use Territory

## Prepare Validation dataset
s1.valid <- dataV[c("Channel", "Territory", "t1", "t2", "nContractQuantity", "nInvoicePrice", "Discount", "isDiscount", "minDisc")]


## Train the models

## [s1.lg1] logistic regression for isDiscount using Quantity, Price, and Channel
s1.lg1.model <- glm(isDiscount ~ nContractQuantity + nInvoicePrice + Channel, data=dataT, family=binomial())
summary(s1.lg1.model)

# validation
s1.valid$s1.lg1.predProb <- predict(s1.lg1.model, newdata=s1.valid, type="response")
s1.valid$s1.lg1.predResp <- s1.valid$s1.lg1.predP>0.5

s1.lg1.table <- table(s1.valid$s1.lg1.predResp, s1.valid$isDiscount)
s1.lg1.table


require(ROCR)
s1.lg1.pred <- prediction(s1.valid$s1.lg1.predProb, s1.valid$isDiscount)
# Plot ROC curve
s1.lg1.ROC <- performance(s1.lg1.pred, measure = "tpr", x.measure = "fpr")
plot(s1.lg1.ROC)
# Plot precision/recall curve
s1.lg1.precision <- performance(s1.lg1.pred, measure = "prec", x.measure = "rec")
plot(s1.lg1.precision)
# Plot accuracy as function of threshold
s1.lg1.accuracy <- performance(s1.lg1.pred, measure = "acc")
plot(s1.lg1.accuracy)




## [s1.lg2] logistic regression 2 (add in Territory variable)
s1.lg2.model <- glm(isDiscount ~ nContractQuantity + nInvoicePrice + Channel + Territory, data=dataT, family=binomial())
summary(s1.lg2.model)

# validation
s1.valid$s1.lg2.predProb <- predict(s1.lg2.model, newdata=s1.valid, type="response")
s1.valid$s1.lg2.predResp <- s1.valid$s1.lg2.predProb>0.5

s1.lg2.table <- table(s1.valid$s1.lg2.predResp, s1.valid$isDiscount)
s1.lg2.table


require(ROCR)
s1.lg2.pred <- prediction(s1.valid$s1.lg2.predProb, s1.valid$isDiscount)
# Plot ROC curve
s1.lg2.ROC <- performance(s1.lg2.pred, measure = "tpr", x.measure = "fpr")
plot(s1.lg2.ROC)
# Plot precision/recall curve
s1.lg2.precision <- performance(s1.lg2.pred, measure = "prec", x.measure = "rec")
plot(s1.lg2.precision)
# Plot accuracy as function of threshold
s1.lg2.accuracy <- performance(s1.lg2.pred, measure = "acc")
plot(s1.lg2.accuracy)





## [s1.dt1] decision tree
require("rpart")
s1.dt1.model <- rpart(as.factor(isDiscount) ~ nContractQuantity + nInvoicePrice + Channel, data=dataT, control = rpart.control(cp = 0.005, minbucket=30))#, parms = list(loss = matrix(c(0, 2, 1,0), 2, 2)))
summary(s1.dt1.model)
printcp(s1.dt1.model)
plotcp(s1.dt1.model)

#validation
s1.valid$s1.dt1.predProb <- predict(s1.dt1.model, newdata=s1.valid, type="prob")[,"TRUE"]
s1.valid$s1.dt1.predResp <- as.logical(predict(s1.dt1.model, newdata=s1.valid, type="class"))

s1.dt1.table <- table(s1.valid$s1.dt1.predResp, s1.valid$isDiscount)
s1.dt1.table

# plot tree 
plot(s1.dt1.model, uniform=TRUE, main="Regression Tree for Mileage ")
text(s1.dt1.model, use.n=TRUE, all=TRUE)


require(ROCR)
s1.dt1.pred <- prediction(s1.valid$s1.dt1.predProb, s1.valid$isDiscount)
# Plot ROC curve
s1.dt1.ROC <- performance(s1.dt1.pred, measure = "tpr", x.measure = "fpr")
plot(s1.dt1.ROC)
# Plot precision/recall curve
s1.dt1.precision <- performance(s1.dt1.pred, measure = "prec", x.measure = "rec")
plot(s1.dt1.precision)
# Plot accuracy as function of threshold
s1.dt1.accuracy <- performance(s1.dt1.pred, measure = "acc")
plot(s1.dt1.accuracy)




## [s1.dt2] decision tree
require("rpart")
s1.dt2.model <- rpart(as.factor(isDiscount) ~ nContractQuantity + nInvoicePrice + Channel + Territory, data=dataT, control = rpart.control(cp = 0.005, minbucket=30)) #, parms = list(loss = matrix(c(0, 2, 1,0), 2, 2)))
summary(s1.dt2.model)
printcp(s1.dt2.model)
plotcp(s1.dt2.model)

#validation
s1.valid$s1.dt2.predProb <- predict(s1.dt2.model, newdata=s1.valid, type="prob")[,"TRUE"]
s1.valid$s1.dt2.predResp <- as.logical(predict(s1.dt2.model, newdata=s1.valid, type="class"))

s1.dt2.table <- table(s1.valid$s1.dt2.predResp, s1.valid$isDiscount)
s1.dt2.table

# plot tree 
plot(s1.dt2.model, uniform=TRUE, main="Regression Tree for Mileage ")
text(s1.dt2.model, use.n=TRUE, all=TRUE)


require(ROCR)
s1.dt2.pred <- prediction(s1.valid$s1.dt2.predProb, s1.valid$isDiscount)
# Plot ROC curve
s1.dt2.ROC <- performance(s1.dt2.pred, measure = "tpr", x.measure = "fpr")
plot(s1.dt2.ROC)
# Plot precision/recall curve
s1.dt2.precision <- performance(s1.dt2.pred, measure = "prec", x.measure = "rec")
plot(s1.dt2.precision)
# Plot accuracy as function of threshold
s1.dt2.accuracy <- performance(s1.dt2.pred, measure = "acc")
plot(s1.dt2.accuracy)






## [s1.rf1] Random Forest prediction of isDiscount
library(randomForest)
s1.rf1.model <- randomForest(as.factor(isDiscount) ~ nContractQuantity  + nInvoicePrice + Channel, data=dataT)
summary(s1.rf1.model)
print(s1.rf1.model) # view results 
importance(s1.rf1.model) #importance of each predictor
plot(s1.rf1.model)

#validation
s1.valid$s1.rf1.predProb <- predict(s1.rf1.model, newdata=s1.valid, type="prob")[,"TRUE"]
s1.valid$s1.rf1.predResp <- as.logical(predict(s1.rf1.model, newdata=s1.valid, type="response"))

s1.rf1.table <- table(s1.valid$s1.rf1.predResp, s1.valid$isDiscount)
s1.rf1.table

require(ROCR)
s1.rf1.pred <- prediction(s1.valid$s1.rf1.predProb, s1.valid$isDiscount)
# Plot ROC curve
s1.rf1.ROC <- performance(s1.rf1.pred, measure = "tpr", x.measure = "fpr")
plot(s1.rf1.ROC)
# Plot precision/recall curve
s1.rf1.precision <- performance(s1.rf1.pred, measure = "prec", x.measure = "rec")
plot(s1.rf1.precision)
# Plot accuracy as function of threshold
s1.rf1.accuracy <- performance(s1.rf1.pred, measure = "acc")
plot(s1.rf1.accuracy)



## [s1.rf2] Random Forest prediction of isDiscount
#s1.rf2.tune <- tune.randomForest(as.factor(isDiscount) ~ nContractQuantity  + nInvoicePrice + Channel + t1 + t2, data=dataT, mtry=1:5)
s1.rf2.model <- randomForest(as.factor(isDiscount) ~ nContractQuantity  + nInvoicePrice + Channel + t1 + t2, data=dataT)
summary(s1.rf2.model)
print(s1.rf2.model) # view results 
importance(s1.rf2.model) #importance of each predictor
plot(s1.rf2.model)

#validation
s1.valid$s1.rf2.predProb <- predict(s1.rf2.model, newdata=s1.valid, type="prob")[,"TRUE"]
s1.valid$s1.rf2.predResp <- as.logical(predict(s1.rf2.model, newdata=s1.valid, type="response"))

s1.rf2.table <- table(s1.valid$s1.rf2.predResp, s1.valid$isDiscount)
s1.rf2.table

require(ROCR)
s1.rf2.pred <- prediction(s1.valid$s1.rf2.predProb, s1.valid$isDiscount)
# Plot ROC curve
s1.rf2.ROC <- performance(s1.rf2.pred, measure = "tpr", x.measure = "fpr")
plot(s1.rf2.ROC)
# Plot precision/recall curve
s1.rf2.precision <- performance(s1.rf2.pred, measure = "prec", x.measure = "rec")
plot(s1.rf2.precision)
# Plot accuracy as function of threshold
s1.rf2.accuracy <- performance(s1.rf2.pred, measure = "acc")
plot(s1.rf2.accuracy)




## [s1.svm1] SVM classification for isDiscount
require(e1071)

## first-time run of SVM requires optimization of parameters gamma and cost
#s1.svm1.tune <- tune.svm(as.factor(isDiscount) ~ nContractQuantity  + nInvoicePrice + Channel, data = dataT, validation.x=dataV, gamma = 2^(-4:4), cost = 10^(-1:5), probability=TRUE)
#s1.svm1.model <- s1.svm1.tune$best.model

## directly use the obtained optimal parameter gamma=1/8, cost=10
s1.svm1.model <- svm(as.factor(isDiscount) ~ nContractQuantity  + nInvoicePrice + Channel, data = dataT, cost = 10, gamma = 0.125, probability=TRUE)
summary(s1.svm1.model)

s1.valid$s1.svm1.predResp <- as.logical(predict(s1.svm1.model, newdata=s1.valid))
s1.valid$s1.svm1.predProb <- attr(predict(s1.svm1.model, newdata=s1.valid, probability=TRUE), "probabilities")[,"TRUE"]

s1.svm1.table <- table(s1.valid$s1.svm1.predResp, s1.valid$isDiscount)
s1.svm1.table

require(ROCR)
s1.svm1.pred <- prediction(s1.valid$s1.svm1.predProb, s1.valid$isDiscount)
# Plot ROC curve
s1.svm1.ROC <- performance(s1.svm1.pred, measure = "tpr", x.measure = "fpr")
plot(s1.svm1.ROC)
# Plot precision/recall curve
s1.svm1.precision <- performance(s1.svm1.pred, measure = "prec", x.measure = "rec")
plot(s1.svm1.precision)
# Plot accuracy as function of threshold
s1.svm1.accuracy <- performance(s1.svm1.pred, measure = "acc")
plot(s1.svm1.accuracy)


#plot(s1.svm1.model, data = dataV, formula = nContractQuantity ~ nInvoicePrice)






## [s1.svm2] SVM classification for isDiscount
require(e1071)

## first-time run of SVM requires optimization of parameters gamma and cost
#s1.svm2.tune <- tune.svm(as.factor(isDiscount) ~ nContractQuantity  + nInvoicePrice + Channel + Territory, data = dataT, validation.x=dataV, gamma = 2^(-4:4), cost = 10^(-1:5), probability=TRUE)
#s1.svm2.model <- s1.svm2.tune$best.model

## directly use the obtained optimal parameter gamma=1/8, cost=10
s1.svm2.model <- svm(as.factor(isDiscount) ~ nContractQuantity  + nInvoicePrice + Channel + Territory, data = dataT, cost = 10, gamma = 0.125, probability=TRUE)
summary(s1.svm2.model)

s1.valid$s1.svm2.predResp <- as.logical(predict(s1.svm2.model, newdata=s1.valid))
s1.valid$s1.svm2.predProb <- attr(predict(s1.svm2.model, newdata=s1.valid, probability=TRUE), "probabilities")[,"TRUE"]

s1.svm2.table <- table(s1.valid$s1.svm2.predResp, s1.valid$isDiscount)
s1.svm2.table

require(ROCR)
s1.svm2.pred <- prediction(s1.valid$s1.svm2.predProb, s1.valid$isDiscount)
# Plot ROC curve
s1.svm2.ROC <- performance(s1.svm2.pred, measure = "tpr", x.measure = "fpr")
plot(s1.svm2.ROC)
# Plot precision/recall curve
s1.svm2.precision <- performance(s1.svm2.pred, measure = "prec", x.measure = "rec")
plot(s1.svm2.precision)
# Plot accuracy as function of threshold
s1.svm2.accuracy <- performance(s1.svm2.pred, measure = "acc")
plot(s1.svm2.accuracy)


#plot(s1.svm2.model, data = dataV, formula = nContractQuantity ~ nInvoicePrice)





## Tabulating Results

s1.result <- data.frame(Model = c("s1.lg1", "s1.lg2", "s1.dt1", "s1.dt2", "s1.rf1", "s1.rf2", "s1.svm1", "s1.svm2"), 
                        error_I = c(s1.lg1.table[1,2], s1.lg2.table[1,2], s1.dt1.table[1,2], s1.dt2.table[1,2], s1.rf1.table[1,2], s1.rf2.table[1,2], s1.svm1.table[1,2], s1.svm2.table[1,2]),
                        error_II = c(s1.lg1.table[2,1], s1.lg2.table[2,1], s1.dt1.table[2,1], s1.dt2.table[2,1], s1.rf1.table[2,1], s1.rf2.table[2,1], s1.svm1.table[2,1], s1.svm2.table[2,1])
                        )
s1.result$error_total <- s1.result$error_I + s1.result$error_II
s1.result


## Choose the best model
s1.bestmodel.str <- as.character(s1.result[s1.result$error_total == min(s1.result$error_total),]$Model)
s1.bestmodel <- eval(parse(text=paste0(s1.bestmodel.str, ".model")))
summary(s1.bestmodel)

## save the best model for future use
save(s1.bestmodel, file="../data/Step1.bestmodel.RData")




