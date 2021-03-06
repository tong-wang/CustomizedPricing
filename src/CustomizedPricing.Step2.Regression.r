require("plyr")
require("ggplot2")

require("hydroGOF")



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
### STEP 1. Regression: How much discount to offer?
### Need to test multiple regression tools: linear regression, decision tree, random forest, and SVM
###########################

## Prepare Validation dataset
s2.valid <- dataV[c("Channel", "Territory", "t1", "t2", "nContractQuantity", "nInvoicePrice", "Discount", "isDiscount", "minDisc")]
s2.valid <- s2.valid[s2.valid$isDiscount,]



## Train the models

# [s2.lm1] linear regression for Discount
s2.lm1.model <- lm(Discount ~ nContractQuantity + nInvoicePrice + Channel, data=dataT[dataT$isDiscount, ])
summary(s2.lm1.model)


# validation
s2.valid$s2.lm1.pred <- predict(s2.lm1.model, newdata=s2.valid)
s2.valid$s2.lm1.pred <- ifelse(s2.valid$s2.lm1.pred<0, 0, s2.valid$s2.lm1.pred) #discount should be non-negative
## Calculating RMSE as the accuracy measure
s2.lm1.RMSE <- rmse(s2.valid$s2.lm1.pred, s2.valid$Discount)
plot(s2.valid$Discount, s2.valid$s2.lm1.pred)





# [s2.lm2] linear regression for Discount
s2.lm2.model <- lm(Discount ~ nContractQuantity + nInvoicePrice + Channel + Territory, data=dataT[dataT$isDiscount, ])
summary(s2.lm2.model)


# validation
s2.valid$s2.lm2.pred <- predict(s2.lm2.model, newdata=s2.valid)
s2.valid$s2.lm2.pred <- ifelse(s2.valid$s2.lm2.pred<0, 0, s2.valid$s2.lm2.pred) #discount should be non-negative
## Calculating RMSE as the accuracy measure
s2.lm2.RMSE <- rmse(s2.valid$s2.lm2.pred, s2.valid$Discount)
plot(s2.valid$Discount, s2.valid$s2.lm2.pred)





# [s2.dt1] rpart regression for Discount with Territory
require("rpart")
s2.dt1.model <- rpart(Discount ~ nContractQuantity + nInvoicePrice + Channel, data=dataT[dataT$isDiscount,], method='anova', cp=0.005, minbucket=30)
summary(s2.dt1.model)


# validation
s2.valid$s2.dt1.pred <- predict(s2.dt1.model, newdata=s2.valid)
s2.valid$s2.dt1.pred <- ifelse(s2.valid$s2.dt1.pred<0, 0, s2.valid$s2.dt1.pred) #discount should be non-negative
## Calculating RMSE as the accuracy measure
s2.dt1.RMSE=rmse(s2.valid$s2.dt1.pred, s2.valid$Discount)
plot(s2.valid$s2.dt1.pred, s2.valid$Discount)






# [s2.dt2] rpart regression for Discount with Territory
require("rpart")
s2.dt2.model <- rpart(Discount ~ nContractQuantity + nInvoicePrice + Channel + Territory, data=dataT[dataT$isDiscount,], method='anova', cp=0.005, minbucket=30)
summary(s2.dt2.model)


# validation
s2.valid$s2.dt2.pred <- predict(s2.dt2.model, newdata=s2.valid)
s2.valid$s2.dt2.pred <- ifelse(s2.valid$s2.dt2.pred<0, 0, s2.valid$s2.dt2.pred) #discount should be non-negative
## Calculating RMSE as the accuracy measure
s2.dt2.RMSE=rmse(s2.valid$s2.dt2.pred, s2.valid$Discount)
plot(s2.valid$s2.dt2.pred, s2.valid$Discount)


## Plotting of the Rpart
#require("rpart.plot")
#prp(s2.dt2.model)
#require("partykit")
#s2.dt2.model.party <- as.party(s2.dt2.model)
#plot(s2.dt2.model.party, main="Plot of Decision Tree", type="simple")




# [s2.rf1] randomForest regression for Discount
s2.rf1.model <- randomForest(Discount ~ nContractQuantity + nInvoicePrice + Channel , data=dataT[dataT$isDiscount,], do.trace=TRUE)
summary(s2.rf1.model)
print(s2.rf1.model) # view results 
importance(s2.rf1.model) #importance of each predictor
plot(s2.rf1.model)

# validation
s2.valid$s2.rf1.pred <- predict(s2.rf1.model, newdata=s2.valid)
s2.valid$s2.rf1.pred <- ifelse(s2.valid$s2.rf1.pred<0, 0, s2.valid$s2.rf1.pred) #discount should be non-negative
s2.rf1.RMSE <- rmse(s2.valid$s2.rf1.pred, s2.valid$Discount)
plot(s2.valid$s2.rf1.pred, s2.valid$Discount)





# [s2.rf2] randomForest regression for Discount
s2.rf2.model <- randomForest(Discount ~ nContractQuantity + nInvoicePrice + Channel + t1 + t2 , data=dataT[dataT$isDiscount,], do.trace=TRUE)
summary(s2.rf2.model)
print(s2.rf2.model) # view results 
importance(s2.rf2.model) #importance of each predictor
plot(s2.rf2.model)


# validation
s2.valid$s2.rf2.pred <- predict(s2.rf2.model, newdata=s2.valid)
s2.valid$s2.rf2.pred <- ifelse(s2.valid$s2.rf2.pred<0, 0, s2.valid$s2.rf2.pred) #discount should be non-negative
s2.rf2.RMSE <- rmse(s2.valid$s2.rf2.pred, s2.valid$Discount)
plot(s2.valid$s2.rf2.pred, s2.valid$Discount)






# [s2.svm2] SVM regression
#s2.svm1.tune <- tune.svm(Discount ~ nContractQuantity  + nInvoicePrice + Channel, data = dataT[dataT$isDiscount,], type="eps-regression", gamma = 2^(-8:0), cost = 10^(-1:4))
#s2.svm1.model <- s2.svm1.tune$best.model
s2.svm1.model <- svm(Discount ~ nContractQuantity  + nInvoicePrice + Channel, data = dataT[dataT$isDiscount,], type="eps-regression", cost = 10, gamma = 2^(-7))
summary(s2.svm1.model)


# validation
s2.valid$s2.svm1.pred <- predict(s2.svm1.model, newdata=s2.valid)
s2.valid$s2.svm1.pred <- ifelse(s2.valid$s2.svm1.pred<0, 0, s2.valid$s2.svm1.pred) #discount should be non-negative
s2.svm1.RMSE <- rmse(s2.valid$s2.svm1.pred, s2.valid$Discount)
plot(s2.valid$s2.svm1.pred, s2.valid$Discount)





# [s2.svm2] SVM regression
#s2.svm2.tune <- tune.svm(Discount ~ nContractQuantity  + nInvoicePrice + Channel + Territory, data = dataT[dataT$isDiscount,], type="eps-regression", gamma = 2^(-5:5), cost = 10^(-1:4))
#s2.svm2.model <- s2.svm2.tune$best.model
s2.svm2.model <- svm(Discount ~ nContractQuantity  + nInvoicePrice + Channel + Territory, data = dataT[dataT$isDiscount,], type="eps-regression", cost = 10, gamma = 0.0625)
summary(s2.svm2.model)


# validation
s2.valid$s2.svm2.pred <- predict(s2.svm2.model, newdata=s2.valid)
s2.valid$s2.svm2.pred <- ifelse(s2.valid$s2.svm2.pred<0, 0, s2.valid$s2.svm2.pred) #discount should be non-negative
s2.svm2.RMSE <- rmse(s2.valid$s2.svm2.pred, s2.valid$Discount)
plot(s2.valid$s2.svm2.pred, s2.valid$Discount)





### Tabulating Results
s2.result <- data.frame(Model = c("s2.lm1", "s2.lm2", "s2.dt1", "s2.dt2", "s2.rf1", "s2.rf2", "s2.svm1", "s2.svm2"), 
                        RMSE = c(s2.lm1.RMSE, s2.lm2.RMSE, s2.dt1.RMSE, s2.dt2.RMSE, s2.rf1.RMSE, s2.rf2.RMSE, s2.svm1.RMSE, s2.svm2.RMSE)
)
s2.result


## Choose the best model
s2.bestmodel.str <- as.character(s2.result[s2.result$RMSE == min(s2.result$RMSE),]$Model)
s2.bestmodel <- eval(parse(text=paste0(s2.bestmodel.str, ".model")))
summary(s2.bestmodel)

## save the best model for future use
save(s2.bestmodel, file="../data/Step2.bestmodel.RData")

## save the Decision Tree model for Step 3 use
save(s2.dt1.model, s2.dt2.model, file="../data/Step2.DTmodels.RData")
