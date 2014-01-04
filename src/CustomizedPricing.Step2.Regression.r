Sys.setenv(LANG = "en")
require("plyr")
require("ggplot2")

#NEED TO FIRST SET R WORKING DIRECTORY TO WHERE THE FILES ARE LOCATED!!!
#setwd("~/PATH/TO/DATA/FILE")

### read the data files
load("../data/data.v5.masked.Training.RData")
load("../data/data.v5.masked.Validation.RData")

load("../data/Step1.output.RData")




###########################
### STEP2. We are extending the work from the source.
## We are considering rpart as the base algorithm from base. 
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
#svm.tune2 <- tune.svm(Discount ~ nContractQuantity  + nInvoicePrice + Channel + Territory, data = dataT[dataT$isDiscount,], type="eps-regression", gamma = 2^(-1:5), cost = 10^(1:4))
#svm.model2 <- svm.tune2$best.model
#svm.model2 <- svm(Discount ~ nContractQuantity  + nInvoicePrice + Channel + Territory, data = dataT[dataT$isDiscount,], type="eps-regression", cost = 10000, gamma = 8)
#summary(svm.model2)

#svm.valid2 <- svm.valid[svm.valid$pred1==1,][c("RecordID", "Channel", "Territory", "nContractQuantity", "nInvoicePrice", "Discount", "isDiscount")]
#svm.valid2$pred <- predict(svm.model2, newdata=svm.valid2)
#summary(svm.valid2)


#plot(svm.valid2$Discount, svm.valid2$pred, xlim=c(0,1), ylim=c(0,1))
#qplot(x=Discount, y=pred, colour=Channel, data=svm.valid2, xlim=c(0,1), ylim=c(0,1))





### Tabulating Results

Results=data.frame(Model_Type=c("Linear Regression","rpart With Territory","rpart Without Territory","randomForest"), RMSE = c(RMSE_linear,RMSE_rpart,RMSE_rpartT,RMSE_rF))
Results
## Rpart without Territory code seems to be best following Ozam razor's rule
