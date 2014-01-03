Sys.setenv(LANG = "en")

#NEED TO FIRST SET R WORKING DIRECTORY TO WHERE THE FILES ARE LOCATED!!!
#setwd("~/PATH/TO/DATA/FILE")

### read the data files
data <- read.csv("data.v5.masked.csv", header=TRUE)





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


## split Training and Validation datasets
# randomly sample 70% of the data for training, the rest for validation
Proportion.T = 0.7
N = nrow(data)
RandomIndex.T = sample(N, as.integer(Proportion.T*N))
dataT <- data[RandomIndex.T, ]
dataV <- data[-RandomIndex.T, ]

###########
## test if the data sets are good
levels(dataT$Channel)==levels(dataV$Channel)
levels(dataT$Territory)==levels(dataV$Territory)

logistic2.model <- glm(isDiscount ~ nContractQuantity + nInvoicePrice + Channel + Territory, data=dataT, family=binomial())
summary(logistic2.model)

logistic2.valid <- dataV[c("Channel", "Territory", "nContractQuantity", "nInvoicePrice", "Discount", "isDiscount")]
logistic2.valid$predP <- predict(logistic2.model, newdata=logistic2.valid, type="response")
############


##output masked data
write.csv(dataT, "data.v5.masked.Training.csv", quote=FALSE, row.names=FALSE)
write.csv(dataV, "data.v5.masked.Validation.csv", quote=FALSE, row.names=FALSE)

### end data preparation

