#NEED TO FIRST SET R WORKING DIRECTORY TO WHERE THE FILES ARE LOCATED!!!
#setwd("~/PATH/TO/DATA/FILE")


### read the data files
data <- read.csv("../data/data.v5.masked.csv", header=TRUE)

## dependent variables
# percentage discount (the ultimate target to predict)
data$Discount <- (data$InvoicePrice - data$NetPrice) / data$InvoicePrice
# binary variable for whether there is a discount
data$isDiscount <- data$InvoicePrice > data$NetPrice





### clean categorical variables Channel and Territory
#Remove rows with <30 observations in “Channel”
c.obs <- ddply(data, .(Channel), .fun = nrow)
c.rm <- c.obs[c.obs$V1<30,]$Channel
data <- data[!(data$Channel %in% c.rm),]
#Remove rows with <30 observations in “Territory”
t.obs <- ddply(data, .(Territory), .fun = nrow)
t.rm <- t.obs[t.obs$V1<30,]$Territory
data <- data[!(data$Territory %in% t.rm),]



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





## Re-factoring categorical variables
# randomForest can only handle categorical variables with <=32 categories;
# the Territory variable violates the constraint;
# here we re-factor Territory into an equivalent pair of categorical variables t1 and t2
refactor <- function (x) {
    t <- as.integer(strsplit(as.character(x$Territory[1]), "_")[[1]][2])
    t1 <- t %/% 32
    t2 <- t %% 32 
    
    x$t1 <- paste("T1", as.character(t1), sep="_")
    x$t2 <- paste("T2", as.character(t2), sep="_")
    
    return(x)
}

dataT <- ddply(dataT, .(Territory), refactor)
dataT$t1 <- as.factor(dataT$t1)
dataT$t2 <- as.factor(dataT$t2)

dataV <- ddply(dataV, .(Territory), refactor)
dataV$t1 <- as.factor(dataV$t1)
dataV$t2 <- as.factor(dataV$t2)
## End of re-factoring



##output masked data
save(dataT, file = "../data/data.v5.masked.Training.RData")
save(dataV, file = "../data/data.v5.masked.Validation.RData")


### end data preparation

