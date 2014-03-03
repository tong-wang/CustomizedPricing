require("plyr")
### Simulate a dataset for customized pricing algorithms
### --- dataset mostly mimics the pharma dataset
### --- but it consists the true minimum acceptable discount (minDisc) and the rejected offers


#NEED TO FIRST SET R WORKING DIRECTORY TO WHERE THE FILES ARE LOCATED!!!
#setwd("~/PATH/TO/DATA/FILE") 
setwd("~/Dropbox/Consulting/Acceval/CustomizedPricing.git/src")


## Parameters for simulating the dataset
# dimensionality
n.channel <- 10     # number of channels
n.territory <- 100  # number of territories

# number of observations in each Channel-Territory is logN(n.obs.mean, n.obs.sd)
n.obs.mean <- 5   # average number of observations for each channel-territory pair
n.obs.sd <- 1   # average number of observations for each channel-territory pair


# ContractQuantity of each deal ~ logN(quant.mean, quant.sd)
quant.mean <- 3
quant.sd <- 1.5

# InvoicePrice of each deal ~ logN(price.mean, price.sd)
price.mean <- 5
price.sd <- 0.5

# Minimum acceptable discount: minDisc ~ N(minDisc.mean, minDisc.mean*minDisc.cv), truncated within [0, 1]
# minDisc.mean depends on
#       (1) Channel effect ~ N(minDisc.c.mean, minDisc.c.sd), for each channel;
#       (2) Territory effect ~ N(minDisc.t.mean, minDisc.t.sd), for each territory;
#       (1) Channel-Territory effect: ~ N(minDisc.ct.mean, minDisc.ct.sd), for each channel-territory pair;
#       (2) Quantity effect: quant.coef * log(quant), quant.coef ~ N(quant.coef.mean, quant.coef.sd), for each deal; 
#       (3) Price effect: price.coef * log(price), price.coef ~ N(price.coef.mean, price.coef.sd), for each deal; 
#       (4) a constant term: minDisc.intercep

# Channel effect parameters
minDisc.c.mean <- sample(c(-0.2, -0.1, 0, 0.1, 0.2), n.channel, replace=TRUE)
minDisc.c.sd <- 0.02

# Territory effect parameters
minDisc.t.mean <- sample(c(-0.2, -0.1, 0, 0.1, 0.2), n.territory, replace=TRUE) 
minDisc.t.sd <- 0.02

# Channel-territory effect parameters
minDisc.ct.mean <- 0
minDisc.ct.sd <- 0.1

# quant.coef ~ N(quant.coef.mean, quant.coef.sd)
quant.coef.mean <- 0.06
quant.coef.sd <- 0.015

# price.coef ~ N(price.coef.mean, price.coef.sd)
price.coef.mean <- 0.03
price.coef.sd <- 0.0075

# constant term is fixed
minDisc.intercep <- -0.45


# minDisc.cv is Channel-Territory specific, and is drawn from U(minDisc.cv.min, minDisc.cv.max)
minDisc.cv.min <- 0.2
minDisc.cv.max <- 0.3




# Actual discount offered to customers: disc ~ N(minDisc.mean + disc.mean, disc.sd), which is then truncated within [0, 0.7]
disc.mean <- 0
disc.sd <- 0.1
disc.max <- 0.7





## Construct the dataset

c.effect <- rnorm(n.channel, mean=minDisc.c.mean, sd=minDisc.c.sd)
t.effect <- rnorm(n.territory, mean=minDisc.t.mean, sd=minDisc.t.sd)

dataset <- data.frame()
for (c in 1:n.channel) {
    for (t in 1:n.territory) {
        n.obs <- ceiling(exp(rnorm(1, n.obs.mean, n.obs.sd)))
        
        quant <- exp(rnorm(n.obs, mean=quant.mean, sd=quant.sd))
        price <- exp(rnorm(n.obs, mean=price.mean, sd=price.sd))
        
        quant.coef <- rnorm(n.obs, mean=quant.coef.mean, sd=quant.coef.sd)
        price.coef <- rnorm(n.obs, mean=price.coef.mean, sd=price.coef.sd)
        
        ct.effect <- rnorm(1, minDisc.ct.mean, minDisc.ct.sd)
        
        minDisc.mean <- c.effect[c] + t.effect[t] + ct.effect + quant.coef * log(quant) + price.coef * log(price) + minDisc.intercep
        minDisc.cv <- runif(1, minDisc.cv.min, minDisc.cv.max)
        minDisc.sd <- abs(minDisc.mean) * minDisc.cv
        minDisc <- rnorm(n.obs, mean=minDisc.mean, sd=minDisc.sd)
        minDisc <- ifelse(minDisc<0, 0, minDisc)
        minDisc <- ifelse(minDisc>1, 1, minDisc)
        
        disc <- rnorm(n.obs, mean=minDisc.mean+disc.mean, sd=disc.sd)
        disc <- ifelse(disc<0, 0, disc)
        disc <- ifelse(disc>disc.max, disc.max, disc)
        
        buy <- ifelse(disc>=minDisc, TRUE, FALSE)
        
        chunk <- data.frame(Channel=rep(paste("Channel", as.character(c), sep="_"), n.obs),
                            Territory=rep(paste("Territory", as.character(t), sep="_"), n.obs),
                            ObsID=seq(1, n.obs),
                            ContractQuantity=ceiling(quant),
                            InvoicePrice=round(price, 2),
                            minDisc=minDisc,
                            Discount=disc,
                            Buy=buy
                            )
        
        # get rid of all the boundary values
        chunk <- chunk[chunk$Discount < disc.max, ]
        
        dataset <- rbind(dataset, chunk)
    }
    
}



################################################
## inspect the simulated dataset
dataset$isDiscount <- (dataset$Discount > 0)
dataset$ExpectDiscount <- (dataset$minDisc > 0)
summary(dataset)

hist(dataset[dataset$Buy,]$Discount)
hist(log(dataset[dataset$Buy,]$InvoicePrice))
hist(log(dataset[dataset$Buy,]$ContractQuantity))

# number of observations
hist(ddply(dataset[dataset$Buy,], .(Channel, Territory), nrow)$V1, breaks=50)

# average discount by channel
ddply(dataset[dataset$Buy,], .(Channel), summarize, mean=mean(Discount))

# average discount by territory
ddply(dataset[dataset$Buy,], .(Territory), summarize, mean=mean(Discount))


disc.model <- lm(Discount~log(ContractQuantity)+log(InvoicePrice) + Channel + Territory,data=dataset[dataset$isDiscount==TRUE,])
summary(disc.model)

################################################



## save the dataset
save(dataset, file="../data/SimulatedDataset.RData")

# generate a sub-sample of the compelete dataset for benchmarking
RandomIndex = sample(nrow(dataset), 30000)
dataset.sample <- dataset[RandomIndex, ]
save(dataset.sample, file="../data/SimulatedDataset_Sample.RData")

# only observe buys
data <- dataset.sample[dataset.sample$Buy,]



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




## write to files
save(dataT, file = "../data/SimulatedDataset_Training.RData")
save(dataV, file = "../data/SimulatedDataset_Validation.RData")


### end data preparation

