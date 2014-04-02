require("plyr")
require("ggplot2")
require("stringr")

#NEED TO FIRST SET R WORKING DIRECTORY TO WHERE THE FILES ARE LOCATED!!!
#setwd("~/PATH/TO/DATA/FILE")

### read the data files

## if using the REAL dataset ##
######load("../data/data.v5.masked.Training.RData")
######load("../data/data.v5.masked.Validation.RData")

## if using the SIMULATED dataset ##
# load the dataset for benchmarking
load("../data/SimulatedDataset_Benchmarking.RData")

load("../data/Step1.bestmodel.RData")
load("../data/Step2.bestmodel.RData")
load("../data/Step2.DTmodels.RData")
load("../data/Step3.optDisc.RData")

load("../data/n1.optDisc.RData")
load("../data/n2.lm.RData")

s4.valid <- dataB

### Naive 1
# construct naive segments (all combinations of Channel and Territory)
apply.n1 <- function (rows) {
    
    c <- str_split(rows[1,]$Channel, "_")[[1]][2]
    t <- str_split(rows[1,]$Territory, "_")[[1]][2]
    ct <- paste(c, t, sep="-")

    if (nrow(n1.optDisc[n1.optDisc$CT==ct,]) > 0)
        rows$n1.offer <- n1.optDisc[n1.optDisc$CT==ct,]$optDisc
    else
        rows$n1.offer <- 0
    
    return(rows)
}

# apply optDisc from Naive 1 model
s4.valid <- ddply(s4.valid, .(Channel, Territory), apply.n1)
s4.valid$n1.buy <- s4.valid$n1.offer >= s4.valid$minDisc


### Naive 2
# construct naive segments (all combinations of Channel and Territory)
apply.n2 <- function (rows) {
    
    c <- str_split(rows[1,]$Channel, "_")[[1]][2]
    t <- str_split(rows[1,]$Territory, "_")[[1]][2]
    ct <- paste(c, t, sep="-")
    
    rows$n2.offer <- NA
    
    if (ct %in% names(n2.lm)) {
        lm <- n2.lm[[eval(ct)]][[1]]
        pred <- predict(lm, rows)
        offer <- pred + summary(lm)$sigma
        rows$n2.offer <- ifelse(offer<0, 0, ifelse(offer>0.7, 0.7, offer))
    }
    
    # set offer to 0.1 if lm cannot predict
    if (is.na(rows$n2.offer))
        rows[is.na(rows$n2.offer),]$n2.offer <- 0.1
    
    return(rows)
}

# apply optDisc from Naive 2 model
s4.valid <- ddply(s4.valid, .(Channel, Territory), apply.n2)
s4.valid$n2.buy <- s4.valid$n2.offer >= s4.valid$minDisc



### Solution 1: use best models from Step 1 and 2
###     --- Use s1.bestmodel to decide whether to offer discount: if TRUE, offer 0
###     --- Use s2.bestmodel to predict discount and set offer equal to the predicted discount

#s4.valid$s1.best <- as.logical(predict(s1.bestmodel, newdata=s4.valid))
## conservative cutoff to reduce false negative rate
## cutoff=0.25 is determined by s1.svm2.accuracy plot in Step 1
s4.valid$s1.best <- attr(predict(s1.bestmodel, newdata=s4.valid, probability=TRUE), "probabilities")[,"TRUE"]> 0.25  

s4.valid$s2.best <- predict(s2.bestmodel, newdata=s4.valid) + 0.1 ## CONSERVATIVE, 0.1 is the RMSE of the model, obtained in Step 2
s4.valid$s2.best <- ifelse(s4.valid$s2.best<0, 0, s4.valid$s2.best) #discount should be non-negative
s4.valid$so1.offer <- ifelse(s4.valid$s1.best, s4.valid$s2.best, 0)
s4.valid$so1.buy <- s4.valid$so1.offer >= s4.valid$minDisc


### Solution 2: use the optimal empirical discount from Decision Tree models
# so2. using model dt2
s2.dt2.model$frame$yval <- as.numeric(rownames(s2.dt2.model$frame))
s4.valid$s2.dt2.segment <- as.factor(predict(s2.dt2.model, newdata=s4.valid))
s4.valid$so2.offer <- merge(s4.valid, s2.dt2.optDisc, by=c("s2.dt2.segment"))$optDisc
s4.valid$so2.offer <- ifelse(s4.valid$s1.best, s4.valid$so2.offer, 0)
s4.valid$so2.buy <- s4.valid$so2.offer >= s4.valid$minDisc


# so2b. using model dt1
s2.dt1.model$frame$yval <- as.numeric(rownames(s2.dt1.model$frame))
s4.valid$s2.dt1.segment <- as.factor(predict(s2.dt1.model, newdata=s4.valid))
s4.valid$so2b.offer <- merge(s4.valid, s2.dt1.optDisc, by=c("s2.dt1.segment"))$optDisc
s4.valid$so2b.offer <- ifelse(s4.valid$s1.best, s4.valid$so2b.offer, 0)
s4.valid$so2b.buy <- s4.valid$so2b.offer >= s4.valid$minDisc





## Tabulating Results

s4.result <- data.frame(Solution = c("current", "n1", "n2", "so1", "so2", "so2b"), 
                        BuyRate = c(sum(s4.valid$Buy), sum(s4.valid$n1.buy), sum(s4.valid$n2.buy), sum(s4.valid$so1.buy), sum(s4.valid$so2.buy), sum(s4.valid$so2b.buy)) / nrow(s4.valid),
                        DiscountRate = c(nrow(s4.valid[s4.valid$Discount>0,]), nrow(s4.valid[s4.valid$n1.offer>0,]), nrow(s4.valid[s4.valid$n2.offer>0,]), nrow(s4.valid[s4.valid$so1.offer>0,]), nrow(s4.valid[s4.valid$so2.offer>0,]), nrow(s4.valid[s4.valid$so2b.offer>0,]) )/nrow(s4.valid),
                        Sales = c(sum(s4.valid$Buy*s4.valid$ContractQuantity), sum(s4.valid$n1.buy*s4.valid$ContractQuantity), sum(s4.valid$n2.buy*s4.valid$ContractQuantity), sum(s4.valid$so1.buy*s4.valid$ContractQuantity), sum(s4.valid$so2.buy*s4.valid$ContractQuantity), sum(s4.valid$so2b.buy*s4.valid$ContractQuantity)),
                        Revenue = c(sum(s4.valid$Buy*s4.valid$ContractQuantity*s4.valid$InvoicePrice*(1-s4.valid$Discount)), sum(s4.valid$n1.buy*s4.valid$ContractQuantity*s4.valid$InvoicePrice*(1-s4.valid$n1.offer)), sum(s4.valid$n2.buy*s4.valid$ContractQuantity*s4.valid$InvoicePrice*(1-s4.valid$n2.offer)), sum(s4.valid$so1.buy*s4.valid$ContractQuantity*s4.valid$InvoicePrice*(1-s4.valid$so1.offer)), sum(s4.valid$so2.buy*s4.valid$ContractQuantity*s4.valid$InvoicePrice*(1-s4.valid$so2.offer)), sum(s4.valid$so2b.buy*s4.valid$ContractQuantity*s4.valid$InvoicePrice*(1-s4.valid$so2b.offer)))
)

s4.result

## Save result
save(s4.result, file="../data/Step4.result.RData")






#### [so1] Performance measures for each step
summary(s4.valid)
table(s4.valid$s1.best, s4.valid$ExpectDiscount)

rmse(s4.valid[s4.valid$isDiscount,]$s2.best, s4.valid[s4.valid$isDiscount,]$minDisc)

rmse(s4.valid$so1.offer, s4.valid$minDisc)
plot(s4.valid$so1.offer, s4.valid$minDisc)
abline(a=0, b=1)
