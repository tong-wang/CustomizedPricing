require("plyr")

#NEED TO FIRST SET R WORKING DIRECTORY TO WHERE THE FILES ARE LOCATED!!!
#setwd("~/PATH/TO/DATA/FILE")

### read the data files

## if using the REAL dataset ##
######load("../data/data.v5.masked.Training.RData")

## if using the SIMULATED dataset ##
load("../data/SimulatedDataset_Training.RData")



## load calibrated Decision Tree models from Step 2
load("../data/Step2.DTmodels.RData")


### use Decision Tree models to construct segments and optimize offer for each segment
###     --- Use the Training dataset with observed Discount
###     --- Use s2.dt1 or s2.dt2 to determine segments
###     --- For each segment, use empirical distribution of discount, and find the optimal offer

# A function to set offer by using the empirical distribution of Discount in the segment
optDiscEmpirical <- function (seg) {
    
    # construct CDF for the segment
    seg.cdf <- ecdf(seg$Discount)
    # enumerate over Discount to find the optimal one that gives the highest expected profit
    disc <- seq(0, 1, 0.01)
    optDisc <- disc[which.max((1-disc)*seg.cdf(disc))]
    
    # return the optimal discount
    return(data.frame(optDisc=optDisc))
}


# A function to set offer by using Logit demand in the segment
optDiscLogit <- function (seg) {
    #TODO
}


## Start with training dataset
s3.train <- dataT[dataT$isDiscount, ]

## construct segments
# model dt2
# Find the leaf number the elements belong to, which is used to label segments (to be used in Step 3)
s2.dt2.model$frame$yval <- as.numeric(rownames(s2.dt2.model$frame))
s3.train$s2.dt2.segment <- as.factor(predict(s2.dt2.model, newdata=s3.train))
plot(s3.train$s2.dt2.segment)

## search for optimal discount for each segment
s2.dt2.optDisc <- ddply(s3.train, .(s2.dt2.segment), optDiscEmpirical)



# model dt1
# Find the leaf number the elements belong to, which is used to label segments (to be used in Step 3)
s2.dt1.model$frame$yval <- as.numeric(rownames(s2.dt1.model$frame))
s3.train$s2.dt1.segment <- as.factor(predict(s2.dt1.model, newdata=s3.train))
plot(s3.train$s2.dt1.segment)

# search for optimal discount for each segment
s2.dt1.optDisc <- ddply(s3.train, .(s2.dt1.segment), optDiscEmpirical)


## Save result
save(s2.dt1.optDisc, s2.dt2.optDisc, file="../data/Step3.optDisc.RData")
