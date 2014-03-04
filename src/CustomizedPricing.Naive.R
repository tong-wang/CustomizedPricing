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
load("../data/SimulatedDataset_Training.RData")
load("../data/SimulatedDataset_Validation.RData")


##### Naive solutions #######

# use the Training dataset
n.data <- dataT

# construct naive segments (all combinations of Channel and Territory)
ct.construct <- function (rows) {
    
    c <- str_split(rows[1,]$Channel, "_")[[1]][2]
    t <- str_split(rows[1,]$Territory, "_")[[1]][2]
    ct <- paste(c, t, sep="-")
    rows$CT <- ct
    
    return(rows)
}

n.data <- ddply(n.data, .(Channel, Territory), ct.construct)
n.data$CT <- as.factor(n.data$CT)


### N1. Use empirial distribution of each segment for optimal discount
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

# search for optimal discount for each segment
n1.optDisc <- ddply(n.data, .(CT), optDiscEmpirical)

save(n1.optDisc, file="../data/n1.optDisc.RData")






### N2. For each segment, construct a linear regression model
lm.construct <- function (seg) {
    seg.lm <- lm(Discount ~ nContractQuantity + nInvoicePrice, data=seg)
    out <- list(seg.lm)
    names(out) <- seg[1,]$CT
    return(out)
}

n2.lm <- dlply(n.data, .(CT), lm.construct)

save(n2.lm, file="../data/n2.lm.RData")
