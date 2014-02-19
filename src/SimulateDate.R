### Simulate a dataset for customized pricing algorithms
### --- dataset mostly mimics the pharma dataset
### --- but it consists the true minimum acceptable discount (minDisc) and the rejected offers

## Parameters for simulating the dataset
# dimensionality
n.channel <- 10     # number of channels
n.territory <- 100  # number of territories
n.obs.mean <- 100   # average number of observations for each channel-territory pair


# ContractQuantity of each deal ~ N(quant.mean, quant.sd)
quant.mean <- 3
quant.sd <- 1.5

# InvoicePrice of each deal ~ N(price.mean, price.sd)
price.mean <- 4.5
price.sd <- 1

# Minimum acceptable discount: minDisc ~ N(minDisc.mean, minDisc.sd), truncated within [0, 1]
# minDisc.mean consists of (1) Channel-Territory effect: a draw from N(minDisc.mean.mean, minDisc.mean.sd), which is the same across the observations in each channel-territory; (2) Quantity effect: quant.coef * log(quant); (3) Price effect: price.coef * log(price); and (4) a constant term: minDisc.intercep
# minDisc.sd is Channel-Territory specific, and is drawn from U(minDisc.sd.min, minDisc.sd.max)
minDisc.mean.mean <- 0
minDisc.mean.sd <- 0.05
minDisc.sd.min <- 0.05
minDisc.sd.max <- 0.3

# quant.coef ~ N(quant.coef.mean, quant.coef.sd)
quant.coef.mean <- 0.06
quant.coef.sd <- 0.02

# price.coef ~ N(price.coef.mean, price.coef.sd)
price.coef.mean <- 0.03
price.coef.sd <- 0.01

# constant term is fixed
minDisc.intercep <- -0.3


# Actual discount offered to customers: disc ~ N(disc.mean, disc.sd), which is truncated within [0, 0.7]
disc.mean <- 0.1
disc.sd <- 0.2
disc.max <- 0.7


## Construct the dataset
dataset <- data.frame()
for (c in 1:n.channel) {
    for (t in 1:n.territory) {
        n.obs <- rpois(1, n.obs.mean)
        
        quant <- exp(rnorm(n.obs, mean=quant.mean, sd=quant.sd))
        price <- exp(rnorm(n.obs, mean=price.mean, sd=price.sd))
        
        quant.coef <- rnorm(n.obs, mean=quant.coef.mean, sd=quant.coef.sd)
        price.coef <- rnorm(n.obs, mean=price.coef.mean, sd=price.coef.sd)
        
        
        minDisc.mean <- rnorm(1, minDisc.mean.mean, minDisc.mean.sd) + quant.coef * log(quant) + price.coef * log(price) + minDisc.intercep
        minDisc.sd <- runif(1, minDisc.sd.min, minDisc.sd.max)
        minDisc <- rnorm(n.obs, mean=minDisc.mean, sd=minDisc.sd)
        minDisc <- ifelse(minDisc<0, 0, minDisc)
        minDisc <- ifelse(minDisc>1, 1, minDisc)
        
        disc <- rnorm(n.obs, mean=disc.mean, sd=disc.sd)
        disc <- ifelse(disc<0, 0, disc)
        disc <- ifelse(disc>disc.max, disc.max, disc)
        buy <- ifelse(disc>=minDisc, TRUE, FALSE)
        
        chunk <- data.frame(Channel=rep(paste("Channel", as.character(c), sep="_"), n.obs),
                            Territory=rep(paste("Territory", as.character(t), sep="_"), n.obs),
                            ObsID=seq(1, n.obs),
                            ContractQuantity=as.integer(quant),
                            InvoicePrice=round(price, 2),
                            minDisc=minDisc,
                            Discount=disc,
                            Buy=buy
                            )
        
        dataset <- rbind(dataset, chunk)
    }
    
}




hist(dataset[dataset$Buy,]$Discount)
hist(log(dataset[dataset$Buy,]$InvoicePrice))
hist(log(dataset[dataset$Buy,]$ContractQuantity))
