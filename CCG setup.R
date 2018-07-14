####~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
### Coin Capacity Game Participant Setup
###
### Purpose
###  * To generate user and stimuli sequence tables for use in the "coin
###    collecting game"
### 
### Notes:
###  * The CCG is an experimental paradigm dealing with capacity investments.
###    the source code for the game is here 
###    https://github.com/swiftsam/CoinCollectingGame
###
### Primary Creator(s): Sam Swift (samswift@berkeley.edu)
####~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
library(reshape2)
library(ggplot2)
library(data.table)

# Generate distributions
# gamma1   <- round(rgamma(n=100, shape= .25, scale=100))
# gamma2   <- round(rgamma(n=100, shape= .5, scale=19))
# gamma3   <- round(rgamma(n=100, shape=  1, scale=6.5))
gamma.r <- round(rgamma(n = 100000, shape = 1, scale=6.3))
uniform <- round(runif(n  = 100000, min  =-.5, max=10.5))
normal  <- round(rnorm(n  = 100000, mean  = 5, sd=2))

# Combine and add the mirrored gamma.r
dists.raw <- data.frame(gamma.r, uniform, normal)
dists.raw$gamma.l <- 10-dists.raw$gamma.r
dists.raw.melt <- melt(dists.raw)

#ggplot(dists.raw.melt, aes(value, color=variable)) + geom_density(position="dodge", size=1.5)

## Truncate distributions by assigning mass outside [0,10] to those outside bins
dists.trunc <- dists.raw.melt
dists.trunc$value[dists.trunc$value > 10] <- 10
dists.trunc$value[dists.trunc$value < 0]  <- 0
#summary(dists.trunc)
ggplot(dists.trunc, aes(value, fill=variable)) + geom_histogram(position="dodge", size=1.5)

# Confirm the means are all 5
dists.trunc <- data.table(dists.trunc)
dists.trunc[,list(mean = mean(value)), by=variable]

# Determine how many observations of each value for each distribution
# counts scaled to a trial with 100 rounds from the original 100k distribution
n.cond.value <- dists.trunc[,list(count = round(.N / 1000)), by=c("variable","value")]
n.cond.value[,list(sum=sum(count)),by=variable]
ggplot(n.cond.value, aes(value, count, fill=variable)) + geom_bar(position="dodge", stat="identity")

# Define the actual sets for each condition
sets <- n.cond.value[, list(coins = rep(value,count)), by=c("variable","value")]
sets$value <- NULL
sets <- rbind(sets, data.frame(variable="uniform", coins= 5)) # uniform needs 1 extra since 9*11=99

# intialize output data.frame
seqs <- data.table()

# generate N participant sequences for each condition
conds <- levels(sets$variable)
for(cond in conds){
  for(i in 1:25){
    coins <- sets[variable == cond, coins]   
    coins <- coins[order(runif(length(coins)))]
    
    user.id <- paste(substr(cond,1,1),
                     substr(cond,nchar(cond),nchar(cond)),
                     paste(sample(c(letters, LETTERS), 8, replace=TRUE),
                           collapse=""), 
                     sep="")
    
    seqs <- rbindlist(list(seqs,
                       data.table("UserID" = user.id,
                                  "Round"  = 1:100,
                                  "Coins"  = coins)))    
  }  
}

write.csv(seqs, "~/Desktop/sequences.csv", row.names=F)

# write user table with userIDs generated above
users <- data.frame("ID" = unique(seqs$UserID),
                    "Name" = unique(seqs$UserID),
                    "IP" = "",
                    "LastActivity" = "",
                    "LastScreen" = "",
                    "FirstActivity" = "",
                    "showTCP" = 0,
                    "showTCC" = 0,
                    "showTCL" = 0,
                    "showCS"  = 0)

write.csv(users, "~/Desktop/users.csv", row.names=F)









