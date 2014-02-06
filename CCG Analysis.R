####~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
### Coin Capacity Game Analysis
###
### Purpose
###  * Analysis of participant responses in CCG
### 
### Notes:
###  * The CCG is an experimental paradigm dealing with capacity investments.
###    the source code for the game is here 
###    https://github.com/swiftsam/CoinCollectingGame
###
### Primary Creator(s): Sam Swift (samswift@berkeley.edu)
####~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

library(ggplot2)
library(data.table)
library(Rmisc)

kDataDir <- file.path("~","Desktop","ccg3b")

se <- function(x) sqrt(var(x)/length(x))

####~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#### Data Prep ####
####~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Read data from the four tables exported by the CCG program
users   <- data.table(read.csv(file.path(kDataDir, "users.csv"),     header=FALSE, stringsAsFactors=F))
blocks  <- data.table(read.csv(file.path(kDataDir, "log_block.csv"), header=FALSE, stringsAsFactors=F))
rounds  <- data.table(read.csv(file.path(kDataDir, "log_round.csv"), header=FALSE, stringsAsFactors=F))
surveys <- data.table(read.csv(file.path(kDataDir, "log_survey.csv"),header=FALSE, stringsAsFactors=F))

# set columns names
setnames(users,   c("user_id","V2","user_ip","last_activity","last_page","first_activity",
                    "show_tcp","show_tcc","show_tcl","show_cs"))
setnames(blocks,  c("log_id","user_id","block","collector","time_stamp"))
setnames(rounds,  c("log_id","user_id","block","round","coins_avail","coins_collected","time_stamp"))
setnames(surveys, c("log_id", "user_id","block","question","response","time_stamp"))

# extract experimental conditions from user_ids
users[, condition:=substr(user_id, 1,2)]
users[, condition:=factor(condition, levels=c("gl","gr","nl","um"),labels=c("gamma-left","gamma-right","normal","uniform"))]

# identify users who exited the study before reaching the final page
users[, finished := substr(last_page, 1,3) == "PGS"]
users.complete <- users[finished == TRUE, user_id]

# merge experimental condition into each of the other tables
rounds  <- merge(rounds, users[,list(user_id, condition)], by="user_id")
rounds  <- rounds[user_id %in% users.complete]
blocks  <- merge(blocks, users[,list(user_id, condition)], by="user_id")
blocks  <- blocks[user_id %in% users.complete]
surveys <- merge(surveys, users[,list(user_id, condition)], by="user_id")
surveys <- surveys[user_id %in% users.complete]

####~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#### Analysis ####
####~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#### Experiences by Condition
coin.n.cond <- rounds[, list(count = .N / length(unique(user_id))), 
                      by=c("coins_avail","condition")]
coin.mean.cond <- rounds[,list(mean=mean(coins_avail)), by=condition]
ggplot(coin.n.cond, aes(coins_avail, count, fill=condition)) + 
  geom_vline(xintercept = 5, linetype="dashed") +
  geom_histogram(origin = -0.5,stat="identity") +
  scale_x_continuous(breaks=0:10)+
  facet_grid(condition~.) + 
  theme_bw(base_size=16)


#### Perception of Experience
# perceptions and accuracy of coin dist previous block
# question text (pbs1) = "How many coins appeared each round, on average, 
#                         over the last 10 rounds?"
pbs1.cond       <- surveys[question == "slider-pbs1", 
                           list(mean = mean(response),
                                ci_low   = CI(response)["lower"],
                                ci_high  = CI(response)["upper"]), 
                           by=condition]

ggplot(pbs1.cond, aes(condition, mean, color=condition)) + 
  geom_hline(yintercept=5, linetype="dashed")+
  geom_point(size = 5, shape=18) + 
  geom_errorbar(aes(ymin=ci_low, ymax=ci_high), width=.25) +
  scale_y_continuous(limits=c(3.5,6.5), breaks=seq(3.5,6.5,.5)) +
  labs(x="Condition", y="Mean Estimate of Previous Block Coins") +
  theme_bw(base_size=16)

pbs1.cond.block <- surveys[question == "slider-pbs1", 
                           list(mean = mean(response),
                                ci_low   = CI(response)["lower"],
                                ci_high  = CI(response)["upper"]), 
                           by=c("condition","block")]

ggplot(pbs1.cond.block, aes(factor(block), mean, color=condition)) + 
  geom_point(size = 3, shape=1) + 
  geom_line(aes(factor(block), mean, group=condition)) +
  geom_hline(yintercept=5, linetype="dashed")+
  geom_smooth(aes(group=1), method="lm") +
  scale_y_continuous(limits=c(3.5,6.5), breaks=seq(3.5,6.5,.5)) +
  labs(x="Block", y="Mean Estimate of Previous Block Coins") +
  facet_grid(. ~ condition) +
  theme_bw(base_size=16)

coin.indiv.block <- rounds[,list(mean_coin = mean(coins_avail)),
                           by=c("block","user_id")]

coin.indiv.block <- merge(coin.indiv.block,
                          surveys[question == "slider-pbs1", 
                                  list(user_id, condition, block, response)], 
                          by=c("user_id","block"))

ggplot(coin.indiv.block, aes(mean_coin, response, color=condition)) +
  geom_abline(yintercept=0, slope=1, linetype="dashed") +
  geom_point(shape=1) +
  geom_smooth(method="lm", alpha=.1)+
  scale_x_continuous(limits=c(2,8), breaks=2:8) +
  scale_y_continuous(limits=c(0,10), breaks=0:10) +
  labs(x="Mean Coins Observed in Prev Block",
       y="Mean Coins Estimated by Participant") +
  theme_bw(base_size=16)

coin.indiv.block[,error:=response-mean_coin]
error.coin.cond <- coin.indiv.block[ ,list(mean = mean(error),
                                           ci_low   = CI(error)["lower"],
                                           ci_high  = CI(error)["upper"]), 
                                      by=condition]

ggplot(error.coin.cond, aes(condition, mean, color=condition)) +
  geom_hline(yintercept=0, linetype="dashed")+
  geom_point(size = 5, shape=18) + 
  geom_errorbar(aes(ymin=ci_low, ymax=ci_high), width=.25) +
  labs(x="Condition", y="Individual Estimate Error of Previous Block Coins") +
  theme_bw(base_size=16)


# perceptions and accuracy of coin dist game to date
cum.coin.indiv.block <- data.table()
for(i in unique(coin.indiv.block$block)){
  tmp <- rounds[block <= i, 
                list(mean_coin = mean(coins_avail)),
                by=user_id]
  tmp$block <- i
  cum.coin.indiv.block <- rbindlist(list(cum.coin.indiv.block, tmp))
}

cum.coin.indiv.block <- merge(cum.coin.indiv.block,
                              surveys[question == "slider-pbs2", 
                                      list(user_id, condition, block, response)], 
                              by=c("user_id","block"))

ggplot(cum.coin.indiv.block, aes(mean_coin, response, color=condition)) +
  geom_abline(yintercept=0, slope=1, linetype="dashed") +
  geom_point(shape=1) +
  geom_smooth(method="lm", alpha=.1)+
  scale_x_continuous(limits=c(2,8), breaks=2:8) +
  scale_y_continuous(limits=c(0,10), breaks=0:10) +
  labs(x="Mean Coins Observed in All Prev Blocks",
       y="Mean Coins Estimated by Participant") +
  theme_bw(base_size=16)

cum.coin.indiv.block[,error:=response-mean_coin]
error.cum.cond <- cum.coin.indiv.block[ ,list(mean = mean(error),
                                              ci_low   = CI(error)["lower"],
                                              ci_high  = CI(error)["upper"]), 
                                       by=condition]

ggplot(error.cum.cond, aes(condition, mean, color=condition)) +
  geom_hline(yintercept=0, linetype="dashed")+
  geom_point(size = 5, shape=18) + 
  geom_errorbar(aes(ymin=ci_low, ymax=ci_high), width=.25) +
  labs(x="Condition", y="Individual Estimate Error of All Prev Block Coins") +
  theme_bw(base_size=16)

error.cum.cond.block <- cum.coin.indiv.block[ ,list(mean = mean(error),
                                                    ci_low   = CI(error)["lower"],
                                                    ci_high  = CI(error)["upper"]), 
                                             by=c("condition","block")]

ggplot(error.cum.cond.block, aes(factor(block), mean, color=condition)) + 
  geom_point(size = 3, shape=1) + 
  geom_line(aes(factor(block), mean, group=condition)) +
  geom_hline(yintercept=0, linetype="dashed")+
  geom_smooth(aes(group=1), method="lm") +
  #scale_y_continuous(limits=c(3.5,6.5), breaks=seq(3.5,6.5,.5)) +
  labs(x="Block", y="Mean Error of All Prev Blocks Coins") +
  facet_grid(. ~ condition) +
  theme_bw(base_size=16)


# perceptions and accuracy of coin dist post game
# perceptions and accuracy of insufficent bucket size

#### Decision making
# mean collector size by round
# number of switches
# variance in collector size
# collector size predicted by mean observed
# behavior as fit by omnicient observer
# behavior as fit by model of rational behavior
# behavior as fit by model of salient events weighted

#### Outcomes
# revenue, profit overall
# change in revenue, profit over time



# 
# ggplot(blocks, aes(as.numeric(block), as.numeric(collector), color=condition)) +
#   geom_point(position="jitter") +
#   geom_smooth(se=F)
# 
# ggplot(blocks, aes(as.numeric(block), as.numeric(collector), color=user_id)) +
#   geom_line()+
#   facet_grid(condition ~ .)
# 
# revenue.user <- rounds[, list(condition = unique(condition), total_collected = sum(coins_collected)), by=user_id]
# 
# ggplot(revenue.user, aes(condition, total_collected)) + geom_boxplot()
# 
# 
# 
# ggplot(surveys[question=="slider-pbs1",], aes(block, response, color=condition)) +
#   geom_smooth(method="lm")
# ggplot(surveys[question=="slider-pbs2",], aes(block, response, color=condition)) +
#   geom_smooth(method="lm")
# 
# 
# surveys[question=="slider-pbs1", mean(response), by=condition]
# surveys[question=="slider-pbs2", mean(response), by=condition]
# 
# surveys[question=="slider-pgs1", mean(response), by=condition]
# surveys[question=="slider-pgs2", mean(response), by=condition]
