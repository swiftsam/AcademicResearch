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

kDataDir <- file.path("~","Desktop","ccg3b")

users   <- data.table(read.csv(file.path(kDataDir, "users.csv"),     header=FALSE, stringsAsFactors=F))
blocks  <- data.table(read.csv(file.path(kDataDir, "log_block.csv"), header=FALSE, stringsAsFactors=F))
rounds  <- data.table(read.csv(file.path(kDataDir, "log_round.csv"), header=FALSE, stringsAsFactors=F))
surveys <- data.table(read.csv(file.path(kDataDir, "log_survey.csv"),header=FALSE, stringsAsFactors=F))

setnames(users,   c("user_id","V2","user_ip","last_activity","last_page","first_activity",
                    "show_tcp","show_tcc","show_tcl","show_cs"))
setnames(blocks,  c("log_id","user_id","block","collector","time_stamp"))
setnames(rounds,  c("log_id","user_id","block","round","coins_avail","coins_collected","time_stamp"))
setnames(surveys, c("log_id", "user_id","block","question","response","time_stamp"))

users[, condition:=substr(user_id, 1,2)]
users[, condition:=factor(condition, levels=c("gl","gr","nl","um"),labels=c("gamma-left","gamma-right","normal","uniform"))]
users[, finished := substr(last_page, 1,3) == "PGS"]
users.complete <- users[finished == TRUE, user_id]

rounds  <- merge(rounds, users[,list(user_id, condition)], by="user_id")
rounds  <- rounds[user_id %in% users.complete]
blocks  <- merge(blocks, users[,list(user_id, condition)], by="user_id")
blocks  <- blocks[user_id %in% users.complete]
surveys <- merge(surveys, users[,list(user_id, condition)], by="user_id")
surveys <- surveys[user_id %in% users.complete]

ggplot(blocks, aes(as.numeric(block), as.numeric(collector), color=condition)) +
         geom_point(position="jitter") +
 geom_smooth(se=F)

ggplot(blocks, aes(as.numeric(block), as.numeric(collector), color=user_id)) +
  geom_line()+
  facet_grid(condition ~ .)

revenue.user <- rounds[, list(condition = unique(condition), total_collected = sum(coins_collected)), by=user_id]

ggplot(revenue.user, aes(condition, total_collected)) + geom_boxplot()



ggplot(surveys[question=="slider-pbs1",], aes(block, response, color=condition)) +
  geom_smooth(method="lm")
ggplot(surveys[question=="slider-pbs2",], aes(block, response, color=condition)) +
  geom_smooth(method="lm")


surveys[question=="slider-pbs1", mean(response), by=condition]
surveys[question=="slider-pbs2", mean(response), by=condition]

surveys[question=="slider-pgs1", mean(response), by=condition]
surveys[question=="slider-pgs2", mean(response), by=condition]

