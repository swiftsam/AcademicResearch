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

kDataDir <- file.path("http://samswift.org/data/")

rational.benchmarks <- data.table("measure"   = rep(c("cost","revenue","profit"),4),
                                  "condition" = c(rep("gamma-left",3),
                                                  rep("gamma-right",3),
                                                  rep("normal",3),
                                                  rep("uniform", 3)),
                                  "benchmark" = c(300, 398, 98,
                                                  200, 292, 92,
                                                  250, 423, 173,
                                                  250, 365, 115))
rational.benchmarks[, measure   := factor(measure, levels=c("cost","revenue","profit"))]

####~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#### Data Prep ####
####~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Read data from the four tables exported by the CCG program
users   <- data.table(read.csv(file.path(kDataDir, "CCG_users_2014-02-04.csv"), header=FALSE, stringsAsFactors=F))
blocks  <- data.table(read.csv(file.path(kDataDir, "CCG_block_2014-02-04.csv"), header=FALSE, stringsAsFactors=F))
rounds  <- data.table(read.csv(file.path(kDataDir, "CCG_round_2014-02-04.csv"), header=FALSE, stringsAsFactors=F))
surveys <- data.table(read.csv(file.path(kDataDir, "CCG_survey_2014-02-04.csv"),header=FALSE, stringsAsFactors=F))

# set columns names
setnames(users,   c("user_id","V2","user_ip","last_activity","last_page","first_activity",
                    "show_tcp","show_tcc","show_tcl","show_cs"))
setnames(blocks,  c("log_id","user_id","block","collector","time_stamp"))
setnames(rounds,  c("log_id","user_id","block","round","coins_avail","coins_collected","time_stamp"))
setnames(surveys, c("log_id", "user_id","block","question","response","time_stamp"))

# extract experimental conditions from user_ids
users[, condition:=substr(user_id, 1,2)]
users[, condition:=factor(condition, levels=c("gl","gr","nl","um"),
                          labels=c("gamma-left","gamma-right","normal","uniform"))]

# identify users who exited the study before reaching the final page
rounds.user    <- rounds[,list(n_round = .N), by=c("user_id")]
users.complete <- rounds.user[n_round == 100, user_id]

# merge experimental condition into each of the other tables
rounds  <- merge(rounds, users[,list(user_id, condition)], by="user_id")
rounds  <- rounds[user_id %in% users.complete]
blocks  <- merge(blocks, users[,list(user_id, condition)], by="user_id")
blocks  <- blocks[user_id %in% users.complete]
surveys <- merge(surveys, users[,list(user_id, condition)], by="user_id")
surveys <- surveys[user_id %in% users.complete]

# calculate completion times
users[, first_activity := as.POSIXct(first_activity, format="%Y-%m-%d %H:%M:%S")]
users[, last_activity  := as.POSIXct(last_activity, format="%Y-%m-%d %H:%M:%S")]
users[, total_time     := as.integer(difftime(last_activity, first_activity, units="mins"))]

p.time.complete <- ggplot(users, aes(condition, total_time)) + geom_boxplot()
time.complete   <- users[, list(mean = mean(total_time, na.rm=T),
                                median = as.numeric(median(total_time, na.rm=T))), 
                         by=condition]

####~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#### Analysis
####~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#### Experiences by Condition ####
coin.n.cond <- rounds[, list(count = .N / length(unique(user_id))), 
                      by=c("coins_avail","condition")]
coin.dist.cond <- rounds[,list(mean=mean(coins_avail),
                               med = median(coins_avail)), 
                         by=condition]
p.coin.dist.cond <- ggplot(coin.n.cond, aes(coins_avail, count, fill=condition)) + 
  geom_vline(data=coin.dist.cond, aes(xintercept = mean), linetype="dashed") +
  geom_vline(data=coin.dist.cond, aes(xintercept = med), linetype="dotted") +
  geom_histogram(origin = -0.5,stat="identity") +
  scale_x_continuous(breaks=0:10, minor_breaks=NULL)+
  labs(x="Number of Coins",
       y="Number of Rounds",
       color="Condition") +
  facet_grid(condition~.) + 
  theme_bw(base_size=16) + 
  theme(legend.position = "none")

#### Perception of Previous Block ####
# perceptions and accuracy of coin dist previous block
# question text (pbs1) = "How many coins appeared each round, on average, 
#                         over the last 10 rounds?"
pbs1.cond       <- surveys[question == "slider-pbs1", 
                           list(mean = mean(response),
                                ci_low   = CI(response)["lower"],
                                ci_high  = CI(response)["upper"]), 
                           by=condition]

pbs1.cond.block <- surveys[question == "slider-pbs1", 
                           list(mean = mean(response),
                                ci_low   = CI(response)["lower"],
                                ci_high  = CI(response)["upper"]), 
                           by=c("condition","block")]

p.pbs1 <- ggplot(pbs1.cond.block, aes(block, mean, color=condition)) +
  geom_hline(yintercept=5, linetype="dashed")+
  geom_point(data=pbs1.cond, 
             aes(x=0,y=mean, color=condition, fill=condition), size=3, shape=21) +
  geom_errorbar(data=pbs1.cond, 
                aes(x=0, ymin=ci_low, ymax=ci_high, color=condition), width=1) +
  geom_point(size = 3, shape=1) + 
  geom_line(aes(block, mean, group=condition)) +
  geom_smooth(aes(group=1), method="lm", alpha=.2) +
  scale_x_continuous(limits=c(-.5,10), breaks=0:10, minor_breaks=NULL, labels=c("Overall",1:10)) +
  labs(x="Block", y="Estimate of Previous Block (coins/rd)") +
  facet_grid(. ~ condition) +
  theme_bw(base_size=16)+
  theme(axis.text.x = element_text(angle = 45, hjust = 1),legend.position="none")

# calculate true mean coins per round for each indiv
coin.indiv.block <- rounds[,list(mean_coin = mean(coins_avail)),
                           by=c("block","user_id")]

# merge true means with perceptions
coin.indiv.block <- merge(coin.indiv.block,
                          surveys[question == "slider-pbs1", 
                                  list(user_id, condition, block, response)], 
                          by=c("user_id","block"))

# Error in Previous Block Perception
coin.indiv.block[,error:=response-mean_coin]
error.coin.cond <- coin.indiv.block[ ,list(mean = mean(error),
                                           ci_low   = CI(error)["lower"],
                                           ci_high  = CI(error)["upper"]), 
                                      by=condition]
error.coin.cond.block <- coin.indiv.block[ ,list(mean = mean(error),
                                                 ci_low   = CI(error)["lower"],
                                                 ci_high  = CI(error)["upper"]), 
                                          by=c("condition","block")]

p.pbs1.error <- ggplot(error.coin.cond.block, 
                       aes(block, mean, color=condition)) +
  geom_hline(yintercept=0, linetype="dashed")+
  geom_point(data=error.coin.cond, 
             aes(x=0,y=mean, color=condition, fill=condition), size=3, shape=21) +
  geom_errorbar(data=error.coin.cond, 
                aes(x=0, ymin=ci_low, ymax=ci_high, color=condition)) +
  geom_point(size = 3, shape=1) + 
  geom_line(aes(block, mean, group=condition)) +
  geom_smooth(aes(group=1), method="lm", alpha=.2) +
  scale_x_continuous(limits=c(-.5,10), breaks=0:10, minor_breaks=NULL, labels=c("Overall",1:10)) +
  labs(x="Block", y="Error in Estimate Coins in Block (coins/rd)") +
  facet_grid(. ~ condition) +
  theme_bw(base_size=16)+
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position="none")

#### Perception of Cumulative Mean  ####
cum.coin.indiv.block <- data.table()
for(i in unique(coin.indiv.block$block)){
  tmp <- rounds[block <= i, 
                list(mean_coin = mean(coins_avail),
                     med_coin  = median(coins_avail)),
                by=user_id]
  tmp$block <- i
  cum.coin.indiv.block <- rbindlist(list(cum.coin.indiv.block, tmp))
}

cum.coin.indiv.block <- merge(cum.coin.indiv.block,
                              surveys[question == "slider-pbs2", 
                                      list(user_id, condition, block, response)], 
                              by=c("user_id","block"))

cum.coin.indiv.block[,error:=response-mean_coin]
error.cum.cond <- cum.coin.indiv.block[ ,list(mean = mean(error),
                                              ci_low   = CI(error)["lower"],
                                              ci_high  = CI(error)["upper"]), 
                                       by=condition]

error.cum.cond.block <- cum.coin.indiv.block[ ,list(mean = mean(error),
                                                    ci_low   = CI(error)["lower"],
                                                    ci_high  = CI(error)["upper"]), 
                                             by=c("condition","block")]

p.error.cum <- ggplot(error.cum.cond.block, aes(block, mean, color=condition)) + 
  geom_hline(yintercept=0, linetype="dashed")+
  geom_point(data=error.cum.cond, 
             aes(x=0,y=mean, color=condition, fill=condition), size=3, shape=21) +
  geom_errorbar(data=error.cum.cond, 
                aes(x=0, ymin=ci_low, ymax=ci_high, color=condition), width=1) +
  geom_point(size = 3, shape=1) + 
  geom_line(aes(block, mean, group=condition)) +
  geom_smooth(aes(group=1), method="lm", alpha=.2) +
  scale_x_continuous(limits=c(-.5,10), breaks=0:10, minor_breaks=NULL, labels=c("Overall",1:10)) +
  labs(x="Block", y="Estimate Error of All Previous Blocks (coins/rd)") +
  facet_grid(. ~ condition) +
  theme_bw(base_size=16)+
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position="none")

#### Profits from fixed collector-size strategies ####
rational.profit <- data.table()
for(i in 0:10){
  tmp <- rounds[, list(
    collector = i,
    user_id = unique(user_id),
    condition = unique(condition),
    revenue   = min(coins_avail, i)), 
    by=log_id]
  rational.profit <- rbindlist(list(rational.profit, tmp))
}

rational.profit[, cost:= collector*.5]
rational.profit[, profit := revenue - cost]

rational.profit.indiv.strat <- rational.profit[, list(condition = unique(condition),
                                                      profit = sum(profit),
                                                      cost   = sum(cost),
                                                      revenue = sum(revenue)),
                                               by=c("collector","user_id")]

rational.profit.cond.strat <- rational.profit.indiv.strat[, list(mean_profit=mean(profit),
                                                                 mean_cost = mean(cost),
                                                                 mean_revenue = mean(revenue)),
                                                          by=c("condition","collector")]

p.rational.profit.cond.strat <- ggplot(rational.profit.cond.strat, 
                                       aes(collector, mean_profit, color=condition))+
  geom_hline(data = rational.benchmarks[measure=="profit"],
             aes(yintercept=benchmark, color=condition),
             linetype="dashed")+ 
  geom_vline(data = rational.benchmarks[measure=="cost"],
             aes(xintercept=benchmark/50, color=condition),
             linetype="dashed")+ 
  geom_line(size=1.3) +
  scale_x_continuous(breaks=1:10, minor_breaks=NULL) +
  labs(x="Collector Size",
       y="Profit after 100 rounds",
       color="Condition") +
  theme_bw(base_size=16)

#### Decision making ####
# mean collector size by round
col.cond <- blocks[,list(mean = mean(collector),
                         ci_low   = CI(collector)["lower"],
                         ci_high  = CI(collector)["upper"]), 
                   by=c("condition")]

col.cond.block <- blocks[,list(mean = mean(collector),
                               ci_low   = CI(collector)["lower"],
                               ci_high  = CI(collector)["upper"]), 
                         by=c("condition","block")]

p.col.cond <- ggplot(col.cond.block, aes(block, mean, color=condition)) + 
  geom_hline(yintercept=5, linetype="dotted") + 
  geom_hline(data = rational.benchmarks[measure=="cost"],
             aes(yintercept=benchmark/50, color=condition),
             linetype="dashed", size=1.1) +
  geom_point(data=col.cond, 
             aes(x=0,y=mean, color=condition, fill=condition), size=3, shape=21) +
  geom_errorbar(data=col.cond, 
                aes(x=0, ymin=ci_low, ymax=ci_high, color=condition), width=1) +
  geom_point(size = 3, shape=1) + 
  geom_line(aes(block, mean, group=condition)) +
  geom_smooth(aes(group=1), method="lm", alpha=.2) +
  scale_x_continuous(limits=c(-.5,10), breaks=0:10, minor_breaks=NULL, labels=c("Overall",1:10)) +
  labs(x="Block", y="Collector Size") +
  facet_grid(. ~ condition) +
  theme_bw(base_size=16) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position="none")

# collector size predicted by mean/median of observed rounds
cum.coin.indiv.block[,block_next := block+1]
cum.coin.indiv.block <- merge(cum.coin.indiv.block,
                              blocks[,list(user_id, block_next = block, collector)],
                              by=c("user_id","block_next"), all.x=TRUE)

cum.coin.indiv.block[, mean_dec_diff:= collector-mean_coin]
cum.coin.indiv.block[, med_dec_diff := collector-med_coin]

dec.diff.cond <- rbind(cum.coin.indiv.block[!is.na(collector),
                                            list(measure  = "mean",
                                                 mean     = mean(mean_dec_diff),
                                                 ci_low   = CI(mean_dec_diff)["lower"],
                                                 ci_high  = CI(mean_dec_diff)["upper"]), 
                                            by=c("condition")],
                       cum.coin.indiv.block[!is.na(collector),
                                            list(measure  = "median",
                                                 mean     = mean(med_dec_diff),
                                                 ci_low   = CI(med_dec_diff)["lower"],
                                                 ci_high  = CI(med_dec_diff)["upper"]), 
                                            by=c("condition")])

dec.diff.cond.block <- rbind(cum.coin.indiv.block[!is.na(collector),
                                                  list(measure  = "mean",
                                                       mean     = mean(mean_dec_diff),
                                                       ci_low   = CI(mean_dec_diff)["lower"],
                                                       ci_high  = CI(mean_dec_diff)["upper"]), 
                                            by=c("condition","block")],
                             cum.coin.indiv.block[!is.na(collector),
                                                  list(measure  = "median",
                                                       mean     = mean(med_dec_diff),
                                                       ci_low   = CI(med_dec_diff)["lower"],
                                                       ci_high  = CI(med_dec_diff)["upper"]), 
                                                  by=c("condition","block")])

p.dec.diff <- ggplot(dec.diff.cond.block, aes(block, mean, color=condition)) + 
  geom_hline(yintercept=0, linetype="dashed")+
  geom_point(data=dec.diff.cond, 
             aes(x=0,y=mean, color=condition, fill=condition), size=3, shape=21) +
  geom_errorbar(data=dec.diff.cond, 
                aes(x=0, ymin=ci_low, ymax=ci_high, color=condition), width=1) +
  geom_point(size = 3, shape=1) + 
  geom_line(aes(block, mean, group=condition)) +
  geom_smooth(aes(group=1), method="lm", alpha=.2) +
  scale_x_continuous(limits=c(-.5,9), breaks=0:9, minor_breaks=NULL, labels=c("Overall",1:9)) +
  labs(x="After N Blocks", y="Decision Deviation from Experience (coins)") +
  facet_grid(measure ~ condition) +
  theme_bw(base_size=16)+
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position="none")

#### Outcomes ####
# revenue, profit overall
rounds <- merge(rounds, blocks[,list(user_id, block, collector)],
                by=c("user_id","block"))
rounds[, capacity_excess := max(collector - coins_avail, 0), by=log_id]
rounds[, capacity_short  := min(collector - coins_avail, 0), by=log_id]
rounds[, capacity_error  := collector-coins_avail, by=log_id]

profit.indiv.block <- rounds[,list(condition       = unique(condition),
                                   collector       = unique(collector),
                                   coins_avail     = sum(coins_avail),
                                   coins_collected = sum(coins_collected),
                                   capacity_excess = sum(capacity_excess),
                                   capacity_short  = sum(capacity_short),
                                   capacity_error  = sum(capacity_error)),
                             by=c("user_id","block")]

profit.indiv.block[, cost   := collector * 5]
profit.indiv.block[, profit := coins_collected - cost]

profit.indiv <- profit.indiv.block[, list(condition       = unique(condition),
                                          coins_collected = sum(coins_collected),
                                          cost            = sum(cost),
                                          profit          = sum(profit)),
                                   by = user_id]

profit.cond <- rbindlist(list(profit.indiv[,list(measure = "cost",
                                                mean    = mean(cost),
                                                ci_low  = CI(cost)["lower"],
                                                ci_high = CI(cost)["upper"]),
                                           by=condition],
                              profit.indiv[,list(measure = "revenue",
                                                mean    = mean(coins_collected),
                                                ci_low  = CI(coins_collected)["lower"],
                                                ci_high = CI(coins_collected)["upper"]),
                                           by=condition],
                              profit.indiv[,list(measure = "profit",
                                                mean    = mean(profit),
                                                ci_low  = CI(profit)["lower"],
                                                ci_high = CI(profit)["upper"]),
                                           by=condition]))

profit.cond[, measure := factor(measure, levels=c("cost","revenue","profit"), ordered=TRUE)]
profit.cond[, mean    := mean/10]
profit.cond[, ci_low  := ci_low/10]
profit.cond[, ci_high := ci_high/10]

# change in revenue, profit over time
profit.cond.block <- rbindlist(list(profit.indiv.block[,list(measure = "cost",
                                                             mean    = mean(cost),
                                                             ci_low  = CI(cost)["lower"],
                                                             ci_high = CI(cost)["upper"]),
                                                       by=c("condition","block")],
                                    profit.indiv.block[,list(measure = "revenue",
                                                             mean    = mean(coins_collected),
                                                             ci_low  = CI(coins_collected)["lower"],
                                                             ci_high = CI(coins_collected)["upper"]),
                                                       by=c("condition","block")],
                                    profit.indiv.block[,list(measure = "profit",
                                                             mean    = mean(profit),
                                                             ci_low  = CI(profit)["lower"],
                                                             ci_high = CI(profit)["upper"]),
                                                       by=c("condition","block")]))

profit.cond.block[, measure := factor(measure, levels=c("cost","revenue","profit"), ordered=TRUE)]

per.possible.profit <- merge(profit.cond[measure=="profit", 
                                         list(condition, mean)],
                             rational.benchmarks[measure=="profit", 
                                                 list(condition,
                                                      benchmark = benchmark/10)], 
                             by="condition")
per.possible.profit[, perc := round(mean/benchmark *100,1)]

p.outcomes <- ggplot(profit.cond.block, aes(block, mean, color=condition)) + 
  geom_hline(data=rational.benchmarks, aes(yintercept=benchmark/10), linetype="dashed") +
  geom_point(data=profit.cond, 
             aes(x=0,y=mean, color=condition, fill=condition), size=3, shape=21) +
  geom_errorbar(data=profit.cond, 
                aes(x=0, ymin=ci_low, ymax=ci_high, color=condition), width=1) +
  geom_point(size = 3, shape=1) + 
  geom_line(aes(block, mean, group=condition)) +
  geom_smooth(aes(group=1), method="lm", alpha=.2) +
  scale_x_continuous(limits=c(-.5,10), breaks=0:10, minor_breaks=NULL, labels=c("Overall",1:10)) +
  labs(x="Block", y="Coins per Block") +
  facet_grid(measure ~ condition, scales="free_y") +
  theme_bw(base_size=16)+
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position="none")

#### Post Game Survey ####
# perceptions and accuracy of coin dist post game

pgs.coins.cond <- surveys[question=="slider-pgs2",
                          list(measure = "post-game",
                               mean = mean(response),
                               ci_low   = CI(response)["lower"],
                               ci_high  = CI(response)["upper"]),
                          by=condition]

pgs.comparison <- rbind(pgs.coins.cond,
                        col.cond.block[block==1,
                                       list(condition = condition,
                                            measure   = "first block",
                                            mean      = mean,
                                            ci_low    = ci_low,
                                            ci_high   = ci_high)],
                        col.cond.block[block==10,
                                       list(condition = condition,
                                            measure   = "last block",
                                            mean      = mean,
                                            ci_low    = ci_low,
                                            ci_high   = ci_high)])

p.pgs.col.cond <- ggplot(pgs.comparison, aes(measure, mean, color=condition)) + 
  geom_hline(data = rational.benchmarks[measure=="cost"],
             aes(yintercept=benchmark/50, color=condition),
             linetype="dashed", size=1.1) +
  geom_hline(yintercept=5, linetype="dotted")+
  geom_point(size = 5, shape=18) + 
  geom_errorbar(aes(ymin=ci_low, ymax=ci_high), width=.25) +
  labs(x="", y="Collector Size") +
  facet_grid(.~condition) +
  theme_bw(base_size=16) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position="none")


# perceptions and accuracy of insufficent bucket size

pgs.insuf.indiv <- merge(surveys[question=="slider-pgs1",
                                 list(user_id, condition, response)],
                         rounds[, list(true_insuf = sum(coins_avail > coins_collected)),
                                by=user_id],
                         by="user_id")
pgs.insuf.indiv[,error:=response-true_insuf]


pgs.insuf.error.cond <- pgs.insuf.indiv[ ,list(mean = mean(error),
                                               ci_low   = CI(error)["lower"],
                                               ci_high  = CI(error)["upper"]), 
                                        by=condition]

p.pgs.insuf.error.cond <- ggplot(pgs.insuf.error.cond, aes(condition, mean, color=condition)) +
  geom_hline(yintercept=0, linetype="dashed")+
  geom_point(size = 5, shape=18) + 
  geom_errorbar(aes(ymin=ci_low, ymax=ci_high), width=.25) +
  labs(x="Condition", y="Error in Insufficient Capacity Recall (rounds)") +
  theme_bw(base_size=16) +
  theme(legend.position="none")


pgs.insuf.cond <- merge(pgs.insuf.indiv[ ,list(mean_resp     = mean(response),
                                               ci_low_resp   = CI(response)["lower"],
                                               ci_high_resp  = CI(response)["upper"]), 
                                        by=condition],
                        pgs.insuf.indiv[ ,list(mean_true     = mean(true_insuf),
                                               ci_low_true   = CI(true_insuf)["lower"],
                                               ci_high_true  = CI(true_insuf)["upper"]), 
                                        by=condition],
                        by="condition")

p.pgs.insuf.cond <- ggplot(pgs.insuf.cond, aes(mean_true, mean_resp, color=condition)) + 
  geom_abline(yintercept=0, slope=1, linetype="dashed") +
  geom_point(size = 5, shape=18) + 
  geom_errorbar(aes(ymin=ci_low_resp, ymax=ci_high_resp), width=1) +
  geom_errorbarh(aes(xmin=ci_low_true, xmax=ci_high_true), height=.25) +
  labs(x="True Frequency of Insufficient Capacity", y="Recalled Frequency of Insufficient Capacity") +
  theme_bw(base_size=16)
