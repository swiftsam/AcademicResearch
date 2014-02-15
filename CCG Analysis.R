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

####~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#### Analysis
####~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#### Experiences by Condition
coin.n.cond <- rounds[, list(count = .N / length(unique(user_id))), 
                      by=c("coins_avail","condition")]
coin.dist.cond <- rounds[,list(mean=mean(coins_avail),
                               med = median(coins_avail)), 
                         by=condition]
p.coin.dist.cond <- ggplot(coin.n.cond, aes(coins_avail, count, fill=condition)) + 
  geom_vline(data=coin.dist.cond, aes(xintercept = mean), linetype="dashed") +
  geom_vline(data=coin.dist.cond, aes(xintercept = med), linetype="dotted") +
  geom_histogram(origin = -0.5,stat="identity") +
  scale_x_continuous(breaks=0:10)+
  labs(x="Number of Coins",
       y="Number of Rounds",
       color="Condition") +
  facet_grid(condition~.) + 
  theme_bw(base_size=16)

#### Perception of Experience ####
# perceptions and accuracy of coin dist previous block
# question text (pbs1) = "How many coins appeared each round, on average, 
#                         over the last 10 rounds?"
pbs1.cond       <- surveys[question == "slider-pbs1", 
                           list(mean = mean(response),
                                ci_low   = CI(response)["lower"],
                                ci_high  = CI(response)["upper"]), 
                           by=condition]

p.pbs1.cond <- ggplot(pbs1.cond, aes(condition, mean, color=condition)) + 
  geom_hline(yintercept=5, linetype="dashed")+
  geom_point(size = 5, shape=18) + 
  geom_errorbar(aes(ymin=ci_low, ymax=ci_high), width=.25) +
  scale_y_continuous(limits=c(3.5,6.5), breaks=seq(3.5,6.5,.5)) +
  labs(x="Condition", y="Estimate of Previous Block Coins") +
  theme_bw(base_size=16)

pbs1.cond.block <- surveys[question == "slider-pbs1", 
                           list(mean = mean(response),
                                ci_low   = CI(response)["lower"],
                                ci_high  = CI(response)["upper"]), 
                           by=c("condition","block")]

p.pbs1.cond.block <- ggplot(pbs1.cond.block, aes(factor(block), mean, color=condition)) + 
  geom_point(size = 3, shape=1) + 
  geom_line(aes(factor(block), mean, group=condition)) +
  geom_hline(yintercept=5, linetype="dashed")+
  geom_smooth(aes(group=1), method="lm") +
  scale_y_continuous(limits=c(3.5,6.5), breaks=seq(3.5,6.5,.5)) +
  labs(x="Block", y="Estimate of Previous Block (coins/rd)") +
  facet_grid(. ~ condition) +
  theme_bw(base_size=16)

coin.indiv.block <- rounds[,list(mean_coin = mean(coins_avail)),
                           by=c("block","user_id")]

coin.indiv.block <- merge(coin.indiv.block,
                          surveys[question == "slider-pbs1", 
                                  list(user_id, condition, block, response)], 
                          by=c("user_id","block"))

p.coin.indiv.block <- ggplot(coin.indiv.block, aes(mean_coin, response, color=condition)) +
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

p.error.coin.cond <- ggplot(error.coin.cond, aes(condition, mean, color=condition)) +
  geom_hline(yintercept=0, linetype="dashed")+
  geom_point(size = 5, shape=18) + 
  geom_errorbar(aes(ymin=ci_low, ymax=ci_high), width=.25) +
  labs(x="Condition", y="Estimate Error of Previous Block (coins/rd)") +
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

p.cum.coin.indiv.block <- ggplot(cum.coin.indiv.block, aes(mean_coin, response, color=condition)) +
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

p.error.cum.cond <- ggplot(error.cum.cond, aes(condition, mean, color=condition)) +
  geom_hline(yintercept=0, linetype="dashed")+
  geom_point(size = 5, shape=18) + 
  geom_errorbar(aes(ymin=ci_low, ymax=ci_high), width=.25) +
  labs(x="Condition", y="Estimate Error of All Prev Blocks (coins/rd)") +
  theme_bw(base_size=16)

error.cum.cond.block <- cum.coin.indiv.block[ ,list(mean = mean(error),
                                                    ci_low   = CI(error)["lower"],
                                                    ci_high  = CI(error)["upper"]), 
                                             by=c("condition","block")]

p.error.cum.cond.block <- ggplot(error.cum.cond.block, aes(factor(block), mean, color=condition)) + 
  geom_point(size = 3, shape=1) + 
  geom_line(aes(factor(block), mean, group=condition)) +
  geom_hline(yintercept=0, linetype="dashed")+
  geom_smooth(aes(group=1), method="lm") +
  #scale_y_continuous(limits=c(3.5,6.5), breaks=seq(3.5,6.5,.5)) +
  labs(x="Block", y="Estimate Error of All Previous Blocks (coins/rd)") +
  facet_grid(. ~ condition) +
  theme_bw(base_size=16)

# perceptions and accuracy of coin dist post game

pgs.coins.cond <- surveys[question=="slider-pgs2",
                          list(mean = mean(response),
                               ci_low   = CI(response)["lower"],
                               ci_high  = CI(response)["upper"]),
                          by=condition]

p.pgs.coins.cond <- ggplot(pgs.coins.cond, aes(condition, mean, color=condition)) + 
  geom_hline(yintercept=5, linetype="dashed")+
  geom_point(size = 5, shape=18) + 
  geom_errorbar(aes(ymin=ci_low, ymax=ci_high), width=.25) +
  labs(x="Condition", y="Post-Game Capacity Judgment") +
  theme_bw(base_size=16)


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
  labs(x="Condition", y="Error in Insufficient Capacity Recall") +
  theme_bw(base_size=16)


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
  geom_errorbar(aes(ymin=ci_low_resp, ymax=ci_high_resp), width=.25) +
  geom_errorbarh(aes(xmin=ci_low_true, xmax=ci_high_true), height=.25) +
  labs(x="True Frequency of Insufficient Capacity", y="Recalled Frequency of Insufficient Capacity") +
  theme_bw(base_size=16)


#### Decision making ####
# mean collector size by round

col.cond <- blocks[,list(mean = mean(collector),
                         ci_low   = CI(collector)["lower"],
                         ci_high  = CI(collector)["upper"]), 
                   by=c("condition")]

p.col.cond <- ggplot(col.cond, aes(x=1, y=mean, color=condition)) + 
  geom_hline(data = rational.benchmarks[measure=="cost"],
             aes(yintercept=benchmark/50, color=condition),
             linetype="dashed", size=1.1) +
  geom_point(size = 5, shape=18) + 
  geom_errorbar(aes(ymin=ci_low, ymax=ci_high), width=.1) +
  scale_y_continuous(limits=c(4,7), breaks=seq(4,7,.5)) +
  scale_x_continuous(breaks=NULL) +
  labs(x="Condition", y="Collector Size") +
  facet_grid(. ~ condition) +
  theme_bw(base_size=16)

col.cond.block <- blocks[,list(mean = mean(collector),
                               ci_low   = CI(collector)["lower"],
                               ci_high  = CI(collector)["upper"]), 
                         by=c("condition","block")]

p.col.cond.block <- ggplot(col.cond.block, aes(factor(block), mean, color=condition)) + 
  geom_point(size = 3, shape=1) + 
  #geom_errorbar(aes(ymin=ci_low, ymax=ci_high), width=.25) +
  geom_line(aes(factor(block), mean, group=condition)) +
  geom_hline(data = rational.benchmarks[measure=="cost"],
             aes(yintercept=benchmark/50, color=condition),
             linetype="dashed", size=1.1) +
  geom_smooth(aes(group=1), method="lm") +
  labs(x="Bock", y="Collector Size") +
  facet_grid(. ~ condition) +
  theme_bw(base_size=16)

# number of switches
# variance in collector size
# collector size predicted by mean observed
cum.coin.indiv.block[,block_next := block+1]
cum.coin.indiv.block <- merge(cum.coin.indiv.block,
                              blocks[,list(user_id, block_next = block, collector)],
                              by=c("user_id","block_next"), all.x=TRUE)

cum.coin.indiv.block[, exp_dec_err:= collector-mean_coin]

exp.dec.err.cond <- cum.coin.indiv.block[!is.na(exp_dec_err),
                                         list(mean = mean(exp_dec_err),
                                               ci_low   = CI(exp_dec_err,)["lower"],
                                               ci_high  = CI(exp_dec_err)["upper"]), 
                                         by=c("condition")]

exp.dec.err.cond.block <- cum.coin.indiv.block[!is.na(exp_dec_err),
                                         list(mean = mean(exp_dec_err),
                                              ci_low   = CI(exp_dec_err,)["lower"],
                                              ci_high  = CI(exp_dec_err)["upper"]), 
                                         by=c("condition","block")]

p.exp.dec.err.cond.block <- ggplot(exp.dec.err.cond.block, aes(factor(block), mean, color=condition)) + 
  geom_point(size = 3, shape=1) + 
  #geom_errorbar(aes(ymin=ci_low, ymax=ci_high), width=.25) +
  geom_line(aes(factor(block), mean, group=condition)) +
  geom_hline(yintercept=0, linetype="dashed")+
  geom_smooth(aes(group=1), method="lm") +
  labs(x="After N Blocks", y="Collector Size Deviation from Experience") +
  facet_grid(. ~ condition) +
  theme_bw(base_size=16)

# behavior as fit by omnicient observer
# behavior as fit by model of rational behavior
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
  scale_x_continuous(breaks=1:10) +
  labs(x="Collector Size",
       y="Profit after 100 rounds",
       color="Condition") +
  theme_bw(base_size=16)


# behavior as fit by model of salient events weighted

#### Outcomes ####
# revenue, profit overall
rounds <- merge(rounds, blocks[,list(user_id, block, collector)],
                by=c("user_id","block"))
rounds[, capacity_excess := max(collector - coins_avail, 0), by=log_id]
rounds[, capacity_short  := min(collector - coins_avail, 0), by=log_id]
rounds[, capacity_error  := collector-coins_avail, by=log_id]


# rounds[, list(rational_revenue = min(7, coins_avail),
#               rationa_cost     = 7*5), by=log_id]
# rational.rev.indiv <- rounds[, list(condition=unique(condition),
#                                     rational_revenue = sum(rational_revenue)), 
#                                     by=user_id]



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



p.profit.cond <- ggplot(profit.cond, aes(condition, mean, color=condition)) + 
 # geom_hline(data=profit.ref.pts, aes(yintercept=benchmark), linetype="dashed") +
  geom_point(size = 5, shape=18) + 
  geom_errorbar(aes(ymin=ci_low, ymax=ci_high), width=.25) +
  labs(x="Condition", y="Coins") +
  theme_bw(base_size=16) + 
  facet_grid(measure ~ ., scales="free_y")

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

p.profit.cond.block <- ggplot(profit.cond.block, aes(factor(block), mean, color=condition)) + 
  geom_point(size = 3, shape=1) + 
  geom_line(aes(factor(block), mean, group=condition)) +
  geom_hline(data=rational.benchmarks, aes(yintercept=benchmark/10), linetype="dashed") +
  #geom_smooth(aes(group=1), method="lm") +
  labs(x="Block", y="Coins") +
  facet_grid(measure~condition, scales="free_y") +
  theme_bw(base_size=16)


