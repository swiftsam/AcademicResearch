ABS.lotto <- read.csv("ABS-lottery.csv", stringsAsFactors=F, na.strings=c(""))
ASM.lotto <- read.csv("ASM-lottery.csv", stringsAsFactors=F, na.strings=c(""))

lotto <- rbind(ABS.lotto,ASM.lotto)

####### ---------------------------------
#######  Pick Lottery Winners
####### ---------------------------------

lotto.CBL <- subset(lotto,lotto$src=="cbl")
winner <- sample(rownames(lotto.CBL),1)
lotto.CBL[winner,]

lotto <- subset(lotto, !lotto$src=="cbl")
winners <- sample(rownames(lotto), round(nrow(lotto)/50))
lotto[winners,]



