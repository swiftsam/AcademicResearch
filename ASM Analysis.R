library(plyr)
library(ggplot2)

#load data
ASM <- read.csv("~/research/ASM.csv", stringsAsFactors=F, na.strings=c(""))

#remove uncessary columns
ASM <- ASM[,!names(ASM) %in% c("X.V1","V2","V3","V4","V5","V7","V10",
                               "Consent2","Consent3","Consent4",
                               "BScen_1","BScenTime_1","BScenTime_2","BScenTime_4",
                               "BArgTime_1","BArgTime_2","BArgTime_4",
                               "BNoArgTime_1","BNoArgTime_2","BNoArgTime_4",
                               "BNoArgExpT_1","BNoArgExpT_2","BNoArgExpT_4",
                               "X")]
ASM <- rename(ASM, c("V6"="IP","V8"="Starttime","V9"="Endtime","Q65"="IssWSafe","IssWFuel"="SSVExcite"))

ASM$ArgCond <- rep(NA, nrow(ASM))
ASM$ArgCond[which(ASM$ArgTime_3] <- "Arg"
ASM$ArgCond[which(ASM$NoArgTime_3] <- "NoArg"
ASM$ArgCond[which(ASM$NoArgTime_3] <- "ArgExp"
ASM$ArgCond <- factor(ASM$ArgCond)
ASM$src <-factor(ASM$src)

