#Give variables sensible names
names(csc)<- c("QualtricsID",
               "ResponseSet",
               "Name",
               "ExtDataRef",
               "QEmail",
               "IP",
               "Status",
               "StartDate",
               "EndDate",
               "Finished",
               "DemSource",
               "ConsGuidelines",
               "ConsText",
               "ConsRead",
               "ConsWilling",
               "Scenario",
               "ScenTFC",
               "ScenTLC",
               "ScenTS",
               "ScenTCC",
               "ArgOtherHigh",
               "ArgOtherHighTFC",
               "ArgOtherHighTLC",
               "ArgOtherHighTS",
               "ArgOtherHighTCC",
               "ArgOwnLow",
               "ArgOwnLowTFC",
               "ArgOwnLowTLC",
               "ArgOwnLowTS",
               "ArgOwnLowTCC",
               "ArgOtherBatna",
               "ArgOtherBatnaTFC",
               "ArgOtherBatnaTLC",
               "ArgOtherBatnaTS",
               "ArgOtherBatnaTCC",
               "ArgOwnBatna",
               "ArgOwnBatnaTFC",
               "ArgOwnBatnaTLC",
               "ArgOwnBatnaTS",
               "ArgOwnBatnaTCC",
               "ArgNone",
               "ArgNoneTFC",
               "ArgNoneTLC",
               "ArgNoneTS",
               "ArgNoneTCC",
               "ArgFree",
               "ArgFreeTFC",
               "ArgFreeTLC",
               "ArgFreeTS",
               "ArgFreeTCC",
               "Null",
               "SatQual",
               "SatNet",
               "SatCust",
               "GlobSat",
               "GlobSatFavorable",
               "GlobSatRecommend",
               "GlobSatRenew",
               "GlobSatTrust",
               "Null2",
               "SalQual",
               "SalNet",
               "SalCust",
               "SalOther",
               "WeightQual",
               "WeightNet",
               "WeightCust",
               "RankQual",
               "RankNet",
               "RankCust",
               "WeightOther",
               "ProcComfort",
               "ProcDifficult",
               "ProbConcession",
               "ExageratePos",
               "ExagerateNeg",
               "Strategy",
               "ChangeRelationship",
               "Null3",
               "RespSat",
               "RespFair",
               "GlobSatPost",
               "GlobSatFavorablePost",
               "GlobSatRecommendPost",
               "GlobSatRenewPost",
               "GlobSatTrustPost",
               "DemYOB",
               "DemState",
               "DemGender",
               "DemEdu",
               "DemLang",
               "DemSubscribe",
               "DemSat",
               "DemNegExp",
               "DemRecall",
               "DemEmail",
               "Comments"
               )

#Identify Conditions
csc[!is.na(csc$ArgNoneTS),"ArgCond"]      <- "None"
csc[!is.na(csc$ArgFreeTS),"ArgCond"]      <- "Free"
csc[!is.na(csc$ArgOtherHighTS),"ArgCond"] <- "OtherHigh"
csc[!is.na(csc$ArgOwnLowTS),"ArgCond"]    <- "OwnLow"
csc[!is.na(csc$ArgOtherBatnaTS),"ArgCond"]<- "OtherBatna"
csc[!is.na(csc$ArgOwnBatnaTS),"ArgCond"]  <- "OwnBatna"

#add simple interger ID#s
csc$ID <- factor(c(1:nrow(csc)))

#replace blank emails with NAs
csc[nchar(csc$DemEmail)==0,"DemEmail"] <- NA

attach(csc)

#Consolidate Arg from each condition into a single set of variables
csc[ArgCond %in% "None",      "ArgTFC"] <- csc[ArgCond %in% "None",      "ArgNoneTFC"]
csc[ArgCond %in% "Free",      "ArgTFC"] <- csc[ArgCond %in% "Free",      "ArgFreeTFC"]
csc[ArgCond %in% "OtherHigh", "ArgTFC"] <- csc[ArgCond %in% "OtherHigh", "ArgOtherHighTFC"]
csc[ArgCond %in% "OwnLow",    "ArgTFC"] <- csc[ArgCond %in% "OwnLow",    "ArgOwnLowTFC"]
csc[ArgCond %in% "OtherBatna","ArgTFC"] <- csc[ArgCond %in% "OtherBatna","ArgOtherBatnaTFC"]
csc[ArgCond %in% "OwnBatna",  "ArgTFC"] <- csc[ArgCond %in% "OwnBatna",  "ArgOwnBatnaTFC"]

csc[ArgCond %in% "None",      "ArgTLC"] <- csc[ArgCond %in% "None",      "ArgNoneTLC"]
csc[ArgCond %in% "Free",      "ArgTLC"] <- csc[ArgCond %in% "Free",      "ArgFreeTLC"]
csc[ArgCond %in% "OtherHigh", "ArgTLC"] <- csc[ArgCond %in% "OtherHigh", "ArgOtherHighTLC"]
csc[ArgCond %in% "OwnLow",    "ArgTLC"] <- csc[ArgCond %in% "OwnLow",    "ArgOwnLowTLC"]
csc[ArgCond %in% "OtherBatna","ArgTLC"] <- csc[ArgCond %in% "OtherBatna","ArgOtherBatnaTLC"]
csc[ArgCond %in% "OwnBatna",  "ArgTLC"] <- csc[ArgCond %in% "OwnBatna",  "ArgOwnBatnaTLC"]

csc[ArgCond %in% "None",      "ArgTS"]  <- csc[ArgCond %in% "None",      "ArgNoneTS"]
csc[ArgCond %in% "Free",      "ArgTS"]  <- csc[ArgCond %in% "Free",      "ArgFreeTS"]
csc[ArgCond %in% "OtherHigh", "ArgTS"]  <- csc[ArgCond %in% "OtherHigh", "ArgOtherHighTS"]
csc[ArgCond %in% "OwnLow",    "ArgTS"]  <- csc[ArgCond %in% "OwnLow",    "ArgOwnLowTS"]
csc[ArgCond %in% "OtherBatna","ArgTS"]  <- csc[ArgCond %in% "OtherBatna","ArgOtherBatnaTS"]
csc[ArgCond %in% "OwnBatna",  "ArgTS"]  <- csc[ArgCond %in% "OwnBatna",  "ArgOwnBatnaTS"]

csc[ArgCond %in% "None",      "ArgTCC"] <- csc[ArgCond %in% "None",      "ArgNoneTCC"]
csc[ArgCond %in% "Free",      "ArgTCC"] <- csc[ArgCond %in% "Free",      "ArgFreeTCC"]
csc[ArgCond %in% "OtherHigh", "ArgTCC"] <- csc[ArgCond %in% "OtherHigh", "ArgOtherHighTCC"]
csc[ArgCond %in% "OwnLow",    "ArgTCC"] <- csc[ArgCond %in% "OwnLow",    "ArgOwnLowTCC"]
csc[ArgCond %in% "OtherBatna","ArgTCC"] <- csc[ArgCond %in% "OtherBatna","ArgOtherBatnaTCC"]
csc[ArgCond %in% "OwnBatna",  "ArgTCC"] <- csc[ArgCond %in% "OwnBatna",  "ArgOwnBatnaTCC"]

csc[ArgCond %in% "Free",      "Arg"]    <- as.character(csc[ArgCond %in% "Free",      "ArgFree"])
csc[ArgCond %in% "OtherHigh", "Arg"]    <- as.character(csc[ArgCond %in% "OtherHigh", "ArgOtherHigh"])
csc[ArgCond %in% "OwnLow",    "Arg"]    <- as.character(csc[ArgCond %in% "OwnLow",    "ArgOwnLow"])
csc[ArgCond %in% "OtherBatna","Arg"]    <- as.character(csc[ArgCond %in% "OtherBatna","ArgOtherBatna"])
csc[ArgCond %in% "OwnBatna",  "Arg"]    <- as.character(csc[ArgCond %in% "OwnBatna",  "ArgOwnBatna"])

#Delineate Argument constructs
library(car)
csc$ArgTarget <- recode(csc$ArgCond,"c('OtherBatna','OtherHigh')='Other';c('OwnBatna','OwnLow')='Own';else=NA")
csc$ArgType <- recode(csc$ArgCond,"c('OtherBatna','OwnBatna')='Batna';c('OtherHigh','OwnLow')='Value';else=NA")

delete <- c("ResponseSet",
            "Name",
            "ExtDataRef",
            "QEmail",
            "Status",
            "ConsGuidelines",
            "ConsText",
            "ConsRead",
            "ConsWilling",
            "Scenario",
            "ScenTFC",
            "ScenTLC",
            "ScenTCC",
            "ArgNone",
            "ArgNoneTFC",
            "ArgNoneTLC",
            "ArgNoneTS",
            "ArgNoneTCC",
            "ArgFree",
            "ArgFreeTFC",
            "ArgFreeTLC",
            "ArgFreeTS",
            "ArgFreeTCC",
            "ArgOtherHigh",
            "ArgOtherHighTFC",
            "ArgOtherHighTLC",
            "ArgOtherHighTS",
            "ArgOtherHighTCC",
            "ArgOwnLow",
            "ArgOwnLowTFC",
            "ArgOwnLowTLC",
            "ArgOwnLowTS",
            "ArgOwnLowTCC",
            "ArgOtherBatna",
            "ArgOtherBatnaTFC",
            "ArgOtherBatnaTLC",
            "ArgOtherBatnaTS",
            "ArgOtherBatnaTCC",
            "ArgOwnBatna",
            "ArgOwnBatnaTFC",
            "ArgOwnBatnaTLC",
            "ArgOwnBatnaTS",
            "ArgOwnBatnaTCC",            
            "Null",
            "Null2",
            "Null3")
csc <- csc[, !(names(csc) %in% delete)]
rm(delete)

detach(csc)
