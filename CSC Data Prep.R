#Give variables sensible names
names(csba)<- c("ID",
               "ResponseSet",
               "Name",
               "ExtDataRef",
               "QEmail",
               "IP",
               "Status",
               "StartDate",
               "EndDate",
               "Finished",
               "Source",
               "ConsGuidelines",
               "ConsText",
               "ConsRead",
               "ConsWilling",
               "Scenario",
               "ScenTFC",
               "ScenTLC",
               "ScenTS",
               "ScenTCC",
               "ArgOtherWeak",
               "ArgOtherWeakTFC",
               "ArgOtherWeakTLC",
               "ArgOtherWeakTS",
               "ArgOtherWeakTCC",
               "ArgOwnStrong",
               "ArgOwnStrongTFC",
               "ArgOwnStrongTLC",
               "ArgOwnStrongTS",
               "ArgOwnStrongTCC",
               "ArgNone",
               "ArgNoneTFC",
               "ArgNoneTLC",
               "ArgNoneTS",
               "ArgNoneTCC",
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
csba[!is.na(csba$ArgNoneTS),"ArgCond"]    <- "None"
csba[!is.na(csba$ArgOtherWeakTS),"ArgCond"] <- "OtherWeak"
csba[!is.na(csba$ArgOwnStrongTS),"ArgCond"] <- "OwnStrong"

#replace qualtrics guids with simple interger ID#s
csba$ID <- factor(c(1:nrow(csba)))

attach(csba)

#Consolidate Arg from each condition into a single set of variables
csba[ArgCond %in% "None","ArgTFC"]      <- csba[ArgCond %in% "None","ArgNoneTFC"]
csba[ArgCond %in% "None","ArgTLC"]      <- csba[ArgCond %in% "None","ArgNoneTLC"]
csba[ArgCond %in% "None","ArgTS"]       <- csba[ArgCond %in% "None","ArgNoneTS"]
csba[ArgCond %in% "None","ArgTCC"]      <- csba[ArgCond %in% "None","ArgNoneTCC"]
csba[ArgCond %in% "OtherWeak","Arg"]    <- as.character(csba[ArgCond %in% "OtherWeak","ArgOtherWeak"])
csba[ArgCond %in% "OtherWeak","ArgTFC"] <- csba[ArgCond %in% "OtherWeak","ArgOtherWeakTFC"]
csba[ArgCond %in% "OtherWeak","ArgTLC"] <- csba[ArgCond %in% "OtherWeak","ArgOtherWeakTLC"]
csba[ArgCond %in% "OtherWeak","ArgTS"]  <- csba[ArgCond %in% "OtherWeak","ArgOtherWeakTS"]
csba[ArgCond %in% "OtherWeak","ArgTCC"] <- csba[ArgCond %in% "OtherWeak","ArgOtherWeakTCC"]
csba[ArgCond %in% "OwnStrong","Arg"]    <- as.character(csba[ArgCond %in% "OwnStrong","ArgOwnStrong"])
csba[ArgCond %in% "OwnStrong","ArgTFC"] <- csba[ArgCond %in% "OwnStrong","ArgOwnStrongTFC"]
csba[ArgCond %in% "OwnStrong","ArgTLC"] <- csba[ArgCond %in% "OwnStrong","ArgOwnStrongTLC"]
csba[ArgCond %in% "OwnStrong","ArgTS"]  <- csba[ArgCond %in% "OwnStrong","ArgOwnStrongTS"]
csba[ArgCond %in% "OwnStrong","ArgTCC"] <- csba[ArgCond %in% "OwnStrong","ArgOwnStrongTCC"]

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
            "ArgOtherWeak",
            "ArgOtherWeakTFC",
            "ArgOtherWeakTLC",
            "ArgOtherWeakTS",
            "ArgOtherWeakTCC",
            "ArgOwnStrong",
            "ArgOwnStrongTFC",
            "ArgOwnStrongTLC",
            "ArgOwnStrongTS",
            "ArgOwnStrongTCC",
            "Null",
            "Null2",
            "Null3")
csba <- csba[, !(names(csba) %in% delete)]
rm(delete)

detach(csba)