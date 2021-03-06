library(plyr)
library(psych)

####### ---------------------------------
#######  Data Prep
####### ---------------------------------

#load ratings data 
sva <- read.csv("http://samswift.org/data/SVA-data-2012-03-21.csv", stringsAsFactors=F)

#set empty strings to NA
sva[sva==""] <- NA

#remove uncessary columns
sva <- sva[,!names(sva) %in% c("V1","V2","V3","V4","V5","V7","V10")]

#rename unnamed columns
sva <- rename(sva, c("V6"="IP","V8"="StartTime","V9"="EndTime","X186k_miles"="186k_miles","X190_2.3"="model_190_2.3","X1991"="year_1991"))

#add simple IDs for each record
sva$ID <- factor(c(1:nrow(sva)))

ratings <- as.data.frame(sapply(sva[4:27],mean))
names(ratings) <- c("mean")
ratings$code <- rownames(ratings)

#combine summer_tires and winter_tires into seasonal_tires
M_seasonsal_tires <- mean(c(ratings[["summer_tires","mean"]],ratings[["winter_tires","mean"]]))
ratings[nrow(ratings)+1,] <- c(M_seasonsal_tires,"seasonal_tires")

#load text mapping table
sva.tmap <- read.csv("http://samswift.org/data/SVA-text-mapping-2012-04-03.csv",stringsAsFactors=F)
sva.tmap$X <-NULL
sva.tmap$words <- apply(sva.tmap, 1, function(row){paste(row[4:ncol(sva.tmap)],collapse=",")})
sva.tmap$words <- gsub(",,","",sva.tmap$words)

#merge mapped values and ratings
sva <- merge(sva.tmap[1:4], ratings, all.x=T, all.y=T)

#create word-value df
sva.rated <- subset(sva,rated=1)

wordValueMap <- data.frame(matrix(ncol = 3))
names(wordValueMap) <- c("word","code","rating")

for(i in 1:nrow(sva.rated)){
  code <- sva.rated[i,"code"]
  rating <-sva.rated[[i,"mean"]]
  words <- sva.rated[i,"words"]
  wordList <- strsplit(words,",")
  for(word in wordList){
    row <- data.frame(word,code,rating)
    names(row) <- c("word","code","rating")
    wordValueMap <- rbind(wordValueMap,row)
  }
}
wordValueMap <- wordValueMap[!is.na(wordValueMap$word),]
wordValueMap$rating <- as.numeric(wordValueMap$rating)
row.names(wordValueMap) <- wordValueMap$word

#clean up local variables
rm(row,code,i,word,wordList,words,rating,M_seasonsal_tires)




