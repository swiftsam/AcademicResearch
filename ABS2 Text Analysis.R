library(plyr)
library(tm)
library(ggplot2)

outputPlots <- FALSE

#ABS Text analysis

####### ---------------------------------
#######  Data Prep
####### ---------------------------------

#load and prep ABS2 data
#ABS2 <- read.csv("http://samswift.org/data/ABS2-data-2012-02-24.csv", stringsAsFactors=F)
#source("ABS2 Data Prep.R")
#table(ABS2$RoleCond)

#load SVA analyses for rated word value
source("SVA Analysis.R", echo=T)

####### ---------------------------------
#######  Functions
####### ---------------------------------

#function to calculate word frequency in a set
wordFreq <- function(vec,likelihood=TRUE){
  corp <- Corpus(VectorSource(vec))
  corp <- tm_map(corp, removePunctuation)
  corp <- tm_map(corp, tolower)
  corp <- tm_map(corp, function(x) removeWords(x, stopwords("english")))
  corp.tdm <- TermDocumentMatrix(corp) 
  
  m <- as.matrix(corp.tdm)
  v <- sort(rowSums(m),decreasing=TRUE)
  if(likelihood){
    l <- v/length(vec)
    d <- data.frame(word = names(v),freq=v,likelihood=l)       
  } else {
    d <- data.frame(word = names(v),freq=v)
  }
}

#function to compare word frequency between two sets
freqDiff <- function(x,y, by="word", minFreq = 0){
  df             <- merge(x,y,all.x=T, all.y=T,by=by)
  if(ncol(df)==3){
    names(df) <- c(by,"freqX","freqY")
  } else if(ncol(df)==5) {
    names(df) <- c(by,"freqX","likelihoodX","freqY","likelihoodY")  
  } else {
    stop("Wrong number of columns")    
  }

  df[is.na(df)]  <- 0
  df$freqDiff    <- df$freqX-df$freqY
  df$freqSum     <- df$freqX + df$freqY
  df$relFreqDiff <- df$freqDiff/df$freqSum
  df             <- arrange(df,relFreqDiff)
  dfMin          <- subset(df,freqSum > minFreq)
}

#function to compute topic frequency given word frequency and wordValueMap
topicFreq <- function(wf, wvm){
  tf <- ddply(wvm, "code", function(df){
    tf <- 0
    for(word in df$word){
      freq <- wf[word,"freq"]
      if(is.na(freq)){ freq <- 0}
      tf <- tf + freq
    }
    "TopicFreq" <- tf
  })
  names(tf) <- c("topic","freq")
  return(tf)
}

#function to compute sum topic value of a set
topicValueSum <- function(arg,wvm=wordValueMap){
  wf <- wordFreq(arg)
  tf <- topicFreq(wf,wvm)
  tf <- merge(tf, wvm, by.x="topic",by.y="code",all.x)
  tf <- tf[c("topic","freq","rating")]
  tf <- unique(tf)
  tf$value <- tf$freq * tf$rating
  tf$value[is.na(tf$value)]<-0
  valueSum <- sum(tf$value)
}

#function to compute the typicality of a argument for its role
argTypicality <- function(arg,wf.Arg.role){
  wf.Arg <- wordFreq(arg)
  argTyp <- merge(wf.Arg, wf.Arg.role[c("word","likelihood")], by="word",all.x=T,all.y=T)
  argTyp[is.na(argTyp)]<-0
  names(argTyp) <- c("word","observed","expected")
  typ <- cor(argTyp$observed, argTyp$expected)
}


####### ---------------------------------
#######  Compare Individual-level Argument, Salience, & Weight topic Values
####### ---------------------------------
#compute individual scores of arguments
for(i in 1:nrow(ABS2)){
  argVal <- 0
  if(!is.na(ABS2[i,"Arg"])){
    argVal <- topicValueSum(ABS2[i,"Arg"])
  }  
  ABS2[i,"ArgValue"] <- argVal
}

#compute individual value scores of salience
for(i in 1:nrow(ABS2)){
  salVal <- 0
  if(!is.na(ABS2[i,"AttrSalFre"])){
    salVal <- topicValueSum(ABS2[i,"AttrSalFre"])
  }  
  ABS2[i,"SalValue"] <- salVal
}

#compute individual value scores of issue weight
for(i in 1:nrow(ABS2)){
  issWVal <- 0
  if(!is.na(ABS2[i,"IssWFree"])){
    issWVal <- topicValueSum(ABS2[i,"IssWFree"])
  }  
  ABS2[i,"IssWValue"] <- issWVal
}


####### ---------------------------------
#######  Generate Word Frequency Lists
####### ---------------------------------

#All free response of salient attributes
wf.Sal <- wordFreq(ABS2[,"AttrSalFre"])

#Free response of salient attributes (Buyer)
wf.Sal.B <- wordFreq(ABS2[ABS2$RoleCond=="Buyer", "AttrSalFre"])

#Free response of salient attributes (Seller)
wf.Sal.S <- wordFreq(ABS2[ABS2$RoleCond=="Seller", "AttrSalFre"])

#All free response of important issues
wf.IssW <- wordFreq(ABS2[,"IssWFree"])

#Free response of important issues (Buyer)
wf.IssW.B <- wordFreq(ABS2[ABS2$RoleCond=="Buyer", "IssWFree"])

#Free response of important issues (Seller)
wf.IssW.S <- wordFreq(ABS2[ABS2$RoleCond=="Seller", "IssWFree"])

#All Arguments
wf.Arg <- wordFreq(ABS2[ABS2$ArgCond=="Arg","Arg"])

#Buyer Arguments
wf.Arg.B <- wordFreq(ABS2[ABS2$ArgCond=="Arg" & ABS2$RoleCond=="Buyer","Arg"])

#Seller Arguments
wf.Arg.S <-wordFreq(ABS2[ABS2$ArgCond=="Arg" & ABS2$RoleCond=="Seller","Arg"])

#Total word frequency across arguments, salience, and weight
wf.Overall <- merge(wf.Sal, wf.IssW, by="word", all.x=T, all.y=T)
wf.Overall <- merge(wf.Overall, wf.Arg, by="word", all.x=T, all.y=T)
names(wf.Overall) <- c("word","salFreq","issWFreq","argFreq")
wf.Overall[is.na(wf.Overall)]<-0
wf.Overall$overallFreq <- rowSums(wf.Overall[2:4])
wf.Overall$mapped <- wf.Overall$word %in% wordValueMap$word

wf.unmapped <- subset(wf.Overall,wf.Overall$mapped==FALSE)
wf.unmapped <- arrange(wf.unmapped,desc(overallFreq))

####### ---------------------------------
#######  Compare Individual-level Argument role typicality
####### ---------------------------------
#compute individual scores of arguments
for(i in 1:nrow(ABS2)){
  argTyp <- 0
  if(!is.na(ABS2[i,"Arg"])){
    roleCond <- ABS2[i,"RoleCond"]
    if(roleCond == "Buyer"){ wf <- wf.Arg.B }
    if(roleCond == "Seller"){ wf <- wf.Arg.S }
    argTyp <- argTypicality(ABS2[i,"Arg"],wf)
  }  
  ABS2[i,"ArgTyp"] <- argTyp
}

####### ---------------------------------
#######  Convert Word Frequency to Topic Frequency
####### ---------------------------------

tf.Arg.B <- topicFreq(wf.Arg.B, wordValueMap)
tf.Arg.S <- topicFreq(wf.Arg.S, wordValueMap)

tf.Sal.B <- topicFreq(wf.Sal.B, wordValueMap)
tf.Sal.S <- topicFreq(wf.Sal.S, wordValueMap)

tf.IssW.B <- topicFreq(wf.IssW.B, wordValueMap)
tf.IssW.S <- topicFreq(wf.IssW.S, wordValueMap)

####### ---------------------------------
#######  Compare Word and Topic Frequency by Role
####### ---------------------------------

#Buyer-Seller differences in salient attributes
wfDiff.BS.Sal <- freqDiff(wf.Sal.B, wf.Sal.S, minFreq=15)

#Buyer-Seller differences in Argument words
wfDiff.BS.Arg <- freqDiff(wf.Arg.B, wf.Arg.S, minFreq=15)

#Buyer-Seller differences in Issue Weight
wfDiff.BS.IssW <- freqDiff(wf.IssW.B, wf.IssW.S, minFreq=10)

#Buyer-Seller differences in Salient Topics
tfDiff.BS.Sal <- freqDiff(tf.Sal.B, tf.Sal.S, by="topic", minFreq=5)

#Buyer-Seller differences in Argument Topics
tfDiff.BS.Arg <- freqDiff(tf.Arg.B, tf.Arg.S, by="topic", minFreq=5)

#Buyer-Seller differences in Issue Weight
tfDiff.BS.IssW <- freqDiff(tf.IssW.B, tf.IssW.S, by="topic", minFreq=5)


####### ---------------------------------
#######  Match Word and Topic Frequency to coded values
####### ---------------------------------

wfDiff.BS.Arg <- merge(wfDiff.BS.Arg,wordValueMap,all.x=T)
wfDiff.BS.Sal <- merge(wfDiff.BS.Sal,wordValueMap,all.x=T)
wfDiff.BS.IssW <- merge(wfDiff.BS.IssW, wordValueMap, all.x=T)

tfDiff.BS.Arg <- merge(tfDiff.BS.Arg,ratings[c("mean","code")],by.x="topic",by.y="code",all.x=T)
tfDiff.BS.Sal <- merge(tfDiff.BS.Sal,ratings[c("mean","code")],by.x="topic",by.y="code",all.x=T)
tfDiff.BS.IssW <- merge(tfDiff.BS.IssW,ratings[c("mean","code")],by.x="topic",by.y="code",all.x=T)

####### ---------------------------------
#######  Plots
####### ---------------------------------

if(outputPlots){
  
#Relative Word Freq in Argument
ggplot(wfDiff.BS.Arg, aes(reorder(factor(word),relFreqDiff),relFreqDiff)) + 
  geom_bar(aes(fill=as.numeric(rating))) + 
  scale_fill_continuous(limits=c(-1.75, 1.75), low="red",high="green",name="Rated Value") +
  coord_flip() +
  scale_y_continuous() +
  xlab("Words") +
  ylab("Relative use by Sellers(-) & Buyers(+)") +
  opts(title="Relative word use in Argument by Role")

#Relative Topic Freq in Argument
ggplot(tfDiff.BS.Arg, aes(reorder(factor(topic),relFreqDiff),relFreqDiff)) + 
  geom_bar(aes(fill=as.numeric(mean))) +
  scale_fill_continuous(limits=c(-1.75, 1.75),low="red",high="green",name="Rated Value") +
  coord_flip() +
  scale_y_continuous() +
  xlab("Topics") +
  ylab("Relative use by Sellers(-) & Buyers(+)") +
  opts(title="Relative Topic use in Argument by Role")

#Relative Word Freq in Salience
ggplot(wfDiff.BS.Sal, aes(reorder(factor(word),relFreqDiff),relFreqDiff)) + 
  geom_bar(aes(fill=as.numeric(rating))) +
  scale_fill_continuous(limits=c(-1.75, 1.75), low="red",high="green",name="Rated Value") +
  coord_flip() +
  scale_y_continuous() +
  xlab("Words") +
  ylab("Relative use by Sellers(-) & Buyers(+)") +
  opts(title="Relative word use in 'Most Memorable Details' by Role")

#Relative Topic Freq in Salience
ggplot(tfDiff.BS.Sal, aes(reorder(factor(topic),relFreqDiff),relFreqDiff)) + 
  geom_bar(aes(fill=as.numeric(mean))) +
  scale_fill_continuous(limits=c(-1.75, 1.75),low="red",high="green",name="Rated Value") +
  coord_flip() +
  scale_y_continuous() +
  xlab("Topics") +
  ylab("Relative use by Sellers(-) & Buyers(+)") +
  opts(title="Relative Topic use in 'Most Memorable Details' by Role")

#Relative Word Freq in Issue Weight
ggplot(wfDiff.BS.IssW, aes(reorder(factor(word),relFreqDiff),relFreqDiff)) + 
  geom_bar(aes(fill=as.numeric(rating))) +
  scale_fill_continuous(limits=c(-1.75, 1.75), low="red",high="green",name="Rated Value") +
  coord_flip() +
  scale_y_continuous() +
  xlab("Words") +
  ylab("Relative use by Sellers(-) & Buyers(+)") +
  opts(title="Relative word use in 'Most Important Attributes' by Role")

#Relative Topic Freq in Issue Weight
ggplot(tfDiff.BS.IssW, aes(reorder(factor(topic),relFreqDiff),relFreqDiff)) + 
  geom_bar(aes(fill=as.numeric(mean))) +
  scale_fill_continuous(low="red", high="green", name="Rated Value") +
  coord_flip() +
  xlab("Topics") +
  ylab("Relative use by Sellers(-) & Buyers(+)") +
  opts(title="Relative Topic use in 'Most Important Attributes' by Role")
}
