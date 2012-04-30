outputPlots <- FALSE

#libraries
library(Hmisc)    #for rcorr()
library(sciplot)  #for bargraph.CI()
library(psych)

#load data
jnm <- read.table("http://swift.cbdr.cmu.edu/data/JNM-data-2010-07-30.csv", header=TRUE, sep=",")

#transform data
jnm$OfferCount <- jnm$OfferCount - 1                #adjust the values of OfferCount since option 1 = 0
jnm$SatisfactionDelta <- jnm$SatisfactionDelta - 4  #adjust the values of Satisfaction Delta since 4 = no change and lower numbers are negative
jnm$Negotiate <- factor(jnm$Negotiate, levels = c(2,1),labels=c("Did Not Negotiate","Negotiated"))
summary(jnm)
describe.by(jnm,group=jnm$Negotiate)


attach(jnm)

#Comparison of Negotiate vs Did Not Negotiate groups
t.test(Satisfaction~Negotiate)
t.test(OfferCount~Negotiate)
t.test(Percentile~Negotiate)
t.test(SatisfactionDelta~Negotiate)
t.test(subset(jnm,Negotiate=="Negotiated")$SatisfactionDelta,mu=0)

#Correlations
rcorr(cbind(Satisfaction,Percentile))
rcorr(cbind(Satisfaction,PersuasiveSelf))
rcorr(cbind(SatisfactionDelta,PersuasiveSelf))

#Graphs
if(outputPlots){
  bargraph.CI(Negotiate,
              OfferCount,
              ylim=c(0,3),
              col=c("darkgoldenrod2","lightblue4"),
              xlab="Choice to Negotiate",
              ylab="Number of Offers",
              main="Number of Offers",
              cex.main=2,
              cex.lab=1.5)

bargraph.CI(Negotiate,
            Percentile,
            ylim=c(0,100),
            col=c("darkgoldenrod2","lightblue4"),
            xlab="Choice to Negotiate",
            ylab="Estimated Percentile",
            main="Offer Percentile",
            cex.main=2,
            cex.lab=1.5)

bargraph.CI(Negotiate,
            Satisfaction, 
            ylim=c(1,7),            
            col=c("darkgoldenrod2","lightblue4"),
            xlab="Choice to Negotiate", 
            ylab="Mean Satisfaction",
            main="Satisfaction with Outcome",
            cex.main=2,
            cex.lab=1.5)

bargraph.CI(Negotiate,
            SatisfactionDelta,
            ylim=c(-.5,1.5),
            col=c("darkgoldenrod2","lightblue4"),
            xlab="Choice to Negotiate",
            ylab="Reported Change in Satisfaction",
            main="Change in Satisfaction\nfrom Received to Accepted",
            cex.main=2,
            cex.lab=1.5)
}

