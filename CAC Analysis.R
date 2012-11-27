# Load libraries
library(data.table)
library(plyr)
library(lme4)
library(ggplot2)

# Define Constants
base.path <- "~/ACE/data"
base.path.yr1 <- "~/ACE/data/Year1"
psf <- read.csv(file.path(paste(base.path.yr1,"processedSurveyForecasts.yr1.csv",sep="/")), stringsAsFactors=F)
ifps <- read.csv(file.path(paste(base.path,"IfpidToQuestionTextAndAnswers.csv",sep="/")), stringsAsFactors=F)
users <- read.csv(file.path(paste(base.path.yr1,"UserActivity.yr1.csv",sep="/")), stringsAsFactors=F)

# 1. Are people overconfident?  By how much? 
## Compare average confidence with average hit rates
### For each forecast, select the option that the forecaster thinks is most likely

psf.dt  <- data.table(psf, key="answer.id")  # convert to a data.table (for speed)
psf.max <- psf.dt[psf.dt[,Value==max(Value),by=answer.id][[2]]] # select the max p from each forecast
psf.max <- psf.max[!duplicated(psf.max),] # remove duplicate rows from .5 forecasts

### Compute hit for each one: 1 if the chosen option occurred, 0 otherwise
psf.max <- join(psf.max, ifps[c("Ifpid","CorrectAnswer")]) # merge correct answers
psf.max$hit <- 0
psf.max$hit[psf.max$AnswerOption == psf.max$CorrectAnswer] <- 1

### Compute mean confidence and overall hit rate.  Does confidence exceed accuracy?
mean(psf.max$Value)
mean(psf.max$hit)

ggplot(psf.max, aes(Value, fill=factor(hit))) + geom_density(alpha=.3) 

### Do this analysis properly, using a mixed model in which confidence is nested within questions and forecasters.  
logit <- glm(hit ~ Value, data = psf.max, family = "binomial")

###    Include experimental condition and training as independent variables.
## Compare confidence and hit rates across the range

# 2. Does it change over time? 
## Re-do the mixed model above (1.a.iv. confidence nested within questions and forecasters) with time as a covariate.  
##     Does confidence change over time within a given question?  It should.  Does it go up faster than does hit rate?

# 3. How does self-rated expertise moderate the effect?
## If we treat self-rated expertise as an independent variable, how much of the variance in confidence does it account for?

# 4. How does self-rated expertise moderate the confidence-accuracy relationship?
## Are self-rated experts more or less overconfident than self-rated novices?

# 5. How does self-rated expertise change with time, feedback, and experience?
## Conduct a mixed model looking at all three independents.
### Time is simply the linear passage of time. 
### Feedback would be the number of closed questions on which the person has made forecasts.
### Experience would be the number of forecasts made.

# 6. How does confidence change with time, feedback, and experience?

# 7. How is the confidence-accuracy relationship moderated by individual difference variables like intelligence and education? 
## Do more intelligent people claim 100% confidence less often? 
## Do more intelligent people claim high expertise more often? 
## Do more educated people claim expertise more often?

# 8. Does experimental condition affect the confidence-accuracy relationship?

# 9. Does self-rated expertise affect resolution (a forecaster’s ability to distinguish between events that occur and those that don’t)?
## To compute resolution, compare average confidence for the outcome that occurs with the summed confidence across all other possible outcomes.  
##     The larger this difference, the better an individual’s resolution.  Compare different expertise ratings.

# 10. Consider trying to use Year2 data on questions that have closed to replicate the analyses above.
