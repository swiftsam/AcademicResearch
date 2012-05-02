## By Benoit Monin 2009 monin@stanford.edu
## Please let me know about any error you may find so I can fix it

## Simple mediation analysis a la Baron & Kenny (1986)
## Version 1.0
## Creates table with a, b, c, c' Sobel, Goodman, and their significance
## Based on normal distribution assumptions

## This is the "lite" version
## For a more detailed output use bm.med.writeup()

## Simply run the code below into R to begin using bm.med()

bm.med<-function(x,med,y) {
  summary(lm(y~x))$coefficients[2,1]->c;
  summary(lm(y~x))$coefficients[2,4]->sigc;
  summary(lm(med~x))$coefficients[2,1]->a;
  summary(lm(med~x))$coefficients[2,2]->sa;
  summary(lm(med~x))$coefficients[2,4]->siga;
  summary(lm(y~x+med))$coefficients[2,1]->cprime;
  summary(lm(y~x+med))$coefficients[2,4]->sigcprime;
  summary(lm(y~x+med))$coefficients[3,1]->b;
  summary(lm(y~x+med))$coefficients[3,2]->sb;
  summary(lm(y~x+med))$coefficients[3,4]->sigb;
  sobelsab<-sqrt(b^2*sa^2+a^2*sb^2+sa^2*sb^2);
  sobelz<-abs(a*b)/sobelsab;
  goodmansab<-sqrt(b^2*sa^2+a^2*sb^2-sa^2*sb^2);
  goodmanz<-abs(a*b)/goodmansab;
  round(rbind(c(c=c,"c'"=cprime,a=a,b=b,ab=a*b,Sobel=sobelz,Goodman=goodmanz),c(sigc,sigcprime,siga,sigb,NA,2*(1-pnorm(sobelz)),2*(1-pnorm(goodmanz)))),3)->output_table;
  rownames(output_table)<-c("Coeff","p val");
  print(output_table);
}



## By Benoit Monin 2009 monin@stanford.edu
## Please let me know about any error you may find so I can fix it

## Bootstrapping mediation based on Preacher & Hayes (2004)
## Version 2.0
## Requires bm.med()
## Includes a bias correction, but no acceleration

bm.bootstrapmed<-function(x,med,y,iterations=1000,alpha=.05,outputPlots=FALSE) {
  as.data.frame(cbind(x,med,y))->vars;
  length(x)->N;
  bootab<-vector()
  for (i in 1:iterations) {
    sample(c(1:N),N,replace=T)->sampnums;
    lm(vars[sampnums,2]~vars[sampnums,1])$coefficients[2]->itera;
    lm(vars[sampnums,3]~vars[sampnums,2]+vars[sampnums,1])$coefficients[2]->iterb;
    (append(bootab,itera*iterb))->bootab
  }
  if(outputPlots){
    hist(bootab,main=paste("Bootsrapped a*b, with",iterations,"iterations"),col="red");
  }
  bm.med(x,med,y)[1,5]->ab
  # Bias correction after Stine (1989)
  sum(bootab<=ab)/iterations->prob
  qnorm(prob)->Z0
  round(pnorm(2*Z0+qnorm(alpha/2)),3)->bcl
  round(pnorm(2*Z0+qnorm(1-alpha/2)),3)->bcu
  print("Bootstrap results:",quote=F)
  print(round(c("Mean(ab*)"=mean(bootab),"p(ab*<ab)"=prob),3))
  print("Uncorrected:",quote=F)
  print(round(quantile(bootab,c(alpha/2,1-alpha/2)),3))
  print("Bias Corrected:",quote=F)
  print(round(quantile(bootab,c(bcl,bcu)),3))
}