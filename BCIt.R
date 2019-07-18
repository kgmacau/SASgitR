
#########################################################
####### Raw data                              ###########
#########################################################

library(boot)
library(caret)

value_tru<-rep(c(0, 1, 1, 0, 1, 0, 1, 0, 0, 0, 1, 1, 0, 0), 20)
value_pre<-rep(c(1, 1, 0, 1, 0, 1, 1, 1, 0, 0, 1, 1, 0, 0), 20)
rawdata<-data.frame(value_tru, value_pre)


### Factor
rawdata$value_tru<-factor(ifelse(rawdata$value_tru >0, "Positive", "Negative"))
rawdata$value_pre<-factor(ifelse(rawdata$value_pre >0, "Positive", "Negative"))
confusionMatrix(data = rawdata$value_pre, reference = rawdata$value_tru)




#########################################################
####### Bootstraping CI for balanced accuracy ###########
#########################################################

bacc<- function(data, i) { 
  d=data[i,]
  bacc<-(sensitivity(d$value_pre, d$value_tru)+specificity(d$value_pre, d$value_tru))/2
  c(bacc)
}


bacc.boot<-boot(rawdata, bacc, R = 10000)
boot.ci(bacc.boot, conf=0.95, type = c("bca", "norm", "basic", "perc"))

help(boot.ci)


################################################################
####### CI for balanced accuracy based on Chen Method###########
################################################################
baccCI<-function(data, alpha=0.05){
  
        bacc<-(sensitivity(data[, 2], data[, 1])+specificity(data[, 2], data[, 1]))/2
        
        crosstable<-table(data[, 2], data[, 1])
        a<-crosstable[1, 1]
        b<-crosstable[1, 2]
        c<-crosstable[2, 1]
        d<-crosstable[2, 2]
        p1<-(d/(b+d))**2
        p2<-a*c/((a+c)**3)
        p3<-(a/(a+c))**2
        p4<-b*d/((b+d)**3)
        varbacc<-p1*p2+p3*p4
        sebacc<-sqrt(varbacc)
        lowerCI<-bacc-qnorm(1-alpha/2)*sebacc
        upperCI<-bacc+qnorm(1-alpha/2)*sebacc
        
        cat(" Balanced accuracy =",  bacc, "\n", 
            "standard error =",  sebacc, "\n",
            "CI for balanced accuracy = [", lowerCI, ",", upperCI, "]")
        
}       

baccCI(rawdata)        



################################################################
####### meta analysis of balanced accuracy using micp###########
################################################################
library(micp)
ks<-rbind(rep(80, 2), rep(80, 2))
ns<-rbind(rep(120, 2), rep(160, 2))
micp.stats(ks, ns)


## https://stat.ethz.ch/pipermail/r-help/2012-February/303977.html
## https://pages.uoregon.edu/flournoy/bootstrapping/bootstrapexample.html
