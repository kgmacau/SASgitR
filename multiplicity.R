
install.packages("xlsx")
library("xlsx")
library(knitr)
library(BSDA)
library(mvtnorm)



####################################################################################################
##### step 1: Draw two correlated normal distribution samples using rmvnorm functionvia simulations
##### step 2: Calculate the whole type I error of five methods
####################################################################################################
multiplicity_type1error<- function (simulation, samplesize, cov, mean1, mean2, var1, var2){
  
  x<-do.call(rbind, lapply(1:simulation, function(i) {
    sigma <- matrix(cov, nrow = 2)
    x <- rmvnorm(samplesize, mean = c(mean1, mean2), sigma = sigma, method = "chol")
    
    x1<-x[, 1]; 
    x2<-x[, 2];
    
    c(p1=z.test(x1, mean1, var1), p2=z.test(x2, mean2, var2))
  }))
  
  
  
  ##########################
  ### Bonferroni Adjustment#
  ##########################
  Bon<-ifelse(x[, 1]<=0.05/2 | x[, 2]<=0.05/2, 1, 2)
  Bon_T1Err<-table(Bon==1)[names(table(Bon==1))=="TRUE"]/simulation
  
  
  
  
  #########################
  ### Jakobsen Adjustment #
  #########################
  Jakobsen<-ifelse(x[, 1]<=0.05*2/3 | x[, 2]<=0.05*2/3, 1, 2)
  Jakobsen_T1Err<-table(Jakobsen==1)[names(table(Jakobsen==1))=="TRUE"]/simulation
  
  
  
  
  #########################
  ### Hochberg Adjustment #
  #########################
  xmax<-apply(x, 1, max)
  xmin<-apply(x, 1, min)
  Hochberg<-ifelse(xmax<=0.05 | (xmax>0.05 & xmin<=0.025), 1, 2)
  Hochberg_T1Err<-table(Hochberg==1)[names(table(Hochberg==1))=="TRUE"]/simulation
  
  
  
  ###############################################
  ### sequence adjustment ############
  ###############################################
  seq<-ifelse(x[, 1]<=0.05, 1, 2)
  seq_T1Err<-table(seq==1)[names(table(seq==1))=="TRUE"]/simulation
  
  
  
  ###############################################
  ### all or nothing 0.05 adjustment ############
  ###############################################
  all<-ifelse(x[, 1]<=0.05 & x[, 2]<=0.05, 1, 2)
  all_T1Err<-table(all==1)[names(table(all==1))=="TRUE"]/simulation
  
  
  c(all_T1Err=all_T1Err, Bon_T1Err=Bon_T1Err, 
    Hochberg_T1Err=Hochberg_T1Err, seq_T1Err=seq_T1Err,
    Jakobsen_T1Err=Jakobsen_T1Err)
  
}





####################################################################################################
##### step 1: Draw two correlated normal distribution samples using rmvnorm functionvia simulations
##### step 2: Calculate the whole power of five methods
####################################################################################################
multiplicity_power<- function (simulation, samplesize, cov, mean, u01, u02, var1, var2){
  
  x<-do.call(rbind, lapply(1:simulation, function(i) {
    library(mvtnorm)
    sigma <- matrix(cov, nrow = 2)
    x <- rmvnorm(samplesize, mean = mean, sigma = sigma, method = "chol")
    
    x1<-x[, 1]; 
    x2<-x[, 2];
    
    c(p1=z.test(x1, u01, var1), p2=z.test(x2, u02, var2))
  }))
  
  
  
  ##########################
  ### Bonferroni power#
  ##########################
  Bon<-ifelse(x[, 1]<=0.05/2 | x[, 2]<=0.05/2, 1, 2)
  Bon_power<-table(Bon==1)[names(table(Bon==1))=="TRUE"]/simulation
  
  
  
  
  #########################
  ### Jakobsen power #
  #########################
  Jakobsen<-ifelse(x[, 1]<=0.05*2/3 | x[, 2]<=0.05*2/3, 1, 2)
  Jakobsen_power<-table(Jakobsen==1)[names(table(Jakobsen==1))=="TRUE"]/simulation
  
  
  
  
  
  #########################
  ### Hochberg Power #
  #########################
  xmax<-apply(x, 1, max)
  xmin<-apply(x, 1, min)
  Hochberg<-ifelse(xmax<=0.05 | (xmax>0.05 & xmin<=0.025), 1, 2)
  Hochberg_power<-table(Hochberg==1)[names(table(Hochberg==1))=="TRUE"]/simulation
  
  
  
  
  ####################################
  ### sequence power############
  ####################################
  seq<-ifelse(x[, 1]<=0.05 & x[, 2]<=0.05, 1, 2)
  seq_power<-table(seq==1)[names(table(seq==1))=="TRUE"]/simulation
  
  
  
  ####################################
  ### all or nothing power############
  ####################################
  all<-ifelse(x[, 1]<=0.05 & x[, 2]<=0.05, 1, 2)
  all_power<-table(all==1)[names(table(all==1))=="TRUE"]/simulation
  
  
  c(all_power=all_power, Bon_power=Bon_power, 
    Hochberg_power=Hochberg_power, seq_power=seq_power, 
    Jakobsen_power=Jakobsen_power)
  
}


### Comparison of whole type I error using five methods
cov<-matrix(seq(0, 1, by=0.01), nrow=101)
error<-matrix(rep(0, 505), nrow=length(cov))

for (i in 1:length(cov)){
     error[i, ]<-multiplicity_type1error(10000, 200, c(1, cov[i], cov[i], 1), 0, 0, 1, 1)
}

error_table<-data.frame(cov, error)

colnames(error_table)<-c("covariance", "All-or-none", "Bonferroni", "Hochberg", "Sequence", "Jakobsen")

kable(error_table)
write.csv(error_table, "C:/Users/212697818/Desktop/multiplicity.csv")


# Reference: https://crumplab.github.io/programmingforpsych/simulating-and-analyzing-data-in-r.html
