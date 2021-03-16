
rm(list = ls())

library(RBesT)
library(mvtnorm)


dbd.rmp.normal<-function(wt, es.source, var.source, N.source, es.target, var.target, N.target, type){
  
  ### variance and standard error
  SE.target=sqrt(var.target/N.target)
  SE.source=sqrt(var.source/N.source)
  sigma.ref<-sqrt(var.target)
  
  ### set prior
  source.prior <- mixnorm(source=c(1, es.source, SE.source), sigma=sigma.ref)
  robust.mix.prior <- robustify(source.prior, weight=(1-wt), m=0, n=1)
  
  ### posterior
  target.posterior <- postmix(robust.mix.prior, m=es.target, se=SE.target)
  post.weight <- target.posterior[1,1]
  
  ### log
  if(type=="log_exp"){
    post.mean <- target.posterior[1,1]*exp(target.posterior[2,1])+target.posterior[1,2]*exp(target.posterior[2,2])
    post.median <- exp(qmix(target.posterior, 0.5))
    lower.Cri<-exp(qmix(target.posterior,0.025))
    upper.Cri<-exp(qmix(target.posterior, 0.975))
  }
  ### normal
  else if(type=="normal"){
    post.mean <- target.posterior[1,1]*target.posterior[2,1]+target.posterior[1,2]*target.posterior[2,2]
    post.median <- qmix(target.posterior, 0.5)
    lower.Cri<-qmix(target.posterior, 0.025)
    upper.Cri<-qmix(target.posterior, 0.975)
  }
  
  ### output
  out = c(N.target=N.target, 
          prior.weight=robust.mix.prior[1,1], post.weight=post.weight, 
          post.mean=post.mean, post.median=post.median, 
          lower.Cri=lower.Cri, upper.Cri=upper.Cri)
  
  class(out) = 'BDB.RMP'
  return(out)
}

print.BDB.RMP = function(x) {
  cat("targer sample size =",  x['N.target'], "\n",
      "prior weight =",  x['prior.weight'], "\n", 
      "post weight =",  x['post.weight'], "\n",
      "posterior mean =",  x['post.mean'], "\n",
      "posterior median =",  x['post.median'], "\n",
      "95% credible interval = [", x['lower.Cri'], ",", x['upper.Cri'], "]")
}

dbd.rmp.normal(0.7, -0.6941, 9.354928, 551, -0.3945, 12.36577, 25, "log_exp")

###########################################
### Bridging design
###########################################
## gloabl data
## trt prop 0.5
## control prop 0.15
## total number 120
###########################################
###########################################
dbd.rmp.normal(0.2, 21, 22.65, 120, 21, 22.65, 60, "normal")



######################################################
#### create a table for weight ranges from 0 to 1 ####
######################################################
weight_range<-do.call(rbind, lapply(1:11, function(i) {
  result=dbd.rmp.normal(0.1*(i-1), -0.6941, 9.354928, 551, -0.3945, 12.36577, 25, "log_exp")
  c(result=result)
}))


range_table<-data.frame(weight_range)
colnames(range_table)<-c("prior_weight", "post_weight", "post_mean", "post_median", 
                          "lower_Cri", "upper_Cri")
kable(range_table)




################################################################
#### create a table for weight ranges and sample size range ####
################################################################
weight_ss=do.call(rbind, lapply(1:551, function(k){
  
  do.call(rbind, lapply(1:11, function(i) {
    result=dbd.rmp.normal(0.1*(i-1), -0.6941, 9.354928, 551, -0.3945, 12.36577, k, "log_exp")
    c(result=result)
  }))
  
}))


weight_ss_table<-data.frame(weight_ss)
colnames(weight_ss_table)<-c("sample_size", "prior_weight", "post_weight", "post_mean", "post_median", 
                             "lower_Cri", "upper_Cri")
# kable(weight_ss_table)
write.csv(weight_ss_table, "C:/Users/e0472791/Desktop/range_table.csv")




######################################################
#### power ####
######################################################
n=1000
N.target=25
es.target=-0.3945
var.target=12.36577
sim<-replicate(n, rnorm(N.target, es.target, sqrt(var.target)))

power_result <- sapply(1:1000, function(i) { 
  dbd.rmp.normal(0.7, -0.6941, 9.354928, 551, mean(sim[,i]), var(sim[,i]), N.target, "log_exp")["upper.Cri"]})
power<-mean(power_result < 1)
power




######################################################
#### type 1 error ####
######################################################
n=1000
N.target=25
es.target=0
var.target=12.36577
sim<-replicate(n, rnorm(N.target, es.target, sqrt(var.target)))

error_result <- sapply(1:1000, function(i) { 
  dbd.rmp.normal(0.7, -0.6941, 9.354928, 551, mean(sim[,i]), var(sim[,i]), N.target, "log_exp")["upper.Cri"]})
error<-mean(power_result < 1)
error



# https://rdrr.io/snippets/