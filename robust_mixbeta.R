
rm(list = ls())
library(RBesT)
library(mvtnorm)


############################################################################
####Bayesian dynamic borrowing with mixture prior for beta distribution ####
############################################################################

dbd.rmp.beta<-function(wt_t, wt_c, n_t, s_t, n_c, s_c){
  
    
      #####################################################
      ## treatment arm in historical global study
      #####################################################
       
      # global control data prior
      trt.source.prior<-mixbeta(source=c(1, 30, 30))
      
      # add non-informative prior
      trtmix <- robustify(trt.source.prior, weight=1-wt_t, mean=0.5)

      
      ### effective sample size
      ess.trt <- ess(trtmix, "moment")
    
      
      ### posterior
      post.trt <- postmix(trtmix, n=n_t, r=s_t) 
      
      ### posterior average
      post.trt.avg<-mixbeta(c(1, 
                              post.trt[1, 1]*post.trt[2, 1]+post.trt[1, 2]*post.trt[2, 2],
                              post.trt[1, 1]*post.trt[3, 1]+post.trt[1, 2]*post.trt[3, 2]
      ))
      
      
      ####################################################
      ## control arm in historical global study
      ####################################################
      
      # global control data prior
      control.source.prior<-mixbeta(source=c(1, 9, 51))
  
      # add non-informative control prior
      controlmix <- robustify(control.source.prior, weight=1-wt_c, mean=0.5)
  
      
      ## effective sample size
      ess.ctr <- ess(controlmix, "moment")
      
      
      ## posterior
      post.ctr <- postmix(controlmix, n=n_c, r=s_c) 
      
      ## posterior average
      post.ctr.avg<-mixbeta(c(1, 
                              post.ctr[1, 1]*post.ctr[2, 1]+post.ctr[1, 2]*post.ctr[2, 2],
                              post.ctr[1, 1]*post.ctr[3, 1]+post.ctr[1, 2]*post.ctr[3, 2]
      ))
      
      
      
      ##################################
      #### effect difference
      ##################################
      ## prior probability of success
      successCrit <- decision2S(0.95, 0, FALSE)
      prior_pos=pos2S(trtmix, controlmix, n_t, n_c, successCrit)
      prior.pos=prior_pos(trtmix, controlmix)
      
      ## power
      oc<-oc2S(trtmix, controlmix, n_t, n_c, successCrit)
      power=oc(s_t/n_t, s_c/n_c)
      
      ## type 1 error assuming with control rate
      error.c=oc(s_c/n_c, s_c/n_c)
      
      ## type 1 error assuming with treatment rate
      error.t=oc(s_t/n_t, s_t/n_t)
      
      ## posterior probability of treatment difference above 0
      prob=pmixdiff(post.trt.avg, post.ctr.avg, 0, FALSE)
      
      ## lower cri
      lower.Cri=qmixdiff(post.trt.avg, post.ctr.avg, 0.025)
      
      ## upper cri
      upper.Cri=qmixdiff(post.trt.avg, post.ctr.avg, 0.975)
      
      ## median
      post.median=qmixdiff(post.trt.avg, post.ctr.avg, 0.975)
      

      ## posterior weight for treatment group
      post.weight.trt=post.trt[1, 1]
      
      ## posterior weight for control group
      post.weight.ctr=post.ctr[1, 1]
      
      out = c(power=power, prior.pos=prior.pos,
              error.t=error.t, error.c=error.c,
              post.weight.trt=post.weight.trt, post.weight.ctr=post.weight.ctr, 
              ess.trt=ess.trt, ess.ctr=ess.ctr,
              post.median=post.median, 
              lower.Cri=lower.Cri, upper.Cri=upper.Cri)
      
      class(out) = 'dbd.rmp.beta'
      return(out)
}

print.dbd.rmp.beta = function(x) {
  cat("Prior probability of success =",  x['prior.pos'], "\n",
      "Power =",  x['power'], "\n",
      "Type 1 error assuming with treatment rate =",  x['error.t'], "\n",
      "Type 1 error assuming with control rate =",  x['error.c'], "\n",
      "Posterior weight for treatment group =",  x['post.weight.trt'], "\n",
      "Posterior weight for control group =",  x['post.weight.ctr'], "\n", 
      "Effective treatment sample size=",  x['ess.trt'], "\n", 
      "Effective control sample size=",    x['ess.ctr'], "\n", 
      "posterior median =",  x['post.median'], "\n",
      "95% credible interval = [", x['lower.Cri'], ",", x['upper.Cri'], "]")
}

dbd.rmp.beta(0.1, 0.1, 30, 15, 30, 5)




######################################################
#### type 1 error ####
######################################################
sim_n=1000
set.seed(12345)
n_trt=20
s_trt=3
p_trt=s_trt/n_trt
sim_trt<-replicate(sim_n, rbinom(n_trt, 1, p_trt))

set.seed(12345)
n_ctr=20
s_ctr=3
p_ctr=s_ctr/n_ctr
sim_ctr<-replicate(sim_n, rbinom(n_ctr, 1, p_ctr))


error_sim <- sapply(1:sim_n, function(i) { 
                          dbd.rmp.beta(0.1, 0.1, 
                                       n_trt, sum(sim_trt[, i]), 
                                       n_ctr, sum(sim_ctr[, i]))["lower.Cri"]})
error<-mean(error_sim > 0)
error



############################################
########## power
############################################
sim_n=1000
set.seed(12345)
n_trt=20
s_trt=10
p_trt=s_trt/n_trt
sim_trt<-replicate(sim_n, rbinom(n_trt, 1, p_trt))

n_ctr=20
set.seed(12345)
s_ctr=3
p_ctr=s_ctr/n_ctr
sim_ctr<-replicate(sim_n, rbinom(n_ctr, 1, p_ctr))


power_sim <- sapply(1:sim_n, function(i) { 
                        dbd.rmp.beta(0.1, 0.1, 
                                      n_trt, sum(sim_trt[, i]), 
                                      n_ctr, sum(sim_ctr[, i]))["lower.Cri"]})
power<-mean(power_sim > 0)
power





################################################################
#### create a table for weight ranges and sample size range ####
################################################################
weight_ss=do.call(rbind, lapply(20:40, function(k){
  
  do.call(rbind, lapply(1:11, function(i) {
                 result=dbd.rmp.beta(0.1*(i-1), 0.1*(i-1), 
                                     k, floor(k*0.5), 
                                     k, ceiling(k*0.15))
                 c(result=result)
  }))
  
}))


weight_ss_table<-data.frame(weight_ss)
colnames(weight_ss_table)<-c("post_trt_weight", "post_control_weight", 
                             "ESS_treatment", "ESS_control", 
                             "post_median", 
                             "lower_Cri", "upper_Cri")
# kable(weight_ss_table)
write.csv(weight_ss_table, "C:/Users/e0472791/Desktop/binary_range_table.csv")




#################################################################
######## Reference #########
#################################################################
## https://github.com/igrave/StudyPrior/tree/master/StudyPrior/R