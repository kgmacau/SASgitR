#### 1
### MRI sample size with expected image quality satisfication rate targeted 0.75
  
samplesize <- function(a, b, p0, pt){
  
  n <- ( qnorm(1-a/2)*sqrt(p0*(1-p0))
         + qnorm(1-b)*sqrt(pt*(1-pt)) )**2 / (pt-p0)**2
  print (n)
}

samplesize(0.05, 0.2, 0.75, 0.90)
samplesize(0.05, 0.2, 0.75, 0.91)
samplesize(0.05, 0.2, 0.75, 0.92)
samplesize(0.05, 0.2, 0.75, 0.93)
samplesize(0.05, 0.2, 0.75, 0.94)
samplesize(0.05, 0.2, 0.75, 0.95)
samplesize(0.05, 0.2, 0.75, 0.96)

#### 2 
### Sample size for non-inferior design

samplesize_ni <- function(a, b, p0, pt, delta){
n <- (qnorm(1-a/2) + qnorm(1-b))**2 *(pt*(1-pt)+p0*(1-p0)) / (abs(pt-p0)-delta)**2
print (n)
}

samplesize_ni(0.05, 0.2, 0.65, 0.65, 0.1)
samplesize_ni(0.05, 0.2, 0.95, 0.95, 0.1)

## ³¬ÉùÖ¸ÄÏ
samplesize_ni(0.05, 0.2, 0.96, 0.96, 0.1)
samplesize_ni(0.05, 0.2, 0.94, 0.94, 0.1)
samplesize_ni(0.05, 0.2, 0.90, 0.90, 0.1)
samplesize_ni(0.05, 0.2, 0.88, 0.88, 0.1)



### 3
# sample size for agreement rate

samplesize_ar <- function(a, b, p0, pt){
  
  n <- ( qnorm(1-a/2)*sqrt(p0*(1-p0))
         + qnorm(1-b)*sqrt(pt*(1-pt)) )**2 / (pt-p0)**2
  print (n)
}

samplesize(0.05, 0.2, 0.85, 0.88)
samplesize(0.05, 0.2, 0.85, 0.90)
samplesize(0.05, 0.2, 0.85, 0.92)
samplesize(0.05, 0.2, 0.85, 0.94)
samplesize(0.05, 0.2, 0.85, 0.95)


### 4
# Sample size for correlation coefficient

samplesize_corr<-function(a, b, rho0, rho1){
  n<-(qnorm(1-a)+qnorm(1-b))**2/(log((1+rho1)/(1-rho1))/2-log((1+rho0)/(1-rho0))/2)**2 + 3
  print(n)
}

samplesize_corr(0.025, 0.2, 0.3, 0)
samplesize_corr(0.025, 0.2, 0.982, 0.975)


# reference
# http://powerandsamplesize.com/Calculators/