
# https://stackoverflow.com/questions/14048401/simulate-data-from-lognormal-in-r

# Code 1: draw a sample with size=10 from log normal distribution 10000 times 
lapply(1:10000, function(i) rlnorm(10, meanlog = 0, sdlog = 1))


# Code 2: draw a sample with size=10 from log normal distribution 10000 times 
lapply(1:10000, function(i) {
  x <- rlnorm(10, meanlog = 0, sdlog = 1)
})


# Code 3: draw a sample with size=10 from log normal distribution 10000 times 
x <- list()
for(i in 1:10000) {
  x[[i]] <- rlnorm(10, meanlog = 0, sdlog = 1)
}


# Draw a large (n=10000) sample from log-normal population and plots it
x <- rlnorm(100000, meanlog = 0, sdlog = 1)
plot(density(x))
hist(x)

# To see what the sampling distribution of mean looks like, we will choose a sample size n, 
# and repeadly take draws of size n from the log-normal distribution, calculate the sample mean, and then plot the distribution of these sample means. 
# When the sample size is above 100, it looks more normal but still skewed. Notice thath x-axis range is smaller - the variablity of the sample 
# mean is now smaller that with n=2 and n=10.

x<-do.call(rbind, lapply(1:10000, function(i) {
  y <- rlnorm(10, meanlog = 0, sdlog = 1)
  c(mean=mean(y))
}))
plot(density(x), xlab="Variablity range", main="Distibution of sample mean (n=10)")

hist(x)
