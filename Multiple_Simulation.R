
## A simulation function
## Question: how can I simulate with different sample sizes and put these multiple simulations in a single dataset?
multiple_simulation<-function(ss){

  sim<-100
  xm<-0.5
  simulation<-rep(1:sim, each=ss)
  subject<-rep(1:ss, sim)
  totn<-ss*sim
  x<-rbinom(totn, size=1, prob=xm)
  y<-rnorm(totn, 0.5*x+1, sd=1)
  smpd0<-data.frame(simulation=simulation, ss=ss, subject=subject, group=x, value=y)

  return(smpd0)

}


# Step 1: Create a table containing multiple cases 
# Create a sequence containing sample size from 20 to 200
ss<-seq(20, 100, by=1)
table<-lapply(ss, multiple_simulation)


# Step 2: Append the individual tables within the big table 
DF <-  data.frame(table[1])

#reading each table within the range and append them to create one file
for (i in 2:length(ss)){
  df <- data.frame(table[i])    
  DF <- rbind(DF, df)    
}
write.csv(DF, "C:/Users/212697818/Desktop/df.csv")


# Step 3: Use do.call instead of for-loop in step 2 because of the slow speed of for-loop 
dataset <- do.call(rbind, lapply(1:length(ss), function(i) {
    
  data.frame(table[i])
  
}))
write.csv(dataset, "C:/Users/212697818/Desktop/dataset.csv")



# Step 4: One step
ds <- do.call(rbind, lapply(20:100, function(ss) {
  
  sim<-100
  xm<-0.5
  simulation<-rep(1:sim, each=ss)
  subject<-rep(1:ss, sim)
  totn<-ss*sim
  x<-rbinom(totn, size=1, prob=xm)
  y<-rnorm(totn, 0.5*x+1, sd=1)
  smpd0<-data.frame(simulation=simulation, ss=ss, subject=subject, group=x, value=y)
  
  return(smpd0)
  
}))

# https://www.stat.berkeley.edu/~s133/Docall.html



