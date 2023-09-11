#---DEPENDENCIES----------------------------------------------------------------
library(ggplot2)

# constructing the population
lam <- 2
population <- rpois(1000000, lambda = lam)

# population distribution

hist(population,
  main = "Poisson distributed population",
  xlab = "values",
  col = "pink",
  border = "black",
  breaks = 1000
)

# defining parameters
ref_mean <- lam # lambda parameter for poisson distribution (given)
pop_mean <- mean(population) # true mean of population

# setting up alpha threshold 
alpha <- 0.05

# for large sample size

# EXPERIMENT -
# running 20 experiments comprising of finding number of times null hypothesis
# is rejected out of 200 tests conducted at a time
large_sample_size <- c() # to store number of times rejected
for (i in 1:20){
  sample_size <- 10000 # sample size (much larger than 30)
  iterations <- 200 # number of experiments performed
  
  # constructing vector to store difference b/w reference mean and sample mean
  diff_mean_sample <- rep(0,iterations)
  
  # constructing vector to store p values
  pvaluetest <- rep(0,iterations)
  
  # constructing vector to store null hypothesis accept or reject
  nullh_ar <- rep(0,iterations)
  
  # Performing t-test multiple times 
  for (x in 1:iterations) { # setting up for-loop for 200 iterations of experiment
    # drawing a sample
    sampledata <- sample(population, sample_size, replace = FALSE)
    
    # appending difference b/w reference mean and sample mean
    diff_mean_sample[x] <- mean(sampledata) - ref_mean
    
    # performing a t-test
    ttestresults <- t.test(sampledata, mu = ref_mean)
    # appending p value of test 
    pvaluetest[x] <- ttestresults$p.value # p-value
    
    # using a conditional to accept or reject null-hypothesis
    # rejecting null hypothesis when p value less than alpha AND
    # reference mean greater than sample mean
    if (ttestresults$p.value < alpha && diff_mean_sample[x] < 0)
    {nullh_ar[x] <- 1} # null hypothesis rejected 
    else
    {nullh_ar[x] <- 0} # null hypothesis accepted
  }
  large_sample_size <- append(large_sample_size,sum(nullh_ar))
}

# for small sample size

# EXPERIMENT -
# running 20 experiments comprising of finding number of times null hypothesis
# is rejected out of 200 tests conducted at a time
small_sample_size <- c() # to store number of times rejected
for (i in 1:20){
  sample_size <- 7 # sample size (much larger than 30)
  iterations <- 200 # number of experiments performed
  
  # constructing vector to store difference b/w reference mean and sample mean
  diff_mean_sample <- rep(0,iterations)
  
  # constructing vector to store p values
  pvaluetest <- rep(0,iterations)
  
  # constructing vector to store null hypothesis accept or reject
  nullh_ar <- rep(0,iterations)
  
  # Performing t-test multiple times 
  for (x in 1:iterations) { # setting up for-loop for 200 iterations of experiment
    # drawing a sample
    sampledata <- sample(population, sample_size, replace = FALSE)
    
    # appending difference b/w reference mean and sample mean
    diff_mean_sample[x] <- mean(sampledata) - ref_mean
    
    # performing a t-test
    ttestresults <- t.test(sampledata, mu = ref_mean)
    # appending p value of test 
    pvaluetest[x] <- ttestresults$p.value # p-value
    
    # using a conditional to accept or reject null-hypothesis
    # rejecting null hypothesis when p value less than alpha AND
    # reference mean greater than sample mean
    if (ttestresults$p.value < alpha && diff_mean_sample[x] < 0)
    {nullh_ar[x] <- 1} # null hypothesis rejected 
    else
    {nullh_ar[x] <- 0} # null hypothesis accepted
  }
  small_sample_size <- append(small_sample_size,sum(nullh_ar))
}

# plot p-value for experiment
pl <- qplot(pvaluetest, binwidth = 0.1, xlab = "p-value", ylab = "Experiments")
plot(pl)

# constructing box plot to see distribution of number of rejections out of 200 iterations
# for large and small sample size
plot(
  boxplot(
    large_sample_size,
    small_sample_size,
    xlab = "sample sizes",
    ylab = "number of rejections out of 200",
    main = "to demostrate t-test results difference b/w large and small sample sizes",
    col = c("pink","orange"),
    names = c("large sample size","small sample size")
  )
)


