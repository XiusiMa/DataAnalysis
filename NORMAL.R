
# Task 2: Generate 1000 standard normal random variables and store in 100*10
# problem 1

# Generate each random sample with mean 0 and variance 1
number.variable <- 10
number.observation <- 100
generate.result <- matrix(0, ncol = number.variable, nrow = number.observation)

for(j in 1:number.variable){
  random.sample <- rnorm(n = number.observation,
                         mean = 0,
                         sd = 1)
  generate.result[,j] <- random.sample
}
head(generate.result)


#problem 2: Calculate the correlation matrix and plot it
sample.corr <- cor(generate.result)
print(sample.corr)
image1 <- image(t(sample.corr)[ncol(sample.corr):1,] )


# Problem 3:matrices of size 1000x10 and 10000x10
generateSize <- function(number_variable,number_observation){
  generate.result <- matrix(0, ncol = number_variable, nrow = number_observation)
  
  for(j in 1:number_variable){
    random.sample <- rnorm(n = number_observation,
                           mean = 0,
                           sd = 1)
    generate.result[,j] <- random.sample
  }
  sample_corr <- cor(generate.result)
  return(sample_corr)
}

sample_corr2 <- generateSize(10,1000)
image2 <- image(t(sample_corr2)[ncol(sample_corr2):1,])
sample_corr3 <- generateSize(10,10000)
image3 <- image(t(sample_corr3)[ncol(sample_corr3):1,])

