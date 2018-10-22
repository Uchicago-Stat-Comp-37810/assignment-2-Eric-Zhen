trueA <- 5    #true parameter value for slope
trueB <- 0    #true parameter value for intercept
trueSd <- 10  #true standard deviation of the noise
sampleSize <- 31  #sample size

# create independent x-values 
x <- (-(sampleSize-1)/2):((sampleSize-1)/2)     #sampled x values
# create dependent values according to ax + b + N(0,sd)
y <-  trueA * x + trueB + rnorm(n=sampleSize,mean=0,sd=trueSd)   #sampled y values

plot(x,y, main="Test Data")  #a plot

likelihood <- function(param){#three parameters
  a = param[1]   #parameter for slope
  b = param[2]   #parameter for intercept
  sd = param[3]  #parameter for standard deviation
  
  pred = a*x + b  #predicted value of y based on the parameters given
  singlelikelihoods = dnorm(y, mean = pred, sd = sd, log = T)   #the log of the probability that we observed each of our sample (x_i,y_i) assuming that the parameters of the distributions are given by param
  sumll = sum(singlelikelihoods)  #the log of the probability that we observed our samples (x,y) assuming that the parameters of the distributions are given by param
  return(sumll)   #return the probability
}

# Example: plot the likelihood profile of the slope a
slopevalues <- function(x){return(likelihood(c(x, trueB, trueSd)))} #the probability of observing the data assuming that the slope is x and the other two paramters have the true values
slopelikelihoods <- lapply(seq(3, 7, by=.05), slopevalues ) #same as above
plot (seq(3, 7, by=.05), slopelikelihoods , type="l", xlab = "values of slope parameter a", ylab = "Log likelihood")    #a plot

# Prior distribution
prior <- function(param){ #a function that computes the prior probability
  a = param[1]  #first parameter: slope
  b = param[2]  #second parameter: intercept
  sd = param[3]  #thrid parameter: standard deviation
  aprior = dunif(a, min=0, max=10, log = T) #the log of the prior probability of slope
  bprior = dnorm(b, sd = 5, log = T)  #the log of the prior probability of intercept
  sdprior = dunif(sd, min=0, max=30, log = T)  #the log of the prior probability of standard deviation
  return(aprior+bprior+sdprior)  #the log of the product of the probability of the priors
}

posterior <- function(param){ #compute the posterior probability
  return (likelihood(param) + prior(param))  #the log of the product of the prior probability and the likelihood
}

proposalfunction <- function(param){ #a function that tells us how to choose the new parameter values given the old ones
  return(rnorm(3,mean = param, sd= c(0.1,0.5,0.3)))  #randomly pick three numbers from a normal distribution with certain means and sds
}

run_metropolis_MCMC <- function(startvalue, iterations){ #the metropolis algorithm
  chain = array(dim = c(iterations+1,3))  #the markov chain
  chain[1,] = startvalue  #give initial values for the first row of the markov chain
  for (i in 1:iterations){  #update the parameter values for a number of times
    proposal = proposalfunction(chain[i,])  #given the old parameter values, generate the proposed new ones that will be checked to be acceptde or not
    
    probab = exp(posterior(proposal) - posterior(chain[i,])) #the probability of p(new)/p(old)
    if (runif(1) < probab){
      chain[i+1,] = proposal #with probability p(new)/p(old) we jump to the new value
    }else{
      chain[i+1,] = chain[i,] #otherwise, we stay at the same old value
    }
  }
  return(chain)  #return the final chain
}

startvalue = c(4,0,10)  #a start value for the chain
chain = run_metropolis_MCMC(startvalue, 10000)  #execute the above algorithm with certain start value

burnIn = 5000  #we are only interested in the values after 5000 iterations of the updating rule in the algorithm since the first several values of the chained may be influenced by the start value
acceptance = 1-mean(duplicated(chain[-(1:burnIn),])) #we calculate the proportion of times of accepting the new values. If a value equals to its previous value, then the new value is rejected and otherwise accepted. This is an approximation of the averaged probability of acceptance

### Summary: #######################

par(mfrow = c(2,3))
hist(chain[-(1:burnIn),1],nclass=30, , main="Posterior of a", xlab="True value = red line" )
abline(v = mean(chain[-(1:burnIn),1]))
abline(v = trueA, col="red" )
hist(chain[-(1:burnIn),2],nclass=30, main="Posterior of b", xlab="True value = red line")
abline(v = mean(chain[-(1:burnIn),2]))
abline(v = trueB, col="red" )
hist(chain[-(1:burnIn),3],nclass=30, main="Posterior of sd", xlab="True value = red line")
abline(v = mean(chain[-(1:burnIn),3]) )
abline(v = trueSd, col="red" )
plot(chain[-(1:burnIn),1], type = "l", xlab="True value = red line" , main = "Chain values of a", )
abline(h = trueA, col="red" )
plot(chain[-(1:burnIn),2], type = "l", xlab="True value = red line" , main = "Chain values of b", )
abline(h = trueB, col="red" )
plot(chain[-(1:burnIn),3], type = "l", xlab="True value = red line" , main = "Chain values of sd", )
abline(h = trueSd, col="red" )

#The aboves are just some diagrams which shows how good our predictions are using the metropolis algorithm

# for comparison:
summary(lm(y~x))  #how good a linear model is, which in this case is the true model. This provides a comparsion between using the metropolis algorithm and using the true model.

