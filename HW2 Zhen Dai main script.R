source("C:/Users/meteorite619/Documents/GitHub/assignment-2-Eric-Zhen/HW2 Zhen Dai code.R")
compare_outcomes = function(iterations){
  i = 1
  burnIn = iterations/2
  c = 0
  while (i < 11){
    a = runif(n = 1, min=0, max=10)
    b = rnorm(n = 1, sd = 5)
    sd = runif(n = 1, min=0, max=30)
    startvalue = c(a,b,sd)
    result = run_metropolis_MCMC(startvalue, iterations)
    pred_a = result[-(1:burnIn),1]
    mean_a = mean(pred_a)
    c = c+ mean_a
    sd_a = sd(pred_a)
    t = c(mean_a,sd_a)
    print(t)
    i = i+1
  }
  mean_final = c/10
  return(mean_final)
}

co1 = compare_outcomes(1000)
co2 = compare_outcomes(10000)
co3 = compare_outcomes(100000)

co1
#The mean of a is 5.247364 for the iteration of 1000 times. Thus, the estimation error is 0.247364.
co2
#The mean of a is 5.253159 for the iteration of 10000 times. Thus, the estimation error is 0.253159.
co3
#The mean of a is 5.256932 for the iteration of 100000 times.Thus, the estimation error is 0.256932. 


#From the results, we see that the predcted value of the slope tends to have a mean around 5.25 with standard deviation around 0.24.
#Also, the outcomes seem to be relatively stable since the means and standard deviations in each
#of the 10 rounds are close to each other.
#Also, the values we get for different iterations are also similar which means that the markov chain is close
#to its stationary distribution when we iterate is over 1000 times.