plot_summary = function(trueA, trueB, trueSd, burnIn, chain){
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
}


