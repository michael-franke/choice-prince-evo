require('gtools') # for dirichlet distribution
require('plyr') # to conveniently manipulate data frames
require('ggplot2')
require('reshape2')

max.t=10
max.a=10
n.rounds = 10000

n.tL = 0
n.aL = 0
u.brL = 0
u.mrL = 0
u.mmL = 0                  

pb <- txtProgressBar(max = n.rounds, style = 3)

for (i in 1:n.rounds) {
  setTxtProgressBar(pb, i)
  if (max.t > 2){
    n.t = sample(2:max.t,1) # number of states
  } else {
    n.t = 2
  }
  if (max.a > 2){
    n.a = sample(2:max.a,1) # number of acts
  } else {
    n.a = 2
  }
  u = matrix(sample(0:10,replace=TRUE,size=n.t*n.a),nrow=n.t,ncol=n.a) # utility matrix
  p = rdirichlet(1,rep(1,n.t))[1,] # probabilities of states (subjective!!)
  # p = rep(1/n.t, n.t)
  # t = sample(1:n.t,1) # true state
  t = sample(1:n.t,1,prob=p) # true state, with bias
  # get best response
  eu = colSums(u * p) # expected utility
  a.br = which(eu == max(eu)) # best responses (can be several)
  # get min-regret
  best.v = sapply(1:n.t, function(x) max(u[x,] )) # best value
  rgrt = sapply(1:n.a, function(x) max(best.v - u[,x])) # regret of each act
  a.mr = which(rgrt == min(rgrt)) # acts that minimize regret (can be several)
  # get maximin security strategty
  mins = sapply(1:n.a, function(x) min(u[,x])) # worst case for each act
  a.mm = which(mins == max(mins))
  # record data
  n.tL[i] = n.t
  n.aL[i] = n.a
  u.brL[i] = sum(u[t,a.br])/length(a.br)
  u.mrL[i] = sum(u[t,a.mr])/length(a.mr)
  u.mmL[i] = sum(u[t,a.mm])/length(a.mm)
}

close(pb)

data = data.frame(n.t = n.tL,
                  n.a = n.aL,
                  u.br = u.brL,
                  u.mr = u.mrL,
                  u.mm = u.mmL)

# save(data,file="data_solitary_decisions_10x10.RDATA")

