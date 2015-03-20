require('gtools') # for dirichlet distribution
require('plyr') # to conveniently manipulate data frames

max.t=10
max.a=10
n.rounds = 1000

data = data.frame(p.true = rep(0,n.rounds),
                  a.best = rep(0,n.rounds),
                  n.t = rep(0,n.rounds),
                  n.a = rep(0,n.rounds),
                  a.br = rep(0,n.rounds),
                  a.mr = rep(0,n.rounds),
                  u.br = rep(0,n.rounds),
                  u.mr = rep(0,n.rounds))

for (i in 1:n.rounds) {
  n.t = sample(2:max.t,1) # number of states
  n.a = sample(2:max.a,1) # number of acts
  u = matrix(sample(-100:100,replace=TRUE,size=n.t*n.a),nrow=n.t,ncol=n.a) # utility matrix
  p = rdirichlet(1,rep(1,n.t))[1,] # probabilities of states
  t = sample(1:n.t,1) # true state
#   t = sample(1:n.t,1,prob=p) # true state, with bias
  # get best response
  tmp = u * p
  eu = sapply(1:n.a, function(x) sum(tmp[,x])) # expected utility
  a.br = which.max(eu) # best response
  # get min-regret
  best.v = sapply(1:n.t, function(x) max(u[x,] )) # best value
  best.a = sapply(1:n.t, function(x) u[x,which.max(u[x,])] ) # best act
  rgrt = sapply(1:n.a, function(x) max(best.v - u[,x])) # regret of each act
  a.mr = which.min(rgrt) # act that minimizes regret 
  # record data
  data$p.true[i] = p[t]
  data$a.best[i] = which.max(u[t,])
  data$n.t[i] = n.t
  data$n.a[i] = n.a
  data$a.br[i] = a.br
  data$a.mr[i] = a.mr
  data$u.br[i] = u[t,a.br]
  data$u.mr[i] = u[t,a.mr]
}

show(mean(data$u.br))
show(mean(data$u.mr))

means = ddply(data, .(data$n.t,data$n.a), summarise, 
              mean.br= mean(u.br) ,
              sd.br = sd(u.br),
              mean.mr= mean(u.mr), 
              sd.mr = sd(u.mr),
              diff = mean(u.br) - mean(u.mr), 
              br.best = mean(u.br) > mean(u.mr))


diff = matrix(means$diff, byrow=TRUE,nrow=max.t-1)
rownames(diff) = paste(rep("t",max.t-1),2:max.t, sep="")
colnames(diff) = paste(rep("a",max.a-1),2:max.a, sep="")

image(diff,xlab="states",ylab="acts")


