require('gtools') # for dirichlet distribution
require('plyr') # to conveniently manipulate data frames
require('ggplot2')
require('reshape2')

max.t=10
max.a=10
n.rounds = 50000

p.trueL = 0
a.bestL = 0
n.tL = 0
n.aL = 0
a.brL = 0
a.mrL = 0
u.brL = 0
u.mrL = 0                  

for (i in 1:n.rounds) {
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
#   p = rdirichlet(1,rep(1,n.t))[1,] # probabilities of states
  p = rep(1/n.t, n.t)
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
  p.trueL[i] = p[t]
  a.bestL[i] = which.max(u[t,])
  n.tL[i] = n.t
  n.aL[i] = n.a
  a.brL[i] = a.br
  a.mrL[i] = a.mr
  u.brL[i] = u[t,a.br]
  u.mrL[i] = u[t,a.mr]
}

data = data.frame(p.true = p.trueL,
                  a.best = a.bestL,
                  n.t = n.tL,
                  n.a = n.aL,
                  a.br = a.brL,
                  a.mr = a.mrL,
                  u.br = u.brL,
                  u.mr = u.mrL)

show(mean(data$u.br))
show(mean(data$u.mr))

means = ddply(data, .(data$n.t,data$n.a), summarise, 
              mean.br= mean(u.br) ,
              sd.br = sd(u.br),
              mean.mr= mean(u.mr), 
              sd.mr = sd(u.mr),
              diff = mean(u.br) - mean(u.mr), 
              br.best = mean(u.br) > mean(u.mr))

meansLong = melt(data = means, id.vars = c("data$n.t", "data$n.a"), measure.vars = c("mean.br", "mean.mr"))
colnames(meansLong)[1] = "n.t"
colnames(meansLong)[2] = "n.a"


meansPlot = ggplot(data = meansLong, aes(x = n.a, y = value, col = variable)) +
  geom_line() + geom_point() + facet_wrap(~ n.t, nrow = 3)

# diff = matrix(means$diff, byrow=TRUE,nrow=max.t-1)
# rownames(diff) = paste(rep("t",max.t-1),2:max.t, sep="")
# colnames(diff) = paste(rep("a",max.a-1),2:max.a, sep="")
# 
# image(diff,xlab="states",ylab="acts")


