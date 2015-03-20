require('gtools') # for dirichlet distribution
require('ggplot2')
require('reshape2')
require('fpc') # for function pamk(.) -> wrapper for kmeans!

load("buffered_meta_game.RDATA")
U = av_U

trials = 50
rounds = 20000

cost_function = function(k,s,a){
  c = sapply(0:k, function(i) (a^i)*s)  
  show(c)
  return(cumsum(c))
}

# plot(0:11,cost_function(11,1.5,1),type="l",col="grey", 
#      ylim=c(0, max(cost_function(11,1.5,1.3))),
#      ylab="cost", xlab="ToM level")
# points(0:11,cost_function(11,1.5,1.1),type="b",col="blue")
# points(0:11,cost_function(11,1.5,1.3),type="b",col="blue")
# points(0:11,cost_function(11,1.5,0.7),type="b",col="red")
# points(0:11,cost_function(11,1.5,0.9),type="b",col="red")

cost_transform = function(U,a=1,s=1,symmetric=T){
  # symmetric: whether to treat ToM depth the same in max-EU and min-Reg
  
  k = dim(U)[1]/2

  #   # normalize by the biggest difference between adjacent levels of ToM-depth
#   x = max(sapply(1:dim(U)[2], function(j) max(c(sapply(1:(k-1), function(i) U[i+1, j] - U[i, j]),
#                                                 sapply(1:(k-1), function(i) U[k+i+1, j] - U[k+i,j]))) ))
#   x = max(U) - min(U)
  if (symmetric){
    cost = rep(cost_function(k-1,s,a),2)
  }
  else{
    cost = c(cost_function(k-1,s,a),0,cost_function(k-2,s,a))
  }
  show(cost)
  plot(0:(k-1),cost[1:k],type="b")
  points(0:(k-1),cost_function(k-1,s=s,a=1),type="l",col="grey")
  X = sweep(U,MARGIN=1,STATS=cost,FUN="-")
  X = X - min(X)
  return(X)
}

theta = (U[2,1]-U[1,1])

U = cost_transform(U,a=1,s=0)

nact = dim(U)[1]

P = matrix(0,ncol=nact,nrow=trials)
colnames(P) = colnames(U)

pb <- txtProgressBar(max = trials,style = 3)

for (t in 1:trials){
  
  setTxtProgressBar(pb, t)
  
  p = as.vector(rdirichlet(1,rep(1,nact)))
  for (r in 1:rounds){
    eu = rowSums(sweep(U,2,p,'*'))
    av_eu = sum(eu*p)
    p = p*eu / av_eu
  }
  P[t,] = p
}

close(pb)

X = round(P,3)
Y = pamk(X)
show(table(Y$pamobject$clustering))
show(Y$pamobject$medoids)

