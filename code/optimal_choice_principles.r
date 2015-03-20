require('gtools') # for dirichlet distribution

create_game = function(ub_util=100, ub_acts=3, symmetric = F){
  # creates a random 2-player strategic form game with upper-bounds
  # on utils and acts
  if (ub_acts == 2){
    n_acts = c(2,2)
  }
  else{
    n_acts = sample(2:ub_acts,2,replace=TRUE)
    if (symmetric == T) {
      n_acts[2] = n_acts[1]
    }
  }
  U = array(0,dim=c(n_acts,2))
  U[,,1] = matrix(sample(1:ub_util,prod(n_acts),replace=TRUE),nrow=n_acts[1])
  if (symmetric==T) {
    U[,,2] = t(U[,,1])
  }
  else{
    U[,,2] = matrix(sample(1:ub_util,prod(n_acts),replace=TRUE),nrow=n_acts[1]) 
  }
  return(U)
}

flip_game = function(U){
  V = array(0,dim=dim(U)[c(2,1,3)])
  V[,,1] = t(U[,,2])
  V[,,2] = t(U[,,1])
  return(V)
}

max_value = function(U){
  if (length(dim(U)) == 3) {
    U = U[,,1]
  }
  eus = rowMeans(U)
  maxs = which.max(eus)
  i = sample(length(maxs),1)
  return(maxs[i])
}

maximin = function(U){
  # returns maximin choice for row player in U
  if (length(dim(U)) == 3) {
    U = U[,,1]
  }
  mins = sapply(1:dim(U)[1], function(x) min(U[x,]))
  maximins = which(mins == max(mins))
  i = sample(length(maximins),1)
  return(maximins[i])
}

regret_transform = function(U){
  # take a payoff matrix and return the game based on negative regrets
  if (length(dim(U)) == 3) {
    U = U[,,1]
  }
  V = t(t(U) - sapply(1:dim(U)[2], function(x) max(U[,x] )))
  return(V)
}

relative_power_transform = function(U){
  # take a game matrix and return equality payoffs
  V = U
  V[,,1] = U[,,1] - U[,,2]
  V[,,2] = U[,,2] - U[,,1]
  return(V)
}

general_power_transform = function(U){
  # take a game matrix and return equality payoffs
  V = U
  V[,,1] = U[,,1] + U[,,2]
  V[,,2] = U[,,1] + U[,,2]
  return(V)
}



rounds = 50000
av_U = matrix(0,nrow=5,ncol=5)
colnames(av_U) = c("RM","MM", "RP", "GP", "EU")
rownames(av_U) = c("RM","MM", "RP", "GP", "EU")

pb <- txtProgressBar(max = rounds,style = 3)


for (i in 1:rounds){
  setTxtProgressBar(pb, i)
  
  U = create_game()
  
  br = matrix(rep(rdirichlet(1,rep(1,dim(U)[2]))[1,], each=dim(U)[1]), nrow=dim(U)[1], ncol=dim(U)[2]) # beliefs of row player
  bc = matrix(rep(rdirichlet(1,rep(1,dim(U)[1]))[1,], each=dim(U)[2]), nrow=dim(U)[2], ncol=dim(U)[1]) # beliefs of col player
  
  # choice of regret minimizers
  a.rm = c(maximin(regret_transform(U[,,1])), # row player
           maximin(regret_transform(t(U[,,2])))) # column player
  # choice of maximin-ers
#   a.mm = c( maximin(U[,,1]) , # row player
#             maximin(t(U[,,2]))) # column player
  a.mm = c(max_value(regret_transform(U[,,1]) * br ), # row player
           max_value(regret_transform(t(U[,,2])) * bc)) # column player
  # choice of relative power maximizers
   a.rp = c(maximin(relative_power_transform(U)), # row player
            maximin(relative_power_transform(flip_game(U)))) # column player
  # choice of general power maximizers
  a.gp = c(maximin(general_power_transform(U)), # row player
           maximin(general_power_transform(flip_game(U)))) # column player
  # choice of utility maximizers
  a.eu = c(max_value(U[,,1]*br), # row player
           max_value(flip_game(U)[,,1]*bc)) # column player

  av_U[1,1] = av_U[1,1] + U[a.rm[1],a.rm[2],1] + U[a.rm[1],a.rm[2],2]
  av_U[1,2] = av_U[1,2] + U[a.rm[1],a.mm[2],1] + U[a.mm[1],a.rm[2],2]
  av_U[1,3] = av_U[1,3] + U[a.rm[1],a.rp[2],1] + U[a.rp[1],a.rm[2],2]
  av_U[1,4] = av_U[1,4] + U[a.rm[1],a.gp[2],1] + U[a.gp[1],a.rm[2],2]
  av_U[1,5] = av_U[1,5] + U[a.rm[1],a.eu[2],1] + U[a.eu[1],a.rm[2],2]
  
  av_U[2,1] = av_U[2,1] + U[a.mm[1],a.rm[2],1] + U[a.rm[1],a.mm[2],2]
  av_U[2,2] = av_U[2,2] + U[a.mm[1],a.mm[2],1] + U[a.mm[1],a.mm[2],2]
  av_U[2,3] = av_U[2,3] + U[a.mm[1],a.rp[2],1] + U[a.rp[1],a.mm[2],2]
  av_U[2,4] = av_U[2,4] + U[a.mm[1],a.gp[2],1] + U[a.gp[1],a.mm[2],2]
  av_U[2,5] = av_U[2,5] + U[a.mm[1],a.eu[2],1] + U[a.eu[1],a.mm[2],2]
  
  av_U[3,1] = av_U[3,1] + U[a.rp[1],a.rm[2],1] + U[a.rm[1],a.rp[2],2]
  av_U[3,2] = av_U[3,2] + U[a.rp[1],a.mm[2],1] + U[a.mm[1],a.rp[2],2]
  av_U[3,3] = av_U[3,3] + U[a.rp[1],a.rp[2],1] + U[a.rp[1],a.rp[2],2]
  av_U[3,4] = av_U[3,4] + U[a.rp[1],a.gp[2],1] + U[a.gp[1],a.rp[2],2]
  av_U[3,5] = av_U[3,5] + U[a.rp[1],a.eu[2],1] + U[a.eu[1],a.rp[2],2]
  
  av_U[4,1] = av_U[4,1] + U[a.gp[1],a.rm[2],1] + U[a.rm[1],a.gp[2],2]
  av_U[4,2] = av_U[4,2] + U[a.gp[1],a.mm[2],1] + U[a.mm[1],a.gp[2],2]
  av_U[4,3] = av_U[4,3] + U[a.gp[1],a.rp[2],1] + U[a.rp[1],a.gp[2],2]
  av_U[4,4] = av_U[4,4] + U[a.gp[1],a.gp[2],1] + U[a.gp[1],a.gp[2],2]
  av_U[4,5] = av_U[4,5] + U[a.gp[1],a.eu[2],1] + U[a.eu[1],a.gp[2],2]
  
  av_U[5,1] = av_U[5,1] + U[a.eu[1],a.rm[2],1] + U[a.rm[1],a.eu[2],2]
  av_U[5,2] = av_U[5,2] + U[a.eu[1],a.mm[2],1] + U[a.mm[1],a.eu[2],2]
  av_U[5,3] = av_U[5,3] + U[a.eu[1],a.rp[2],1] + U[a.rp[1],a.eu[2],2]
  av_U[5,4] = av_U[5,4] + U[a.eu[1],a.gp[2],1] + U[a.gp[1],a.eu[2],2]
  av_U[5,5] = av_U[5,5] + U[a.eu[1],a.eu[2],1] + U[a.eu[1],a.eu[2],2]
}

close(pb)

av_U = av_U / (2*rounds)
show(av_U)