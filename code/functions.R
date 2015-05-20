require('gtools') # for dirichlet distribution
require('plyr')
require('reshape2')
require('ggplot2')
require('xtable')

source('~/Desktop/data/svn/programming/R/helpers/helpers.R')

create_game = function(ub_util=10, ub_acts=2, symmetric = T){
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

classify_game = function(U){
  # takes a utility matrix U of a symmetric 2-by-2 game
  # outputs a string that classifies the game in to one of
  # 1. coordition, 2. anti-coordination, 
  # 3. strict dominance, 4. weak dominance
  # 5. indifference
  
  if (length(dim(U)) == 3) {
    U = U[,,1]
  }
  
  out = ""
  
  if (U[1,1] > U[2,1] & U[2,2] > U[1,2]){
    out = paste(out, "coordination",sep="")
  }
  
  if (U[1,1] < U[2,1] & U[2,2] < U[1,2]){
    out = paste(out, "anti-coordination",sep="")
  }
  
  if ( (U[1,1] - U[2,1]) * (U[2,2] - U[1,2]) < 0  ){
    out = paste(out, "str_dominance",sep="")
  }
  
  if ( xor(U[1,1] - U[2,1] == 0, U[2,2] - U[1,2] == 0) ){
    out = paste(out, "weak_dominance",sep="")
  }
  
  if (U[1,1] == U[2,1] & U[2,2] == U[1,2]){
    out = paste(out, "indifference",sep="")
  }
  
  if (out == ""){
    show(U)
  }
  
  return(out)
}

flip_game = function(U){
  V = array(0,dim=dim(U)[c(2,1,3)])
  V[,,1] = t(U[,,2])
  V[,,2] = t(U[,,1])
  return(V)
}

max_value = function(U, br){
  if (length(dim(U)) == 3) {
    U = U[,,1]
  }
  U = U * br
  eus = rowMeans(U)
  maxs = which(eus == max(eus))
  return(maxs)
}

maximin = function(U){
  # returns maximin choice for row player in U
  if (length(dim(U)) == 3) {
    U = U[,,1]
  }
  mins = sapply(1:dim(U)[1], function(x) min(U[x,]))
  maximins = which(mins == max(mins))
  return(maximins)
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
  if (length(dim(U)) == 2) {
    X = array(0,dim=c(dim(U),2))
    X[,,1] = U
    X[,,2] = t(U)
    U = X
  }
  V = U
  V[,,1] = U[,,1] - U[,,2]
  V[,,2] = U[,,2] - U[,,1]
  return(V)
}

general_power_transform = function(U){
  # take a game matrix and return equality payoffs
  if (length(dim(U)) == 2) {
    X = array(0,dim=c(dim(U),2))
    X[,,1] = U
    X[,,2] = t(U)
    U = X
  }
  V = U
  V[,,1] = U[,,1] + U[,,2]
  V[,,2] = U[,,1] + U[,,2]
  return(V)
}

AA_transform = function(U, bel){
  # bel is a pair [x,y] with 0 < x < y < 1
  b1 = c(bel[1], 1 - bel[1])
  b2 = c(bel[2], 1 - bel[2])
  V = cbind(rowSums(sweep(U,MARGIN=2,b2,FUN='*')),
            rowSums(sweep(U,MARGIN=2,b1,FUN='*')))  
  return(V)
}