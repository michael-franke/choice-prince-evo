require('gtools') # for dirichlet distribution
require('plyr')
require('reshape2')
require('ggplot2')
require('xtable')

source('../../helpers/helpers.R')

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

CP_names = c("Sec-Reg", "Sec-Id", "Sec-Alt", "Sec-Comp", 
             "Bel-Reg", "Bel-Id", "Bel-Alt", "Bel-Comp")

rounds = 10000
trial = c()
# game_type = c()
row_choice = c()
col_choice = c()
row_util = c()

pb <- txtProgressBar(max = rounds,style = 3)


for (i in 1:rounds){
  setTxtProgressBar(pb, i)
  
  U = create_game(ub_util=10, ub_acts=6, symmetric = T)[,,1]
#   gt = classify_game(U)
  
#   br = matrix(rep(rdirichlet(1,rep(1,dim(U)[2]))[1,], each=dim(U)[1]), nrow=dim(U)[1], ncol=dim(U)[2]) # beliefs of row player
  br = 1 # flat beliefs
    
  choices = c(maximin(regret_transform(U)), #Sec-Reg
              maximin(U), # Sec-Id
              maximin(relative_power_transform(U)), # Sec-Alt
              maximin(general_power_transform(U)),  # Sec-Comp
              max_value(regret_transform(U)), #Bel-Reg
              max_value(U), # Bel-Id
              max_value(relative_power_transform(U)), # Bel-Alt
              max_value(general_power_transform(U))  # Bel-Comp
              )
  
  choices.c = c(maximin(regret_transform(U)), #Sec-Reg
              maximin(U), # Sec-Id
              maximin(relative_power_transform(U)), # Sec-Alt
              maximin(general_power_transform(U)),  # Sec-Comp
              max_value(regret_transform(U)), #Bel-Reg
              max_value(U), # Bel-Id
              max_value(relative_power_transform(U)), # Bel-Alt
              max_value(general_power_transform(U))  # Bel-Comp
  )
  
  
  for (c in 1:length(CP_names)){
    for (d in 1:length(CP_names)){
#       trial = c(trial,i)
#       game_type = c(game_type,gt)
<<<<<<< .mine
#       row_choice = c(row_choice,CP_names[c])
#       col_choice = c(col_choice,CP_names[d])
=======
>>>>>>> .r572
      row_util = c(row_util,U[choices[c],choices.c[d]])
    }
  }  

}

data = data.frame( #trial = factor(trial), 
#                   game_type = factor(game_type, levels=c("coordination", "anti-coordination",
#                                                          "str_dominance","weak_dominance","indifference")),
<<<<<<< .mine
                  row_choice = factor(rep(CP_names,length(CP_names)),levels = CP_names),
#                   row_choice = factor(row_choice, levels = CP_names),
#                   col_choice = factor(col_choice, levels = CP_names),
                  col_choice = factor(rep(CP_names, each = length(CP_names)),levels = CP_names),
=======
                  row_choice = factor(rep(rep(CP_names, each = 8) , times = rounds), levels = CP_names),
                  col_choice = factor(rep(CP_names, times = 8*rounds), levels = CP_names),
>>>>>>> .r572
                  row_util = row_util)

close(pb)

# means = daply(data, c("row_choice","col_choice","game_type"), function(data) mean(data$row_util))
# show(means)
means_total = daply(data, c("row_choice","col_choice"), function(data) mean(data$row_util))
show(round(means_total,3))

# save(means_total,file="buffered_meta_game_2x2.RDATA")

<<<<<<< .mine
# show(xtable(means_total))
=======
show(xtable(means_total, digits = 3))
>>>>>>> .r572

### plotting 

# datasum = summarySE(data,measurevar="row_util", groupvars=c("game_type", "col_choice", "row_choice"))

# ggplot(datasum, aes(x=row_choice, y=row_util)) + 
#   geom_bar(position=position_dodge(.9), colour="black", fill="white", stat="identity")  +
#   geom_errorbar(position=position_dodge(.9), width=.25, aes(ymin=row_util-ci, ymax=row_util+ci)) +
#   facet_grid(col_choice ~ game_type)
