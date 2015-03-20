require('gtools') # for dirichlet distribution
require('abind')

create_game = function(ub_util=10, ub_acts=10, symmetric = F){
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
  rownames(U) = 1:dim(U)[1]
  colnames(U) = 1:dim(U)[2]
  return(U)
}

create_TD = function(k=100){
  # creates traveller's dilemma
  U = array(0,dim=c(k-1,k-1,2))
  for (x in 2:k){
    for (y in 2:k){
      U[k-x+1,k-y+1,1] = min(x,y) + 2*sign(y-x)
    }
  }
  U[,,2] = t(U[,,1])
  rownames(U)=1:(k-1)
  colnames(U)=1:(k-1)
  return(U)
}

flip_game = function(U){
  V = array(0,dim=dim(U)[c(2,1,3)])
  V[,,1] = t(U[,,2])
  V[,,2] = t(U[,,1])
  rownames(V) = colnames(U)
  colnames(V) = rownames(U)
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
#   choices = rep(0,dim(U)[1])
#   choices[maximins]=1
  return(maximins)
}

regret_transform = function(U){
  # take a payoff matrix and return the game based on negative regrets
  if (length(dim(U)) == 3) {
    U = adrop(U[,,1,drop=FALSE],drop=3)
  }
  V = t(t(U) - sapply(1:dim(U)[2], function(x) max(U[,x] )))
  return(V)
}

maxeu = function(U,bel){
  choices = rep(0,dim(U)[1])
  choices[max_value(sweep(U[,,1],MARGIN=2,bel,'*'))] = 1 
  return(choices)
}

create_belief = function(k,belieftype="random"){
  if (belieftype == "flat"){
    return(rep(1/k,k))
  }
  if (belieftype == "random"){
    return(rdirichlet(1,rep(1,k)))
  }
}

iterate_maxeu = function(U,n,belieftype="random"){
  if (n == 0) {
    bel = create_belief(dim(U)[2],belieftype=belieftype)
    return(maxeu(U,bel))
  }
  else {
    c = iterate_maxeu(flip_game(U),n-1,belieftype=belieftype)
    beltmp = create_belief(dim(U)[2],belieftype=belieftype)
    bel = beltmp*c
    bel = bel/sum(bel)
    return(maxeu(U,bel))
  }
}

iterate_rm = function(U,n){
  if (n==0){
    choices = list(rownames(U)[maximin(regret_transform(U))], 
                   colnames(U)[maximin(regret_transform(flip_game(U)))])
  }
  else{
    prevchoices = iterate_rm(U,n-1)
    V = U[prevchoices[[1]],prevchoices[[2]],,drop=F]
    choices = list(rownames(V)[maximin(regret_transform(V))], 
                   colnames(V)[maximin(regret_transform(flip_game(V)))])
  }
  return(choices)
}

get_result = function(U, r, c){
  V = sweep(U,1,r,'*')
  V = sweep(V,2,c,'*') 
  V = V/(sum(r)*sum(c))
  return(c(sum(V[,,1]), sum(V[,,2])))
}

# belieftype="random"
belieftype="flat"
max_depth = 10

rounds = 10000
av_U = matrix(0,nrow=2*(max_depth+1),
                ncol=2*(max_depth+1))
colnames(av_U) = c(paste("EU-L",0:max_depth,sep=""),paste("RM-L",0:max_depth,sep=""))
rownames(av_U) = c(paste("EU-L",0:max_depth,sep=""),paste("RM-L",0:max_depth,sep=""))

pb <- txtProgressBar(max = rounds,style = 3)

for (i in 1:rounds){
  setTxtProgressBar(pb, i)
  
  colstrats = list()
  rowstrats = list()
  
  U = create_game()
#   U = create_TD(100)
  
  for (j in 1:(max_depth+1)){
    rowstrats[[j]] = iterate_maxeu(U,j-1,belieftype)
    colstrats[[j]] = iterate_maxeu(flip_game(U),j-1,belieftype)
  }
  for (j in (max_depth+2):(2*(max_depth+1))){
    rowchoice = rep(0,dim(U)[1])
    colchoice = rep(0,dim(U)[2])
    rowchoice[as.integer(iterate_rm(U,j-2-max_depth)[[1]])] = 1
    colchoice[as.integer(iterate_rm(flip_game(U),j-2-max_depth)[[1]])] = 1
    rowstrats[[j]] = rowchoice
    colstrats[[j]] = colchoice
  }
  
  for (row in 1:(2*(max_depth+1))){
    for (col in 1:(2*(max_depth+1))){
      av_U[row,col] = av_U[row,col] + get_result(U,rowstrats[[row]], colstrats[[col]])[1]
    }
  }
  
}

close(pb)

av_U = av_U / (rounds)

save(av_U,file="buffered_meta_game_flat.RDATA")


## 'standardizing'
# V = av_U - min(av_U)
# V = V/max(V)
# show(round(V*1000,0))
