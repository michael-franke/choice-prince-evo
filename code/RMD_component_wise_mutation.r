require('gtools') # for dirichlet distribution

# get utilities
# load(file = 'buffered_meta_game_2x2.RDATA')
load(file = 'buffered_meta_game_10x10.RDATA')
U = means_total

# parameters
e = 0.001 # epsilon mutation probability

getMutationMatrix = function(e){
  x = 1 - 5*e - 3*e^2
  m = matrix(c(x, e, e, e, e, e^2, e^2, e^2,
              e, x, e, e, e^2, e, e^2, e^2,
              e, e, x, e, e^2, e^2, e, e^2,
              e, e, e, x, e^2, e^2, e^2, e,
              e, e^2, e^2, e^2, x, e, e, e,
              e^2, e, e^2, e^2, e, x, e, e,
              e^2, e^2, e, e^2, e, e, x, e,
              e^2, e^2, e^2, e, e, e, e, x), nrow=8, byrow = T)
  colnames(m) = colnames(U)
  rownames(m) = rownames(U)
  return(m)
}

# initial population
p = rdirichlet(1, rep(1,8))[1,]
show(p)


m = getMutationMatrix(e)

for (i in 1:100000){
  eu = rowSums(sweep(U,2,p,'*'))
  av_eu = sum(eu*p)
  p = p*eu / av_eu
  p = colSums(sweep(m,1,p,'*'))
}

show(p)

barplot(height = p, names.arg = names(p))

d = data.frame(p = p, strategy = factor(names(p), levels = names(p)))
myplot = ggplot(d, aes(x = strategy, y = p)) + geom_bar(stat = "identity")
show(myplot)
