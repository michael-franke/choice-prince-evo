require('gtools') # for dirichlet distribution
require('ggplot2') # for plotting

# get utilities
load(file = 'buffered_meta_game_2x2_5000samples_flatBel.RDATA')
U = means_total

# parameters
p = 0.01 # probability of being lacking beliefs / playing "maximin"

utilTransform = function(U, p) {
  # expects a 8x8 meta-game utility matrix
  # outputs the weighted average of utils if agents play maximin with probabitliy p
  # maximin play is in rows / columns (1:4)
  V = U[1:4,1:4]
  for (i in 1:4){
    for (j in 1:4){
      V[i,j] = (p^2 * U[i, j]  + 
                p*(1-p) * U[i, j+4]  +
                p*(1-p) * U[i+4 ,j]  +
                (1-p)^2 * U[i+4 ,j+4] )
    }
  }
  return(V)
}

V = utilTransform(U,p)
show(V)

# initial population
p = rdirichlet(1, rep(1,4))[1,]
show(p)

for (i in 1:10000){
  eu = rowSums(sweep(V,2,p,'*'))
  av_eu = sum(eu*p)
  p = p*eu / av_eu
}

show(p)

d = data.frame(p = p, strategy = factor(names(p)))
myplot = ggplot(d, aes(x = strategy, y = p)) + geom_bar(stat = "identity")
show(myplot)
