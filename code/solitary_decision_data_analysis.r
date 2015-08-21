require('gtools') # for dirichlet distribution
require('plyr') # to conveniently manipulate data frames
require('ggplot2')
require('reshape2')

# load('data_solitary_decisions_10x10.RDATA')

grandMeans = c(br = mean(data$u.br), mr = mean(data$u.mr), mm = mean(data$u.mm))
show(grandMeans)

means = ddply(data, .(data$n.t,data$n.a), summarise,
              N = length(u.br),
              mean.br= mean(u.br) ,
              sd.br = sd(u.br),
              mean.mr= mean(u.mr), 
              sd.mr = sd(u.mr),
              mean.mm= mean(u.mm), 
              sd.mm = sd(u.mm),
              best  = c("br", "mr", "mm")[which.max(c(mean(u.br), mean(u.mr), mean(u.mm)))],
              worst = c("br", "mr", "mm")[which.min(c(mean(u.br), mean(u.mr), mean(u.mm)))])

meansLong = melt(data = means, id.vars = c("data$n.t", "data$n.a"), measure.vars = c("mean.br", "mean.mr", "mean.mm"))
colnames(meansLong)[1] = "n.t"
colnames(meansLong)[2] = "n.a"


meansPlot = ggplot(data = meansLong, aes(x = n.a, y = value, col = variable)) +
  geom_line() + geom_point() + facet_wrap(~ n.t, nrow = 3)

show(meansPlot)




