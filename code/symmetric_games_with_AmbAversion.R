source('functions.R')


CP_names = c("Sec-Reg", "Sec-Id", # security strategy
             "BFl-Reg", "BFl-Id", # flat belief
             "BAr-Reg", "BAr-Id", # arbitrary belief
             "AAs-Reg", "AAs-Id", # symmetric ambiguity aversion
             "AAa-Reg", "AAa-Id"  # aymmetric ambiguity aversion
             )

rounds = 50000
# game_type = c()
row_util = rep(0,rounds*length(CP_names)^2)
nActs = rep(2,rounds)

pb <- txtProgressBar(max = rounds,style = 3)


for (i in 1:rounds){
  setTxtProgressBar(pb, i)
  
  U = create_game(ub_util=10, ub_acts=2, symmetric = T)[,,1]
#   nActs[i] = dim(U)[1]
  
  #   gt = classify_game(U)
  
  BAr = matrix(rep(rdirichlet(1,rep(1,dim(U)[2]))[1,], each=dim(U)[1]), nrow=dim(U)[1], ncol=dim(U)[2]) # arbitrary belief
  x = runif(n = 1, min = 0, max = 0.5)
  BAAs = c(x, 1-x)
  BAAa = sort(runif(n = 2, min = 0, max = 1))

  choices = c(maximin(regret_transform(U)), #Sec-Reg
              maximin(U), # Sec-Id
              max_value(regret_transform(U)), #BFl-Reg
              max_value(U), # BFl-Id
              max_value(regret_transform(U) * BAr), #BAr-Reg
              max_value(U * BAr), # BAr-Id
              maximin(AA_transform(regret_transform(U), BAAs)), #AAs-Reg
              maximin(AA_transform(U, BAAs)), # AAs-Id
              maximin(AA_transform(regret_transform(U), BAAa)), #AAa-Reg
              maximin(AA_transform(U, BAAa)) # AAa-Id
  )
  
  choices.c = c(maximin(regret_transform(U)), #Sec-Reg
            maximin(U), # Sec-Id
            max_value(regret_transform(U) * BAr), #BAr-Reg
            max_value(U * BAr), # BAr-Id
            max_value(regret_transform(U)), #BFl-Reg
            max_value(U), # BFl-Id
            maximin(AA_transform(regret_transform(U), BAAs)), #AAs-Reg
            maximin(AA_transform(U, BAAs)), # AAs-Id
            maximin(AA_transform(regret_transform(U), BAAa)), #AAa-Reg
            maximin(AA_transform(U, BAAa)) # AAa-Id
  )

  
  for (c in 1:length(CP_names)){
    for (d in 1:length(CP_names)){
      row_util[((i-1)*length(CP_names)^2) + ((c-1)*length(CP_names))+(d)] = U[choices[c],choices.c[d]]
    }
  }  
  
}

data = data.frame( #trial = factor(trial), 
  #                   game_type = factor(game_type, levels=c("coordination", "anti-coordination",
  #                                                          "str_dominance","weak_dominance","indifference")),
  row_choice = factor(rep(rep(CP_names, each = length(CP_names)) , times = rounds), levels = CP_names),
  col_choice = factor(rep(CP_names, times = length(CP_names)*rounds), levels = CP_names),
  row_util = row_util)

close(pb)

# means = daply(data, c("row_choice","col_choice","game_type"), function(data) mean(data$row_util))
# show(means)
means_total = daply(data, c("row_choice","col_choice"), function(data) mean(data$row_util))
show(means_total)

# save(means_total,file="buffered_meta_game_2x2.RDATA")

# show(xtable(means_total))

### plotting 

# datasum = summarySE(data,measurevar="row_util", groupvars=c("game_type", "col_choice", "row_choice"))

# ggplot(datasum, aes(x=row_choice, y=row_util)) + 
#   geom_bar(position=position_dodge(.9), colour="black", fill="white", stat="identity")  +
#   geom_errorbar(position=position_dodge(.9), width=.25, aes(ymin=row_util-ci, ymax=row_util+ci)) +
#   facet_grid(col_choice ~ game_type)
