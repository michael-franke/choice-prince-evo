source('functions.R')
CP_names = c("Sec-Reg", "Sec-Id", "Sec-Comp", "Sec-Alt", 
             "Bel-Reg", "Bel-Id", "Bel-Comp", "Bel-Alt")

rounds = 1
# game_type = c()
row_util = rep(0,rounds*length(CP_names)^2)
nActs = rep(2,rounds)

pb <- txtProgressBar(max = rounds,style = 3)


for (i in 1:rounds){
  setTxtProgressBar(pb, i)
  
  U = create_game(ub_util=10, ub_acts=2, symmetric = T)[,,1]
  nActs[i] = dim(U)[1]
  
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
