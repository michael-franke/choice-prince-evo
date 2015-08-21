# computes averages for expected fitness overly randomly sampled stage games
# outputs an approximation of a "meta-game"

source('functions.R')

CP_names = c("Sec-Reg", "Sec-Id", "Sec-Comp", "Sec-Alt", 
             "Bel-Reg", "Bel-Id", "Bel-Comp", "Bel-Alt")

rounds = 100000
max_acts = 2

row_util = rep(0,rounds*length(CP_names)^2)
nActs = rep(2,rounds)

pb <- txtProgressBar(max = rounds,style = 3)


for (i in 1:rounds){
  setTxtProgressBar(pb, i)
  
  U = create_game(ub_util=10, ub_acts=max_acts, symmetric = T)[,,1]
  nActs[i] = dim(U)[1]

  br = 1 # flat beliefs
  
  choices = list(maximin(regret_transform(U)), #Sec-Reg
              maximin(U), # Sec-Id
              maximin(relative_power_transform(U)), # Sec-Alt
              maximin(general_power_transform(U)),  # Sec-Comp
              max_value(regret_transform(U), br), #Bel-Reg
              max_value(U, br), # Bel-Id
              max_value(relative_power_transform(U), br), # Bel-Alt
              max_value(general_power_transform(U), br)  # Bel-Comp
  )
  
#   choices.c = list(maximin(regret_transform(U)), #Sec-Reg
#                 maximin(U), # Sec-Id
#                 maximin(relative_power_transform(U)), # Sec-Alt
#                 maximin(general_power_transform(U)),  # Sec-Comp
#                 max_value(regret_transform(U)), #Bel-Reg
#                 max_value(U), # Bel-Id
#                 max_value(relative_power_transform(U)), # Bel-Alt
#                 max_value(general_power_transform(U))  # Bel-Comp
#   )
  
  for (c in 1:length(CP_names)){
    for (d in 1:length(CP_names)){
      payoff = mean(as.vector(U[choices[[c]],choices[[d]]]))
      row_util[((i-1)*length(CP_names)^2) + ((c-1)*length(CP_names))+(d)] = payoff
    }
  }  
  
}

data = data.frame( #trial = factor(trial), 
  #                   game_type = factor(game_type, levels=c("coordination", "anti-coordination",
  #                                                          "str_dominance","weak_dominance","indifference")),
  row_choice = factor(rep(rep(CP_names, each = length(CP_names)) , times = rounds), levels = CP_names),
  col_choice = factor(rep(CP_names, times = length(CP_names)*rounds), levels = CP_names),
  row_util = row_util,
  nActs = nActs)

close(pb)

# means = daply(data, c("row_choice","col_choice","game_type"), function(data) mean(data$row_util))
# show(means)
means_total = daply(data, c("row_choice","col_choice"), function(data) mean(data$row_util))
show(means_total)

# save(means_total,file="buffered_meta_game_10x10.RDATA")

show(xtable(means_total, digits = 3))

### plotting 

# datasum = summarySE(data,measurevar="row_util", groupvars=c("game_type", "col_choice", "row_choice"))

# ggplot(datasum, aes(x=row_choice, y=row_util)) + 
#   geom_bar(position=position_dodge(.9), colour="black", fill="white", stat="identity")  +
#   geom_errorbar(position=position_dodge(.9), width=.25, aes(ymin=row_util-ci, ymax=row_util+ci)) +
#   facet_grid(col_choice ~ game_type)
