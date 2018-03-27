# 
# 
jamset <- c(2.5,3.0,3.5)
tauset <- c(0.3,0.5,0.7)

scenario <- merge(jamset,tauset)
# 
# for(sim in 1:10){

sim = 9
  jam_mean <- scenario[sim , 1]
  tau_mean <- scenario[sim , 2]
  
  source("Exec_Sim.R")
# }