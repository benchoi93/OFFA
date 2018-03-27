#package----
if(!("EnvStats" %in% installed.packages())){
  install.packages("EnvStats")
}
library("EnvStats")

if(!("GA" %in% installed.packages())){
  install.packages("GA")
}
library("GA")

if(!("doParallel" %in% installed.packages())){
  install.packages("doParallel")
}
library("doParallel")


#source code----
source("Simulation_General.R")
source("Global Variables.R")
source("Veh_Generation.R")
source("M1_CF.R")
source("Simulation_Update.R")
source("Simulation_Restore.R")
source("Simulation_Analysis.R")



##############################################################
##############################################################
##################### demand <- 1200 #########################
##############################################################
##############################################################


total_delay <- c(Sim = 0, delay = 0)

sim = 1


#create directories----
dir.create(file.path(getwd(),"Data"),FALSE)
dir.create(file.path(getwd(),"Data","Plot"),FALSE)
dir.create(file.path(getwd(),"Data",paste0("Sim",sim)),FALSE)
dir.create(file.path(getwd(),"Data",paste0("Sim",sim),"Vehicle"),FALSE)
dir.create(file.path(getwd(),"Data",paste0("Sim",sim),"Plot"),FALSE)

save_path <- file.path(getwd(),"Data",paste0("Sim",sim))

#Simulation Run---- 
source("Global Variables.R")

#Vehicle Queue Generation
Vehq        <- Generate_Vehq()
VehicleAtt0 <- Generate_VehAtt0(Vehq)
print("Vehicle Queue Generation Ended")

dummy_gen()
Vehicle <- Vehicle[-which(Vehicle$Veh_ID == 0),]
VehicleAtt <- VehicleAtt[-which(VehicleAtt$Veh_ID == 0),]

templc <- c()
temptemp <- c()

del_t <- 100 / dt

for(i in 1:ceiling(t1/del_t)){
  # for(i in 1:5){
  
  dir.create(file.path(getwd(),"Data","Plot"),FALSE)
  
  # for(i in 1:18){
  
  for(t in ((i-1)*del_t+1):(i*del_t)){
    
    Generate_Veh(t)
    Sim_Update(t)
    Sim_Output(t)
    
  }
  
  print(paste0(i,"- simulation end"))
  
  
  for(lane in 1:max(Section$NoLane[which(Section$Ramp == 0)])){
    file <- file.path(save_path,"Plot",paste0("lane",lane,"_",i,".png"))
    png(file,width = 1920 ,height = 1058)
    plot_trajectory((i-1)*del_t+1,i*del_t,lane)
    plot_trucks((i-1)*del_t+1,i*del_t,lane,color = "red",first = FALSE)
    dev.off()
  }
  
  for(lane in 1:max(Section$NoLane[which(Section$Ramp == 1)])){
    file <- file.path(save_path,"Plot",paste0("ramp",lane,"_",i,".png"))
    png(file,width = 1920 ,height = 1058)
    plot_trajectory((i-1)*del_t+1,i*del_t,lane,main = F,add=300)
    plot_trucks((i-1)*del_t+1,i*del_t,lane,color = "red",main=FALSE,first = FALSE ,add = 300)
    dev.off()
  }
  
  print(paste0(i,"- plot end"))
  
  # print(paste0(i,"- plot end"))
  
}




