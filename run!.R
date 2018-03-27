source("Exec_Sim.R")




VehicleData  <- VD_setting_main(1500,1600)

temp<- VehicleData[which(VehicleData$lane == 3),]


plot_trajectory(1500,1600,3,lim = c(500,1500))

plot_trucks(1500,1600,3,color = "red",first = FALSE)


plot_spl(1500,1600,"211200011",3)



sam <- VehicleData[which(VehicleData$Veh_ID == "211200011"),]
sam2 <- VehicleData[which(VehicleData$Veh_ID == "211100011"),]
