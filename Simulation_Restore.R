

processVehicle <- function(t,Vehicle){
  VehicleSave <- cbind(time = t ,Vehicle[,c("Veh_ID","DMode","section","lane","x","y")])
  return(VehicleSave)
}

Sim_Output <- function(t){
  
  if(notexist(Vehicle$Veh_ID)){
    saveRDS(object = cbind(Time = t , Vehicle0)
            , file   = file.path(save_path,"Vehicle",paste0("Vehicle_",formatC(t,width = 6 ,flag="0"),".R"))
    )
  }else{
    saveRDS(object = cbind(Time = t , Vehicle)
            , file   = file.path(save_path,"Vehicle",paste0("Vehicle_",formatC(t,width = 6 ,flag="0"),".R"))
    )
  }
  
#   if(notexist(Vehicle$Veh_ID)){
#     saveRDS(object = processVehicle(Vehicle0)
#             , file   = file.path(getwd(),"Data","Vehicle",paste0("Vehicle_",formatC(t,width = 6 ,flag="0"),".R"))
#     )
#   }else{
#     saveRDS(object = processVehicle(Vehicle)
#             , file   = file.path(getwd(),"Data","Vehicle",paste0("Vehicle_",formatC(t,width = 6 ,flag="0"),".R"))
#     )
#   }
#   
}