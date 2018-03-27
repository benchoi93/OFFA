


result <- c()
for(t in 2:15000){
  
  Veh1 <- readRDS(file.path(save_path,"Vehicle",paste0("Vehicle_",formatC(t-1,width = 6 ,flag="0" , format = "fg"),".R")))
  Veh2 <- readRDS(file.path(save_path,"Vehicle",paste0("Vehicle_",formatC(t,width = 6 ,flag="0", format = "fg"),".R")))
  
  Veh1 <- Veh1[which(Veh1$Veh_ID != 0 ),]
  Veh2 <- Veh2[which(Veh2$Veh_ID != 0 ),]
  
  
  for(vehid in Veh1$Veh_ID){
    
    if(vehid %in% Veh2$Veh_ID){
      
      sec0 <- Veh1$section[which(Veh1$Veh_ID == vehid)]
      next_sec <- Get_NextSection(sec0)
      
      if(next_sec == Veh2$section[which(Veh2$Veh_ID == vehid)]){
        lane <- Veh2$lane[which(Veh2$Veh_ID == vehid)]
        result <- rbind(result , c(t , next_sec , lane))
        
      }
      
      
    }else{
      lane <- Veh1$lane[which(Veh1$Veh_ID == vehid)]
      result <- rbind(result , c(t, 0 , lane))
      
      
    }
    
  }
  
}

result2 <-c()
for(t in 1:15000){
  
  Veh1 <- readRDS(file.path(save_path,"Vehicle",paste0("Vehicle_",formatC(t,width = 6 ,flag="0" , format = "fg"),".R")))
  
  result2 <- rbind(result2, c(t , 
                              length(Veh1$Veh_ID[Veh1$section == 1 ]),
                              length(Veh1$Veh_ID[Veh1$section == 2 ]),
                              length(Veh1$Veh_ID[Veh1$section == 3 ]),
                              length(Veh1$Veh_ID[Veh1$section == 4 ]),
                              length(Veh1$Veh_ID[Veh1$section == 5 ]),
                              length(Veh1$Veh_ID[Veh1$section == 6 ])
                              ) )
  
}

temp <- c(1,1:130*100)
temp <- c(1,1:43*300)

result1 <- c()
result3 <- c()

for(i in 1:(length(temp)-1)){
  
  target <- temp[i]:temp[i+1]
  
  temp1 <- result[result[,1] %in% target , ]
  
  result1 <- rbind(result1, c(length(temp1[which(temp1[,2] == 2)]) *3600 / 30 /3 ,
                              length(temp1[which(temp1[,2] == 3)]) *3600 / 30 /3 ,
                              length(temp1[which(temp1[,2] == 4)]) *3600 / 30 /3 ,
                              length(temp1[which(temp1[,2] == 5)]) *3600 / 30 /3 ,
                              length(temp1[which(temp1[,2] == 6)]) *3600 / 30 /3 ,
                              length(temp1[which(temp1[,2] == 0)]) *3600 / 30 /3 
                              ))
  result3 <- rbind(result3 , c(  colMeans(result2[which(result2[,1] %in% target) ,])[2:7] / 0.5 ))
}

res <-c()
for(i in 1:6){
  for(j in 1:43){
    
    res <- rbind(res , c(section = i , time =  j , flow = result1[j,i] , density = result3[j,i]))
    
  }
}



plot(res[,4] ,res[,3])
