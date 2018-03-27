
VD_setting_main <- function(t1,t2){
  main_sec <- Section$SectionID[which(Section$Ramp == 0)]
  VD <- readRDS(file.path(save_path,"Vehicle",paste0("Vehicle_",formatC(t1,width = 6 ,flag="0"),".R")))
  for( t in (t1+1):t2){
    temp <- readRDS(file.path(save_path,"Vehicle",paste0("Vehicle_",formatC(t,width = 6 ,flag="0"),".R")))
    VD <- rbind(VD , temp)
  }
  return(VD)
}



plot_spl <- function(t1,t2,VehID,lane){
  if(dim(VehicleData)[1] == 0){
    VehicleData <- VD_setting_main(t1,t2)
  }
  temp <- VehicleData[which(VehicleData$Veh_ID == VehID & VehicleData$Time <= t2 & VehicleData$Time >=t1 & VehicleData$lane == lane ),]
  
  sec_order <- Get_SecOrder()
  sec_set <- unique(temp$section)
  for(sec in sec_set){
    l <- 0
  
    loc <- which(sec_order == sec,arr.ind = TRUE)[1,2]
    sec_temp <- 1:dim(sec_order)[2]
    sec_temp <- sec_temp[which(sec_temp < loc)]
    
    if(!notexist(sec_temp)){
      
      for(i in 1:length(sec_temp)){
        l <- l + Get_SecLength(sec_order["main",sec_temp[i]])
      }
      
    }
    temp$x[temp$section == sec] <- temp$x[temp$section == sec] + l
  }
  
  
  
  lines(temp$Time,temp$x,col="red")
}

plot_trajectory <- function(t1,t2,lane, main = TRUE,color = "black",first=TRUE,lim=0,add=0){
  total_length <- sum(Section$SectionLength[which(Section$Ramp ==0)])
  length <- total_length
  # length <- 1000
  if(lim==0){
    lim <- c(0,length)
  }
  
  
  ramp_sec <- Section$SectionID[which(Section$Ramp != 0)]
  main_sec <- Section$SectionID[which(Section$Ramp == 0)]
  
  if(first){
    plot(NA,NA, xlim = c(t1,t2),ylim=c(lim[1],lim[2]))
    
    x_outline <- seq(round(t1,-2),t2,100)
    for(i in 1:length(x_outline)){
      lines(c(x_outline[i],x_outline[i]),c(0,total_length),col="blue")
    }
    
    y_outline <- c(0)
    l <- 0
    for(i in 1:length(main_sec)){
      l <- l + Section$SectionLength[which(Section$SectionID == main_sec[i])]
      y_outline <- append(y_outline,l)
    }
    for(i in 1:length(y_outline)){
      lines(c(t1,t2),c(y_outline[i],y_outline[i]),col="red")
    }
    
  }
  for( t in t1:t2){
    temp <- readRDS(file.path(save_path,"Vehicle",paste0("Vehicle_",formatC(t,width = 6 ,flag="0"),".R")))
    
    delete <- which(grepl("dummy",temp$Veh_ID))
    if(!notexist(delete)){
      temp <- temp[-delete,]
    }
    
    
    
    if(main){
      temp <- temp[which(temp$section %in% main_sec &  temp$lane == lane),]
    }else{
      temp <- temp[which(temp$section %in% ramp_sec &  temp$lane == lane),]
    }
    
    
    # temp <- temp[,c("Time","Veh_ID","x","section")]
    sec_order <- Get_SecOrder()
    
    sec_set <- unique(temp$section)
    

    
    for(sec in sec_set){
      l <- 0
      
      temp2 <- temp[temp$section == sec,]
      
      loc <- which(sec_order == sec,arr.ind = TRUE)[1,2]
      sec_temp <- 1:dim(sec_order)[2]
      sec_temp <- sec_temp[which(sec_temp < loc)]
      
      if(!notexist(sec_temp)){
        
        for(i in 1:length(sec_temp)){
          l <- l + Get_SecLength(sec_order["main",sec_temp[i]])
        }
        
      }
      temp2$x <- temp2$x + l
      if(!notexist((temp2$Veh_ID))){
        points(temp2$Time,temp2$x + add,col=color)
      }
    }
    
    

  }
}


plot_trucks <- function(t1,t2,lane, main = TRUE,color = "black",first=TRUE,add = 0){
  length <- sum(Section$SectionLength[which(Section$Ramp ==0)])
  # length <- 1000
  
  ramp_sec <- Section$SectionID[which(Section$Ramp != 0)]
  main_sec <- Section$SectionID[which(Section$Ramp == 0)]
  
  if(first){
    plot(NA,NA, xlim = c(t1,t2),ylim=c(0,length))
    
    x_outline <- seq(round(t1,-2),t2,100)
    for(i in 1:length(x_outline)){
      lines(c(x_outline[i],x_outline[i]),c(0,length),col="blue")
    }
    
    y_outline <- c(0)
    l <- 0
    for(i in 1:length(main_sec)){
      l <- l + Section$SectionLength[which(Section$SectionID == main_sec[i])]
      y_outline <- append(y_outline,l)
    }
    for(i in 1:length(y_outline)){
      lines(c(t1,t2),c(y_outline[i],y_outline[i]),col="red")
    }
    
  }
  for( t in t1:t2){
    temp <- readRDS(file.path(save_path,"Vehicle",paste0("Vehicle_",formatC(t,width = 6 ,flag="0"),".R")))
    delete <- which(grepl("dummy",temp$Veh_ID) | temp$Veh_ID == 0)
    if(!notexist(delete)){
      temp <- temp[-delete,]
    }    
    if(length(temp$Veh_ID) > 0){
      temp1 <- c()
      for(i in 1:length(temp$Veh_ID)){
        class <- strsplit(temp$Veh_ID[i],"")[[1]][2]
        if(class == 2){
          temp1 <- rbind(temp1,temp[i,])
        }
      }
      temp <- temp1
      
    }
    
    
    if(main){
      temp <- temp[which(temp$section %in% main_sec &  temp$lane == lane),]
    }else{
      temp <- temp[which(temp$section %in% ramp_sec &  temp$lane == lane),]
    }
    
    
    # temp <- temp[,c("Time","Veh_ID","x","section")]
    sec_order <- Get_SecOrder()
    
    sec_set <- unique(temp$section)
    
    
    
    for(sec in sec_set){
      l <- 0
      
      temp2 <- temp[temp$section == sec,]
      
      loc <- which(sec_order == sec,arr.ind = TRUE)[1,2]
      sec_temp <- 1:dim(sec_order)[2]
      sec_temp <- sec_temp[which(sec_temp < loc)]
      
      if(!notexist(sec_temp)){
        
        for(i in 1:length(sec_temp)){
          l <- l + Get_SecLength(sec_order["main",sec_temp[i]])
        }
        
      }
      temp2$x <- temp2$x + l
      if(!notexist((temp2$Veh_ID))){
        points(temp2$Time,temp2$x+add,col=color)
      }
    }
    
    
    
  }
}





plot_veh <- function(t, lane = 1, main = TRUE){
  ramp_sec <- Section$SectionID[which(Section$Ramp != 0)]
  main_sec <- Section$SectionID[which(Section$Ramp == 0)]
  
  temp <- readRDS(file.path(save_path,"Vehicle",paste0("Vehicle_",formatC(t,width = 6 ,flag="0"),".R")))
  
  if(main){
    temp <- temp[which(temp$section %in% main_sec &  temp$lane == lane),]
  }else{
    temp <- temp[which(temp$section %in% ramp_sec &  temp$lane == lane),]
  }
  
  
  # temp <- temp[,c("Time","Veh_ID","x","section")]
  sec_order <- Get_SecOrder()
  
  sec_set <- unique(temp$section)
  
  
  
  for(sec in sec_set){
    l <- 0
    
    temp2 <- temp[temp$section == sec,]
    
    loc <- which(sec_order == sec,arr.ind = TRUE)[1,2]
    sec_temp <- 1:dim(sec_order)[2]
    sec_temp <- sec_temp[which(sec_temp < loc)]
    
    if(!notexist(sec_temp)){
      
      for(i in 1:length(sec_temp)){
        l <- l + Get_SecLength(sec_order["main",sec_temp[i]])
      }
      
    }
    temp2$x <- temp2$x + l
    if(!notexist((temp2$Veh_ID))){
      points(temp2$Time,temp2$x)
    }
  }
}






n_curve <- function(arrival,departure){
  departure_cum <- departure
  for ( i in 1: length(departure$time)){
    departure_cum$veh[i] <- sum( departure$veh[1:i])
  }
  arrival_cum <- arrival
  for ( i in 1: length(arrival$time)){
    arrival_cum$veh[i] <- sum( arrival$veh[1:i])
  }
  arrival_cum2 <- arrival_cum
  
  
  time_dif <- min(which(departure_cum$veh != 0 )-1) - min(which(arrival_cum$veh != min(arrival_cum$veh) )-1)
  
  
  arrival_cum2$time <- arrival_cum$time + time_dif
  
  
  arrival_cum3   <- arrival_cum2[1:(dim(arrival_cum2)[1]-time_dif),]
  departure_cum3 <- departure_cum[(time_dif+1):dim(departure_cum)[1],]
  
  cumul <- cbind(time = arrival_cum3$time, num_veh = arrival_cum3$veh-departure_cum3$veh)
  ttdelay <- sum(cumul[,2])
  
  
  filename1<- file.path(save_path,"Plot",paste0("N_curve.png"))
  png(file = filename1,width = 960 ,height = 529)
  plot(x = NA,y = NA,xlim = c(min(arrival_cum$time),max(arrival_cum$time)),ylim = c( min( min(arrival_cum$veh),min(departure_cum$veh)   ) , max( max(arrival_cum$veh),max(departure_cum$veh)   )   ),     xlab = "time", ylab = "# of veh",main = "Cumulative Plot",cex.lab = 1.5,cex.main=2)
  lines(departure_cum$time,departure_cum$veh)
  lines(arrival_cum$time, arrival_cum$veh,col="red")
  lines(arrival_cum2$time, arrival_cum2$veh,col="blue")
  legend(10000,300,legend=c("departure curve","arrival curve","virtual arrival curve"),lty=c(1,1,1),col = c("black","red","blue"),cex = 1.5)
  text(2000,400,paste0(ttdelay))
  dev.off()
  
  # plot(cumul,type="l")
  return(ttdelay)
}