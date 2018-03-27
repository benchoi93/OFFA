



#Vehicle Queue Generation----
  
  select <- function(ratio){
    i <- 1
    p <- 1
    
    if(sum(ratio) != 1 ){
      print("input error")
      break
    }
    
    while(TRUE){
      if ( runif(1) < ratio[i]/p ){
        return(i)
      }else{
        p <- 1-ratio[i]
        i <- i+1
      }
    }
    
  }
  
  Get_OriginLanes <- function(){
    origin_sec <- Section[which(Section$O == 1),]
    origin_lane <- 0
    
    for(i in 1:length(origin_sec$SectionID)){
      NoLane <- origin_sec$NoLane[i]
      
      for ( j in 1:NoLane){
        
        origin_lane <- append(origin_lane, paste0(origin_sec$SectionID[i],"_",j))
        
      }
      
    }
    origin_lane <- origin_lane[-1]
    return(origin_lane)
  }
  
  vehq_gen <- function(Time,origin_sec,origin_lane,local_t_ratio){
    main <- FALSE
    if(Section$Ramp[Get_Sec(origin_sec)] == 0){
      main <- TRUE
    }
    
    if(main){
      demand   <- Demand$Demand[which(Demand$Time == Time)]
    }else{
      demand   <- Demand$RampDemand[which(Demand$Time == Time)]
    }
    T_length <- Demand$T_length[which(Demand$Time==Time)]/dt
    
    if(demand == 0){
      vehq <- data.frame(matrix(0,nrow=1,ncol=6))
      colnames(vehq) <- c("VehID","Time_in","Origin","Destination","lane","Class")
      return(vehq)
    }else{
      time_in <- max(int_arrival,round(rexp(1,demand/3600)/dt))
      
      while(sum(time_in) < T_length){
        time_in <- append(time_in,max(int_arrival,round(rexp(1,demand/3600)/dt)))
      }
      
      for(i in 1:(length(time_in)-1)){
        time_in[i+1] <- time_in[i]+time_in[i+1]
      }
      
      time_in <- time_in[!duplicated(time_in)]
      
      num_veh <- length(time_in)
      vehq <- data.frame(matrix(0,nrow=num_veh,ncol=6))
      colnames(vehq) <- c("VehID","Time_in","Origin","Destination","lane","Class")
      
      vehq$Time_in <- time_in + 1
      
      # truck_ratio <- Demand$Truck_ratio[which(Demand$Time == Time)]
      truck_ratio <- local_t_ratio
      
      for (i in 1:num_veh){
        vehq$Origin[i]      <-  origin_sec
        vehq$Destination[i] <-  destin[ select(destination_ratio) ]
        vehq$Class[i]       <-  select(c(1-truck_ratio,truck_ratio))   # Class 1 = normal vehicle // Class 2 = truck vehicle
        vehq$lane[i]        <-  origin_lane
        vehq$VehID[i]       <-  paste0(Time,vehq$Class[i],vehq$Origin[i],vehq$lane[i],formatC(i,width=5,flag="0"))
      }
      
      return(vehq)
    }
    
  }
  
  Generate_Vehq <- function(){
    NoTime <- length(Demand$Time)
    origins <- Get_OriginLanes()
    Vehq <- NA
    
    for(origin in origins){
      origin_sec  <- as.numeric(unlist(strsplit(origin,"_"))[1])
      origin_lane <- as.numeric(unlist(strsplit(origin,"_"))[2])
    

      
      for ( Time in 1:NoTime){
        truck_ratio <- Demand$Truck_ratio[which(Demand$Time == Time)]
        
        
        if(origin_lane == 1){
          local_t_ratio <- truck_split[1]*truck_ratio
        }else if(origin_lane == 2){
          local_t_ratio <- truck_split[2]*truck_ratio
        }else{
          local_t_ratio <- min(1,(1-sum(truck_split))*truck_ratio)
        }
        
        
        
        if(notexist(Vehq)){
          Vehq<- vehq_gen(Time,origin_sec,origin_lane,local_t_ratio)
        }else{
          newVehq <- vehq_gen(Time,origin_sec,origin_lane,local_t_ratio)
          
          Time0 <- sum(Demand$T_length[which(Demand$Time<Time)])/dt
          newVehq$Time_in <- newVehq$Time_in + Time0
          
          Vehq <- rbind(Vehq,newVehq)
        }
      }
      
    }
    
    Vehq <- Vehq[which(Vehq$VehID != 0),]
    Vehq <- Vehq[order(Vehq$Time_in),]
    row.names(Vehq) <- NULL
    
    return(Vehq)
  }
  
  Generate_VehAtt0<-function(Vehq){
    VehicleAtt0 <- data.frame(matrix(0,nrow=length(Vehq$VehID),ncol=11))
    colnames(VehicleAtt0) <- c("Veh_ID","tau","jamgap","length","acc","dec","class","vf","LC_prep","Origin","Destination")
    
    for(i in 1:length(VehicleAtt0$Veh_ID)){
      VehicleAtt0[i,] <- as.numeric(gen_newvehicleatt(Vehq$VehID[i],Vehq))
    }
    
    return(VehicleAtt0)
  }
  
  gen_newvehicleatt <- function(VehID,Vehq){
    veh <- Vehq[which(Vehq$VehID == VehID),]
    veh_class <- veh$Class
    

    #normal
    if(veh_class == 1){
      newvehicleatt <- c(Veh_ID = VehID,
                         tau    = qlnormTrunc(runif(1),0.5   ,0.4     ,min=1 ,max=2),
#                          jamgap = qnormTrunc(runif(1) ,3.78   ,0.98    ,min=2.5 ,max=10),
                         jamgap = qnormTrunc(runif(1) ,2   ,0.98    ,min=1.5 ,max=10),

#                          tau    = qnormTrunc(runif(1) ,0.5   ,0.2     ,min=0.3 ,max=0.7),
#                          jamgap = qnormTrunc(runif(1) ,1     ,0.1     ,min=0.7 ,max=2),
                         
                         
                         length = qnormTrunc(runif(1) ,5      ,0.2     ,min=4.5 ,max=6),
                         acc    = qnormTrunc(runif(1) ,4.4844 ,0.80598 ,min=2.5 ,max=7),
                         dec    = -qnormTrunc(runif(1),4.3632 ,0.81507 ,min=2.5 ,max=7),
                         class  = veh_class,
                         vf     = qnormTrunc(runif(1),vf,0.5           ,min=25,max=33.33),
                         LC_prep= 10,
                         Origin = veh$Origin,
                         Destination = veh$Destination)
      
    }else if(veh_class == 2){
      newvehicleatt <- c(Veh_ID = VehID,
                         # tau    = qlnormTrunc(runif(1),0.93    ,0.35    ,min=0.5 ,max=4),
                         tau    = qlnormTrunc(runif(1),0.5    ,0.4    ,min=1 ,max=4)/alpha,
                         
#                          jamgap = qnormTrunc(runif(1) ,3.78    ,0.98    ,min=3   ,max=20),
                          jamgap = qnormTrunc(runif(1) ,2/alpha   ,0.98    ,min=1.5 ,max=10),

                         length = qnormTrunc(runif(1) ,10      ,2       ,min=7   ,max=15),
                         acc    = qnormTrunc(runif(1) ,4.7874  ,0.80598 ,min=2.5 ,max=7),
                         dec    = -qnormTrunc(runif(1),4.52379 ,0.81507 ,min=2.5 ,max=7),
                         class  = veh_class,
                         vf     = qnormTrunc(runif(1) ,vf*alpha,3       ,min=25*alpha ,max=33.33*alpha),
                         LC_prep= 10,
                         Origin = veh$Origin,
                         Destination = veh$Destination)
    }
    return(newvehicleatt)
    
  }
  
  gen_newvehicle    <- function(VehID,Vehq){
    veh <- Vehq[Vehq$VehID %in% VehID,]
    
    newvehicle <- data.frame(matrix(0,nrow=length(veh$VehID),ncol=14))
    colnames(newvehicle)<-c("Veh_ID","DMode","section","lane","targetlane","leaderVeh","followerVeh","Coop","LC","LC_tryt","x","y","v","a")
    
    for ( i in 1:length(veh$VehID)){
      
      newvehicle$Veh_ID[i] = veh$VehID[i]
      newvehicle$DMode[i] = "000001"
      newvehicle$section[i] = veh$Origin[i]
      newvehicle$lane[i] = veh$lane[i]
      newvehicle$targetlane[i] = veh$lane[i]
      
      newvehicle$leaderVeh[i] = Sec_LastVeh[where.am.i(veh$lane[i],veh$Origin[i])[1],where.am.i(veh$lane[i],veh$Origin[i])[2]]
      
      if(newvehicle$leaderVeh[i] !=0 ){
        if(Get_x(newvehicle$leaderVeh[i]) - Get_length(newvehicle$leaderVeh[i]) < 0 ){
          return(VehicleNA)
        }
      }
      
      newvehicle$followerVeh[i] = 0
      newvehicle$Coop[i] = 0
      newvehicle$LC[i] = 0
      newvehicle$LC_tryt[i] = 0
      newvehicle$x[i] = 0
      newvehicle$y[i] = veh$lane[i]
      newvehicle$a[i] = 0
      
      leader_v <- Get_v(newvehicle$leaderVeh[i])
      
      if(newvehicle$leaderVeh[i] == 0){
        
        newvehicle$v[i] = Get_vf(newvehicle$Veh_ID[i])
      
      }else{
        
        newvehicle$v[i] = leader_v
        if(leader_v == 0){
          return(VehicleNA)
        }
      }
      

      
    }
    
    return(newvehicle)
    
  }
  
  

  
  dummy_gen <- function(){

    result <- c()
    
    for(i in 1:dim(config)[1]){
      for(j in 1:(dim(config)[2]-1)){
        
        cursec  <- config[i,j]
        
        nextsec <- config[i,j+1]
        
        if(cursec != 0 & nextsec == 0){
          result <- cbind(result, c(i,j))
        }
      }
    }
    
    if(!notexist(result)){
      if(dim(result)[2] != 0 ){
        for(i in 1:dim(result)[2]){
          
          VehID <- paste0("dummy",result[2,i],result[1,i])
          s_length <- Get_SecLength(result[2,i])
          
          
          newvehicle <- c(Veh_ID = VehID,
                          DMode  = "000000",
                          section = result[2,i],
                          lane    = result[1,i],
                          targetlane = result[1,i],
                          leaderVeh = 0 ,
                          followerVeh = 0,
                          Coop = 0,
                          LC = 0,
                          LC_tryt = 0 ,
                          x = s_length,
                          y = result[1,i],
                          v = 0,
                          a = 0
          )
          
          newvehicleatt <- c(Veh_ID = VehID,
                             tau    = 0,
                             jamgap = 3,
                             length = 0,
                             acc    = 0,
                             dec    = 0,
                             class  = 0,
                             vf     = 0,
                             LC_prep= 0,
                             Origin = result[2,i],
                             Destination = result[2,i]
          )
          
          newvehicle    <- data.frame(lapply(newvehicle, as.character), stringsAsFactors=FALSE)
          newvehicleatt <- data.frame(lapply(newvehicleatt, as.character), stringsAsFactors=FALSE)
          
          newvehicle[,3:dim(newvehicle)[2]] <- as.numeric(as.character(newvehicle[,3:dim(newvehicle)[2]]))
          newvehicleatt[,2:dim(newvehicleatt)[2]] <- as.numeric(as.character(newvehicleatt[,2:dim(newvehicleatt)[2]]))
          
          
          Vehicle <<- rbind(Vehicle,newvehicle)
          VehicleAtt <<- rbind(VehicleAtt,newvehicleatt)
          
        }
        
      }
    }
    
    
  }
  
  
  
#Vehicle Generation in Simulation----
  
  Generate_Veh <- function(t){

    
    
    newveh <- Vehq$VehID[which(Vehq$Time_in == t)]
    
    if(!notexist(newveh)){
      VehicleAtt <<- rbind(VehicleAtt , VehicleAtt0[which(VehicleAtt0$Veh_ID %in% newveh),])
      Vehicle    <<- rbind(Vehicle    , gen_newvehicle(newveh,Vehq))
      
    }
    
  }
  
  