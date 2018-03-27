
Gen_Section <- function(TotalLength , SectionLength, MergePoint, MinMergeLength){
  
  NumSec <- ceiling(TotalLength/SectionLength)
  
  temp <- data.frame(matrix(0 , nrow = NumSec , ncol = 11))
  colnames(temp) <- c("SectionID","PreviousSection", "NextSection",  "MergingType","DivergingType","SectionLength","NoLane","Grade","Ramp","Origin","Destination")
  
  temp$SectionID        <- 1:NumSec
  temp$PreviousSection  <- 0:(NumSec-1)
  temp$NextSection      <- c(2:NumSec,0)
  temp$SectionLength    <- SectionLength
  temp$Origin[1] <- 1
  temp$Destination[NumSec] <- 1
  
  MergeSec <- ceiling(MergePoint / SectionLength)
  
  temp$NoLane[1:MergeSec] <- 3
  temp$NoLane[(MergeSec + 1):length(temp$SectionID)] <- 2
  
  
  NumMerge <-   ceiling(MinMergeLength / SectionLength)
  
  temp$MergingType[(MergeSec - NumMerge + 1):MergeSec] <- 1
  
  return(temp)
}


demand_gen <- function(demand=1000,truck=0.2){
  temp  <- data.frame(matrix(0,nrow=5,ncol=5))
  colnames(temp) <- c("Time"," T_length","Demand"," RampDemand"," Truck_ratio")
  
  temp[,1]   <- 1:5
  temp[,2]   <- c(100,360,360,360,360)
  temp[,3]   <- c(demand/4,demand,demand,0,0)
  temp[,5]   <- c(0,truck,truck,0,0)
  
  row.names(temp) <- NULL
  write.csv(temp,"test_demand.R",row.names = F)
  
}

notexist <- function(value){
  return(  ( (is.na(value)) || (length(value) == 0) || is.null(value) )  )
}

Get_Veh <- function(VehID){
  return(which(Vehicle$Veh_ID == VehID))
}

Get_VehAtt <- function(VehID){
  return(which(VehicleAtt$Veh_ID == VehID))
}

Get_Sec<-function(SecID){
  return(which(Section$SectionID==SecID))
}

#Call and Assign Vehicle Information----
  
  Get_DMode <- function(VehID){
    if(notexist(VehID) | VehID ==0){
      return("000001")
    }else{
      return(Vehicle$DMode[Get_Veh(VehID)])
    }
  }
  
  Set_DMode <- function(VehID,value){
    Vehicle$DMode[Get_Veh(VehID)] <<- value
  }
  
  Get_section <- function(VehID){
    if(notexist(VehID) | VehID ==0){
      return(0)
    }else{
      return(Vehicle$section[Get_Veh(VehID)])
    }
  }
  
  Set_section <- function(VehID,value){
    Vehicle$section[Get_Veh(VehID)] <<- value
  }
  
  Get_lane <- function(VehID){
    if(notexist(VehID) | VehID ==0){
      return(0)
    }else{
      return(Vehicle$lane[Get_Veh(VehID)])
    }
  }
  
  Set_lane <- function(VehID,value){
    Vehicle$lane[Get_Veh(VehID)] <<- value
  }
  
  Get_targetlane <- function(VehID){
    if(notexist(VehID) | VehID ==0){
      return(0)
    }else{
      return(Vehicle$targetlane[Get_Veh(VehID)])
    }
  }
  
  Set_targetlane <- function(VehID,value){
    Vehicle$targetlane[Get_Veh(VehID)] <<- value
  }
  
  Get_leaderVeh <- function(VehID){
    if(notexist(VehID) | VehID ==0){
      return(0)
    }else{
      return(Vehicle$leaderVeh[Get_Veh(VehID)])
    }
  }
  
  Set_leaderVeh <- function(VehID,value){
    Vehicle$leaderVeh[Get_Veh(VehID)] <<- value
  }
  
  Get_followerVeh <- function(VehID){
    if(notexist(VehID) | VehID ==0){
      return(0)
    }else{
      return(Vehicle$followerVeh[Get_Veh(VehID)])
    }
  }
  
  Set_followerVeh <- function(VehID,value){
    Vehicle$followerVeh[Get_Veh(VehID)] <<- value
  }
  
  Get_Coop <- function(VehID){
    if(notexist(VehID) | VehID ==0){
      return(0)
    }else{
      return(Vehicle$Coop[Get_Veh(VehID)])
    }
  }
  
  Set_Coop <- function(VehID,value){
    Vehicle$Coop[Get_Veh(VehID)] <<- value
  }
  
  Get_LC <- function(VehID){
    if(notexist(VehID) | VehID ==0){
      return(0)
    }else{
      return(Vehicle$LC[Get_Veh(VehID)])
    }
  }
  
  Set_LC <- function(VehID,value){
    Vehicle$LC[Get_Veh(VehID)] <<- value
  }
  
  Get_LC_tryt <- function(VehID){
    if(notexist(VehID) | VehID ==0){
      return(0)
    }else{
      return(Vehicle$LC_tryt[Get_Veh(VehID)])
    }
  }
  
  Set_LC_tryt <- function(VehID,value){
    Vehicle$LC_tryt[Get_Veh(VehID)] <<- value
  }
  
  Get_x <- function(VehID){
    if(notexist(VehID) | VehID ==0){
      return(0)
    }else{
      return(Vehicle$x[Get_Veh(VehID)])
    }
  }
  
  Set_x <- function(VehID,value){
    Vehicle$x[Get_Veh(VehID)] <<- value
  }
  
  Get_y <- function(VehID){
    if(notexist(VehID) | VehID ==0){
      return(0)
    }else{
      return(Vehicle$y[Get_Veh(VehID)])
    }
  }
  
  Set_y <- function(VehID,value){
    Vehicle$y[Get_Veh(VehID)] <<- value
  }
  
  Get_v <- function(VehID){
    if(notexist(VehID) | VehID ==0){
      return(vf*1.1)
    }else{
      return(Vehicle$v[Get_Veh(VehID)])
    }
  }
  
  Set_v <- function(VehID,value){
    Vehicle$v[Get_Veh(VehID)] <<- value
  }
  
  Get_a <- function(VehID){
    if(notexist(VehID) | VehID ==0){
      return(5)
    }else{
      return(Vehicle$a[Get_Veh(VehID)])
    }
  }
  
  Set_a <- function(VehID,value){
    Vehicle$a[Get_Veh(VehID)] <<- value
  }

#Call and Assign VehicleAtt Information----
  
  Get_tau <- function(VehID){
    if(notexist(VehID) | VehID ==0){
      return(2)
    }else{
      return(VehicleAtt$tau[Get_VehAtt(VehID)])
    }
  }
  
  Set_tau <- function(VehID,value){
    VehicleAtt$tau[Get_VehAtt(VehID)] <<- value
  }
  
  Get_jamgap <- function(VehID){
    if(notexist(VehID) | VehID ==0){
      return(2)
    }else{
      return(VehicleAtt$jamgap[Get_VehAtt(VehID)])
    }
  }
  
  Set_jamgap <- function(VehID,value){
    VehicleAtt$jamgap[Get_VehAtt(VehID)] <<- value
  }
  
  Get_length <- function(VehID){
    if(notexist(VehID) | VehID ==0){
      return(10)
    }else{
      return(VehicleAtt$length[Get_VehAtt(VehID)])
    }
  }
  
  Set_length <- function(VehID,value){
    VehicleAtt$length[Get_VehAtt(VehID)] <<- value
  }
  
  Get_acc <- function(VehID){
    if(notexist(VehID) | VehID ==0){
      return(5)
    }else{
      return(VehicleAtt$acc[Get_VehAtt(VehID)])
    }
  }
  
  Set_acc <- function(VehID,value){
    VehicleAtt$acc[Get_VehAtt(VehID)] <<- value
  }
  
  Get_dec <- function(VehID){
    if(notexist(VehID) | VehID ==0){
      return(-5)
    }else{
      return(VehicleAtt$dec[Get_VehAtt(VehID)])
    }
  }
  
  Set_dec <- function(VehID,value){
    VehicleAtt$dec[Get_VehAtt(VehID)] <<- value
  }
  
  Get_class <- function(VehID){
    if(notexist(VehID) | VehID ==0){
      return(1)
    }else{
      return(VehicleAtt$class[Get_VehAtt(VehID)])
    }
  }
  
  Set_class <- function(VehID,value){
    VehicleAtt$class[Get_VehAtt(VehID)] <<- value
  }
  
  Get_vf <- function(VehID){
    if(notexist(VehID) | VehID ==0){
      return(vf)
    }else{
      return(VehicleAtt$vf[Get_VehAtt(VehID)])
    }
  }
  
  Set_vf <- function(VehID,value){
    VehicleAtt$vf[Get_VehAtt(VehID)] <<- value
  }
  
  Get_LC_prep <- function(VehID){
    if(notexist(VehID) | VehID ==0){
      return(10)
    }else{
      return(VehicleAtt$LC_prep[Get_VehAtt(VehID)])
    }
  }
  
  Set_LC_prep <- function(VehID,value){
    VehicleAtt$LC_prep[Get_VehAtt(VehID)] <<- value
  }

#Call and Assign Section Information ----
  Get_PreviousSection <- function(SecID){
    return(Section$PreviousSection[Get_Sec(SecID)])
  }
  
  Get_NextSection <- function(SecID){
    return(Section$NextSection[Get_Sec(SecID)])
  }
  
  Get_MergingType <- function(SecID){
    return(Section$MergingType[Get_Sec(SecID)])
  }

  Get_DivergingType <- function(SecID){
    return(Section$DivergingType[Get_Sec(SecID)])
  }
  
  Get_SecLength <- function(SecID){
    if(notexist(SecID)){
      return(0)
    }else{
      return(Section$SectionLength[Get_Sec(SecID)])
    }  
  }
  
  Get_NoLane <- function(SecID){
    if(notexist(SecID) || SecID == 0){
      return(0)
    }else{
      return(Section$NoLane[Get_Sec(SecID)])
    }

  }
  
  Get_Grade <- function(SecID){
    if(notexist(SecID)|| SecID == 0){
      return(0)
    }else{
      return(Section$Grade[Get_Sec(SecID)])
    }  }
  
  Get_Ramp <- function(SecID){
    if(notexist(SecID)|| SecID == 0){
      return(0)
    }else{
      return(Section$Ramp[Get_Sec(SecID)])
    }  }

#LC leader/follower check----
  Get_RampType <- function(sec){
    ramp <- Get_Ramp(sec)
    if(ramp == 0){
      return("noramp")
    }else{
      prev_sec <- Get_PreviousSection(sec)
      next_sec <- Get_NextSection(sec)
      
      if(prev_sec == 0){
        return("onramp")
      }else if(next_sec == 0){
        return("offramp")
      }else{
        return("noramp")
      }
    }
  }
  
  Get_SecOrder <- function(){
    sec <- Section$SectionID[which(Section$O == 1 & Section$Ramp == 0)]
    secorder <- sec
    
    while(Get_NextSection(sec) != 0){
      sec<-Get_NextSection(sec)
      secorder <- append(secorder,sec)
    }
    ramp <- Section$SectionID[which(Section$Ramp != 0)]
    secorder <- rbind(main = secorder, ramp = 0)
    if(!notexist(ramp)){
      for(i in 1:length(ramp)){
        ramp_type <-Get_RampType(ramp[i])
        if(ramp_type == "onramp"){
          next_sec <- Get_NextSection(ramp[i])
          pair_sec <- Get_PreviousSection(next_sec)
          
          secorder["ramp",which(secorder["main",] == pair_sec)] <- ramp[i]
          
        }else if(ramp_type == "offramp"){
          prev_sec <- Get_PreviousSection(ramp[i])
          pair_sec <- Get_NextSection(prev_sec)
          
          secorder["ramp",which(secorder["main",] == pair_sec)] <- ramp[i]
          
        }
      }
    }
    return(secorder)
  }
  
  
  # lane_config <- function(){
  #   
  # 
  #   sec <- Get_SecOrder()["main",1]
  #   next_sec <- Get_NextSection(sec)
  #   prev_sec <- Get_PreviousSection(sec)
  #   
  #   config <- c()
  #   
  #   while(sec != 0){
  #     
  #     NoLane <- Get_NoLane(sec)
  #     
  #     lanes <- rep(sec,NoLane)
  #     names(lanes) <-NULL
  #     
  #     if(next_sec != 0 ){
  #       next_NoLane <- Get_NoLane(next_sec)
  #       
  #       if(next_NoLane > NoLane){
  #         
  #         div.type <- Get_DivergingType(next_sec)
  #         
  #         if(div.type == 1){
  #           lanes <- append(lanes,0)
  #         }else if(div.type == 2){
  #           lanes <- append(0,lanes)
  #         }
  #         
  #       }
  #     }
  #     
  #     if(prev_sec != 0 ){
  #       prev_NoLane <- Get_NoLane(prev_sec)
  #       
  #       if(prev_NoLane > NoLane){
  #         
  #         mer.type <- Get_MergingType(prev_sec)
  #         
  #         if(mer.type == 1){
  #           lanes <- append(lanes,0)
  #         }
  #         
  #       }
  #     }
  #   
  #     ramp <- secorder["ramp",which(secorder == sec,arr.ind = T)[2]]
  #     if(ramp != 0){
  #       lanes <- append(lanes,ramp)
  #     }
  #     
  #     if(!notexist(config)){
  #       if(dim(config)[1] < length(lanes)){
  #         
  #         if(which(lanes == 0) == 1){
  #           config <- rbind(0,config)
  #         }
  #         
  #         if(which(lanes == 0 ) == length(lanes)){
  #           config <- rbind(config,0)
  #         }
  #         
  #       }
  #       
  #       while(dim(config)[1] != length(lanes)){
  #         lanes <- append(lanes,0)
  #       }
  #     }
  #     
  #     
  #       
  #     
  #     config <- cbind(config,lanes)
  #     
  #     sec <- next_sec
  #     next_sec <- Get_NextSection(sec)
  #     prev_sec <- Get_PreviousSection(sec)
  #       
  #   }
  #   
  #   
  #   
  #   
  #   return(config)
  # }
  
  
  lane_config <- function(){
    
    
    sec <- Get_SecOrder()["main",1]
    next_sec <- Get_NextSection(sec)
    prev_sec <- Get_PreviousSection(sec)
    
    config <- c()
    
    while(sec != 0){
      
      NoLane <- Get_NoLane(sec)
      
      lanes <- rep(sec,NoLane)
      names(lanes) <-NULL
      
      if(next_sec != 0 ){
        next_NoLane <- Get_NoLane(next_sec)
        
        if(next_NoLane > NoLane){
          
          div.type <- Get_DivergingType(next_sec)
          
          if(div.type == 1){
            lanes <- append(lanes,0)
          }else if(div.type == 2){
            lanes <- append(0,lanes)
          }
          
        }
      }
      
      if(prev_sec != 0 ){
        prev_NoLane <- Get_NoLane(prev_sec)
        
        if(prev_NoLane > NoLane){
          
          mer.type <- Get_MergingType(prev_sec)
          
          if(mer.type == 1){
            lanes <- append(lanes,0)
          }
          
        }
      }
      
      ramp <- secorder["ramp",which(secorder == sec,arr.ind = T)[2]]
      if(ramp != 0){
        lanes <- append(lanes,rep(ramp , Get_NoLane(ramp)))
      }
      
      if(!notexist(config)){
        if(dim(config)[1] < length(lanes)){
          
          if(length(which(lanes == 0)) == 1){
            config <- rbind(0,config)
          }
          
          if(length(which(lanes == 0 )) == length(lanes)){
            config <- rbind(config,0)
          }
          
        }
        
        #         while(dim(config)[1] != length(lanes)){
        #           lanes <- append(lanes,0)
        #         }
        while(1){
          if (dim(config)[1] < length(lanes)){config <- rbind(config,0)}
          else if (dim(config)[1] > length(lanes)){lanes <- append(lanes,0)}
          else {break}
          # lanes <- append(lanes,0)
        }
      }
      
      
      
      
      config <- cbind(config,lanes)
      
      sec <- next_sec
      next_sec <- Get_NextSection(sec)
      prev_sec <- Get_PreviousSection(sec)
      
    }
    return(config)
  }
  
  
  Get_Distance <- function(VehID1,VehID2){

    sec_1 <- Get_section(VehID1)
    sec_2 <- Get_section(VehID2)
    
    if(sec_1 ==0){
      return(1000)
    }
    
    if(sec_2 ==0){
      return(1000)
    }
    
    if(sec_1 == sec_2){
      x_1   <- Get_x(VehID1)
      x_2   <- Get_x(VehID2)
      
      distance <- abs(x_1-x_2)
    }else{
      x_1   <- Get_x(VehID1)
      loc_1 <- which(secorder == sec_1,arr.ind = TRUE)[1,2]
      length_1 <- Get_SecLength(sec_1)
      
      x_2   <- Get_x(VehID2)
      loc_2 <- which(secorder == sec_2,arr.ind = TRUE)[1,2]
      length_2 <- Get_SecLength(sec_2)

      
      if(loc_1 > loc_2){
        btw_sec <- (loc_1:loc_2)
        btw_sec <- btw_sec[-which(btw_sec == loc_1)]
        btw_sec <- btw_sec[-which(btw_sec == loc_2)]
        
        btw_length <- 0
        if(!notexist(btw_sec)){
          btw_length <- sum(Section$SectionLength[which(Section$SectionID %in% btw_sec)])
        }
        
        distance <- x_1 + length_2 - x_2 + btw_length
      }else if(loc_1 < loc_2){
        btw_sec <- (loc_1:loc_2)
        btw_sec <- btw_sec[-which(btw_sec == loc_1)]
        btw_sec <- btw_sec[-which(btw_sec == loc_2)]
        
        btw_length <- 0
        if(!notexist(btw_sec)){
          btw_length <- sum(Section$SectionLength[which(Section$SectionID %in% btw_sec)])
        }
        
        distance <- x_2 + length_1 - x_1 + btw_length
      }else{
        distance <- abs((length_1 - x_1) - (length_2 - x_2))
      }
      
      
    }
    return(distance)
    
    
    
  }
  
  where.am.i <- function(lane,sec){
    
    
    col1 <- unique(which(config == sec,arr.ind = TRUE)[,"col"])
    
    config <- config[,col1]
    
    count <- 0
    
    for( i in 1:length(config)){
      
      if( config[i] == sec){
        count <- count + 1
        
        if(count == lane){
          row1 <- i
        }
        
        
      }else{
        
        next
      }
      
    }
    
    return(c(row1,col1))
    
  }
  Check_leader    <- function(VehID,lane,jamspacing=FALSE){
    
    sub_x    <-  Get_x(VehID)
    sub_sec  <-  Get_section(VehID)
    
    leader_candidate <- Vehicle$Veh_ID[which( (Vehicle$section == sub_sec) & (Vehicle$lane == lane) & (Vehicle$x >sub_x))]
    leader_candidate <- leader_candidate[which(leader_candidate != VehID)]
    leader_candidate <- sapply(leader_candidate,Get_x)
    
    if(!notexist(leader_candidate)){
      leader <- names(leader_candidate[which(leader_candidate == min(leader_candidate))])  
    }else{
      
      if(notexist(sub_sec) || sub_sec == 0){
      
        leader <- 0
        
      }else{
        
        leader <- Sec_LastVeh[where.am.i(lane,sub_sec)[1],where.am.i(lane,sub_sec)[2]+1]
       
        
        if(notexist(leader)){
          leader <- 0
        }
        
        if(leader == VehID){
          leader <- Get_leaderVeh(VehID)
        }
      }
    }
    
    if(leader != 0){
      if(jamspacing == TRUE){
          distance <- Get_Distance(leader,VehID)
        while(distance < Get_jamgap(VehID) + Get_length(leader)){
          leader <- Get_leaderVeh(leader)
          if(leader == 0){
            break
          }else{
            distance <- Get_Distance(leader,VehID)
          }
        }
        
      }
    }


    
    return(leader)
  }
  
  Check_follower  <- function(VehID,lane,jamspacing = FALSE){
    
    sub_x    <- Get_x(VehID)
    sub_sec  <- Get_section(VehID)
    
    
    follower_candidate <- Vehicle$Veh_ID[which( (Vehicle$section == sub_sec) & (Vehicle$lane == lane) & (Vehicle$x < sub_x)) ]
    follower_candidate <- sapply(follower_candidate,Get_x)
    
    if(!notexist(follower_candidate)){
      follower <- names(follower_candidate[which(follower_candidate == max(follower_candidate))])
      
    }else{
      
      
      # leader <- Sec_LastVeh[where.am.i(lane,sub_sec)[1],where.am.i(lane,sub_sec)[2]+1]
      
      if(notexist(sub_sec) || sub_sec == 0){
        
        follower <- 0
        
      }else{
        
        follower <- Sec_LeaderVeh[where.am.i(lane,sub_sec)[1],where.am.i(lane,sub_sec)[2]-1]
        
        
        
        
        if(notexist(follower)){
          follower <- 0
        }
        
        
        if(follower == VehID){
          follower <- Get_followerVeh(VehID)
        }
      }
      
      # if(notexist(sub_sec) || sub_sec == 0){
      #   
      #   follower <- 0
      #   
      # }else{
      #   
      #   follower <- Sec_LeaderVeh[lane,sub_sec]
      #   
      # }
      # 
      
      
    }
    
    if(length(follower) > 1){
      follower <- follower[1]
    }
    
    
    if(follower != 0){
      if(jamspacing == TRUE){
        distance<-Get_Distance(follower,VehID)
        Gap_crit <-Get_jamgap(follower) + Get_length(VehID)
        while(distance < Gap_crit){
          follower <- Get_followerVeh(follower)
          
          if(follower == 0){
            break
          }else{
            distance <- Get_Distance(follower,VehID)
            Gap_crit <-Get_jamgap(follower) + Get_length(VehID)
          }
          
        }
        
      }
    }
    
    return(follower)
  }

  
##
is.onramp <- function(sec){
  pre_sec  <- Get_PreviousSection(sec)
  next_sec <- Get_NextSection(sec)
  ramp     <- Get_Ramp(sec)
  
  onramp <- FALSE
  
  if(ramp == 1){
    if(pre_sec == 0){
      if(next_sec != 0){
        onramp <- TRUE
      }
    }
  }
  
  return(onramp)
}

is.offramp <- function(sec){
  pre_sec  <- Get_PreviousSection(sec)
  next_sec <- Get_NextSection(sec)
  ramp     <- Get_Ramp(sec)
  
  offramp <- FALSE
  
  if(ramp == 1){
    if(next_sec == 0){
      if(pre_sec != 0){
        offramp <- TRUE
      }
    }
  }
  
  return(offramp)
}


to.offramp <- function(VehID){
  to_offramp <- FALSE
  lane <- Get_lane(VehID)
  sec  <- Get_section(VehID)
  NoLane <- Get_NoLane(sec)
  
  if(lane == NoLane){
    next_ramp <- Section$SectionID[which(Section$PreviousSection == sec & Section$Ramp == 1)]
    to_offramp <- !notexist(next_ramp)
  }
  return(to_offramp)
}