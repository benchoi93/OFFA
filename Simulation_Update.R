

Update_SecInfo <- function(sec){
  
  NoLane <- Get_NoLane(sec)
  
  for(lane in 1:NoLane){
    
    veh <- Vehicle$Veh_ID[which((Vehicle$section == sec)&(Vehicle$lane == lane))]
    
    if(notexist(veh)){
      
      lead<-0
      last<-0
      
    }else{
      veh <- data.frame(VehID = veh, x = unlist(lapply(X = veh , FUN = Get_x)))
      veh$VehID <- as.character(veh$VehID)
      
      
      lead <- veh$VehID[which(veh$x == max(veh$x))]
      last <- veh$VehID[which(veh$x == min(veh$x))]
      
      if(notexist(lead)){
        lead <- 0
      }
      if(notexist(last)){
        last <- 0
      }
    }
    if(length(last) > 1){
      last <- last[1]
    }
    
    Sec_LastVeh[where.am.i(lane,sec)[1],where.am.i(lane,sec)[2]]   <<-  last
    Sec_LeaderVeh[where.am.i(lane,sec)[1],where.am.i(lane,sec)[2]] <<-  lead
  
  }
}

DModeChange <- function(VehID,t){
  DMode0 <- Get_DMode(VehID)
  DMode0 <- unlist(strsplit(DMode0,split=""))
  DMode  <- DMode0
#   
#   if(VehID == "999963"){
#     print(VehID)
#   }
#   
  
  if(DMode0[6] == 0){
    DMode <- "000000"
    Set_DMode( VehID , DMode )
    return()
  }
  
  ##BCF Mode###
  if(DMode0[3] == "1"){
    bcf <- 0
  }else{
    
#     if(Get_section(VehID) == 2 ){
#       print(1)
#     }
    
    bcf <- as.numeric(NeedBCF(VehID,t))
    
    
    
    if((bcf == 1) && (DMode0[1] == 0)){
      if(Get_lane(VehID) != Get_targetlane(VehID)){
        Set_LC_tryt(VehID,t)
        Coop_Request(VehID)
      }
    }
  }
  
  ##MCF Mode###
  mcf <- as.numeric(NeedMCF(VehID))
  # mcf<-0
  if((mcf == 1) && (DMode0[2] == 0 | Get_LC_tryt(VehID) == 0)){
    if(Get_lane(VehID) != Get_targetlane(VehID)){
      Set_LC_tryt(VehID,t)
    }
  }
  
  ##DCF Mode###
  dcf <- 0
  
  if( (DMode0[1] == "1") || (DMode0[2] == "1") || (DMode0[3] == "1")){
    if(Get_LC_tryt(VehID) != 0 ){
      
      lc_prep <- Get_LC_prep(VehID)
      
      cur_sec <- Get_section(VehID)
      div_type <- Get_DivergingType(cur_sec)
      
      if(div_type == 2){
        lc_prep <- 0
        
        temp <- as.numeric(NeedDCF(VehID))
        
        if(temp == 0){
          Set_LC_tryt(VehID,0)
          Set_targetlane(VehID,Get_lane(VehID))
          Coop_Deact(VehID)
          Set_LC(VehID,0)
        }
      }
      
      
      if( (t - Get_LC_tryt(VehID)) > lc_prep){
        dcf <- as.numeric(NeedDCF(VehID))
      }
      
    }
  }
  
  ##CCF Mode###
  ccf <- as.numeric(NeedCCF(VehID))
  
  ##ACF Mode###
  acf <- as.numeric(NeedACF(VehID))
  
  ##CF Mode###
  cf  <- 1
  
  DMode <- paste0( bcf , mcf , dcf , ccf , acf , cf)
  # DMode <- paste0(0,0,0,0,0,1)
  
  
  Set_DMode( VehID , DMode )
}

Get_Move_Leader <- function(VehID){
  DMode <- Get_DMode(VehID)
  DModeset <- unlist(strsplit(DMode,""))
  
  leader<-0
  
  #CF Mode
  if(DModeset[6] == "1"){
    leader <- Get_leaderVeh(VehID) 
  }
  
  #BCF Mode
  if(DModeset[1] == "1"){
    targetlane <- Get_targetlane(VehID)
    leader <- append(leader, Check_leader( VehID , targetlane ))
  }
  
  #MCF Mode
  if(DModeset[2] == "1"){
    sec <- Get_section(VehID)
    lane <- Get_lane(VehID)
    targetlane <- Get_MergingLane(sec)[which(Get_MergingLane(sec) != lane)]
    
    if(length(targetlane) == 1){
      if(targetlane != 0){
        if(lane != targetlane){
          merge_leader <- Check_leader( VehID , targetlane )
          
          if ( Get_class(merge_leader) == 0){
            merge_leader <- 0
          }
          
          # if( Get_v(merge_leader) < v_off){
          if( Get_v(VehID) < v_off){
              merge_leader <- 0
          }
          
          
          leader <- append(leader, merge_leader)
        }
      }
    }
  }
  
  #DCF Mode
  if(DModeset[3] == "1"){
    targetlane <- Get_targetlane(VehID)
    leader <- append(leader, Check_leader( VehID , targetlane ))
  }
  
  #CCF Mode
  if(DModeset[4] == "1"){
    
    coop_leader <- Get_Coop(VehID)
    # if( Get_v(coop_leader) < v_off){
    if( Get_v(VehID) < v_off){
        
      coop_leader <- 0
    }
    
    leader <- append(leader,  coop_leader)
  }
  
  
  return(leader)
}

Move_Veh_x  <- function(VehID){
  x <- Get_x(VehID)
  org.x <- x
  
  
  leader <- Get_Move_Leader(VehID)

  if(notexist(leader)){
    break
  }
  
  x_set  <- c()
  
  for(i in 1:length(leader)){
    x_set  <-  append(x_set,cf_x(VehID,leader[i]))
  }
  x <- min(x_set)
  
  v <- (x - org.x)/dt
  
  Set_v(VehID,v)
  
  sec <- Get_section(VehID)
  sec_length <- Get_SecLength(sec)
  
  if(x > sec_length){
    next_x <- (x-sec_length)
    
    if(is.onramp(sec)){
      lane0 <- Get_lane(VehID)
      # sec0  <- Get_section(VehID)
      
      next_sec <- config[where.am.i(lane = lane0 , sec = sec)[1],where.am.i(lane = lane0 , sec = sec)[2] + 1]
      next_lane <- where.am.i(lane = lane0 , sec = sec)[1]
      
      # next_sec <- Get_NextSection(sec)
      # next_lane <- Get_NoLane(next_sec)
      hi <- TRUE
    }else if(to.offramp(VehID)){
      next_sec <- Section$SectionID[which(Section$PreviousSection == sec & Section$Ramp == 1)]
      next_lane <- 1
      hi <- TRUE
    }else{
      next_sec <- Get_NextSection(sec)
      next_lane <- Get_lane(VehID)
      hi <- FALSE
    }
    
    
    div_type <- Get_DivergingType(next_sec)
    if(!notexist(div_type)){
      if(div_type == 2 ){
        DMode <- strsplit(Get_DMode(VehID),"")
        if(DMode[[1]][3] != 1){
          NoLane <- Get_NoLane(next_sec)
          
          next_lane <- min(next_lane + 1, NoLane)
          
          veh.class <- Get_class(VehID)
          
          if(veh.class == 1){
            cur_lane <- Get_lane(VehID)
            
            if(cur_lane == 1){
              
              if(runif(1) < pass_rate){
                next_lane <- 1 
                diverge <<- rbind(diverge , c(VehID = VehID , diverge = 1 ))
                
              }else{
                diverge <<- rbind(diverge , c(VehID = VehID , diverge = 2 ))
              }
              
            }
            
          }
          
          Set_targetlane(VehID,next_lane)
          
        }
      }
    }
    
    
    
    ##save data
    
    if(next_sec %in% v_check_sec){
      temp <- data.frame(time = t,vehID=VehID, speed = Get_v(VehID) , lane = Get_lane(VehID))
      
      assign(   paste0("speed_profile",next_sec)  ,    
                rbind(   get(paste0("speed_profile",next_sec))  ,temp  )    ,
                envir = .GlobalEnv)
    }
    
    
    if(next_sec == 2 && sec == 1 ){
      arrival$veh[t] <<- arrival$veh[t]+1
    }
    
    if(next_sec == 5 && sec == 4){
      departure$veh[t] <<- departure$veh[t]+1
    }
    
    Set_section(VehID,next_sec)
    Set_lane(VehID,next_lane)
    Set_y(VehID,next_lane)
    Set_x(VehID,next_x)
    
    if(Get_targetlane(VehID) > Get_NoLane(next_sec)){
      Set_targetlane(VehID,Get_lane(VehID))
    }
    
    if(hi){
        Set_targetlane(VehID,Get_lane(VehID))
        Set_LC(VehID,0)
        Set_LC_tryt(VehID,0)
        Set_Coop(VehID,0)
      }
    
  }else{
    Set_x(VehID,x)
  }
  
}

Move_Veh_y  <- function(VehID){
  DMode <- Get_DMode(VehID)
  y     <- Get_y(VehID)
  lane  <- Get_lane(VehID)
  targetlane <- Get_targetlane(VehID)
  DModeset <- unlist(strsplit(DMode,""))
  
  
  if(DModeset[3] == "1"){
    if(lane != targetlane){
      y <- y + (targetlane-lane) * v_y
      
      if( round(y - targetlane,5) == 0 ){
        Set_y(VehID,targetlane)
        
        ##save data
        LC_history <<- rbind(LC_history, c(time =t , VehID = VehID))
        
        
        Set_lane(VehID,targetlane)

        Set_LC(VehID,t)
        Coop_Deact(VehID)
        Set_LC_tryt(VehID, 0)
        
        org.leader   <- as.character( Get_leaderVeh(VehID)              )
        org.follower <- as.character( Get_followerVeh(VehID)            )
        new.leader   <- as.character( Check_leader(VehID,targetlane)    )
        new.follower <- as.character( Check_follower(VehID,targetlane)  )
        
        lf_update_set <- c(org.leader,org.follower,new.leader,new.follower,VehID)
        for(i in 1:length(lf_update_set)){
          if( lf_update_set[i] != "0"){
            LF_update(lf_update_set[i])
          }
        }
      }else{
        Set_y(VehID,y)
      }
    }else{
      Coop_Deact(VehID)
      Set_LC_tryt(VehID, 0)
    }
  }

    
}

LF_update <-function(VehID){
  lane <- Get_lane(VehID)
  
  leader   <- Check_leader(VehID,lane)
  follower <- Check_follower(VehID,lane)
  
  Set_leaderVeh(VehID,leader)
  Set_followerVeh(VehID,follower)
}

get_avg_speed <- function( VehID , lane){
  if( notexist( lane )){
    return( 0 )
  }
  
  sec <- Get_section(VehID)
  NoLane <- Get_NoLane(sec)
  
  if( lane < 1 | lane > NoLane){
    return( 0 ) 
  }
  
  x <- Get_x(VehID) 
  
  
  leader <- Check_leader(VehID,lane)
  front_veh <- c(leader)
  distance <- Get_Distance(leader,VehID)
  
  while(distance < visible_distance){
    leader <- Get_leaderVeh(leader)
    distance <- Get_Distance(leader,VehID)
    if(distance < visible_distance){
      front_veh <- append(front_veh,leader)
    }
  }
  
  v_set  <-  sapply(front_veh,Get_v)
  
  avg_v <- mean(v_set)
  
  return(avg_v)
}

Get_LC_probability <- function(cur_speed, targetlane_speed, v_ff){
  pLC <- (targetlane_speed - cur_speed )/v_ff
  return(pLC)
}



LCChoice_control <-function(VehID,t){
  macro_t <- ceiling( (t %% 3000) * dt /macro_dt)
  
  if(macro_t == 0){
    macro_t <- 1
  }
  
  SecNumCell <- MacroSectionLength/SectionLength
  
  cur_sec <- Get_section(VehID)
  MacroSec <- ceiling(cur_sec / SecNumCell)
  
  cur_lane <- Get_lane(VehID)
  
  targetlane <- cur_lane
  lc <- FALSE
  
  if(cur_lane == 1){
    
    nlc_temp <- nlc[c(1,2),MacroSec,macro_t]
    
    if(nlc_temp[1] > 0){
      
      targetlane <- 2
      nlc[1,MacroSec , macro_t ] <<- nlc[1,MacroSec , macro_t ] - 1
      
    }else if(nlc_temp[2] > 0){
      
      targetlane <- 3
      nlc[2,MacroSec , macro_t ] <<- nlc[1,MacroSec , macro_t ] - 1
      
    }
    
  }else if(cur_lane == 2){
    
    nlc_temp <- nlc[c(3,4),MacroSec,macro_t]
    
    if(nlc_temp[1] > 0){
      
      targetlane <- 1
      nlc[3,MacroSec , macro_t ] <<- nlc[3,MacroSec , macro_t ] - 1
      
    }else if(nlc_temp[2] > 0){
      
      targetlane <- 3
      nlc[4,MacroSec , macro_t ] <<- nlc[4,MacroSec , macro_t ] - 1
      
    }
    
  }else if(cur_lane == 3){
    
    nlc_temp <- nlc[c(5,6),MacroSec,macro_t]
    
    if(nlc_temp[1] > 0){
      
      targetlane <- 2
      nlc[5,MacroSec , macro_t ] <<- nlc[5,MacroSec , macro_t ] - 1
      
    }else if(nlc_temp[2] > 0){
      
      targetlane <- 3
      nlc[6,MacroSec , macro_t ] <<- nlc[6,MacroSec , macro_t ] - 1
      
    }
    
  }
  
  # nlc[,7,macro_t]
  
  
  
  if(targetlane != cur_lane){
    lc <- TRUE
    }
    
  return(c(lc,targetlane))
  
  
}


LCChoice  <- function(VehID){
  lc <- FALSE
  
  cur_sec <- Get_section(VehID)
  NoLane <- Get_NoLane(cur_sec)
  
  ramp <- Get_Ramp(cur_sec)
  cur_lane <- Get_lane(VehID)
  LCT <- Get_LC(VehID)
  
  no_lc <- ((ramp != 0) | (t-LCT < LC_block) | (cur_sec == 1))
  
  if((t-LCT < LC_block)){
    Set_LC(VehID,0)
  }
  
  if(no_lc){
    return(c(FALSE , cur_lane))
  }
  
  
  div_type <- Get_DivergingType(cur_sec)
  
  if(!notexist(div_type)){
    if(div_type == 1){
      
      veh_class <- Get_class(VehID)
      if(veh_class == 2){
        
        
        if(!(VehID %in% diverge$VehID)){
          if( runif(1) < comp_rate ){
            diverge <<- rbind(diverge , c(VehID = VehID , diverge = NoLane))
          }else{
            diverge <<- rbind(diverge , c(VehID = VehID , diverge = cur_lane))
          }
        }
        
        target <- as.numeric(diverge$diverge[which(diverge$VehID == VehID)])
        
        if(target != cur_lane){
          move_to <- (target - cur_lane) / abs(target - cur_lane)
          targetlane <- cur_lane + move_to
          
          return(c(TRUE, targetlane))
        }
        
      }
    }
#     }else if(div_type == 2){
#       
#       
#       if(!(VehID %in% diverge$VehID)){
#         
#         veh_class <- Get_class(VehID)
#         
#         if(veh_class == 1){
#           
#           if( runif(1) < pass_rate ){
#             diverge <<- rbind(diverge , c(VehID = VehID , diverge = max(1,cur_lane - 1) ))
#           }else{
#             diverge <<- rbind(diverge , c(VehID = VehID , diverge = cur_lane))
#           }
#           
#         }else 
#           if(veh_class == 2){
#           
#           diverge <<- rbind(diverge , c(VehID = VehID , diverge = cur_lane))
#           
#         }
#       }
#       
#       target <- as.numeric(diverge$diverge[which(diverge$VehID == VehID)])
#       
#       if(notexist(target)){
#         target <- cur_lane
#       }
#       
#       
#       if(target != cur_lane){
#         move_to <- (target - cur_lane) / abs(target - cur_lane)
#         targetlane <- cur_lane + move_to
#         
#         return(c(TRUE, targetlane))
#       }
#       
#     }
  }
  
  if( div_type != 0 ){
    return(c(FALSE,cur_lane))
  }
  
  right_lane <- cur_lane + 1
  left_lane  <- cur_lane - 1
  
  right_speed <- get_avg_speed( VehID , right_lane )
  left_speed  <- get_avg_speed( VehID , left_lane  )
  cur_speed   <- get_avg_speed( VehID , cur_lane   )
  
  v_ff <- Get_vf(VehID)
  
  p_LC_right <- round(Get_LC_probability(cur_speed, right_speed , v_ff), 4) * sstvt
  p_LC_left  <- round(Get_LC_probability(cur_speed, left_speed  , v_ff), 4) * sstvt
  
  class <- Get_class(VehID)
  if(class == 1){
    p_LC_left <- p_LC_left * left_prefer
    
  }else if(class == 2){
    p_LC_right <- p_LC_right * right_prefer
  }
  
  
  sub.veh.class    <-   Get_class(VehID)
  leader.veh.class <-   Get_class(Get_leaderVeh(VehID))
  
  if(sub.veh.class == 1){
    if(leader.veh.class == 2){
      p_LC_left  <- p_LC_left  * avoid_truck
      # p_LC_right <- p_LC_right * avoid_truck
    }
  }
  
  
  sum_p_LC <- sum(p_LC_right , p_LC_left)
  if(sum_p_LC < 1){
    sum_p_LC <- 1
  }
  
  p_left  <- p_LC_left  / sum_p_LC
  p_right <- p_LC_right / sum_p_LC
  p_cur <- 1 - p_left - p_right
  
  prob <- runif(1)
  if(prob < p_left){
    targetlane <- left_lane
  }else if(prob < p_left + p_right){
    targetlane <- right_lane
  }else{
    targetlane <- cur_lane
  }
  
  # 
  # p_LC <- max(0,p_right,p_left)
  # 
  # if(p_LC == 0){
  #   targetlane <- cur_lane
  # }else if(p_LC == p_LC_right){
  #   targetlane <- right_lane
  # }else if(p_LC == p_LC_left){
  #   targetlane <- left_lane
  # }else{
  #   targetlane <- cur_lane
  # }
  
  
  
  # if(targetlane != cur_lane){
  # 
  #   
  #   if(p_LC != 0){
  #     if(runif(1) > p_LC){
  #       targetlane <- cur_lane
  #     }
  #     
  #   }
  #   
  # }
  

  
  if(targetlane != cur_lane){
    lc <- TRUE
    
    if(VehID %in% diverge$VehID){
      diverge$diverge[which(diverge$VehID == VehID)] <<- targetlane
    }
    

    
  }
  
  return(c(lc,targetlane))
  
}


get_w <-function(k1, v1, k2, v2 ){
  
  q1 = k1 * v1
  q2 = k2 * v2
  
  w <- (q1-q2)/(k1-k2)
  
  return(c(q1,w))
  
}



get_data <- function( sec0,  x0 , lane0 ,  distance , data = "speed" , front = TRUE ){
  
  if(front){
    temp     <- Vehicle[which(Vehicle$section == sec0 & Vehicle$lane == lane0 & Vehicle$x >= x0) , ]
    minVehID <- temp$Veh_ID[which(temp$x == min(temp$x))]
    
    temp_sec0   <- sec0
    next_sec0 <- Get_NextSection(sec0)
    length <- Get_SecLength(sec0)
    
    while(notexist(minVehID)){
    
      if(notexist(next_sec0) | next_sec0 == 0){
        result <- 0
        return(result)
      }
      
      minVehID <- Sec_LastVeh[ lane0 ,next_sec0]
      length   <- length + Get_SecLength(next_sec0)
      next_sec0 <- Get_NextSection(next_sec0)
      
      if(notexist(minVehID)){
        
        minVehID <- 0
      }
      
      if(minVehID == 0 ){
        
        minVehID <- NA
        
      }
      
    }
    
    veh_x <- Get_x(minVehID)
    
    cur_dist <- veh_x - x0
    
    if(cur_dist < 0){
      result <- 0
      return(result)
    }
    
    
    veh <- minVehID
    data_veh <- c()
    
    while(cur_dist < distance){
      data_veh <- append(data_veh , veh)
      
      
      leaderID <- Get_leaderVeh(veh)
      delx <- Get_Distance(leaderID , veh)
      cur_dist <- cur_dist + delx
      veh  <- leaderID
      
    }
  }else{
    
    temp <- Vehicle[which(Vehicle$section == sec0 & Vehicle$lane == lane0 & Vehicle$x <= x0) ,]
    maxVehID <- temp$Veh_ID[which(temp$x == max(temp$x))]
    
    temp_sec0 <- sec0
    prev_sec0 <- Get_PreviousSection(sec0)
    length    <- Get_SecLength(sec0)
    
    while(notexist(maxVehID)){
      
      if(notexist(prev_sec0) | prev_sec0 == 0){
        result <- 0
        return(result)
      }
      
      maxVehID <- Sec_LeaderVeh[ lane0 ,prev_sec0]
      prev_sec0 <- Get_PreviousSection(prev_sec0)
      
      if(notexist(maxVehID)){
        
        maxVehID <- 0
      }
      
      if(maxVehID == 0 ){
        
        maxVehID <- NA
        
      }
    }
    
    veh_x <- Get_x(maxVehID)
    cur_dist <- x0 - veh_x
    
    if(cur_dist < 0){
      result <- 0
      return(result)
    }
    
    tmp <- 0
    veh <- maxVehID
    data_veh <- c()
    
    while(cur_dist < distance){
      
      data_veh <- append(data_veh , veh)
      
      
      folID <- Get_followerVeh(veh)
      delx <- Get_Distance(folID , veh)
      cur_dist <- cur_dist + delx
      veh  <- folID
    }
    
  }
  
  
  
  
  
  if ( notexist(data_veh) ) {
    result <- 0
  }else{
    
    if(data == "speed"){
      
      result <- sapply(data_veh , Get_v)
      result <- mean(result)
      
    }else if( data == "density"){
      
      result <- length(data_veh)
      result <- result / (distance * 0.001)
      
    }else{
      result <- 0
    }
    
  }
  
  
  return( result )
  
}




LCChoice_AV_ind <- function(VehID){
  
  cur_sec  <- Get_section(VehID)
  cur_lane <- Get_lane(VehID)
  cur_x    <- Get_x(VehID)
  
  cur_NoLane <- Get_NoLane(cur_sec)
  
  need <- FALSE
  targetlane <- cur_lane
  
  next_sec <- Get_NextSection(cur_sec)
  
  right_lane <- cur_lane + 1
  left_lane  <- cur_lane - 1
  
  # cur_lanes <- as.matrix(c(right_lane , cur_lane , left_lane))
  # cur_lanes <- as.vector(apply(X = cur_lanes , MARGIN = 2 , FUN = function(x) return(cur_lanes[which(cur_lanes %in% 1:cur_NoLane)]) ))
  
  speed_data    <- matrix(0 , nrow = 3 , ncol = 2)
  density_data  <- matrix(0 , nrow = 3 , ncol = 2)
  
  ############# data fill in ################
  speed_data[2,1]   <- get_data( sec0 = cur_sec , x0 = cur_x , lane0 = cur_lane , distance = distance0, data = "speed",front = F)
  speed_data[2,2]   <- get_data( sec0 = cur_sec , x0 = cur_x , lane0 = cur_lane , distance = distance0, data = "speed",front = T)
  density_data[2,1] <- get_data( sec0 = cur_sec , x0 = cur_x , lane0 = cur_lane , distance = distance0, data = "density",front = F)
  density_data[2,2] <- get_data( sec0 = cur_sec , x0 = cur_x , lane0 = cur_lane , distance = distance0, data = "density",front = T)
  
  
  speed_data[3,1]   <- get_data( sec0 = cur_sec , x0 = cur_x , lane0 = right_lane , distance = distance0, data = "speed",front = F)
  speed_data[3,2]   <- get_data( sec0 = cur_sec , x0 = cur_x , lane0 = right_lane , distance = distance0, data = "speed",front = T)
  density_data[3,1] <- get_data( sec0 = cur_sec , x0 = cur_x , lane0 = right_lane , distance = distance0, data = "density",front = F)
  density_data[3,2] <- get_data( sec0 = cur_sec , x0 = cur_x , lane0 = right_lane , distance = distance0, data = "density",front = T)
  
  
  speed_data[1,1]   <- get_data( sec0 = cur_sec , x0 = cur_x , lane0 = left_lane , distance = distance0, data = "speed",front = F)
  speed_data[1,2]   <- get_data( sec0 = cur_sec , x0 = cur_x , lane0 = left_lane , distance = distance0, data = "speed",front = T)
  density_data[1,1] <- get_data( sec0 = cur_sec , x0 = cur_x , lane0 = left_lane , distance = distance0, data = "density",front = F)
  density_data[1,2] <- get_data( sec0 = cur_sec , x0 = cur_x , lane0 = left_lane , distance = distance0, data = "density",front = T)
  ############## code it ####################
  
  v_sub <- Get_v(VehID)
  #################################################################
  ######################keep current lane##########################
  #################################################################
  
  flow_data <- speed_data * 3.6 * density_data
  total_flow_cur <- sum(flow_data[,1])
  
  Wcur <- 0
  for(l in 1:3){
    Wcur <- Wcur + (flow_data[l,1]/total_flow_cur)*(flow_data[l,1] - flow_data[l,2])/(density_data[l,1] - density_data[l,2])
  }
  
  #################################################################
  ######################change to the left#########################
  #################################################################
  speed_data_temp <- speed_data
  L <- Get_SecLength(cur_sec) * 0.001
  
  speed_data_temp[1,1] <- speed_data[1,1] * (density_data[1,1] * L + 1 ) / (density_data[1,1] * L + speed_data[1,1]/v_sub )
  speed_data_temp[2,1] <- speed_data[2,1] * (density_data[2,1] * L - 1 ) / (density_data[2,1] * L - speed_data[2,1]/v_sub )
  
  density_data_temp <- density_data
  density_data_temp[1,1] <- density_data[1,1] + 1/L
  density_data_temp[2,1] <- density_data[2,1] - 1/L
  
  flow_data_temp <- speed_data_temp * 3.6 * density_data_temp
  total_flow_cur <- sum(flow_data_temp[,1])
  
  
  Wleft <- 0
  for(l in 1:3){
    Wleft <- Wleft + (flow_data_temp[l,1]/total_flow_cur)*(flow_data_temp[l,1] - flow_data_temp[l,2])/(density_data_temp[l,1] - density_data_temp[l,2])
  }
  #################################################################
  ######################change to the right########################
  #################################################################
  speed_data_temp <- speed_data
  L <- Get_SecLength(cur_sec) * 0.001
  
  speed_data_temp[2,1] <- speed_data[2,1] * (density_data[2,1] * L - 1 ) / (density_data[2,1] * L - speed_data[2,1]/v_sub )
  speed_data_temp[3,1] <- speed_data[3,1] * (density_data[3,1] * L + 1 ) / (density_data[3,1] * L + speed_data[1,1]/v_sub )
  
  density_data_temp <- density_data
  density_data_temp[2,1] <- density_data[2,1] - 1/L
  density_data_temp[3,1] <- density_data[3,1] + 1/L
  
  flow_data_temp <- speed_data_temp * 3.6 * density_data_temp
  total_flow_cur <- sum(flow_data_temp[,1])
  
  Wright <- 0
  for(l in 1:3){
    Wright <- Wright + (flow_data_temp[l,1]/total_flow_cur)*(flow_data_temp[l,1] - flow_data_temp[l,2])/(density_data_temp[l,1] - density_data_temp[l,2])
  }
  
  if(notexist(Wright)){
    Wright <- -Inf
  }
  
  if(notexist(Wleft)){
    Wleft <- -Inf
  }
  
  if(notexist(Wcur)){
    Wcur <- 0
  }

  if(Wright == Inf){
    Wright = 200
  }
  if(Wleft == Inf){
    Wleft = 200
  }
  if(Wcur == Inf){
    Wcur = 200
  }
  
  dino   <- exp(Wcur) + exp(Wleft) + exp(Wright)
  if(dino == 0 | dino == Inf){
    pcur   <- 1
    pleft  <- 0
    pright <- 0
  }else{
    pleft  <- exp(Wleft)  /  dino
    pright <- exp(Wright) /  dino
    pcur   <- 1 - pleft - pright
    
  }
  
  
  templc <<- rbind(templc , c(cur_sec , cur_lane , pleft, pright, pcur))
  print(c(cur_sec , cur_lane , pleft, pright, pcur))
  prob <- runif(1)
  
  if(prob < pleft){
    targetlane <- cur_lane - 1
  }else if(prob < pleft + pcur){
    targetlane <- cur_lane
  }else if( prob < pleft + pcur + pright){
    targetlane <- cur_lane + 1
  }else{
    targetlane <- cur_lane
  }
  
  
  
  NoLane <- Get_NoLane(cur_sec)
  if(!(targetlane %in% 1:NoLane) ){
    
    targetlane <- cur_lane
    
  }
  
  
  
  
  
  
  if(cur_lane != targetlane){
    need <- TRUE
  }
  
  return(c(need,targetlane))
  
} 


LCChoice_AV <- function(VehID){
  cur_sec <- Get_section(VehID)
  cur_lane <- Get_lane(VehID)
  
  need <- FALSE
  targetlane <-cur_lane
  
  next_sec <- Get_NextSection(cur_sec)
  
  if(notexist(next_sec) || next_sec == 0){
    return(c(need,targetlane))
  }
  
  right_lane <- cur_lane + 1
  left_lane  <- cur_lane - 1 
  
  check <- c(cur_lane)
  
  if((right_lane %in% 1:NoLane)){
    check <- append(check, right_lane)
  }
  
  if((left_lane %in% 1:NoLane)){
    check <- append(check, left_lane)
  }
  

  
  next_veh <- Vehicle[which(Vehicle$section == next_sec),]
  cur_veh  <- Vehicle[which(Vehicle$section == cur_sec & Vehicle$Veh_ID != VehID),]
  sub_veh  <- Vehicle[which(Vehicle$Veh_ID == VehID),]
  

#   if(length(unique(next_veh$lane)) == 3 ){
#     print(1)
#   }  

  result <- c()
  
  for ( sub_lane in check){
    for(i in 1:length(check)){
      temp <- next_veh$v[next_veh$lane == check[i] ]
      assign(paste0("next_veh",i), temp)
      
      temp2 <- cur_veh$v[cur_veh$lane == check[i]]
      if(check[i] == sub_lane){
        temp2 <- cbind(temp2, sub_veh$v)
      }
      assign(paste0("cur_veh",i),temp2)
    }
    
    total_wave <-0
    
    for(i in 1:length(check)){
      
      cur_sec_l  <- Get_SecLength(cur_sec)
      next_sec_l <- Get_SecLength(next_sec)
      
      next_n <- length(get(paste0("next_veh",i)))
      cur_n  <- length(get(paste0("cur_veh",i)))
      
      next_v <- mean(get(paste0("next_veh",i)))
      cur_v  <- mean(get(paste0("cur_veh",i)))
      
      if(notexist(next_v)){
        next_v <- 35
      }
      if(notexist(cur_v)){
        cur_v <- 35
      }
      
      
      assign(paste0("next_k",i),next_n/next_sec_l)
      assign(paste0("next_q",i),next_n/next_sec_l*next_v)
      
      assign(paste0("cur_k",i),cur_n/cur_sec_l)
      assign(paste0("cur_q",i),cur_n/cur_sec_l*cur_v)
      
      # get(paste0("next_k",i)),get(paste0("next_q",i)),get(paste0("cur_k",i)),get(paste0("cur_q",i))
      
      
      num  <- get(paste0("next_q",i)) - get(paste0("cur_q",i))
      dino <- get(paste0("next_k",i)) - get(paste0("cur_k",i))
      
      if(dino == 0){
        w <- 35
      }else{
        w <- num/dino
      }
      total_wave <- total_wave + w
    }
    temp <- c(sub_lane=sub_lane,check = i,wave = total_wave)
    result <- rbind(result,temp)
  }
  
  result <- as.data.frame(result)
  targetlane <- result$sub_lane[which(result$wave == max(result$wave))]
  
  if(cur_lane != targetlane){
    need <- TRUE
  }

  return(c(need,targetlane))
  
}

NeedBCF  <- function(VehID,t){
  lane <- Get_lane(VehID)
  targetlane <- Get_targetlane(VehID)
  
#   cur_sec <- Get_section(VehID)
#   if(cur_sec == 2){
#     print(1)
#   }

  
  if(lane == targetlane){
    
    cur_x <- Get_x(VehID)
    
    
    # if(control){
    #   LC <- LCChoice_control(VehID,t)
    # }else{
    #   LC <- LCChoice(VehID)
    # }
    
    
    LC <- LCChoice(VehID)
    # LC <- LCChoice_AV_ind(VehID)
    
    need <- LC[1]
    
    if(need){
      targetlane <- LC[2]
      Set_targetlane(VehID,targetlane)
    }
    
  }else{
    LCT <- Get_LC_tryt(VehID)
    
    if(t-LCT > LC_reset){
      
      cur_sec <- Get_section(VehID)
      div_type <- Get_DivergingType(cur_sec)
      
      if(div_type == 0){
        need <- FALSE
        
        Set_targetlane(VehID,lane)
        Set_LC_tryt(VehID,0)
        Coop_Deact(VehID)
        
      }
    }
    
    need <- TRUE
    
  }
  
  return(need)
}

NeedDCF  <- function(VehID){
  need <- FALSE
  
  targetlane <- Get_targetlane(VehID)
  
  need <- Gap_acceptible(VehID , targetlane)
  
  return(need)
}

NeedACF  <- function(VehID){
  need <- FALSE
  return(need)
}

NeedMCF  <- function(VehID){
  cur_sec <- Get_section(VehID)
  cur_lane <- Get_lane(VehID)
  
  MergeLanes <- Get_MergingLane(cur_sec)
  
  need <- (cur_lane %in% MergeLanes)
  
  if(need){
  
    if(cur_lane == MergeLanes[2]){
      targetlane <- MergeLanes[1]
      Set_targetlane(VehID,targetlane)
    }
    
  }
  
  return(need)
}

NeedCCF  <- function(VehID){
  need <- (Get_Coop(VehID) != 0)
  return(need)
}

Get_MergingLane <- function(sec){
  MergeType <- Get_MergingType(sec)
  
  if(MergeType == 0){
    return(c(0,0))
  }else if(MergeType == 1 ){
    NoLane <- Get_NoLane(sec)
    main <- NoLane - 1
    aux  <- NoLane
    return(c(main,aux))
  }else{
    return(c(0,0))
  }

}

Coop_Request  <- function(VehID){
  targetlane <- Get_targetlane(VehID)
  coop_veh <- Check_follower(VehID,targetlane,jamspacing = TRUE)
  
  cur_sec <- Get_section(VehID)
  cur_lane <- Get_lane(VehID)
  
  
  DMode <- strsplit(Get_DMode(VehID),"")
  if(DMode[[1]][3] != 1 ){
    if(Get_DivergingType(cur_sec) == 2){
      no_coop <- Sec_LeaderVeh[  where.am.i(cur_lane,cur_sec)[1]  ,  where.am.i(cur_lane,cur_sec)[2]-1  ]
      if(!notexist(no_coop)){
        if(no_coop == coop_veh){
          coop_veh <- 0
        }
      }
    }

  }
  
  
  
  
  Set_Coop(coop_veh,VehID)
}

Coop_Deact  <- function(VehID){
  coop_veh <- Vehicle$Veh_ID[which(Vehicle$Coop == VehID)]
  
  for(i in 1:length(coop_veh)){
    Set_Coop(coop_veh[i],0)
  }
}

Gap_check <- function(VehID1,VehID2){
  gap <- FALSE
  
  #Vehicle 1 must be the vehicle in front and Vehicle 2 must be the vehicle at behind
  
  #Veh1
  v1      <- Get_v(VehID1)
  length1 <- Get_length(VehID1)
  dec1    <- Get_dec(VehID1)
  
  #Veh2
  v2      <- Get_v(VehID2)
  tau2    <- Get_tau(VehID2)
  jamgap2 <- Get_jamgap(VehID2)
  dec2    <- Get_dec(VehID2)
  
  Gap_crit <- jamgap2 + length1
  cur_gap <- Get_Distance(VehID1,VehID2)
  if(Gap_crit >= cur_gap){
    gap <- FALSE
    return(gap)
  }
  
  Gap_dec <- (cur_gap 
              - ((v1^2) / (2*dec1)) 
              - v2 * tau2 
              + ((v2^2) / (2*dec2))
              
              )
  
  gap <- (Gap_crit < Gap_dec)
  return( gap )
}

Gap_acceptible <- function(VehID,targetlane){
  leader   <- Check_leader( VehID , targetlane )
  follower <- Check_follower( VehID , targetlane )
  
  if(notexist(leader) || leader == 0){
    lead_gap <- TRUE
  }else{
    lead_gap <- Gap_check( leader , VehID )
  }
  
  
  if(notexist(follower) || follower == 0){
    follow_gap <- TRUE
  }else{
    follow_gap <- Gap_check( VehID , follower )
  }
  
  return( ( lead_gap & follow_gap ) )
}






Sim_Update <- function(t){
  
  VehID_sort <- Vehicle$Veh_ID
  
  for(VehID in VehID_sort){
    DModeChange( VehID ,t)
    Move_Veh_x( VehID )
    Move_Veh_y( VehID )
  }
  
  for(sec in Section$SectionID){
    Update_SecInfo( sec )
  }
  
  delete_veh <- Vehicle$Veh_ID[which(Vehicle$section == 0)]
  if(length(delete_veh) != 0){
    for(i in 1: length(delete_veh)){
      Coop_Deact(delete_veh[i])
#       departure$veh[t] <<- departure$veh[t] + 1
    }
    
    Vehicle <<- Vehicle[-which(Vehicle$Veh_ID %in% delete_veh),]
    VehicleAtt <<- VehicleAtt[-which(VehicleAtt$Veh_ID %in% delete_veh),]
  }
  
  for(VehID in Vehicle$Veh_ID){
    LF_update(VehID)
  }
  
  
  
}
