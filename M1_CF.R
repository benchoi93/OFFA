# M1_CF Model
# Car-follwoing model 
#
# Car-follwoing mode: 
# CF, BCF, DCF, ACF, MCF (Merging), CCF
# basic cf model: cf0


#----------------------------------------------------------------------
# base car-following function returning the x location of the follwoing vehicle for time t+1

cf0_x <- function(x0, v0, x1, dec, acc, v_free, jamgap, dt, length_v1, tau, SGMode,VehID,dec1,v1){
#   RESULT: x, subject vehicle location at next time step

#   INPUT
#   x0: subject vehicle location
#   v0: subject vehicle speed
#   x1: leader vehicle location
#   dec: subject vehicle maximum deceleartion
#   acc: subject vehicle maximum acceleration
#   v_free: subject vehicle freeflow speed
#   jamgap: subject vehicle jam gap
#   length_v1: leader vehicle length
#   tau: subject vehicle reaction time
#   SGMode: whether D-mode or not during LC
#   vehID: subject vehicle ID
#   dec1: leader vehicle maximum deceleration
#   v1: leader vehicle speed
  

  
  #Grade Consideration
  cur_sec <- Get_section(VehID)
  
  ramp <- Get_Ramp(cur_sec)
  if(ramp != 0 ){
    v_free <- v_free*0.6
  }
  
  grade <- 0.01 * Get_Grade(cur_sec)
  g   <- 9.8 ## gravitational acceleration (m/s^2)
  a_vf <- 0
  #vehicletype = 1 : normal auto
  #vehicletype = 0 : truck
  vehicletype <- strsplit(VehID,split="")[[1]][2]  ## vehicle type ###  ex) 3210011 = 1/2/1/0011 = 3rd time section, lane 2, normal auto, 11th vehicle
  
  if(vehicletype == 0){
    acc  <-  acc - grade*g
    a_vf <-  -grade*g
    dec  <-  dec + grade*g
  }
  #Grade Consideration End
  
  
  #Deceleration mode during LC, shorter headway accepted
  if (SGMode==1){
    jamgap<-jamgap*0.7
    tau<-tau*0.7    
  }
  #Deceleration mode during LC, shorter headway accepted End
  
  
  x<-x0
  
  x_p<-x1-v1*tau 
  #Newell distance
  x_n <- x_p - length_v1-jamgap
  # Max Acc distance 
  x_acc <-x0+ v0*dt + acc*(dt^2)
  
  # free flow distance 
  x_ff <- x0 + v_free*dt + a_vf*(dt^2)
  
  #safety distance
  if(dec1 == 0){
    d1 <-0
  }else{
    d1 <- (-(v1^2/(2*dec1)))
    
  }
  #d1 <- (v1^2/(2*dec1))
  D <- (dec*tau)^2 - 2*dec*(x1-x0-(length_v1+jamgap)+ d1)
  if(D<0){
    D <-0 
  }
  
  dx_safe <-dt*(dec*tau+   sqrt(D)  )
    
  x_safe <-x0 +dx_safe
  
  
  #maximum decceleration x
  x_maxd <- max(x0, x0 + v0*dt + dec*(dt^2))
  

  
  #car-follwoing distance 

  x<- max(   min(x_n, x_acc, x_ff,x_safe)   ,   x_maxd   )
  
#   if(Get_leaderVeh(VehID) != 0){
#     print(VehID)
#   }
  

  return (x)
}




#----------------------------------------------------------------------
# Car-follwing for the Normal CF Mode 
# SGMode =0( Normal), 1=Short Gap Mode

cf_x <- function(VehID,leaderID,SGMode=0){
#   INPUT: subject vehicle ID, leader vehicle ID, SGMode
#   Create input for function cf0_x:
#   OUTPUT: subject vehicle location x
  
  #Retrieve information from Vehicle
  x0 <- Get_x(VehID)    #location value (0, length(EACH section, not entire simulation length)]
  v0 <- Get_v(VehID)
  
  #Retrieve information from VehicleAtt
  dec    <- Get_dec(VehID)
  acc    <- Get_acc(VehID)
  v_free <- Get_vf(VehID)
  jamgap <- Get_jamgap(VehID)
  tau    <- Get_tau(VehID)
  
  #No leader vehicle
  if(leaderID==0){
    x1         <- 100000000
    length_v1  <- 0
    dec1       <- -5
    v1         <- Get_vf(leaderID)
    sec1  <-  Get_section(leaderID)   #leader  section
    
      
  } else{   #There is a leader vehicle
    x1 <- Get_x(leaderID)
    v1 <- Get_v(leaderID)
    
    length_v1 <- Get_length(leaderID)
    dec1      <- Get_dec(leaderID)
    sec1  <-  Get_section(leaderID)   #leader  section
    
  } 
  
  sec0  <-  Get_section(VehID)      #vehicle section
  
  if(is.na(sec1) || length(sec1)==0){ #......prevent error
    x1<-x1
  
  }else{
    
    if(sec0 != sec1){
      x1<-   x1 + Get_SecLength(sec0)
  
    }
  }
  
  x <- cf0_x(x0, v0, x1, dec, acc, v_free, jamgap, dt, length_v1, tau, SGMode,VehID   ,dec1,v1)

#   if(!notexist(leaderID)){
#     if(leaderID != 0){
#       if(sec0 != 1 ){
#         
#       
#         if(x > (x1 - length_v1) ){
#           print(paste0("crash - ",VehID))
#         }
#         
#       }
#     }
#   }
  
  return(x)
}



