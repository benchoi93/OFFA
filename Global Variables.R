# Gloabl Variables
# This file defiens the variables used by multiple modules
# Following variables are global variables 
# Section, Vehicle VehicleAtt, Demand, Sec_LeaderVeh, Sec_LastVeh, dt, t0, t1 

#----------------------------------------------------------------------
# Reading Road section info
# SecrionID, NextSection, MergignType, Merging1,Merging2,SectionLength,NLane, grade
# Diverging Type (0=normal, 1=pocket, 2=passing )
# Merging Type   (0=normal, 1=pocket/continuous, 2=indepent)
# Ramp (0 = main road, 1 = on-ramp, 2 = off-ramp)

# Section <- read.csv("section.r", header = TRUE, sep = ",", quote = "\"",dec = ".", fill = TRUE, comment.char = "")
# Section <- read.csv("ramp_section.r", header = TRUE, sep = ",", quote = "\"",dec = ".", fill = TRUE, comment.char = "")
Section <- read.csv("test_section.r", header = TRUE, sep = ",", quote = "\"",dec = ".", fill = TRUE, comment.char = "")
# MacroSectionLength <- 400
# SectionLength <- 100

# Section <- Gen_Section(3200,SectionLength,1600,400)

#----------------------------------------------------------------------
# Demand Definition 
# Time, AutoLane1, AutoLane2, TruckLane1, TruckLane2,
# time: 10min, units= veh/hr  
# Demand <- read.csv("Demand.r", header = TRUE, sep = ",", quote = "\"",dec = ".", fill = TRUE, comment.char = "")
Demand <- read.csv("test_demand.r", header = TRUE, sep = ",", quote = "\"",dec = ".", fill = TRUE, comment.char = "",col.names = c("Time","T_length","Demand","RampDemand","Truck_ratio"))

# 
# load(file = "flowdata.rdata")
# FlowData <- FlowData[43:67,]
# 
# Demand <- data.frame(matrix(0,nrow = 25 , ncol = 5))
# colnames(Demand) <- c("Time","T_length","Demand","RampDemand","Truck_ratio")
# Demand$Time <- 1:25
# Demand$T_length <- 300
# Demand$Demand     <- FlowData[,2]/3*12
# Demand$RampDemand <- FlowData[,3]/2*12
# Demand$Truck_ratio <- 0.3
# 


#----------------------------------------------------------------------
# Vehicle Repository 
# Vehcile Driving Data
Vehicle <- data.frame(Veh_ID=0, DMode=0, section=0, lane=0,targetlane=0, leaderVeh=0, followerVeh=0, Coop=0,LC=0,LC_tryt=0,x=0, y=0, v=0, a=0)
Vehicle0 <- Vehicle
VehicleNA <- Vehicle0[-1,]
# Vehcile Attribute data (static)
VehicleAtt <- data.frame(Veh_ID=0, tau=0, jamgap=0,length=0, acc=0, dec=0, class=0,vf=0,LC_prep=0,Origin=0,Destination=0)
#### vf (free flow speed == 100)
# Vehicle Driving Data Save
VehicleData <- data.frame(matrix(ncol=16,nrow=0))
colnames(VehicleData)<-c("time","Veh_ID","DMode","section","lane","targetlane","leaderVeh","followerVeh","Coop","LC","LC_tryt","x","y","v","a")

#  

# Lane/Section vehicle order Database
NoLane <- max(Section$NoLane) 
NoSection <- length(Section$SectionID)

# Sec_LeaderVeh: the first vehID in the Sec_LeaderVeh[lane, section]
# Sec_LastVeh: the last vehID in the Sec_LeaderVeh[lane, section]
# The first section is source section
# the final section is destination section 

secorder <- Get_SecOrder()
config <- lane_config()

Sec_LeaderVeh <- data.frame(matrix(data = 0, nrow = dim(config)[1], ncol = dim(config)[2], byrow = FALSE, dimnames = NULL))
# colnames(Sec_LeaderVeh) <-c(1:NoSection)

Sec_LastVeh <- data.frame(matrix(data = 0, nrow = dim(config)[1], ncol = dim(config)[2], byrow = FALSE, dimnames = NULL))
# colnames(Sec_LastVeh) <-c(1:NoSection)

#Simulation Denifition
dt <- 0.2                         # simualtion time step 
t0 <- 1                           # simualtion start time =0 
t1 <- sum(Demand$T_length)/dt     # Simulation end = 1hr 
vf <- 105*1000/3600               # free flow speed (m/s)
v_y <- 1                          # lateral speed ()
LC_block    <- 1/dt                    # block lc for a while after lc 
LC_reset    <- 3/dt                    # reset lc (to no lc) 
int_arrival  <- 2/dt                 # minimum vehicle generation time interval
visible_distance <- 100           # lc probability check
sstvt <- 0.5*dt                     # lc sensitivity
v_off <- 2.5                      # cooperation disabling speed threshold (m/s)
avoid_truck <- 1.5                # LC probability amplifier in truck-normal case
distance0 <- 1000

truck_split <- c(0.05,0.1)                #ratio of trucks in lane 1 (only for 2 lane road)


comp_rate <- 0.3
pass_rate <- 1


alpha <- 0.85


# Macro_search_t <- 300
# macro_dt <- 10 #s
# nlc <-array(data= 0 ,dim = c(max(Cell$Lane) * 2 , NumCell , timestep))
# control <- FALSE



left_prefer   <-  1
right_prefer  <-  1

diverge <- as.data.frame(matrix(0,nrow=1,ncol=2))
colnames(diverge) <- c("VehID","diverge")

#
v_check_sec <- Section$SectionID[which(Section$Ramp != 1)]

for ( i in v_check_sec){
  temp<- data.frame(time = NA,vehID=NA, speed = NA , lane = NA)
  temp <- temp[-which(is.na(temp))]
  assign(paste0("speed_profile",i),temp)
}

LC_history <- data.frame(time = 0, VehID = 0)

arrival   <-  data.frame(time=1:(sum(Demand$T_length)/dt), veh = 0)
departure <-  data.frame(time=1:(sum(Demand$T_length)/dt), veh = 0)
#
origin_ratio      <- c(0.8,0.2)   #c(main, aux1, aux2, ...)
destination_ratio <- c(0.8,0.2)   #c(main, aux1, aux2, ...)



origin <- Section$SectionID[which(Section$O == 1)]
destin <- Section$SectionID[which(Section$D == 1)]
