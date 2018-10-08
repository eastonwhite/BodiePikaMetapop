#Easton R. White 
#created 21-Mar-2016
#Last edited 21-Mar-2016

#studying the effects of the mainland patches. This is adapted from the 4 model experiment

######### LOAD all the initial conditions
source('Scripts/Load_Initial_Conditions.R')
#########


###########################
#Standard Bodie model *with Mainland patches 64 and 65 included*
#creates matrix of patchs within specified distance of each other (300m is default dispersal distance)
inter_patch_distances<-read.table('Scripts/inter_patch_distances.txt')
inter_patch_distances[inter_patch_distances<=300]=1
inter_patch_distances[inter_patch_distances>300]=0
inter_patch_distances=as.matrix(inter_patch_distances)
diag(inter_patch_distances)=0 #makes it so pikas cannot disperse back to their own natal patch



trials=1000
IC=IC1972 #set initial conditions (1972 data by default)

trial_mean=matrix(0,nrow=1,ncol=trials)
trial_mean_sd=matrix(0,nrow=1,ncol=trials)
trial_variance= matrix(0,nrow=1,ncol=trials)
trial_ext_year=matrix(0,nrow=1,ncol=trials)
trial_occupancy=matrix(0,nrow=1,ncol=trials)
trial_occupancy_sd=matrix(0,nrow=1,ncol=trials)
trial_ext_events=matrix(0,nrow=1,ncol=trials)
trial_recol_events=matrix(0,nrow=1,ncol=trials)
trial_error=matrix(0,nrow=1,ncol=trials)
trial_pop=list()
trial_sampled_pop=list()

#Run model numerous times
for (k in 1:trials){
  source("Scripts/SimpleBodieModel_model.R")
  
  ######take measurements of model#####
  #APika_sample=APika #for long sample
  #APika_sample[which(is.na(NA_matrix[1])),]=NA #for long sample
  
  #different measurements to take depending on if we start with 1991 or 1972 initial conditions (we use 1991 for inverse modeling approach, 1972 elsewhere)
  if (sum(IC==IC1991)==79){
    APika_sample=NA_matrix[,20:39]*APika #for 19 year model
    APika_sample=APika_sample[,-c(12,17)] # for 19 year model
    trial_error[,k]=sum((colSums(sampled_census_bodie[,4:21],na.rm=T) - colSums(APika_sample,na.rm=T))^2)
  }else if(sum(IC==IC1972)==79){
    APika_sample=NA_matrix*APika
    APika_sample=APika_sample[,-c(2:5,7:17,19,31,36)]
    trial_error[,k]=sum((colSums(sampled_census_bodie[,1:21],na.rm=T) - colSums(APika_sample,na.rm=T))^2)
  }
  #########
  
  trial_mean[,k]=mean(colSums(APika_sample,na.rm=T))
  trial_mean_sd[,k]=sd(colSums(APika_sample,na.rm=T))
  trial_variance[,k]=var(colSums(APika_sample,na.rm=T))
  trial_ext_year[,k]=(1971 + which((colSums(APika[c(2:19,21:37,57,58),]))<14)[1])
  trial_occupancy[,k]=mean(colSums(APika_sample[1:79,]>0,na.rm=T)/66)
  trial_occupancy_sd[,k]=sd(colSums(APika_sample[1:79,]>0,na.rm=T)/66)
  trial_sampled_pop[[k]]=APika_sample
  trial_pop[[k]]=APika
  
  
  ext_events = matrix(0,nrow=1,ncol=ncol(APika_sample)-1)
  recol_events = matrix(0,nrow=1,ncol=ncol(APika_sample)-1)
  for (j in 1:(ncol(APika_sample)-1)){
    ext_events[,j] = sum(APika_sample[,j]>0 & APika_sample[,j+1]==0,na.rm=T)
    recol_events[,j] = sum(APika_sample[,j]==0 & APika_sample[,j+1]>0,na.rm=T)
  }
  
  trial_ext_events[,k]=sum(ext_events)
  trial_recol_events[,k]=sum(recol_events)	
  
  if (k %in% seq(50,1000,by=50)) print(k)  #a simple counter
}



#####################################################################
#####################################################################
#####################################################################
#####################################################################

###########################
#Standard Bodie model *without Mainland patches 64 and 65*
#creates matrix of patchs within specified distance of each other (300m is default dispersal distance)
inter_patch_distances<-read.table('Scripts/inter_patch_distances.txt')
inter_patch_distances[inter_patch_distances<=300]=1
inter_patch_distances[inter_patch_distances>300]=0
inter_patch_distances=as.matrix(inter_patch_distances)
diag(inter_patch_distances)=0 #makes it so pikas cannot disperse back to their own natal patch

#Bodie 1972 conditions where mainland patches are now smaller. Patches 44, 64, and 65 are all now K=5, instead of default
 territories=c(6,5,3,7,3,4,3,4,5,5,5,3,3,4,3,3,4,4,3,3,
               5,5,3,3,3,4,4,3,3,2,3,4,3,3,4,3,5,5,9,3,5,
               5,4,5,4,5,5,7,13,4,3,3,3,4,2,5,11,12,3,13,
               14,13,7,5,5,3,12,7,4,4,3,5,16,10,3,4,11,12,3)

#Initial population size data from spreadsheet 'MeasurementsCompleteBodieCensusData1972-2009_EW'
#fill in NAs
IC1972=c(0,3,1,3,0,4,2,0,3,4,4,1,0,0,0,2,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,4,0,5,3,4,4,3,10,1,1,1,3,4,1,1,0,1,1,1,2,7,9,1,6,7,6,2,5,5,1,3,2,0,0,0,0,8,5,1,1,6,2,2)
IC=IC1972 #set initial conditions (1972 data by default)

no_mainlands_mean=matrix(0,nrow=1,ncol=trials)
no_mainlands_mean_sd=matrix(0,nrow=1,ncol=trials)
no_mainlands_variance= matrix(0,nrow=1,ncol=trials)
no_mainlands_ext_year=matrix(0,nrow=1,ncol=trials)
no_mainlands_occupancy=matrix(0,nrow=1,ncol=trials)
no_mainlands_occupancy_sd=matrix(0,nrow=1,ncol=trials)
no_mainlands_ext_events=matrix(0,nrow=1,ncol=trials)
no_mainlands_recol_events=matrix(0,nrow=1,ncol=trials)
no_mainlands_error=matrix(0,nrow=1,ncol=trials)
no_mainlands_pop=list()
no_mainlands_sampled_pop=list()

#Run model numerous times
for (k in 1:trials){
  source("Scripts/SimpleBodieModel_model.R")
  
  ######take measurements of model#####
  #APika_sample=APika #for long sample
  #APika_sample[which(is.na(NA_matrix[1])),]=NA #for long sample
  
  #different measurements to take depending on if we start with 1991 or 1972 initial conditions (we use 1991 for inverse modeling approach, 1972 elsewhere)
  if (sum(IC==IC1991)==79){
    APika_sample=NA_matrix[,20:39]*APika #for 19 year model
    APika_sample=APika_sample[,-c(12,17)] # for 19 year model
    no_mainlands_error[,k]=sum((colSums(sampled_census_bodie[,4:21],na.rm=T) - colSums(APika_sample,na.rm=T))^2)
  }else if(sum(IC==IC1972)==79){
    APika_sample=NA_matrix*APika
    APika_sample=APika_sample[,-c(2:5,7:17,19,31,36)]
    no_mainlands_error[,k]=sum((colSums(sampled_census_bodie[,1:21],na.rm=T) - colSums(APika_sample,na.rm=T))^2)
  }
  
  no_mainlands_mean[,k]=mean(colSums(APika_sample,na.rm=T))
  no_mainlands_mean_sd[,k]=sd(colSums(APika_sample,na.rm=T))
  no_mainlands_variance[,k]=var(colSums(APika_sample,na.rm=T))
  no_mainlands_ext_year[,k]=(1971 + which((colSums(APika[c(2:19,21:37,57,58),]))<14)[1])
  no_mainlands_occupancy[,k]=mean(colSums(APika_sample[1:79,]>0,na.rm=T)/66)
  no_mainlands_occupancy_sd[,k]=sd(colSums(APika_sample[1:79,]>0,na.rm=T)/66)
  no_mainlands_sampled_pop[[k]]=APika_sample
  no_mainlands_pop[[k]]=APika
  
  
  ext_events = matrix(0,nrow=1,ncol=20-1)
  recol_events = matrix(0,nrow=1,ncol=20-1)
  for (j in 1:(20-1)){
    ext_events[,j] = sum(APika_sample[,j]>0 & APika_sample[,j+1]==0,na.rm=T)
    recol_events[,j] = sum(APika_sample[,j]==0 & APika_sample[,j+1]>0,na.rm=T)
  }
  
  no_mainlands_ext_events[,k]=sum(ext_events)
  no_mainlands_recol_events[,k]=sum(recol_events)	
  
  if (k %in% seq(50,1000,by=50)) print(k)  #a simple counter
}




#save(IC,trials,max.time,trial_mean,trial_mean_sd,trial_variance,trial_ext_year,trial_occupancy,trial_occupancy_sd,trial_ext_events,trial_recol_events,trial_error,trial_sampled_pop,trial_pop,no_mainlands_mean,no_mainlands_mean_sd,no_mainlands_variance,no_mainlands_ext_year,no_mainlands_occupancy,no_mainlands_occupancy_sd,no_mainlands_ext_events,no_mainlands_recol_events,no_mainlands_error,no_mainlands_sampled_pop,no_mainlands_pop,file='Model_Outputs/Mainland_Effect_1000trials.Rdata')

