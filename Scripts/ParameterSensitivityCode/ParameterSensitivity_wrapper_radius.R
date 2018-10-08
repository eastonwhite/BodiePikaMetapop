#Created by Easton R White
#Created on 31-Jul-2015
#Last edited 7-Aug-2015

#This is code to test the sensitivity of different model parameters. The code is very similar to 
#what I used in the inverse_modeling_approach_wrapper.R setup

######### LOAD all the initial conditions
source('Scripts/Load_Initial_Conditions.R')
#########


d_m_vector=seq(0.05,0.95,by=0.01) 
radius_vector=seq(50,1500,by=25)
d_prop_vector=seq(0.05,0.95,by=0.01) 
#r_vector= seq(0.5,6,by=0.25)
u_vector=seq(0.05,0.95,by=0.01) 
weaning_m_vector=seq(0.05,0.95,by=0.01) 

#empty vectors to store measurements made for each dispersal mortality value
trial_trial_mean=matrix(0,nrow=1,ncol=length(radius_vector)) #mean population size
trial_trial_mean_sd=matrix(0,nrow=1,ncol=length(radius_vector)) #sd of population size
trial_trial_variance= matrix(0,nrow=1,ncol=length(radius_vector)) #variance in pop size
trial_trial_ext_year=matrix(0,nrow=1,ncol=length(radius_vector)) # year of southern extinction
trial_trial_occupancy=matrix(0,nrow=1,ncol=length(radius_vector)) #patch occupancy
trial_trial_occupancy_sd=matrix(0,nrow=1,ncol=length(radius_vector))
trial_trial_ext_events=matrix(0,nrow=1,ncol=length(radius_vector)) #number of extinction events
trial_trial_recol_events=matrix(0,nrow=1,ncol=length(radius_vector)) #number of recolonization events
trial_trial_error=matrix(0,nrow=1,ncol=length(radius_vector)) #error between simulation and field



#########################################
#########################################

#start vector for testing different dispersal mortality values
for (IM in 1:length(radius_vector)){

  ## Have to comment out all "vector_name[IM]" except for one you are testing
  #d_m = d_m_vector[IM] #set disperser mortality rate
  radius = radius_vector[IM]
  #d_prop=d_prop_vector[IM]
  #r=r_vector[IM]
  #u=u_vector[IM]
  #weaning_m = weaning_m_vector[IM]
  
  ####default model parameters#
  #radius=300     #from Smith and Gilpin 1997
  #r= 1.625      #1.625 (3.25) Smith: Ecology 1974a 
  u= 0.37        #0.37 Smith: Ecology 1974a, 1978
  d_m=0.61        #from this paper
  d_prop = 0.25 #Nagy unpublished, Smith 1987
  weaning_m =0.48 #from this paper
  ####default model parameters#

  #creates matrix of patchs within specified distance of each other (300m is default dispersal distance)
  inter_patch_distances<-read.table('Scripts/inter_patch_distances.txt')
  inter_patch_distances[inter_patch_distances<=radius]=1
  inter_patch_distances[inter_patch_distances>radius]=0
  inter_patch_distances=as.matrix(inter_patch_distances)
  diag(inter_patch_distances)=0 #makes it so pikas cannot disperse back to their own natal patch
  
  #Start model setup here
  trials=100
  IC=IC1972
    
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
      source("Scripts/ParameterSensitivityCode/ParameterSensitivity_model.R")
      
      ######take measurements of model#####
      #APika_sample=APika #for long sample
      #APika_sample[which(is.na(NA_matrix[1])),]=NA #for long sample
      
      #different measurements to take depending on if we start with 1991 or 1972 initial conditions (we use 1991 for inverse modeling approach, 1972 elsewhere)
      if (sum(IC==IC1991)==79){
        APika_sample=NA_matrix[,20:38]*APika #for 19 year model
        APika_sample=APika_sample[,-c(12,17)] # for 19 year model
        trial_error[,k]=sum((colSums(sampled_census_bodie[,4:21],na.rm=T) - colSums(APika_sample,na.rm=T))^2)
      }else if(sum(IC==IC1972)==79){
        APika_sample=NA_matrix*APika
        APika_sample=APika_sample[,-c(2:5,7:17,19,31,36)]
        trial_error[,k]=sum((colSums(sampled_census_bodie[,1:21],na.rm=T) - colSums(APika_sample,na.rm=T))^2)
      }
      
      trial_mean[,k]=mean(colSums(APika_sample,na.rm=T))
      trial_mean_sd[,k]=sd(colSums(APika_sample,na.rm=T))
      trial_variance[,k]=var(colSums(APika_sample,na.rm=T))
      trial_ext_year[,k]=(1971 + which((colSums(APika[c(2:19,21:37,57,58),]))<14)[1])
      trial_occupancy[,k]=mean(colSums(APika_sample[1:79,]>0,na.rm=T)/66)
      trial_occupancy_sd[,k]=sd(colSums(APika_sample[1:79,]>0,na.rm=T)/66)
      trial_sampled_pop[[k]]=APika_sample
      trial_pop[[k]]=APika
      
      
      ext_events = matrix(0,nrow=1,ncol=17-1)
      recol_events = matrix(0,nrow=1,ncol=17-1)
      for (j in 1:(17-1)){
        ext_events[,j] = sum(APika_sample[,j]>0 & APika_sample[,j+1]==0,na.rm=T)
        recol_events[,j] = sum(APika_sample[,j]==0 & APika_sample[,j+1]>0,na.rm=T)
      }
      
      trial_ext_events[,k]=sum(ext_events)
      trial_recol_events[,k]=sum(recol_events)	
      
      #print(k)
    }
  
  #calculate mean values for each measurement in the model
  trial_trial_mean[IM]=mean(trial_mean)
  trial_trial_mean_sd[IM]=mean(trial_mean_sd)
  trial_trial_variance[IM]= mean(trial_variance)
  trial_trial_ext_year[IM]=mean(trial_ext_year)
  trial_trial_occupancy[IM]=mean(trial_occupancy)
  trial_trial_occupancy_sd[IM]=sd(trial_occupancy_sd)
  trial_trial_ext_events[IM]=mean(trial_ext_events)
  trial_trial_recol_events[IM]=mean(trial_recol_events)
  trial_trial_error[IM]=mean(trial_error)

print(paste('value',IM,sep='')) #a simple counter

}

save(IC,trials,radius_vector,trial_trial_mean,trial_trial_mean_sd,trial_trial_variance,trial_trial_ext_year,trial_trial_occupancy,trial_trial_occupancy_sd,trial_trial_ext_events,trial_trial_recol_events,trial_trial_error,file='Model_Outputs/ParameterSensitivity_100trials_radius.Rdata')
