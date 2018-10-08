#Easton R. White 
#Bodie Pika Dispersal Model wrapper
#created 22-June-2012
#Last edited 1-Sep-2015

#allows the 'SimpleBodieModel_model.R' to be run over and over while making a number of measurements
#model includes females and males, only females reproduce, patch 1 is isolated by distance, cleaned up PC, TC, n_t things

ptm=proc.time() #model timer

#creates matrix of patchs within specified distance of each other (300m is default dispersal distance)
inter_patch_distances <- read.table('Scripts/inter_patch_distances.txt')
inter_patch_distances[inter_patch_distances<=300]=1
inter_patch_distances[inter_patch_distances>300]=0
inter_patch_distances=as.matrix(inter_patch_distances)
diag(inter_patch_distances)=0 #makes it so pikas cannot disperse back to their own natal patch

######### LOAD all the initial conditions
source('Scripts/Load_Initial_Conditions.R')
#########
remove_degraded_patches='no'
if (remove_degraded_patches == 'yes'){
  inter_patch_distances[which(rowSums(sampled_census_bodie)==0),]=0
  inter_patch_distances[,which(rowSums(sampled_census_bodie)==0)]=0
  territories[which(rowSums(sampled_census_bodie)==0)]=0
  IC1972[which(rowSums(sampled_census_bodie)==0)]=0
}

  trials=30
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
        #trial_error[,k]=sum((colSums(sampled_census_bodie[,seq(4,20,by=2)],na.rm=T) - colSums(APika_sample[,seq(1,17,by=2)],na.rm=T))^2) #use training set from 1991 onward
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
      
      #the extinction and recolonization events are counted for only sampled years from 1972-2009. This can be easily changed
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



end_time=proc.time()-ptm #calculate total time model takes to run


#save(IC,trials,d_m,weaning_m,u,d_prop,max.time,trial_mean,trial_mean_sd,trial_variance,trial_ext_year,trial_occupancy,trial_occupancy_sd,trial_ext_events,trial_recol_events,trial_error,trial_sampled_pop,trial_pop,file='Model_Outputs/Modeloutput_300years_dm048_wm0.52_1000trials.Rdata')



