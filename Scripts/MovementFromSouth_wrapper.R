#Easton R. White 
#Scenario where south invades the rest of study site
#created 28-Feb-2016
#Last edited 28-Feb-2016

# /Users/easton2/Desktop/Research/Nagy Lab/Pikas/Modeling

#This model starts with a few pikas in the south in 1900 and runs dynamics forward to see if they will invade the study site
#*At moment it appears south can't sustain itself without north or long distance dispersal

ptm=proc.time() #model timer

#Need to set the dispersal radius
#dispersal_radius = 300

#creates matrix of patchs within specified distance of each other (300m is default dispersal distance)
inter_patch_distances<-read.table('Scripts/inter_patch_distances.txt')
inter_patch_distances[inter_patch_distances<=dispersal_radius]=1
inter_patch_distances[inter_patch_distances>dispersal_radius]=0
inter_patch_distances=as.matrix(inter_patch_distances)
diag(inter_patch_distances)=0 #makes it so pikas cannot disperse back to their own natal patch


######### LOAD all the initial conditions
source('Scripts/Load_Initial_Conditions.R')
#########


#only some pikas in the most southern 10 patches
IC1900=rep(0,times=79)
IC1900[1:10]=rep(3,times=10)

  trials=100
  IC=IC1900 #set initial conditions (1972 data by default)
    
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
      APika_sample=APika #for long sample
      APika_sample[which(is.na(NA_matrix[1])),]=NA #for long sample
      
      trial_mean[,k]=mean(colSums(APika_sample,na.rm=T))
      trial_mean_sd[,k]=sd(colSums(APika_sample,na.rm=T))
      trial_variance[,k]=var(colSums(APika_sample,na.rm=T))
      trial_ext_year[,k]=(1971 + which((colSums(APika[c(2:19,21:37,57,58),]))<14)[1])
      trial_occupancy[,k]=mean(colSums(APika_sample[1:79,]>0,na.rm=T)/66)
      trial_occupancy_sd[,k]=sd(colSums(APika_sample[1:79,]>0,na.rm=T)/66)
      trial_sampled_pop[[k]]=APika_sample
      trial_pop[[k]]=APika
      
      #the extinction and recolonization events are counted for only sampled years from 1972-2009. This can be easily changed
      ext_events = matrix(0,nrow=1,ncol=20-1)
      recol_events = matrix(0,nrow=1,ncol=20-1)
      for (j in 1:(20-1)){
        ext_events[,j] = sum(APika_sample[,j]>0 & APika_sample[,j+1]==0,na.rm=T)
        recol_events[,j] = sum(APika_sample[,j]==0 & APika_sample[,j+1]>0,na.rm=T)
      }
      
      trial_ext_events[,k]=sum(ext_events)
      trial_recol_events[,k]=sum(recol_events)	
    
      #if (k %in% seq(50,1000,by=50)) print(k)  #a simple counter

      #points(colSums(APika_sample,na.rm=TRUE),ylim=c(0,30),type='l',col='yellow')
      }



#end_time=proc.time()-ptm #calculate total time model takes to run


#save(IC,trials,d_m,weaning_m,u,d_prop,max.time,trial_mean,trial_mean_sd,trial_variance,trial_ext_year,trial_occupancy,trial_occupancy_sd,trial_ext_events,trial_recol_events,trial_error,trial_sampled_pop,trial_pop,file='Modeloutput_300years_dm048_wm0.52_1000trials.Rdata')



