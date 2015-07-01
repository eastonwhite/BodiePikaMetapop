#Created by Easton R White
#Created on 1-Jul-2015
#Last edited 1-Jul-2015

#This code does a sensitivity analysis of the dispersal radius parameter

radius_vector=seq(100,700,by=50) #vector of dispersal mortality rates to test

trial_trial_mean=matrix(0,nrow=1,ncol=length(radius_vector))
trial_trial_variance= matrix(0,nrow=1,ncol=length(radius_vector))
trial_trial_ext_year=matrix(0,nrow=1,ncol=length(radius_vector))
trial_trial_occupancy=matrix(0,nrow=1,ncol=length(radius_vector))
trial_trial_ext_events=matrix(0,nrow=1,ncol=length(radius_vector))
trial_trial_recol_events=matrix(0,nrow=1,ncol=length(radius_vector))

#start vector for radius_vector approach
for (IM in 1:length(radius_vector)){
  #creates matrix of patchs within specified distance of each other
  main<-read.table("~/Desktop/Research/Nagy Lab/Pikas/Modeling/SimpleBodieModel/patchdistance.txt")
  main=main[1:79,1:79]
  main[main<=radius_vector[IM]]=1
  main[main>radius_vector[IM]]=0
  
  
  trials=10
  #Bodie 1972 conditions
  TC=TC_test
  IC=IC_test  
  
  trial_mean=matrix(0,nrow=1,ncol=trials)
  trial_variance= matrix(0,nrow=1,ncol=trials)
  trial_ext_year=matrix(0,nrow=1,ncol=trials)
  trial_occupancy=matrix(0,nrow=1,ncol=trials)
  trial_ext_events=matrix(0,nrow=1,ncol=trials)
  trial_recol_events=matrix(0,nrow=1,ncol=trials)
  trial_pop=list()
  
  #Run model numerous times
  for (k in 1:trials){
    source("~/Desktop/Research/Nagy Lab/Pikas/Modeling/SimpleBodieModel/SimpleBodieModel_model.R")
    trial_mean[,k]=mean(colSums(APika))
    trial_variance[,k]=var(colSums(APika))
    trial_ext_year[,k]=(1971 + which((2*colSums(APika[c(1:19,21:23,57,58),]))<7)[1])
    trial_occupancy[,k]=mean(colSums(APika>0)/79)
    trial_pop[[k]]=APika
    
    ext_events = matrix(0,nrow=1,ncol=max.time-1)
    recol_events = matrix(0,nrow=1,ncol=max.time-1)
    for (j in 1:(max.time-1)){
      ext_events[,j] = sum(APika[,j]>0 & APika[,j+1]==0)
      recol_events[,j] = sum(APika[,j]==0 & APika[,j+1]>0)
    }
    
    trial_ext_events[,k]=sum(ext_events)
    trial_recol_events[,k]=sum(recol_events)
    print(k) #a simple counter
  }
  
  #calculate mean values for each measurement in the model
  trial_trial_mean[IM]=mean(trial_mean)
  trial_trial_variance[IM]= mean(trial_variance)
  trial_trial_ext_year[IM]=mean(trial_ext_year)
  trial_trial_occupancy[IM]=mean(trial_occupancy)
  trial_trial_ext_events[IM]=mean(trial_ext_events)
  trial_trial_recol_events[IM]=mean(trial_recol_events)
  
  print(paste('value',IM,sep='')) #a simple counter
  
}