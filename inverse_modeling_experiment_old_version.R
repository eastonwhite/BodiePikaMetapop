#Created by Easton R White
#Created on 25-Jun-2015
#Last edited 28-Jun-2015

#This is code to perform an inverse modeling scheme to find reasonable values for dispersal survival rate

d_m_vector=seq(0.35,0.95,by=0.05) #vector of dispersal mortality rates to test

trial_trial_mean=matrix(0,nrow=1,ncol=length(d_m_vector))
trial_trial_variance= matrix(0,nrow=1,ncol=length(d_m_vector))
#trial_trial_ext_year=matrix(0,nrow=1,ncol=length(d_m_vector))
trial_trial_occupancy=matrix(0,nrow=1,ncol=length(d_m_vector))
trial_trial_ext_events=matrix(0,nrow=1,ncol=length(d_m_vector))
trial_trial_recol_events=matrix(0,nrow=1,ncol=length(d_m_vector))

#creates matrix of patchs within specified distance of each other
main<-read.table('patchdistance.txt')
attach(main)
main=main[1:79,1:79]
main[main<=300]=1
main[main>300]=0
main[1:37,1:37]=0
main=as.matrix(main)
diag(main)=1


#start vector for d_m approach
for (IM in 1:length(d_m_vector)){

d_m = d_m_vector[IM]
  
  trials=30
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
      source("~/Desktop/Research/Nagy Lab/Pikas/Modeling/SimpleBodieModel/inverse_modeling_experiment_model.R")
      
      APika_sample=NA_matrix[,20:38]*APika
      APika_sample=APika_sample[,-c(12,17)]
      trial_mean[,k]=mean(colSums(APika_sample,na.rm=T))
      trial_variance[,k]=var(colSums(APika_sample,na.rm=T))
      #trial_ext_year[,k]=(1971 + which((colSums(APika[c(2:19,21:37,57,58),]))<8)[1])
      trial_occupancy[,k]=mean(colSums(APika_sample[38:79,]>0,na.rm=T)/31)
      trial_pop[[k]]=APika_sample
      
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
  trial_trial_variance[IM]= mean(trial_variance)
  #trial_trial_ext_year[IM]=mean(trial_ext_year)
  trial_trial_occupancy[IM]=mean(trial_occupancy)
  trial_trial_ext_events[IM]=mean(trial_ext_events)
  trial_trial_recol_events[IM]=mean(trial_recol_events)

print(paste('value',IM,sep='')) #a simple counter

}