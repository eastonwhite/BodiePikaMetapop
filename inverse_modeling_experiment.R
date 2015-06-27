#Created by Easton R White
#Created on 25-Jun-2015
#Last edited 28-Jun-2015

#This is code to perform an inverse modeling scheme to find reasonable values for dispersal survival rate

d_m_vector=seq(0.05,0.95,by=0.10) #vector of dispersal mortality rates to test

trial_trial_mean=matrix(0,nrow=1,ncol=length(d_m_vector))
trial_trial_variance= matrix(0,nrow=1,ncol=length(d_m_vector))
trial_trial_ext_year=matrix(0,nrow=1,ncol=length(d_m_vector))
trial_trial_occupancy=matrix(0,nrow=1,ncol=length(d_m_vector))

#creates matrix of patchs within specified distance of each other
main<-read.table('patchdistance.txt')
attach(main)
main=main[1:79,1:79]
main[main<=300]=1
main[main>300]=0



#start vector for d_m approach
for (IM in 1:length(d_m_vector)){

d_m = d_m_vector[IM]
  
  trials=10
  #Bodie 1972 conditions
  TC=TC_test
  IC=IC_test  
    
  trial_mean=matrix(0,nrow=1,ncol=trials)
  trial_variance= matrix(0,nrow=1,ncol=trials)
  trial_ext_year=matrix(0,nrow=1,ncol=trials)
  trial_occupancy=matrix(0,nrow=1,ncol=trials)
  trial_pop=list()
  
    #Run model numerous times
    for (k in 1:trials){
      source("~/Desktop/Research/Nagy Lab/Pikas/Modeling/SimpleBodieModel/SimpleBodieModel_model.R")
      trial_mean[,k]=mean(colSums(APika))
      trial_variance[,k]=var(colSums(APika))
      trial_ext_year[,k]=(1971 + which((2*colSums(APika[c(1:19,21:23,57,58),]))<7)[1])
      trial_occupancy[,k]=mean(colSums(APika>0)/79)
      trial_pop[[k]]=APika
      print(k) #a simple counter
    }
  
  #calculate mean values for each measurement in the model
  trial_trial_mean[IM]=mean(trial_mean)
  trial_trial_variance[IM]= mean(trial_variance)
  trial_trial_ext_year[IM]=mean(trial_ext_year)
  trial_trial_occupancy[IM]=mean(trial_occupancy)

print(paste('value',IM,sep='')) #a simple counter

}