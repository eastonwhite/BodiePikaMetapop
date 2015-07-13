#Script to compare Bodie model to Bodie census directly

#Easton R. White 
#Bodie Pika Dispersal Model wrapper
#created 14-Jul-2015
#Last edited 14-Jul-2015

# /Users/easton2/Desktop/Research/Nagy Lab/Pikas/Modeling

#allows the 'SimpleBodieModel_model.R' to be run over and over while making a number of measurements

ptm=proc.time() #model timer

trials=20 #number of times to run 'SimpleBodieModel_model.R'

#test values from random rounding script 
TC=TC_test
IC=IC_test

trial_mean=matrix(0,nrow=1,ncol=trials)
trial_variance= matrix(0,nrow=1,ncol=trials)
trial_ext_year=matrix(0,nrow=1,ncol=trials)
trial_occupancy=matrix(0,nrow=1,ncol=trials)
trial_ext_events=matrix(0,nrow=1,ncol=trials)
trial_recol_events=matrix(0,nrow=1,ncol=trials)
trial_pop=list()

#creates matrix of patchs within specified distance of each other
main<-read.table("/Users/easton2/Desktop/Research/Nagy Lab/Pikas/Modeling/SimpleBodieModel/patchdistance.txt")
main=main[1:79,1:79]
main[main<=300]=1
main[main>300]=0

#Run model numerous times
for (k in 1:trials){
  source("/Users/easton2/Desktop/Research/Nagy Lab/Pikas/Modeling/SimpleBodieModel/SimpleBodieModel_model.R")
  
  APika_sample=NA_matrix*APika
  APika_sample=APika_sample[,-c(2,3,4,5,7,8,9,10,11,12,13,14,15,16,17,19,31,36)]
  trial_mean[,k]=mean(colSums(APika_sample,na.rm=T))
  trial_variance[,k]=var(colSums(APika_sample,na.rm=T))
  trial_ext_year[,k]=(1971 + which((colSums(APika[c(2:19,21:37,57,58),]))<8)[1])
  trial_occupancy[,k]=mean(colSums(APika_sample>0,na.rm=T)/66)
  trial_pop[[k]]=APika_sample
  
  ext_events = matrix(0,nrow=1,ncol=20-1)
  recol_events = matrix(0,nrow=1,ncol=20-1)
  for (j in 1:(20-1)){
    ext_events[,j] = sum(APika_sample[,j]>0 & APika_sample[,j+1]==0,na.rm=T)
    recol_events[,j] = sum(APika_sample[,j]==0 & APika_sample[,j+1]>0,na.rm=T)
  }
  
  trial_ext_events[,k]=sum(ext_events)
  trial_recol_events[,k]=sum(recol_events)	
  
  print(k)
}


end_time=proc.time()-ptm #calculate total time model takes to run



