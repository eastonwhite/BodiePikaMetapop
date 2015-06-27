#Easton R. White 
#Bodie Pika Dispersal Model wrapper
#created 22-June-2012
#Last edited 28-Jun-2015

# /Users/easton2/Desktop/Research/Nagy Lab/Pikas/Modeling

#allows the 'SimpleBodieModel_model.R' to be run over and over while making a number of measurements

ptm=proc.time() #model timer

trials=10 #number of times to run 'SimpleBodieModel_model.R'

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
attach(main)
main=main[1:79,1:79]

main[main<=300]=1
main[main>300]=0

  #Run model numerous times
  for (k in 1:trials){
  	source("/Users/easton2/Desktop/Research/Nagy Lab/Pikas/Modeling/SimpleBodieModel/SimpleBodieModel_model.R")
  	
  	trial_mean[,k]=mean(colSums(APika))
  	trial_variance[,k]=var(colSums(APika))
  	trial_ext_year[,k]=(1971 + which((colSums(APika[c(1:19,21:37,57,58),]))<8)[1])
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
  
  print(k)
  }


end_time=proc.time()-ptm #calculate total time model takes to run



