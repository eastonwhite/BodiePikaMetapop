#Created by Easton R White
#Created on 25-Jun-2015
#Last edited 7-Aug-2015

#This is code to perform an inverse modeling scheme to find reasonable values for dispersal survival rate
#I run simulations using different values of dispersal mortality to see which value fits
#the census data from 1991-2009 (where we have the most complete census data)

#Parameters not known from field
d_m_vector=rep(seq(0.02,0.98,by=0.02),times=49)
weaning_m_vector=rep(seq(0.02,0.98,by=0.02), each = 49, length = 2401) #vector of dispersal mortality rates to test

#from cross validation scheme d_m=0.77,weaning_m=0.44

#empty vectors to store measurements made for each dispersal mortality value
trial_trial_mean=matrix(0,nrow=1,ncol=length(d_m_vector)) #mean population size
trial_trial_mean_sd=matrix(0,nrow=1,ncol=length(d_m_vector)) #sd of population size
trial_trial_variance= matrix(0,nrow=1,ncol=length(d_m_vector)) #variance in pop size
trial_trial_ext_year=matrix(0,nrow=1,ncol=length(d_m_vector)) # year of southern extinction
trial_trial_occupancy=matrix(0,nrow=1,ncol=length(d_m_vector)) #patch occupancy
trial_trial_occupancy_sd=matrix(0,nrow=1,ncol=length(d_m_vector))
trial_trial_ext_events=matrix(0,nrow=1,ncol=length(d_m_vector)) #number of extinction events
trial_trial_recol_events=matrix(0,nrow=1,ncol=length(d_m_vector)) #number of recolonization events
trial_trial_error=matrix(0,nrow=1,ncol=length(d_m_vector)) #error between simulation and field

#creates matrix of patchs within specified distance of each other (300m is default dispersal distance)
main<-read.table('patchdistance.txt')
main=main[1:79,1:79]
main[main<=300]=1
main[main>300]=0
main=as.matrix(main)
diag(main)=0 #makes it so pikas cannot disperse back to their own natal patch

#Bodie territory counts (constant over time)
territories=c(6,5,3,7,3,4,3,4,5,5,5,3,3,4,3,3,4,4,3,3,
              5,5,3,3,3,4,4,3,3,2,3,4,3,3,4,3,5,5,9,3,5,
              5,4,14,4,5,5,7,13,4,3,3,3,4,2,5,11,12,3,13,
              14,13,7,15,50,3,12,7,4,4,3,5,16,10,3,4,11,12,3)

#Initial population size data from spreadsheet 'MeasurementsCompleteBodieCensusData1972-2009_EW'
#fill in NAs
IC1972=c(0,3,1,3,0,4,2,0,3,4,4,1,0,0,0,2,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,4,0,5,3,4,4,3,10,1,1,1,3,4,1,1,0,1,1,1,2,7,9,1,6,7,6,2,9,50,1,3,2,0,0,0,0,8,5,1,1,6,2,2)
IC1991=c(0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,2,2,0,0,0,0,0,0,0,2,0,4,1,0,0,3,6,0,5,0,2,4,1,0,0,NA,2,1,2,1,0,1,7,10,11,3,7,13,1,7,3,2,2,0,2,6,4,3,3,6,NA,1)
IC1991[53]=1 #these three changes are averages based on all other years (can't have NAs to initialize simulations)
IC1991[78]=6
IC1991[65]=50

#########################################
#########################################

#start vector for testing different dispersal mortality values
for (IM in 1:length(d_m_vector)){

  d_m = d_m_vector[IM] #set disperser mortality rate
  weaning_m = weaning_m_vector[IM]
 
  trials=100
  IC=IC1991
    
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
      source("~/Desktop/Research/Nagy Lab/Pikas/Modeling/SimpleBodieModel/inverse_modeling_approach_model.R")
      
      ######take measurements of model#####
      #APika_sample=APika #for long sample
      #APika_sample[which(is.na(NA_matrix[1])),]=NA #for long sample
      
      #different measurements to take depending on if we start with 1991 or 1972 initial conditions (we use 1991 for inverse modeling approach, 1972 elsewhere)
      if (sum(IC==IC1991)==79){
        APika_sample=NA_matrix[,20:38]*APika #for 19 year model
        APika_sample=APika_sample[,-c(12,17)] # for 19 year model
        trial_error[,k]=sum((colSums(sampled_census_bodie[,4:20],na.rm=T) - colSums(APika_sample,na.rm=T))^2)
        #trial_error[,k]=sum((colSums(sampled_census_bodie[,seq(4,20,by=2)],na.rm=T) - colSums(APika_sample[,seq(1,17,by=2)],na.rm=T))^2) #use training set from 1991 onward
      }else if(sum(IC==IC1972)==79){
        APika_sample=NA_matrix*APika
        APika_sample=APika_sample[,-c(2:5,7:17,19,31,36)]
        trial_error[,k]=sum((colSums(sampled_census_bodie[,1:20],na.rm=T) - colSums(APika_sample,na.rm=T))^2)
      }
      
      trial_mean[,k]=mean(colSums(APika_sample,na.rm=T))
      trial_mean_sd[,k]=sd(colSums(APika_sample,na.rm=T))
      trial_variance[,k]=var(colSums(APika_sample,na.rm=T))
      trial_ext_year[,k]=(1971 + which((colSums(APika[c(2:19,21:37,57,58),]))<14)[1])
      trial_occupancy[,k]=mean(colSums(APika_sample[1:79,]>0,na.rm=T)/66)
      trial_occupancy_sd[,k]=sd(colSums(APika_sample[1:79,]>0,na.rm=T)/66)
      trial_sampled_pop[[k]]=APika_sample
      trial_pop[[k]]=APika
      
      #Currently this code is set of to count extinction and recolonization events only since 1991
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

#save(IC,trials,d_m_vector,weaning_m_vector,trial_trial_mean,trial_trial_mean_sd,trial_trial_variance,trial_trial_ext_year,
#     trial_trial_occupancy,trial_trial_occupancy_sd,trial_trial_ext_events,
#    trial_trial_recol_events,trial_trial_error,file='inverse_modeling_100trials_900combo_d_and_w.Rdata')
