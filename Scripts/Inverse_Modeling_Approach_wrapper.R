#Created by Easton R White
#Created on 25-Jun-2115
#Last edited 7-Aug-2015

#This is code to perform an inverse modeling scheme to find reasonable values for dispersal survival rate
#I run simulations using different values of dispersal mortality to see which value fits
#the census data from 1991-2009 (where we have the most complete census data)

require(progress)

set.seed(12345)
#Parameters not known from field
d_m_values=seq(0.1,0.9,by=0.01)
weaning_m_values=seq(0.3,0.7,by=0.01) #vector of dispersal mortality rates to test

parameter_values = expand.grid(d_m_values,weaning_m_values)

pb <- progress_bar$new(
  format = "  downloading [:bar] :percent eta: :eta",
  total = nrow(parameter_values), clear = FALSE, width= 60)

d_m_vector = parameter_values[,1]
weaning_m_vector = parameter_values[,2]

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
inter_patch_distances<-read.table('Scripts/inter_patch_distances.txt')
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
  IC1991[which(rowSums(sampled_census_bodie)==0)]=0
}

#start vector for testing different dispersal mortality values
for (IM in 1:length(d_m_vector)){

  d_m = d_m_vector[IM] #set disperser mortality rate
  weaning_m = weaning_m_vector[IM]
 
  trials=50
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
      source("Scripts/Inverse_Modeling_Approach_model.R")
      
      ######take measurements of model#####
      #APika_sample=APika #for long sample
      #APika_sample[which(is.na(NA_matrix[1])),]=NA #for long sample
      
      #different measurements to take depending on if we start with 1991 or 1972 initial conditions (we use 1991 for inverse modeling approach, 1972 elsewhere)
      if (sum(IC==IC1991)==79){
        APika_sample=NA_matrix[,20:39]*APika #for 19 year model
        APika_sample=APika_sample[,-c(12,17)] # for 19 year model
        trial_error[,k]=sum((colSums(sampled_census_bodie[,4:21],na.rm=T) - colSums(APika_sample,na.rm=T))^2)
        #trial_error[,k]=sum((colSums(sampled_census_bodie[,seq(4,21,by=2)],na.rm=T) - colSums(APika_sample[,seq(1,17,by=2)],na.rm=T))^2) #use training set from 1991 onward
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
      
      #Currently this code is set of to count extinction and recolonization events only since 1991
      ext_events = matrix(0,nrow=1,ncol=ncol(APika_sample)-1)
      recol_events = matrix(0,nrow=1,ncol=ncol(APika_sample)-1)
      for (j in 1:(ncol(APika_sample)-1)){
        ext_events[,j] = sum(APika_sample[,j]>0 & APika_sample[,j+1]==0,na.rm=T)
        recol_events[,j] = sum(APika_sample[,j]==0 & APika_sample[,j+1]>0,na.rm=T)
      }
      # 
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

#if (IM %in% seq(1,7000,by=100)){print(paste('value',IM,sep=''))} #a simple counter
  pb$tick()

}


trial_trial_error=c(trial_trial_error)

model_outputs = as.data.frame(cbind(d_m_vector,weaning_m_vector,trial_trial_error))
model_outputs = model_outputs[order(model_outputs$trial_trial_error),]

model_outputs$trial_trial_error_ranking = 1:nrow(model_outputs)

model_outputs$total_survival = 0.63*(0.25*((1-model_outputs$d_m_vector)*(1-model_outputs$weaning_m_vector)) + 0.75*(1-model_outputs$weaning_m_vector))
model_outputs$total_survival_ranking = 1:nrow(model_outputs)
bodie_first_year_total_mortality = 0.889
model_outputs$survival_error = (model_outputs$total_survival- (1-bodie_first_year_total_mortality))^2
#model_outputs = model_outputs[order(model_outputs$survival_error),]
#model_outputs$survival_error_ranking = 1:nrow(model_outputs)

#model_outputs$master_ranking = model_outputs$survival_error_ranking+model_outputs$trial_trial_error_ranking


#save(IC,trials,d_m_vector,weaning_m_vector,model_outputs,trial_trial_mean,trial_trial_mean_sd,trial_trial_variance,trial_trial_ext_year,trial_trial_occupancy,trial_trial_occupancy_sd,trial_trial_ext_events,trial_trial_recol_events,trial_trial_error,file='Model_Outputs/inverse_modeling_50trials.Rdata')
