#Run four different model setups for comparison

##PULL in neccessary stuffs##

sampled_census_bodie = read.table('sampled_census_bodie.txt',header=T,sep=' ')
names(sampled_census_bodie) <- c(1972,1977,1989,1991,1992,1993,1994,1995,1996,1997,1998,1999,2000,2001,2003,2004,2005, 2006,2008,2009)
NA_matrix = read.table('NA_matrix.txt',header=T,sep=' ')
names(NA_matrix) <- c(1972,1977,1989,1991,1992,1993,1994,1995,1996,1997,1998,1999,2000,2001,2003,2004,2005, 2006,2008,2009)

###########################
#Standard model with Bodie **spatial strucute + patch hetereogeneity**
#creates matrix of patchs within specified distance of each other (300m is default dispersal distance)
main<-read.table('patchdistance.txt')
main=main[1:79,1:79]
main[main<=300]=1
main[main>300]=0
main=as.matrix(main)
diag(main)=0 #makes it so pikas cannot disperse back to their own natal patch

#Bodie 1972 conditions
territories=c(6,5,3,7,3,4,3,4,5,5,5,3,3,4,3,3,4,4,3,3,
              5,5,3,3,3,4,4,3,3,2,3,4,3,3,4,3,5,5,9,3,5,
              5,4,14,4,5,5,7,13,4,3,3,3,4,2,5,11,12,3,13,
              14,13,7,15,50,3,12,7,4,4,3,5,16,10,3,4,11,12,3)

#Initial population size data from spreadsheet 'MeasurementsCompleteBodieCensusData1972-2009_EW'
#fill in NAs
IC1972=c(0,3,1,3,0,4,2,0,3,4,4,1,0,0,0,2,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,4,0,5,3,4,4,3,10,1,1,1,3,4,1,1,0,1,1,1,2,7,9,1,6,7,6,2,9,50,1,3,2,0,0,0,0,8,5,1,1,6,2,2)



trials=1000
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
  source("~/Desktop/Research/Nagy Lab/Pikas/Modeling/SimpleBodieModel/SimpleBodieModel_model.R")
  
  ######take measurements of model#####
  #APika_sample=APika #for long sample
  #APika_sample[which(is.na(NA_matrix[1])),]=NA #for long sample
  
  #different measurements to take depending on if we start with 1991 or 1972 initial conditions (we use 1991 for inverse modeling approach, 1972 elsewhere)
  if (sum(IC==IC1991)==79){
    APika_sample=NA_matrix[,20:38]*APika #for 19 year model
    APika_sample=APika_sample[,-c(12,17)] # for 19 year model
    trial_error[,k]=sum((colSums(sampled_census_bodie[,4:20],na.rm=T) - colSums(APika_sample,na.rm=T))^2)
  }else if(sum(IC==IC1972)==79){
    APika_sample=NA_matrix*APika
    APika_sample=APika_sample[,-c(2:5,7:17,19,31,36)]
    trial_error[,k]=sum((colSums(sampled_census_bodie[,1:20],na.rm=T) - colSums(APika_sample,na.rm=T))^2)
  }
  #########
  
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
  
  #if (k %in% seq(50,1000,by=50)) print(k)  #a simple counter
}



#####################################################################
#####################################################################
#####################################################################
#####################################################################

###########################
#Standard model with Bodie spatial strucute + **patch homogeneity**
#creates matrix of patchs within specified distance of each other (300m is default dispersal distance)
main<-read.table('patchdistance.txt')
main=main[1:79,1:79]
main[main<=300]=1
main[main>300]=0
main=as.matrix(main)
diag(main)=0 #makes it so pikas cannot disperse back to their own natal patch

#Bodie 1972 conditions
territories=rep(6,times=79)

#Initial population size data from spreadsheet 'MeasurementsCompleteBodieCensusData1972-2009_EW'
#fill in NAs
IC1972=sample(c(2:3),size=79,replace=T)
trials=1000
IC=IC1972 #set initial conditions (1972 data by default)

two_trial_mean=matrix(0,nrow=1,ncol=trials)
two_trial_mean_sd=matrix(0,nrow=1,ncol=trials)
two_trial_variance= matrix(0,nrow=1,ncol=trials)
two_trial_ext_year=matrix(0,nrow=1,ncol=trials)
two_trial_occupancy=matrix(0,nrow=1,ncol=trials)
two_trial_occupancy_sd=matrix(0,nrow=1,ncol=trials)
two_trial_ext_events=matrix(0,nrow=1,ncol=trials)
two_trial_recol_events=matrix(0,nrow=1,ncol=trials)
two_trial_error=matrix(0,nrow=1,ncol=trials)
two_trial_pop=list()
two_trial_sampled_pop=list()

#Run model numerous times
for (k in 1:trials){
  source("~/Desktop/Research/Nagy Lab/Pikas/Modeling/SimpleBodieModel/SimpleBodieModel_model.R")
  
  ######take measurements of model#####
  #APika_sample=APika #for long sample
  #APika_sample[which(is.na(NA_matrix[1])),]=NA #for long sample
  
  #different measurements to take depending on if we start with 1991 or 1972 initial conditions (we use 1991 for inverse modeling approach, 1972 elsewhere)
  if (sum(IC==IC1991)==79){
    APika_sample=NA_matrix[,20:38]*APika #for 19 year model
    APika_sample=APika_sample[,-c(12,17)] # for 19 year model
    two_trial_error[,k]=sum((colSums(sampled_census_bodie[,4:20],na.rm=T) - colSums(APika_sample,na.rm=T))^2)
  }else if(sum(IC==IC1972)==79){
    APika_sample=NA_matrix*APika
    APika_sample=APika_sample[,-c(2:5,7:17,19,31,36)]
    two_trial_error[,k]=sum((colSums(sampled_census_bodie[,1:20],na.rm=T) - colSums(APika_sample,na.rm=T))^2)
  }
  
  two_trial_mean[,k]=mean(colSums(APika_sample,na.rm=T))
  two_trial_mean_sd[,k]=sd(colSums(APika_sample,na.rm=T))
  two_trial_variance[,k]=var(colSums(APika_sample,na.rm=T))
  two_trial_ext_year[,k]=(1971 + which((colSums(APika[c(2:19,21:37,57,58),]))<14)[1])
  two_trial_occupancy[,k]=mean(colSums(APika_sample[1:79,]>0,na.rm=T)/66)
  two_trial_occupancy_sd[,k]=sd(colSums(APika_sample[1:79,]>0,na.rm=T)/66)
  two_trial_sampled_pop[[k]]=APika_sample
  two_trial_pop[[k]]=APika
  
  
  ext_events = matrix(0,nrow=1,ncol=17-1)
  recol_events = matrix(0,nrow=1,ncol=17-1)
  for (j in 1:(17-1)){
    ext_events[,j] = sum(APika_sample[,j]>0 & APika_sample[,j+1]==0,na.rm=T)
    recol_events[,j] = sum(APika_sample[,j]==0 & APika_sample[,j+1]>0,na.rm=T)
  }
  
  two_trial_ext_events[,k]=sum(ext_events)
  two_trial_recol_events[,k]=sum(recol_events)	
  
  #if (k %in% seq(50,1000,by=50)) print(k)  #a simple counter
}



#####################################################################
#####################################################################
#####################################################################
#####################################################################

###########################
#Standard model with **global dispersal + patch heterogeneity**
#creates matrix of patchs within specified distance of each other (300m is default dispersal distance)
main<-read.table('patchdistance.txt')
main=main[1:79,1:79]
main[main<=3000]=1
main[main>3000]=0
main=as.matrix(main)
diag(main)=0 #makes it so pikas cannot disperse back to their own natal patch

#Bodie 1972 conditions
territories=c(6,5,3,7,3,4,3,4,5,5,5,3,3,4,3,3,4,4,3,3,
              5,5,3,3,3,4,4,3,3,2,3,4,3,3,4,3,5,5,9,3,5,
              5,4,14,4,5,5,7,13,4,3,3,3,4,2,5,11,12,3,13,
              14,13,7,15,50,3,12,7,4,4,3,5,16,10,3,4,11,12,3)

#Initial population size data from spreadsheet 'MeasurementsCompleteBodieCensusData1972-2009_EW'
#fill in NAs
IC1972=c(0,3,1,3,0,4,2,0,3,4,4,1,0,0,0,2,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,4,0,5,3,4,4,3,10,1,1,1,3,4,1,1,0,1,1,1,2,7,9,1,6,7,6,2,9,50,1,3,2,0,0,0,0,8,5,1,1,6,2,2)

trials=1000
IC=IC1972 #set initial conditions (1972 data by default)

three_trial_mean=matrix(0,nrow=1,ncol=trials)
three_trial_mean_sd=matrix(0,nrow=1,ncol=trials)
three_trial_variance= matrix(0,nrow=1,ncol=trials)
three_trial_ext_year=matrix(0,nrow=1,ncol=trials)
three_trial_occupancy=matrix(0,nrow=1,ncol=trials)
three_trial_occupancy_sd=matrix(0,nrow=1,ncol=trials)
three_trial_ext_events=matrix(0,nrow=1,ncol=trials)
three_trial_recol_events=matrix(0,nrow=1,ncol=trials)
three_trial_error=matrix(0,nrow=1,ncol=trials)
three_trial_pop=list()
three_trial_sampled_pop=list()

#Run model numerous times
for (k in 1:trials){
  source("~/Desktop/Research/Nagy Lab/Pikas/Modeling/SimpleBodieModel/SimpleBodieModel_model.R")
  
  ######take measurements of model#####
  #APika_sample=APika #for long sample
  #APika_sample[which(is.na(NA_matrix[1])),]=NA #for long sample
  
  #different measurements to take depending on if we start with 1991 or 1972 initial conditions (we use 1991 for inverse modeling approach, 1972 elsewhere)
  if (sum(IC==IC1991)==79){
    APika_sample=NA_matrix[,20:38]*APika #for 19 year model
    APika_sample=APika_sample[,-c(12,17)] # for 19 year model
    three_trial_error[,k]=sum((colSums(sampled_census_bodie[,4:20],na.rm=T) - colSums(APika_sample,na.rm=T))^2)
  }else if(sum(IC==IC1972)==79){
    APika_sample=NA_matrix*APika
    APika_sample=APika_sample[,-c(2:5,7:17,19,31,36)]
    three_trial_error[,k]=sum((colSums(sampled_census_bodie[,1:20],na.rm=T) - colSums(APika_sample,na.rm=T))^2)
  }
  
  three_trial_mean[,k]=mean(colSums(APika_sample,na.rm=T))
  three_trial_mean_sd[,k]=sd(colSums(APika_sample,na.rm=T))
  three_trial_variance[,k]=var(colSums(APika_sample,na.rm=T))
  three_trial_ext_year[,k]=(1971 + which((colSums(APika[c(2:19,21:37,57,58),]))<14)[1])
  three_trial_occupancy[,k]=mean(colSums(APika_sample[1:79,]>0,na.rm=T)/66)
  three_trial_occupancy_sd[,k]=sd(colSums(APika_sample[1:79,]>0,na.rm=T)/66)
  three_trial_sampled_pop[[k]]=APika_sample
  three_trial_pop[[k]]=APika
  
  
  ext_events = matrix(0,nrow=1,ncol=17-1)
  recol_events = matrix(0,nrow=1,ncol=17-1)
  for (j in 1:(17-1)){
    ext_events[,j] = sum(APika_sample[,j]>0 & APika_sample[,j+1]==0,na.rm=T)
    recol_events[,j] = sum(APika_sample[,j]==0 & APika_sample[,j+1]>0,na.rm=T)
  }
  
  three_trial_ext_events[,k]=sum(ext_events)
  three_trial_recol_events[,k]=sum(recol_events)	
  
  #if (k %in% seq(50,1000,by=50)) print(k)  #a simple counter
}



#####################################################################
#####################################################################
#####################################################################
#####################################################################

###########################
#Standard model with global dispersal + patch homogeneity
#creates matrix of patchs within specified distance of each other (300m is default dispersal distance)
main<-read.table('patchdistance.txt')
main=main[1:79,1:79]
main[main<=3000]=1
main[main>3000]=0
main=as.matrix(main)
diag(main)=0 #makes it so pikas cannot disperse back to their own natal patch

#Bodie 1972 conditions
#Bodie 1972 conditions
territories=rep(6,times=79)

#Initial population size data from spreadsheet 'MeasurementsCompleteBodieCensusData1972-2009_EW'
#fill in NAs
IC1972=sample(c(2:3),size=79,replace=T)
trials=1000
IC=IC1972 #set initial conditions (1972 data by default)

four_trial_mean=matrix(0,nrow=1,ncol=trials)
four_trial_mean_sd=matrix(0,nrow=1,ncol=trials)
four_trial_variance= matrix(0,nrow=1,ncol=trials)
four_trial_ext_year=matrix(0,nrow=1,ncol=trials)
four_trial_occupancy=matrix(0,nrow=1,ncol=trials)
four_trial_occupancy_sd=matrix(0,nrow=1,ncol=trials)
four_trial_ext_events=matrix(0,nrow=1,ncol=trials)
four_trial_recol_events=matrix(0,nrow=1,ncol=trials)
four_trial_error=matrix(0,nrow=1,ncol=trials)
four_trial_pop=list()
four_trial_sampled_pop=list()

#Run model numerous times
for (k in 1:trials){
  source("~/Desktop/Research/Nagy Lab/Pikas/Modeling/SimpleBodieModel/SimpleBodieModel_model.R")
  
  ######take measurements of model#####
  #APika_sample=APika #for long sample
  #APika_sample[which(is.na(NA_matrix[1])),]=NA #for long sample
  
  #different measurements to take depending on if we start with 1991 or 1972 initial conditions (we use 1991 for inverse modeling approach, 1972 elsewhere)
  if (sum(IC==IC1991)==79){
    APika_sample=NA_matrix[,20:38]*APika #for 19 year model
    APika_sample=APika_sample[,-c(12,17)] # for 19 year model
    four_trial_error[,k]=sum((colSums(sampled_census_bodie[,4:20],na.rm=T) - colSums(APika_sample,na.rm=T))^2)
  }else if(sum(IC==IC1972)==79){
    APika_sample=NA_matrix*APika
    APika_sample=APika_sample[,-c(2:5,7:17,19,31,36)]
    four_trial_error[,k]=sum((colSums(sampled_census_bodie[,1:20],na.rm=T) - colSums(APika_sample,na.rm=T))^2)
  }
  
  four_trial_mean[,k]=mean(colSums(APika_sample,na.rm=T))
  four_trial_mean_sd[,k]=sd(colSums(APika_sample,na.rm=T))
  four_trial_variance[,k]=var(colSums(APika_sample,na.rm=T))
  four_trial_ext_year[,k]=(1971 + which((colSums(APika[c(2:19,21:37,57,58),]))<14)[1])
  four_trial_occupancy[,k]=mean(colSums(APika_sample[1:79,]>0,na.rm=T)/66)
  four_trial_occupancy_sd[,k]=sd(colSums(APika_sample[1:79,]>0,na.rm=T)/66)
  four_trial_sampled_pop[[k]]=APika_sample
  four_trial_pop[[k]]=APika
  
  
  ext_events = matrix(0,nrow=1,ncol=17-1)
  recol_events = matrix(0,nrow=1,ncol=17-1)
  for (j in 1:(17-1)){
    ext_events[,j] = sum(APika_sample[,j]>0 & APika_sample[,j+1]==0,na.rm=T)
    recol_events[,j] = sum(APika_sample[,j]==0 & APika_sample[,j+1]>0,na.rm=T)
  }
  
  four_trial_ext_events[,k]=sum(ext_events)
  four_trial_recol_events[,k]=sum(recol_events)	
  
  if (k %in% seq(50,1000,by=50)) print(k)  #a simple counter
}


save(IC,trials,r,d_m,u,d_prop,max.time,trial_mean,trial_mean_sd,trial_variance,trial_ext_year,
     trial_occupancy,trial_occupancy_sd,trial_ext_events,trial_recol_events,
     trial_error,trial_sampled_pop,trial_pop,two_trial_mean,two_trial_mean_sd,two_trial_variance,two_trial_ext_year,
          two_trial_occupancy,two_trial_occupancy_sd,two_trial_ext_events,two_trial_recol_events,
          two_trial_error,two_trial_sampled_pop,two_trial_pop,three_trial_mean,three_trial_mean_sd,three_trial_variance,three_trial_ext_year,
          three_trial_occupancy,three_trial_occupancy_sd,three_trial_ext_events,three_trial_recol_events,
          three_trial_error,three_trial_sampled_pop,three_trial_pop,four_trial_mean,four_trial_mean_sd,four_trial_variance,four_trial_ext_year,
          four_trial_occupancy,four_trial_occupancy_sd,four_trial_ext_events,four_trial_recol_events,
          four_trial_error,four_trial_sampled_pop,four_trial_pop,file='FourModelExperiment_1000trials500years.Rdata')


#####################################################################
#####################################################################
#####################################################################
#####################################################################