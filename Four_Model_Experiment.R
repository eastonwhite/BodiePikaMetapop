#Run four different model setups for comparison

###########################
#Standard model with Bodie spatial strucute + patch hetereogeneity

trials=10

#Bodie 1972 conditions- should see if this is accurate given Bodie data 
TC=c(2,2,2,4,1,2,1,1,2,2,2,1,1,1,1,2,1,1,1,2,1,1,1,1,1,1,2,2,1,1,1,1,1,1,1,1,2,2,5,2,3,3,3,7,1,3,2,3,6,2,1,1,2,2,1,2,5,5,3,7,7,7,3,8,25,2,5,3,2,2,1,4,8,5,2,2,6,6,1)
IC=c(1,2,1,2,1,1,1,0,1,2,2,1,0,0,0,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,2,0,1,1,2,2,1,5,1,1,1,2,2,1,1,0,1,1,1,1,3,4,1,3,3,3,1,5,20,1,1,1,0,0,0,0,4,3,1,1,3,1,1)
#IC=c(0,0,0,0,2,0,0,0,0,2,0,0,0,0,0,2,0,0,0,0,2,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)

trial_mean=matrix(0,nrow=1,ncol=trials)
trial_variance= matrix(0,nrow=1,ncol=trials)
trial_ext_year=matrix(0,nrow=1,ncol=trials)
trial_occupancy=matrix(0,nrow=1,ncol=trials)
trial_pop=list()

#creates matrix of patchs within specified distance of each other
main<-read.table('patchdistance.txt')
attach(main)
main=main[1:79,1:79]
main[main<=300]=1
main[main>300]=0

#Run model numerous times
for (k in 1:trials){
	source("~/Desktop/Research/Nagy Lab/Pikas/Modeling/SimpleBodieModel/SimpleBodieModel_model.R")
	trial_mean[,k]=mean(colSums(APika))
	trial_variance[,k]=var(colSums(APika))
	trial_ext_year[,k]=(1971 + which((2*colSums(APika[c(1:19,21:23,57,58),]))<7)[1])
	trial_occupancy[,k]=mean(colSums(APika>0)/79)
	trial_pop[[k]]=APika
print(k)
}


#Model with Bodie spatial strucuture + patch homogeneity
#Bodie 1972 conditions- should see if this is accurate given Bodie data 
TC=rep(3,times=79)
IC=sample(0:3,79,replace=T)
#IC=c(0,0,0,0,2,0,0,0,0,2,0,0,0,0,0,2,0,0,0,0,2,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)

two_trial_mean=matrix(0,nrow=1,ncol=trials)
two_trial_variance= matrix(0,nrow=1,ncol=trials)
two_trial_ext_year=matrix(0,nrow=1,ncol=trials)
two_trial_occupancy=matrix(0,nrow=1,ncol=trials)
two_trial_pop=list()

#creates matrix of patchs within specified distance of each other
main<-read.table('patchdistance.txt')
attach(main)
main=main[1:79,1:79]
main[main<=300]=1
main[main>300]=0

#Run model numerous times
for (k in 1:trials){
	source("~/Desktop/Research/Nagy Lab/Pikas/Modeling/SimpleBodieModel/SimpleBodieModel_model.R")
	two_trial_mean[,k]=mean(colSums(APika))
	two_trial_variance[,k]=var(colSums(APika))
	two_trial_ext_year[,k]=(1971 + which((2*colSums(APika[c(1:19,21:23,57,58),]))<7)[1])
	two_trial_occupancy[,k]=mean(colSums(APika>0)/79)
	two_trial_pop[[k]]=APika
print(k)
}




#Model with no spatial structure + patch heterogeneity
TC=c(2,2,2,4,1,2,1,1,2,2,2,1,1,1,1,2,1,1,1,2,1,1,1,1,1,1,2,2,1,1,1,1,1,1,1,1,2,2,5,2,3,3,3,7,1,3,2,3,6,2,1,1,2,2,1,2,5,5,3,7,7,7,3,8,25,2,5,3,2,2,1,4,8,5,2,2,6,6,1)
IC=c(1,2,1,2,1,1,1,0,1,2,2,1,0,0,0,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,2,0,1,1,2,2,1,5,1,1,1,2,2,1,1,0,1,1,1,1,3,4,1,3,3,3,1,5,20,1,1,1,0,0,0,0,4,3,1,1,3,1,1)
#IC=c(0,0,0,0,2,0,0,0,0,2,0,0,0,0,0,2,0,0,0,0,2,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)

three_trial_mean=matrix(0,nrow=1,ncol=trials)
three_trial_variance= matrix(0,nrow=1,ncol=trials)
three_trial_ext_year=matrix(0,nrow=1,ncol=trials)
three_trial_occupancy=matrix(0,nrow=1,ncol=trials)
three_trial_pop=list()

#creates matrix of patchs within specified distance of each other
main<-read.table('patchdistance.txt')
attach(main)
main=main[1:79,1:79]
main[main<=3000]=1
main[main>3000]=0

#Run model numerous times
for (k in 1:trials){
	source("~/Desktop/Research/Nagy Lab/Pikas/Modeling/SimpleBodieModel/SimpleBodieModel_model.R")
	three_trial_mean[,k]=mean(colSums(APika))
	three_trial_variance[,k]=var(colSums(APika))
	three_trial_ext_year[,k]=(1971 + which((2*colSums(APika[c(1:19,21:23,57,58),]))<7)[1])
	three_trial_occupancy[,k]=mean(colSums(APika>0)/79)
	three_trial_pop[[k]]=APika
print(k)
}



#Model with no spatial structure + patch homogeneity
TC=rep(3,times=79)
IC=sample(0:3,79,replace=T)
#IC=c(0,0,0,0,2,0,0,0,0,2,0,0,0,0,0,2,0,0,0,0,2,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)

four_trial_mean=matrix(0,nrow=1,ncol=trials)
four_trial_variance= matrix(0,nrow=1,ncol=trials)
four_trial_ext_year=matrix(0,nrow=1,ncol=trials)
four_trial_occupancy=matrix(0,nrow=1,ncol=trials)
four_trial_pop=list()

#creates matrix of patchs within specified distance of each other
main<-read.table('patchdistance.txt')
attach(main)
main=main[1:79,1:79]
main[main<=3000]=1
main[main>3000]=0

#Run model numerous times
for (k in 1:trials){
	source("~/Desktop/Research/Nagy Lab/Pikas/Modeling/SimpleBodieModel/SimpleBodieModel_model.R")
	four_trial_mean[,k]=mean(colSums(APika))
	four_trial_variance[,k]=var(colSums(APika))
	four_trial_ext_year[,k]=(1971 + which((2*colSums(APika[c(1:19,21:23,57,58),]))<7)[1])
	four_trial_occupancy[,k]=mean(colSums(APika>0)/79)
	four_trial_pop[[k]]=APika
print(k)
}