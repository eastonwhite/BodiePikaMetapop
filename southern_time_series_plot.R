#Easton R. White 
#created 22-June-2015
#Last edited 28-Jun-2015

#alternative time series plot where patches not sampled in the field are also NOT sampled in simulation
census_bodie = read.table('/Users/easton2/Desktop/Research/Nagy Lab/Pikas/Census data/bodiedata.txt',header=T)
NA_matrix=census_bodie
NA_matrix[is.na(NA_matrix)==F]=1

YEARS=c(1972,1977,1989,1991:2001,2003,2004,2005,2006,2008,2009)

South_pop=matrix(0,nrow=trials,ncol=length(YEARS))

for (j in 2:trials){
  APika=trial_pop[[j]]
  census_sim_years=APika[,c(1,6,18,20:30,32:35,37,38)]
  census_sim_like_field=census_sim_years*NA_matrix

    South_pop[j,1:length(YEARS)]=as.numeric(colSums(census_sim_like_field[c(1:19,21:37,57,58),],na.rm=T))
    points(YEARS,as.numeric(colSums(census_sim_like_field[c(1:19,21:37,57,58),],na.rm=T)),ylim=c(0,40),type='l',col=rgb(96,96,96,alpha=200,max=255),las=1,xlim=c(1970,2010),ylab='Population size',xlab='Time (years)')
}

points(YEARS,colSums(census_bodie[c(1:19,21:37,57,58),]/2,na.rm=T),ylim=c(0,30),col='black',type='o',lwd=2)
abline(h=8,lwd=2,lty=2)

#add average of all runs
points(YEARS,colMeans(South_pop[1:trials,]),col='grey',lwd=5,type='l')

