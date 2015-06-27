#Easton R. White 
#plot northern and southern Bodie populations from simulation output
#created 22-June-2015
#Last edited 28-Jun-2015

#call in actual census data (need to modify to account for patches not sampled every year)
census_bodie = read.table('/Users/easton2/Desktop/Research/Nagy Lab/Pikas/Census data/bodiedata.txt',header=T)
census_north=(colSums(census_bodie[c(38:56,59:63,66:79),],na.rm=T))
census_south=colSums(census_bodie[c(1:19,21:37,57,58),],na.rm=T)

#time series North and South

for (j in 2:trials){
  APika=trial_pop[[j]]
  North=colSums(APika[c(38:56,59:63,66:79),],na.rm=T)
  South=colSums(APika[c(1:19,21:37,57,58),])
  
  points((1972:(1971+max.time)),North,ylim=c(0,max(North+10)),type='l',ylab='Population size (number of females)',xlab='Time (years)',col='grey')
  points((1972:(1971+max.time)),South,type='l',ylab='Population size (number of females)',xlab='Time (years)',col='grey')
}

abline(h=8,lty=2,lwd=3)
years=c(1,6,18,20:30,32:35,37,38)
points(years+1971,census_south/2,col='black',lwd=2,pch='*',type='l')
points(years+1971,census_north/2,col='black',lwd=2,pch='*',type='l')
