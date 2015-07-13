#Easton R. White 
#plot northern and southern Bodie populations from simulation output
#created 22-June-2015
#Last edited 13-Jul-2015


par(mfrow=c(2,1),mgp=c(2.2,0.45,0), tcl=-0.4, mar=c(3.3,3.6,1.1,1.1))
plot(1972:2009,census_north/2,ylim=c(0,100),pch=16,las=1,
     ylab='Population size (number of females)',xlab='Time (years)')


  for (j in 11:20){
    APika=trial_pop[[j]]
    points(names(APika),colSums(APika[c(38:56,59:63,66:79),],na.rm=T),pch=16,col='grey')
  }

points(1972:2009,census_north/2,ylim=c(0,100),pch=16)

plot(1972:2009,census_south/2,ylim=c(0,30),pch=16,las=1,
     ylab='Population size (number of females)',xlab='Time (years)')


  for (j in 1:20){
    APika=trial_pop[[j]]
    points(names(APika),colSums(APika[c(1:19,21:37,57,58),],na.rm=T),pch=16,col='grey')
  }

points(1972:2009,census_south/2,ylim=c(0,100),pch=16,type='l')

abline(h=7,lty=2,lwd=3)
