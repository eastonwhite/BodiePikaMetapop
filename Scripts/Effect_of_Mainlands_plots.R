#Easton R. White 
#created 21-Mar-2016
#Last edited 21-Mar-2016


#Plot for Mainland island removal experiment
par(mfrow=c(2,2),mar=c(3,5,1,1),oma=c(2,0.5,0,0))
boxplot(c(trial_mean),c(two_trial_mean),xlab=' ',las=1,ylab=' ',outline=T,medlwd=0.7,outpch=20,boxlty=0,outcol=rgb(0.3,0.3,0.3,0.5),notch=F,at=1:2,col=c(rgb(0.3,0.3,0.3,1)),border='black',ylim=c(0,180))

text(x=c(1,2), y=par()$usr[3]-0.05*(par()$usr[4]-par()$usr[3]),xpd=T,labels=c('Mainland', "No mainland"),cex=1)

mtext('Mean population size',side=2,line=3,outer=F,cex=1)
#mtext('Model setup',side=1,line=0.5,outer=T,at =0.55)
abline(h=87,lty=2,lwd=2)


boxplot(c(trial_variance),c(two_trial_variance),xlab=' ',las=1,ylab=' ', outline=T, medlwd=0.7,outpch=20,boxlty=0, outcol=rgb(0.3,0.3,0.3,0.5),notch=F,at=1:2,col=c(rgb(0.3,0.3,0.3,1)),border='black',ylim=c(0,2500))

text(x=c(1,2), y=par()$usr[3]-0.05*(par()$usr[4]-par()$usr[3]),xpd=T,labels=c('Mainland', "No mainland"),cex=1)


#legend('topleft',legend=c('Patch heterogeneity','Patch Homogeneity'), col=c(rgb(0.3,0.3,0.3,1)),pch=15)

mtext('Variance in pop. size',side=2,line=3.8,outer=F)
#mtext('Model setup',side=1,line=0.5,outer=T,at =0.55)
abline(h=860,lty=2,lwd=2)



boxplot(c(trial_occupancy),c(two_trial_occupancy),xlab=' ',las=1,ylab=' ',outline=T, medlwd=0.7,outpch=20, boxlty=0,outcol=rgb(0.3,0.3,0.3,0.5), notch=F,at=1:2,col=c(rgb(0.3,0.3,0.3,1)),border='black',ylim=c(0,1))

text(x=c(1,2), y=par()$usr[3]-0.05*(par()$usr[4]-par()$usr[3]),xpd=T,labels=c('Mainland', "No mainland"),cex=1)


#legend('topleft',legend=c('Patch heterogeneity','Patch homogeneity'), col=c(rgb(0.3,0.3,0.3,1)),pch=15)

#legend('topleft',legend=c('Patch heterogeneity','Patch homogeneity'),col=c(rgb(0.3,0.3,0.3,1),
#                                                                          rgb(0.7,0.7,0.7,1)),pch=15)

mtext('Patch occupancy',side=2,line=3,outer=F)
#mtext('Model setup',side=1,line=0.5,outer=T,at =0.55)
abline(h=0.38,lty=2,lwd=2)



boxplot(c(trial_ext_year),c(two_trial_ext_year),
        xlab=' ',las=1,ylab=' ',outline=T,medlwd=0.7,outpch=20,boxlty=0,outcol=rgb(0.3,0.3,0.3,0.5),notch=F,at=1:2,col=c(rgb(0.3,0.3,0.3,1)),border='black',ylim=c(1970,2080))

text(x=c(1,2), y=par()$usr[3]-0.05*(par()$usr[4]-par()$usr[3]),xpd=T,labels=c('Mainland', "No mainland"),cex=1)




#legend('topleft',legend=c('Patch heterogeneity','Patch homogeneity'),col=c(rgb(0.3,0.3,0.3,1),
#                                                                          rgb(0.7,0.7,0.7,1)),pch=15)

mtext('Year of southern collapse',side=2,line=3.8,outer=F)
mtext('Model setup',side=1,line=0.5,outer=T,at =0.55)
abline(h=1990,lty=2,lwd=2)



