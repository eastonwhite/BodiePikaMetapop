#Easton R. White 
#created 21-Mar-2016
#Last edited 21-Mar-2016


#Plot for invasion of whole study site by South, simply shows long time series

#I used 3 panel plot in situation to show importance of dispersal radius (long-distance dispersal)
#par(mfrow=c(3,1),mar=c(3,5,1,1),oma=c(2,2,0,0))

par(mfrow=c(3,1),mar=c(3,5,1,1),oma=c(2,2,0,0))

#300 meter plot
for (dispersal_radius_vector in c(300,500,1000)){
  
  dispersal_radius = dispersal_radius_vector
  
  source('Scripts/MovementFromSouth_wrapper.R')
  total_pop=matrix(0,nrow=trials,ncol=max.time)
  plot(1900:(max.time+1899),colSums(trial_pop[[j]]),col=rgb(0.5,0.5,0.5,alpha=0.5),type='l',ylim=c(0,100),ylab=' ',xlab=' ',las=1,cex.axis=1.2)
  for (j in 1:trials){
    points(1900:(max.time+1899),colSums(trial_pop[[j]][1:79,]),col=rgb(0.5,0.5,0.5,alpha=0.5),type='l')
    total_pop[j,]=colSums(trial_pop[[j]])
  }
  points(1900:(max.time+1899),colMeans(total_pop),type='l',lwd=2)
  mtext(paste('max dispersal distance = ',dispersal_radius, 'meters'),3,line=-2)
}

mtext('Total population size',2,outer=T,cex=1.2)
mtext('Year',side = 1,outer=F,cex=1.2,padj =0.5,line=2.5)
