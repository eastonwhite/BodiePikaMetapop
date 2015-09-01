#Easton R. White 
#Bodie Pika Dispersal Model parameter sensitivity model script (almost same as regular model script but parameters are commented out)
#created 22-June-2012
#Last edited 1-Sep-2015

#particular code gives births to new pikas and splilts them into groups, some automatically disperse. Others stay are patch, compete and get territory or die

#time for model to run
max.time= 38#38#19 #38 years to match bodie census length to 2009

#matrixes for adult and juvenile pika, and for territory count
APika = matrix(IC,nrow= 79,ncol=max.time)
JPika = matrix(0,nrow= 79,ncol=max.time)
DJPika = matrix(0,nrow= 79,ncol=1)

#Model parameters- commented out for sensitivity analysis
#r= 3.25       #1.625 (3.25) Smith: Ecology 1974a, we are currently using actual litter size data
#u= 0.37         #0.37 Smith: Ecology 1974a, 1978
#d_m=0.7        # from this paper
#d_prop = 0.25   #Nagy unpublished, Smith 1987
#weaning_m=0.5  #value from this paper, Millar 1973 found a value of 0.21, but this was in Alberta


##########################################################
##########################################################
for (j in 2:(max.time)){
  for (i in 1:79){
    
    #over winter mortality function
    APika[i,j] = APika[i,j-1] - rbinom(1,APika[i,j-1],u) #adult mortality rate to correspond with the census occuring before winter
    
    #birth functions
    reproducers=round(APika[i,j]/2 + sample(c(-0.1,0.1),size=1)) #the bizarre command inside the rpois function simply allows for a patch with 2.5 females to be 2 or 3 females 
    #JPika[i,j] = sum(rpois(reproducers, r))  #this command produces juveniles but uses a Poisson distribution instead of actual litter size data
    JPika[i,j] = sum(sample(1:5,reproducers,replace = TRUE,prob =c(0.02326, 0.2093, 0.3256, 0.3256, 0.1163) ) ) #this command samples litter sizes from Smith 1978 and Tapper 1955
    JPika[i,j]= JPika[i,j] - rbinom(1,JPika[i,j],weaning_m) #0.21 from Millar 1973 for pikas in Alberta
    
    #dispersal process
    DJPika[i,1] = rbinom(1,JPika[i,j],d_prop) #subset of pikas (DJPika) automatically disperse
    JPika[i,j] = JPika[i,j]- DJPika[i,1]
    
    if (JPika[i,j] < territories[i] - APika[i,j]){
      APika[i,j] = APika[i,j] + JPika[i,j]
      JPika[i,j] = 0
    }else if (JPika[i,j] == territories[i] - APika[i,j]){
      APika[i,j] = APika[i, j] + JPika[i,j]
      JPika[i,j] = 0
    }else{
      APika[i,j] = territories[i] 
      JPika[i,j] = JPika[i,j] - (territories[i] - APika[i, j])   
    }
    JPika[i,j]=0
    #DJPika[i,1]= JPika[i,j]+DJPika[i,1] #alternative model where juveniles without a territory on natal patch also disperse (not used in paper)
    DJPika[i,1]= DJPika[i,1]- rbinom(1,DJPika[i,1],d_m)  #dispersal mortality function
  }
  
  #call to randomly take DJPika (without territories) and send them to nearby patches
  #This code chuck ignores contribution from isolated patches to rest of populatiton (it is too far from any other patch)
  DOP=matrix(0,nrow=79,ncol=79)
  for (g in 1:79){
    if (sum(main[g,1:79])>0){
      DOP[1:79,g]=rmultinom(1,DJPika[g,1],prob=main[g,1:79])
    }else{
      DOP[1:79,g]=0
    }
  }
  DOP1=rowSums(DOP)
  
  #call allows JPika that dispersed to new patches to acquire territories  
  for (q in 1:79){
    JPika[1:79,j]=DOP1
    
    if (JPika[q,j] < territories[q] - APika[q,j]){
      APika[q,j] = APika[q, j] + JPika[q,j]
      JPika[q,j] = 0
    }else if (JPika[q,j] == territories[q] - APika[q,j]){
      APika[q,j] = APika[q, j] + JPika[q,j]
      JPika[q,j] = 0
    }else{
      APika[q,j] = territories[q] 
      JPika[q,j] = JPika[q,j] - (territories[q] - APika[q, j])   
    }
    JPika[q,j] = 0
  }
  
}#end year loop

