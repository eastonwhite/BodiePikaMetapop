#Easton R. White 
#Bodie Pika Dispersal Model
#created 22-June-2012
#Last edited 28-Jun-2015

#particular code gives births to new pikas and splilts them into groups, some automatically disperse. Others stay are patch, compete and get territory or die. May not be a particularly interesting model, biologically

PC=matrix(TC, nrow=79,ncol=1) #the first digit in the parethesis is the number of territories on every patch #(in this case 5)

#time for model to run
max.time= 19 #38 years to match bodie census length to 2009

#IC1991_nosouth=c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
#         0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 4, 1, 0, 
 #        0, 3, 6, 0, 5, 0, 2, 4, 1, 0, 0, NA, 2, 1, 2, 1, 0, 1, 7, 10, 
  #       11, 3, 7, 13, 1, 7, 3, 2, 2, 0, 2, 6, 4, 3, 3, 6, NA, 1)
#IC1991_nosouth[53]=1
#IC1991_nosouth[78]=6
#IC1991_nosouth[65]=50

############
#random rounding of IC
#IC1991_females=IC1991_nosouth/2 #calculate only females
#IC_test=sapply(IC1991_females,random_rounding,simplify=T)

#TC_test-IC_test#Evaluate to make sure IC<TC


###########

#matrixes for adult and juvenile pika, and for territory count
APika = matrix(IC_test,nrow= 79,ncol=max.time)
JPika = matrix(0,nrow= 79,ncol=max.time)
N_t = matrix(PC,nrow= 79,ncol=max.time)
DJPika = matrix(0,nrow= 79,ncol=1)

#Model parameters
r= 1.625 #1.625 (3.25) Smith: Ecology 1974a 
u= 0.37 #0.37 Smith: Ecology 1974a, 1978
#d_m=0.75#  0.57-0.75 from Nagy's presentation?
d_prop = 0.25 #Nagy unpublished- value based on Smith 74ab, 78

#########
for (j in 2:(max.time)){
  for (i in 1:79){
    
    APika[i,j] = APika[i,j-1] - rbinom(1,APika[i,j-1],u) #adult mortality rate to correspond with the census occuring before winter
    JPika[i,j] = sum(rpois(APika[i,j], r))
    
    DJPika[i,1] = rbinom(1,JPika[i,j],d_prop) #have to enter dispersal rate
    JPika[i,j] = JPika[i,j]- DJPika[i,1]
    
    if (JPika[i,j] < PC[i] - APika[i,j]){
      APika[i,j] = APika[i,j] + JPika[i,j]
      JPika[i,j] = 0
    }else if (JPika[i,j] == PC[i] - APika[i,j]){
      APika[i,j] = APika[i, j] + JPika[i,j]
      JPika[i,j] = 0
    }else{
      APika[i,j] = PC[i] 
      JPika[i,j] = JPika[i,j] - (N_t[i] - APika[i, j])   
    }
    JPika[i,j]=0
    #DJPika[i,1]= JPika[i,j]+DJPika[i,1] #alternative model where juveniles without a territory on natal patch also disperse (not used in paper)
    DJPika[i,1]= DJPika[i,1]- rbinom(1,DJPika[i,1],d_m)  #dispersal mortality function
  }
  
  #call to randomly take remaining DJPika(without territories) and send them to nearby patches
  DOP=matrix(0,nrow=79,ncol=79)
  for (g in 1:79){
    DOP[1:79,g]=rmultinom(1,DJPika[g,1],prob=main[g,1:79])
  }
  DOP1=rowSums(DOP)
  
  #call allows JPika that dispersed to new patches to acquire territories  
  for (q in 1:79){
    JPika[1:79,j]=DOP1
    
    if (JPika[q,j] < PC[q] - APika[q,j]){
      APika[q,j] = APika[q, j] + JPika[q,j]
      JPika[q,j] = 0
    }else if (JPika[q,j] == PC[q] - APika[q,j]){
      APika[q,j] = APika[q, j] + JPika[q,j]
      JPika[q,j] = 0
    }else{
      APika[q,j] = PC[q] 
      JPika[q,j] = JPika[q,j] - (N_t[q] - APika[q, j])   
    }
    
    JPika[q,j] = 0
  }
  
  
  
}#end year loop

