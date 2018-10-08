
#Created by Easton R White
#Created on 31-Oct-2017
#Last edited 31-Oct-2017


# This script loads the initial conditions, territory counts, and the actual census counts


##########################

# Load matrix where NAs are in place during years where patches were not sampled
NA_matrix <- read.table('Scripts/NA_matrix.txt',header=T)
names(NA_matrix) <- 1972:2010

#Bodie Territory Counts (from J.D. Nagy field notes)
territories=c(6,5,3,7,3,4,3,4,5,5,5,3,3,4,3,3,4,4,3,3,
              5,5,3,3,3,4,4,3,3,2,3,4,3,3,4,3,5,5,9,3,5,
              5,4,14,4,5,5,7,13,4,3,3,3,4,2,5,11,12,3,13,
              14,13,7,15,13,3,12,7,4,4,3,5,16,10,3,4,11,12,3)


#Initial population size data from spreadsheet 'MeasurementsCompleteBodieCensusData1972-2009_EW'
#fill in NAs
IC1972=c(0,3,1,3,0,4,2,0,3,4,4,1,0,0,0,2,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,4,0,5,3,4,4,3,10,1,1,1,3,4,1,1,0,1,1,1,2,7,9,1,6,7,6,2,9,13,1,3,2,0,0,0,0,8,5,1,1,6,2,2)
IC1991=c(0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,2,2,0,0,0,0,0,0,0,2,0,4,1,0,0,3,6,0,5,0,2,4,1,0,0,NA,2,1,2,1,0,1,7,10,11,3,7,13,1,7,3,2,2,0,2,6,4,3,3,6,NA,1)
#these three changes are averages based on all other years (can't have NAs to initialize simulations)
IC1991[53]=1 
IC1991[78]=6
IC1991[65]=13#IC1991[65]=50

# Load actual census data
sampled_census_bodie <- read.table('Scripts/sampled_census_bodie.txt',header=T,sep='\t')
names(sampled_census_bodie) <- names(sampled_census_bodie) <- c(1972, 1977,1989,1991,1992,1993,1994,1995,1996,1997,1998,1999,2000,2001,2003,2004,2005, 2006,2008,2009,2010)

