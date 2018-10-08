Pika metapopulation project
=====

This repository contains R scripts for analyses for White & Smith 2018 Ecology. Please contact Easton White (eawhite@ucdavis.edu or eastonrwhite@gmail.com) for more information. 

The supplementary material is provided as a .Rmd file and a pdf. This repository contains all the appendices combined in a single file or individual appendices according to the formatting of Ecology. The figures all built using previously run simulations. The outputs of these simulations are saved as .Rdata files in the Model_Outputs folder. 

The R script `SimpleBodieModel_model.R` is a stochastic simulation of the Bodie pika metapopulation as a spatially explicit array of patches (n=79) that incorporates field data on patch locations. The model was parameterized with demographic and spatial data from the Bodie metapopulation, which has been studied nearly continuously for 6 decades. The other R scripts run various analyses that are described in the manuscript and the supplementary material:

* ` 	Load_Initial_Conditions.R` is a script that includes territory counts and the initial pika abundances recorded in 1972. This script, combined with the sampled_census_bodie.txt and inter_patch_distances.txt are all the pieces of information for the Bodie pika population specifically. 
* `SimpleBodieModel_wrapper.R` is a script to run the `SimpleBodieModel_model.R` code repeatedly.
* `Four_Model_Experiment.R` compares different simulation scenarios including different spatial structures and varying the number of territories per patch
* `ParameterSensitivity_model` and `ParameterSensitivity_wrapper` are used to test the sensitivity of different model parameters including dispersal radius, disperser mortality, birth rate, adult mortality probability, and disperser propensity. 
