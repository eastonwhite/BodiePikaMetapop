Pika metapopulation project
=====

This repository contains R scripts for analyses for White & Smith 2018 Ecology. Please contact Easton White (eawhite@ucdavis.edu) for more information. 

The R script SimpleBodieModel_model.R is a stochastic simulation of the Bodie pika metapopulation as a spatially explicit array of patches (n=79) that incorporates field data on patch locations. The model was parameterized with demographic and spatial data from the Bodie metapopulation, which has been studied nearly continuously for 6 decades. The other R scripts run various analyses that are described in the manuscript and the supplementary material:

* `Four_Model_Experiment.R` compares different simulation scenarios including different spatial structures and varying the number of territories per patch
* `ParameterSensitivity_model` and `ParameterSensitivity_wrapper` are used to test the sensitivity of different model parameters including dispersal radius, disperser mortality, birth rate, adult mortality probability, and disperser propensity. 
