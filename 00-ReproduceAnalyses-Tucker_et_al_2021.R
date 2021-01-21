## Master script to run all the analysis in Tucker, Idrissi & Almeida (2020) ---
## Author: Diogo Almeida <diogo@nyu.edu>
## Date: 2020-12-28
## Description:
## This script basically calls "source" on all the individual analysis files.
## The scripts are run in the order the experiments are presented in the paper.
##
## The scripts could nonetheless be run in arbitrary order, with the exception
## of the last one for the meta-analysis, which needs to be the last one called 
## (as it relies on data produced by the other scripts).
##
## However, if the meta-analysis is run independently or in the absence of all 
## the data created by the other analysis scripts, it will download the required
## data from figshare.
##
## NOTE: All the scripts here rely on the Rproject structure, and are thus
##       better run from an RStudio session. I believe it may be possible to use
##       Rprojects outside of RStudio, but I have no idea on how to do it, and I
##       cannot provide support for it.

## Load packages ---------------------------------------------------------------
library(fs)
library(here)

## Run the scripts -------------------------------------------------------------
source("01-Experiment1-Analysis-Frontiers.R")
cat("----- Experiment 1 done! -----\n")
source("02-Experiment2-Analysis-Frontiers.R")
cat("----- Experiment 2 done! -----\n")
source("03-Experiment3-Analysis-Frontiers.R")
cat("----- Experiment 3 done! -----\n")
source("04-Experiment4-Analysis-Frontiers.R")
cat("----- Experiment 4 done! -----\n")
source("05-Experiment5-Analysis-Frontiers.R")
cat("----- Experiment 5 done! -----\n")
source("06-PrepareTucker2015.R")
cat("----- Preparation of data from Tucker et al. 2015 done! -----\n")
source("07-MetaAnalysis.R")
cat("----- Meta-Analysis done! -----\n")

## Clean up --------------------------------------------------------------------
rm(list=ls())
cat("----- Reproduction of figures and tables for Tucker et al. 2020 done! -----\n")
