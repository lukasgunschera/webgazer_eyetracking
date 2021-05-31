######################## Exploratory Analyses of Webcam Eyetracking Data ############################

rm(list=ls())
myPackages <- c('dplyr','eyetrackingR','labelled','ggplot2','stringr','data.table','Matrix','lme4','zoo','sur','rlist')
lapply(myPackages, require, character.only = TRUE) 

setwd('/Users/lukasgunschera/Documents/UvA/Intern/data/filedrop')
data_window_clean <- list.load('data_window_clean.rds')
eyetrackingRdat <- list.load('eyetrackingRdat.rds')
