############################## LONDON FACE SELECTION SCRIPT #############################################
# https://figshare.com/articles/dataset/Face_Research_Lab_London_Set/5047666/3

rm(list=ls())
setwd("/Users/lukasgunschera/Documents/UvA/Intern/stimuli/londonFaces")

library(dplyr)

##import data
d1 <- read.csv("london_faces_ratings.csv")
d2 <- read.csv("london_faces_info.csv")
d1cut <- d1[,4:105]

##descriptive for raters
par(mar=c(1,1,1,1))
avgRater <- rowMeans(d1cut)
length(avgRater)
sdRater <- apply(d1cut,1,sd)
length(sdRater)

box <- boxplot(sdRater)
box$stats

##exclude raters who show unusually low variation in their ratings as indicated by the boxplot
outlierSd <- boxplot(sdRater)$out
lowSd <- outlierSd[outlierSd < 0.4537426]
length(lowSd)

d1Clean <- d1cut[sdRater >= 0.4537426,]
length(d1Clean)

nrow(d1cut) - length(lowSd) == nrow(d1Clean)

##mean and sd for cleaned data across face
avgRatingCl <- colMeans(d1Clean)
sdRatingCl <- apply(d1Clean, 2, sd)


##mean and sd across face
avgRating <- colMeans(d1cut)
length(avgRating)

sdRating <- apply(d1cut,MARGIN = 2,sd)
length(sdRating)

##combine into one dataframe
d2 <- cbind(d2,avgRating)
d2 <- cbind(d2,sdRating)
d2 <- cbind(d2,avgRatingCl)
d2 <- cbind(d2,sdRatingCl)

d2 <- cbind(d2,c(1:nrow(d2)))
names(d2)[names(d2) == "c(1:nrow(d2))"] <- "faceNr"
d2$faceNr <- c(1:102)

## combine all possible combinations 
library(tidyr)
d2$ind <- 1

##select face pair with the highest difference in average rating for attention check 
joinDat <- inner_join(d2, d2, by = c('ind')) %>% filter(faceNr.x > faceNr.y)
joinDat$ratingDif <- joinDat$avgRatingCl.x - joinDat$avgRatingCl.y

joinDat <- joinDat %>% filter(face_eth.x == face_eth.y)
joinDat <- joinDat %>% filter(face_sex.x == face_sex.y)

joinDat[order(joinDat$ratingDif),][1,c('faceNr.x','faceNr.y','ratingDif')]

########## MAIN STIMULI SELECTION ###############
attDat <- inner_join(d2,d2, by = c('ind')) %>% filter(faceNr.x > faceNr.y)
attDat$ratingDif <- abs(attDat$avgRatingCl.x - attDat$avgRatingCl.y)
attDat$sdAvg <- (attDat$sdRatingCl.x+attDat$sdRatingCl.y)/2

attDatClFem <- attDat %>% filter(face_eth.x == face_eth.y & face_sex.x == face_sex.y & face_sex.x == 'female'  & abs(face_age.x - face_age.y)<5)
attDatClMal <- attDat %>% filter(face_eth.x == face_eth.y & face_sex.x == face_sex.y & face_sex.x == 'male'  & abs(face_age.x - face_age.y)<5)

attDatClFem[order(attDatClFem$ratingDif, attDatClFem$sdAvg),][1:25,c('faceNr.x','faceNr.y','ratingDif','sdAvg','face_eth.x','face_eth.y')]
attDatClMal[order(attDatClMal$ratingDif, attDatClMal$sdAvg),][1:20,c('faceNr.x','faceNr.y','ratingDif','sdAvg','face_eth.x','face_eth.y')]






