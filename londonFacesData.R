rm(list=ls())
setwd("/Users/lukasgunschera/Documents/UoA/Intern/londonFaces")

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

##select face pairs for which conditions are met
##conditions: ethnicity, gender, agvRating <= .25
faceMatch <- data.frame()

for(g in 1:(nrow(d2)-1)){
  face1 <- d2$avgRatingCl[d2$faceNr == g]
  ethn1 <- d2$face_eth[d2$faceNr == g]
  sex1 <- d2$face_sex[d2$faceNr == g]
  sd1 <- d2$sdRatingCl[d2$faceNr == g]
  age1 <- d2$face_age[d2$faceNr == g]
  
  for(i in (g+1):nrow(d2)){
    face2 <- d2$avgRatingCl[d2$faceNr == i]
    ethn2 <- d2$face_eth[d2$faceNr == i]
    sex2 <- d2$face_sex[d2$faceNr == i]
    sd2 <- d2$sdRatingCl[d2$faceNr == i]
    age2 <- d2$face_age[d2$faceNr == i]
    
    if(face1 - face2 <= 0.25 & face1 - face2 >= -0.25 & ethn1 == ethn2 & sex1 == sex2){
      dat <- data.frame(age1 = age1, age2 = age2, ethnicity = ethn1, sex = sex1, ratingDif = face1 - face2, sdAvg = mean(c(sd1,sd2)), face1 = g, face2 = i, arating = avgRatingCl)
      faceMatch <- rbind(faceMatch,dat)
    }
  }
}

##select image pairs with an age difference <5

logical <- faceMatch$age1 - faceMatch$age2 <= 5 & faceMatch$age1 - faceMatch$age2 >= -5
length(logical)
nrow(faceNoNa)
nrow(faceMatch)

faceAgeMatch1 <- faceMatch[logical,]
nrow(faceAgeMatch1)
faceAgeMatch2 <- faceAgeMatch1[complete.cases(faceAgeMatch1)==T,]
nrow(faceAgeMatch2)

faceDecrSd <- faceAgeMatch2[order(faceAgeMatch2$sdAvg),c(1:9)]


##select 18 unique pairs with the lowest average sd

old <- numeric()
g <- 1
df <- data.frame(matrix(ncol = 7, nrow = 0))

for(i in 1:nrow(faceDecrSd)){
  if(!any(faceDecrSd[i,c("face1","face2")] %in% old)){
    df <- rbind(df,faceDecrSd[i,c("arating","face1","face2","ethnicity","sex","age1","sdAvg")])
    g <- g + 1
    old <- c(old,faceDecrSd[i,c("face1","face2")])
  }
}

## select 18 unique pairs with even female and male participants

fdf <- df[df$sex == "female",]
mdf <- df[df$sex == "male",]

mixedDf <- rbind(fdf[1:9,],mdf[1:9,])
mixedDf










