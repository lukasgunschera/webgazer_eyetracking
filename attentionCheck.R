############################################## Attention Checks #########################################################

rm(list=ls())
dev.off()

myPackages <- c('dplyr','eyetrackingR','labelled','ggplot2','stringr','data.table','Matrix','lme4','zoo','sur','rlist')
lapply(myPackages, require, character.only = TRUE) 

#Loading Data prepared with 'pilotPre' script
setwd('/Users/lukasgunschera/Documents/UvA/Intern/data')
load('faceTrack.Rda')

length(yPilot[lapply(yPilot,length)>25])
length(yPilot[lapply(yPilot,length)<=25])

yPilot <- yPilot[lapply(yPilot,length)>25]

attDat <- rbindlist(yPilot)
attDat <- attDat[-(1:22),]

attDat <- attDat[!is.na(attDat$gaze_x)]

# ATTENTION CHECKS ##########################################################################################################################

#attention check 2: do participants select the same face as more attractive if the last face-pair from block 1 is presented again
iteration <- 1
a <- logical()
for(qq in unique(attDat$ID)){
  attSub <- subset(attDat, ID == qq & block == 1)
  attSub2 <- subset(attDat, ID == qq & block == 2)
  attPair <- unique(attSub$pair)
  lenAttPair <- length(attPair)
  ifelse(attSub$selection[attSub$pair == attPair[lenAttPair] & attSub$cond == 'response'] == attSub2$selection[attSub2$pair == 1 & attSub2$cond == 'response'], a[iteration] <- TRUE, a[iteration] <- FALSE)
  iteration <- iteration + 1
}

table(a)
participants <- unique(attDat$ID)
parAttentive1 <- participants[a]

#attention check 2:

iteration <- 1
b <- logical()
for(dd in unique(attDat$ID[attDat$block == 2])){
  attSub2 <- subset(attDat, ID == dd & block == 2 & pair == 2)
  b[iteration] <- attSub2$selection[attSub2$cond == 'response'] == 'left'
  
  iteration <- iteration + 1
}

table(b)
parAttentive2 <- participants[b]

#attention check 3:

iteration <- 1
c <- logical()
for(ff in unique(attDat$ID[attDat$block == 2])){
  attSub2 <- subset(attDat, ID == ff & block == 2 & pair == 3)
  c[iteration] <- attSub2$selection[attSub2$cond == 'response'] == 'left'
  
  iteration <- iteration + 1
}

table(c)
parAttentive3 <- participants[c]

# create variable indicating which participants to select
strictInclAtt <- intersect(intersect(parAttentive1,parAttentive2),parAttentive3)
lenientInclAtt <- parAttentive3[parAttentive3 %in% parAttentive1 | parAttentive2]

strictInclAtt


strictInclAtt %>%
  datapasta::vector_paste()
c(1L, 3L, 5L, 6L, 7L, 8L, 9L, 10L, 11L, 13L, 14L, 15L, 16L, 18L, 19L, 20L, 21L, 22L, 23L, 24L, 25L, 26L, 27L, 28L, 29L, 30L, 31L, 32L, 34L, 36L, 37L, 40L, 41L, 42L, 45L, 46L, 47L, 51L, 52L, 53L, 55L, 57L, 58L, 59L, 60L, 62L, 65L, 66L, 69L, 70L, 71L, 72L, 74L, 76L, 77L, 78L, 79L, 80L, 81L, 82L, 83L, 84L, 85L, 86L, 87L, 88L, 89L, 90L, 92L, 94L, 95L, 96L, 99L, 100L, 101L, 102L, 103L, 104L, 106L, 107L, 110L, 113L, 114L, 115L, 116L, 117L, 118L, 119L, 120L, 121L, 123L, 124L, 125L, 127L, 128L, 129L, 130L, 131L, 132L, 133L, 134L, 135L, 136L, 137L, 138L, 139L, 140L, 141L, 142L, 143L, 144L, 145L, 146L, 147L, 148L, 149L, 150L, 151L, 152L, 153L, 154L, 155L, 156L, 157L, 158L, 160L, 161L, 162L)

lenientInclAtt %>%
  datapasta::vector_paste()
c(1L, 2L, 3L, 4L, 5L, 6L, 7L, 8L, 9L, 10L, 11L, 12L, 13L, 14L, 15L, 16L, 17L, 18L, 19L, 20L, 21L, 22L, 23L, 24L, 25L, 26L, 27L, 28L, 29L, 30L, 31L, 32L, 34L, 36L, 37L, 38L, 39L, 40L, 41L, 42L, 44L, 45L, 46L, 47L, 51L, 52L, 53L, 54L, 55L, 56L, 57L, 58L, 59L, 60L, 61L, 62L, 63L, 64L, 65L, 66L, 68L, 69L, 70L, 71L, 72L, 73L, 74L, 76L, 77L, 78L, 79L, 80L, 81L, 82L, 83L, 84L, 85L, 86L, 87L, 88L, 89L, 90L, 92L, 94L, 95L, 96L, 99L, 100L, 101L, 102L, 103L, 104L, 105L, 106L, 107L, 108L, 110L, 111L, 112L, 113L, 114L, 115L, 116L, 117L, 118L, 119L, 120L, 121L, 122L, 123L, 124L, 125L, 126L, 127L, 128L, 129L, 130L, 131L, 132L, 133L, 134L, 135L, 136L, 137L, 138L, 139L, 140L, 141L, 142L, 143L, 144L, 145L, 146L, 147L, 148L, 149L, 150L, 151L, 152L, 153L, 154L, 155L, 156L, 157L, 158L, 160L, 161L, 162L)


