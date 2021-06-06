############################################## ANALYSIS FACE-ATTRACTIVENESS #########################################################

rm(list=ls())
dev.off()

myPackages <- c('dplyr','eyetrackingR','labelled','ggplot2','stringr','data.table','Matrix','lme4','zoo','sur','rlist')
lapply(myPackages, require, character.only = TRUE) 

#Loading Data prepared with 'pilotPre' script
setwd('/Users/lukasgunschera/Documents/UvA/Intern/data')
load('faceTrack.Rda')

multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  plots <- c(list(...), plotlist)
  numPlots = length(plots)
  if (is.null(layout)) {
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  if (numPlots==1) {
    print(plots[[1]])
  } else {
    
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    for (i in 1:numPlots) {
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

#remove list entries that are empty; those entires indicate partial completions of participants
length(yPilot[lapply(yPilot,length)>25])
length(yPilot[lapply(yPilot,length)<=25])

yPilot <- yPilot[lapply(yPilot,length)>25]

longData <- rbindlist(yPilot)
longData <- longData[-(1:22),]

longData <- longData[!is.na(longData$gaze_x)]
longData <- longData[longData$block == 1]

#transform sexual preference variable

longData %<>%
  mutate(pair_gender = case_when(
    pair == 1 ~ 'F', pair == 2 ~ 'F', pair == 3 ~ 'F', pair == 4 ~ 'F', pair == 5 ~ 'F',
    pair == 6 ~ 'F', pair == 7 ~ 'F', pair == 8 ~ 'F', pair == 9 ~ 'F', pair == 10 ~ 'F',
    pair == 11 ~ 'M', pair == 12 ~ 'M', pair == 13 ~ 'M', pair == 14 ~ 'M', pair == 15 ~ 'M',
    pair == 16 ~ 'M', pair == 17 ~ 'M', pair == 18 ~ 'M', pair == 19 ~ 'M'
  )
  )

longData %<>%
  mutate(gender_match = case_when(
    pair_gender == 'F' & attracted == 'females' ~ 'match',
    pair_gender == 'M' & attracted == 'males' ~ 'match',
    attracted == 'both' ~ 'match',
  ))

#upcoming selection variable added at each measurement point
longData$selection_trial <- 0
selection_rev <- rev(longData$selection)
rev_selection <- na.locf(selection_rev)
longData$selection_trial <- rev(rev_selection)

paste('aligned results:', table(longData$selection == longData$selection_trial), 'TRUE')
var_label(longData$selection_trial) <- "selected face on trial"

###################################### DATA CLEANING ###############################################################################
#(1)participant removal with no variation in eye-gaze estimation (1)
uniqueID_long <- unique(longData$ID)


clean_ind <- character()
for (k in 1:length(uniqueID_long)){
  cat(round(k/length(uniqueID_long)*100,2),"%    \r")
  cleaning <- subset(longData, longData$ID == uniqueID_long[k],)
  par_sd <- sd(cleaning$gaze_x, na.rm = TRUE)
  
  if(par_sd == 0){ #par_sd determines minimum variation to be excluded
    print(uniqueID_long[k])
    clean_ind <- c(clean_ind, as.character(uniqueID_long[k]))
  }
}

longData <- longData[!longData$ID %in% clean_ind,]

#calculate participant loss percentage 
paste(1-length(unique(longData$ID))/length(uniqueID_long), '= participant loss percentage no gaze variation')

#(2)participant removal with no variation in responses
uniqueID_long <- unique(longData$ID)
noResVar <- character()

for(i in 1:length(uniqueID_long)){
  cat(round(i/length(uniqueID_long)*100,2),"%    \r")
  ind <- uniqueID_long[i]
  ind <- as.character(ind)
  uniqueResPar <- unique(longData$selection_trial[longData$ID == ind])
  
  if(length(uniqueResPar) < 2){
    print(uniqueID_long[i])
    noResVar <- as.character(c(noResVar, ind))
  }
}

longData <- longData[!longData$ID %in% noResVar, ]
paste(1-length(unique(longData$ID))/length(uniqueID_long),'= participant loss percentage due to no response variation:')

#trial removal with extreme responses times (>30 seconds & <.5)
min(longData$trial_time[longData$cond == 'response'], na.rm = TRUE)
median(longData$trial_time[longData$cond == 'response'], na.rm = TRUE)
mean(longData$trial_time[longData$cond == 'response'], na.rm = TRUE)
sd(longData$trial_time[longData$cond == 'response'], na.rm = TRUE)

uniqueID_long <- unique(longData$ID)
max(longData$trial_time, na.rm = TRUE)

for(z in 1:length(uniqueID_long)){
  cat(round(z/length(uniqueID_long)*100,2),"%    \r")
  subPar <- subset(longData, longData$ID == uniqueID_long[z])
  a <- unique(subPar$pair[which(subPar$trial_time > 30)])
  longData <- longData[!(longData$ID == uniqueID_long[z] & longData$pair %in% a),]
}


for(d in 1:length(uniqueID_long)){
  cat(round(d/length(uniqueID_long)*100,2),"%    \r")
  subPar <- subset(longData, longData$ID == uniqueID_long[d])
  b <- unique(subPar$pair[which(subPar$trial_time[subPar$cond == 'response'] < .5)])
  longData <- longData[!(longData$ID == uniqueID_long[z] & longData$pair %in% b),]
}

paste('check too short:', which(longData$trial_time[longData$cond == 'response'] < .5), 'check too long:', which(longData$trial_time[longData$cond == 'response'] > 30))

################################## FLEXIBLE MIDLINE EXCLUSION AND ADJUSTMENT #######################################################
#trials further than 1/4 of the screen width away from the midline are excluded 
#trials with midline deadzones exceeding 1/2 of the screen are excluded

#variable midline based on preceeding fixations (tailing 80% to exclude early fluctuations)
longData$var_midline <- NA
for(ff in 1:length(uniqueID_long)){
  cat(round(ff/length(uniqueID_long)*100,2),"%    \r")
  for(gg in 1:length(unique(longData$pair))){
    longData$var_midline[longData$ID == uniqueID_long[ff] & longData$pair == gg] <- median(tail(longData$gaze_x[longData$ID == uniqueID_long[ff] & longData$pair == gg & longData$cond == 'fixation']),round(.8*length(longData$gaze_x[longData$ID == uniqueID_long[ff] & longData$pair == gg & longData$cond == 'fixation'])))
    longData$var_midline_sd[longData$ID == uniqueID_long[ff] & longData$pair == gg] <- sd(tail(longData$gaze_x[longData$ID == uniqueID_long[ff] & longData$pair == gg & longData$cond == 'fixation']),round(.8*length(longData$gaze_x[longData$ID == uniqueID_long[ff] & longData$pair == gg & longData$cond == 'fixation'])))
  }
}

setwd('/Users/lukasgunschera/Documents/UvA/Intern/data/filedrop')
list.save(longData,'longDat.rds')

#trial exclusion large midline area

longData$mid_offset <- abs(longData$var_midline - longData$mid_x)
longData$exclude <- ifelse(longData$res_x*.25 <= longData$mid_offset, TRUE, FALSE)
excl_middistance <- percent.table(longData$exclude)
longData <- longData[longData$exclude == FALSE]

#trial exclusion large midline deviation
longData$exclude <- ifelse(longData$var_midline_sd >= longData$res_x*.25, TRUE, FALSE)
#longData$exclude <- longData$var_midline_sd >= longData$mid_4
excl_midarea <- percent.table(longData$exclude)
longData <- longData[longData$exclude == FALSE]

paste('midline area included %:', excl_midarea[1], 'midline variation included%:', excl_middistance[1])

########################################### ADD AOIS #############################################################################
# add important variables
longData$aoi <- FALSE
longData$aoi_left <- NA
longData$aoi_right <- NA

uniqueID <- unique(longData$ID)
longData$Trial <- 1:nrow(longData)
uniqueIDL <- longData$Trial

uniqueBottomL <- rep(0, nrow(longData))
uniqueTopL <- longData$res_y

# add bounds
responsiveMidX <- longData$var_midline - 1/4*longData$var_midline_sd
responsiveMidY <- longData$var_midline + 1/4*longData$var_midline_sd
responsiveLeft <- rep(0, nrow(longData)) + longData$mid_offset
responsiveRight <- longData$res_x + longData$mid_offset

# create AOI dataset with left and right 
aoiLeft <- data.frame(Trial = uniqueIDL, 
                      Left = responsiveLeft, Right = responsiveMidX,
                      Top = uniqueTopL, Bottom = uniqueBottomL)

aoiRight <- data.frame(Trial = uniqueIDL,
                       Left = responsiveMidY, Right = responsiveRight,
                       Top = uniqueTopL, Bottom = uniqueBottomL)


datAoi <- add_aoi(data = longData, aoi_dataframe = aoiLeft,
                  x_col = "gaze_x", y_col = "gaze_y",
                  aoi_name = "aoi_left",
                  x_min_col = "Left", x_max_col = "Right",
                  y_min_col = "Bottom", y_max_col = "Top")

datAoi <- add_aoi(data = datAoi, aoi_dataframe = aoiRight,
                  x_col = "gaze_x", y_col = "gaze_y",
                  aoi_name = "aoi_right",
                  x_min_col = "Left", x_max_col = "Right",
                  y_min_col = "Bottom", y_max_col = "Top")

#create reverse time vector that leads up to the response (timepoint = 0)
datAoi$trackloss <- FALSE
datAoi$rev_time <- NA

for(dd in 1:length(uniqueID)){
  for(ii in 1:length(unique(datAoi$pair))){
    datAoi$rev_time[datAoi$pair == ii & datAoi$ID == uniqueID[dd]] <- rev(datAoi$trial_time[datAoi$pair == ii & datAoi$ID == uniqueID[dd]])
  }
}

datAoi$rev_time[datAoi$cond == 'response'] <- 0
datAoi$rev_time <- datAoi$rev_time * -1

################################# DATA HAVE BEEN LAODED ##############################################################################
table(datAoi$aoi_left)
table(datAoi$aoi_right)
paste('trials falling outside of AOIs =',sum(datAoi$aoi_left == FALSE & datAoi$aoi_right == FALSE))
paste('NA AOI data =',sum(is.na(datAoi$aoi_left)))

data <- make_eyetrackingr_data(datAoi, 
                               participant_column = "ID",
                               trial_column = "Trial",
                               item_columns = "pair",
                               time_column = "rev_time",
                               trackloss_column = "trackloss",
                               aoi_columns = c('aoi_left','aoi_right'),
                               treat_non_aoi_looks_as_missing = TRUE
)

#remove fixation and all other trials that are irrelevant to the analyses
data <- data %>% filter(cond == 'begin image display' | cond == 'image display' | cond == 'response')

# compute proportion of data falling outside of AOIs 
paste('percent trackloss =',nrow(data[data$trackloss == TRUE,])/nrow(data)*100)

### select the participants who managed to complete more than half (i.e., 10+) of all face-pairs
count <- 1
idx <- logical()
for(oo in unique(data$ID)){
  ifelse(max(as.numeric(data$pair[data$ID == oo]))>=10, idx[count] <- TRUE, idx[count] <- FALSE)
  count <- count + 1
}

a <- unique(data$ID)
completeIDs <- a[idx]

data_complete <- subset(data, ID %in% completeIDs)

### Trackloss Analysis #########################################################################
# trackloss >.5 participant exclusion 
# time-window set at 1.6 seconds 

time_window <- -1.6
paste('the time window starts at:', time_window, 'seconds')

dat_win <- subset_by_window(data_complete, window_start_time = time_window, window_end_time = 0, rezero = FALSE, remove = TRUE)
trackloss <- trackloss_analysis(data = dat_win)

dat_win_cl <- clean_by_trackloss(dat_win,
                                 participant_prop_thresh = 0.5,
                                 window_start_time = -Inf,
                                 window_end_time = 0)

droplevels(dat_win_cl$ID)
dat_win_cl$target <- as.factor(ifelse(test = grepl('left', dat_win_cl$selection_trial),
                                      yes = 'left',
                                      no = 'right'))

### Data Inspection #########################################################################

track_dat_win_cl <- trackloss_analysis(data = dat_win_cl)
track_dat_win_cl_participant <- unique(trackloss_data_window_clean[, c('ID','TracklossForParticipant')])

mean_trackloss <- mean(track_dat_win_cl_participant$TracklossForParticipant)
paste(mean_trackloss, '= average trackloss trials for participants')

dat_sum_left <- describe_data(dat_win_cl,
                              describe_column = 'aoi_left',
                              group_columns = c('target','ID'))

plot(dat_sum_left)

#summarize samples contributed per trial
paste('sample % contributed for trial =',mean(1-track_dat_win_cl_participant$TracklossForParticipant))
paste('sample % deviation =',sd(track_dat_win_cl_participant$TracklossForParticipant))

#summarize number of trials contributed by each participant
summary <- describe_data(dat_win_cl, 'aoi_left', 'ID')
paste('trial avg per participant =',mean(summary$NumTrials))
paste('sd of trial avg =',sd(summary$NumTrials))

clean_index <- summary$ID[summary$NumTrials<50 | summary$SD == 0]
clean <- data_window_clean[!data_window_clean$ID %in% clean_index,]

### Analysis #########################################################################

dat_win_cl["aoi_overall"] <- NA
dat_win_cl$aoi_overall <- ifelse(((dat_win_cl$target == "left" & dat_win_cl$aoi_left == TRUE)|(dat_win_cl$target == "right" & dat_win_cl$aoi_right == TRUE)), TRUE, FALSE)
seq_win_cl <- make_time_sequence_data(dat_win_cl,
                                      time_bin_size = .05,
                                      aois = c('aoi_overall'))

plot(seq_win_cl)+
  xlab('time until decision')+
  ylab('proportion viewing time')+
  coord_cartesian(ylim = c(0.45,0.7))+
  theme_bw()+
  ggtitle('Likelihood of gaze being directed toward selected face')+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title = element_text(face="bold", size = rel(1), hjust = 0.5),
        axis.line = element_line(color = "black"),
        axis.title.x = element_text(vjust = 0, size = rel(0.9)),
        axis.title.y = element_text(vjust = 1.1, size = rel(0.9)),
        axis.text.x = element_text(margin = margin(10,10,10,10,"pt")),
        axis.text.y = element_text(margin = margin(10,10,10,0,"pt")),
        axis.ticks.length = unit(-2, "mm"),
        text = element_text(size = 14, family = "Times"),
        panel.border = element_blank())

# One-sided Binomial test for difference from chance level #########################################################################
p <- mean(seq_win_cl$Prop[seq_win_cl$TimeBin == -1|seq_win_cl$TimeBin == -2], na.rm = TRUE)
n <- round(length(seq_win_cl$Prop[seq_win_cl$TimeBin == -1|seq_win_cl$TimeBin == -2 & !is.na(seq_win_cl$Prop)]))
np <- round(n*p)

binom.test(np,n,0.5,alternative = 'greater')

# Viewing proportions per left and right face selection 
seq_win_cl_right <- make_time_sequence_data(dat_win_cl,
                                            time_bin_size = .05,
                                            predictor_columns = 'target',
                                            aois = c('aoi_right'))


seq_win_cl_left <- make_time_sequence_data(dat_win_cl,
                                           time_bin_size = .05,
                                           predictor_columns = 'target',
                                           aois = c('aoi_left'))

aa <- plot(seq_win_cl_left[seq_win_cl_left$target == 'left',], predictor_column = 'target')+
  xlab('time until decision')+
  ylab('proportion viewing time')+
  coord_cartesian(xlim = c(-1.6,0), ylim = c(.45,.7))
bb <- plot(seq_win_cl_right[seq_win_cl_right$target == 'right',], predictor_column = 'target')+
  xlab('time until decision')+
  ylab('proportion viewing time')+
  coord_cartesian(xlim = c(-1.6,0), ylim = c(.45,.7))

multiplot(aa,bb, cols = 2)

#offload datafiles for further exploratory analysis in seperate R script
setwd('/Users/lukasgunschera/Documents/UvA/Intern/data/filedrop')

list.save(data_window_clean, 'dat_win_cl.rds')
list.save(data, 'eyetrackingRdat.rds')

length(unique(seq_win_cl_left$ID))
length(unique(seq_win_cl_right$ID))

###############################################################################################################################################################################
######################## ATTENTION CHECK ANALYSIS #############################################################################################################################

strictInclAtt <- c(1L, 3L, 5L, 6L, 7L, 8L, 9L, 10L, 11L, 13L, 14L, 15L, 16L, 18L, 19L, 20L, 21L, 22L, 23L, 24L, 25L, 26L, 27L, 28L, 29L, 30L, 31L, 32L, 34L, 36L, 37L, 40L, 41L, 42L, 45L, 46L, 47L, 51L, 52L, 53L, 55L, 57L, 58L, 59L, 60L, 62L, 65L, 66L, 69L, 70L, 71L, 72L, 74L, 76L, 77L, 78L, 79L, 80L, 81L, 82L, 83L, 84L, 85L, 86L, 87L, 88L, 89L, 90L, 92L, 94L, 95L, 96L, 99L, 100L, 101L, 102L, 103L, 104L, 106L, 107L, 110L, 113L, 114L, 115L, 116L, 117L, 118L, 119L, 120L, 121L, 123L, 124L, 125L, 127L, 128L, 129L, 130L, 131L, 132L, 133L, 134L, 135L, 136L, 137L, 138L, 139L, 140L, 141L, 142L, 143L, 144L, 145L, 146L, 147L, 148L, 149L, 150L, 151L, 152L, 153L, 154L, 155L, 156L, 157L, 158L, 160L, 161L, 162L)
lenientInclAtt <- c(1L, 2L, 3L, 4L, 5L, 6L, 7L, 8L, 9L, 10L, 11L, 12L, 13L, 14L, 15L, 16L, 17L, 18L, 19L, 20L, 21L, 22L, 23L, 24L, 25L, 26L, 27L, 28L, 29L, 30L, 31L, 32L, 34L, 36L, 37L, 38L, 39L, 40L, 41L, 42L, 44L, 45L, 46L, 47L, 51L, 52L, 53L, 54L, 55L, 56L, 57L, 58L, 59L, 60L, 61L, 62L, 63L, 64L, 65L, 66L, 68L, 69L, 70L, 71L, 72L, 73L, 74L, 76L, 77L, 78L, 79L, 80L, 81L, 82L, 83L, 84L, 85L, 86L, 87L, 88L, 89L, 90L, 92L, 94L, 95L, 96L, 99L, 100L, 101L, 102L, 103L, 104L, 105L, 106L, 107L, 108L, 110L, 111L, 112L, 113L, 114L, 115L, 116L, 117L, 118L, 119L, 120L, 121L, 122L, 123L, 124L, 125L, 126L, 127L, 128L, 129L, 130L, 131L, 132L, 133L, 134L, 135L, 136L, 137L, 138L, 139L, 140L, 141L, 142L, 143L, 144L, 145L, 146L, 147L, 148L, 149L, 150L, 151L, 152L, 153L, 154L, 155L, 156L, 157L, 158L, 160L, 161L, 162L)

paste('the lenient attention check includes', length(lenientInclAtt)-length(strictInclAtt), 'more participants than the strict ones')

# variables: strictInclAtt & lenientInclAtt come from 'attentionCheck' script
data_attention_strict <- subset(data, ID %in% strictInclAtt)
data_attention_lenient <- subset(data, ID %in% lenientInclAtt)

########################### STRICT #############################################################################################

time_window <- -1.6
paste('the time window starts at:', time_window, 'seconds')

dat_win1 <- subset_by_window(data_attention_strict, window_start_time = time_window, window_end_time = 0, rezero = FALSE, remove = TRUE)
trackloss <- trackloss_analysis(data = dat_win1)

dat_win1_cl <- clean_by_trackloss(dat_win1,
                                  participant_prop_thresh = 0.5,
                                  window_start_time = -Inf,
                                  window_end_time = 0)

droplevels(dat_win1_cl$ID)
dat_win1_cl$target <- as.factor(ifelse(test = grepl('left', dat_win1_cl$selection_trial),
                                       yes = 'left',
                                       no = 'right'))

### Data Inspection 

track_dat_win1_cl <- trackloss_analysis(data = dat_win1_cl)
track_dat_win1_cl_participant <- unique(track_dat_win1_cl[, c('ID','TracklossForParticipant')])

mean_trackloss1 <- mean(track_dat_win1_cl_participant$TracklossForParticipant)
paste(mean_trackloss1, '= average trackloss trials for participants')

dat_sum_left1 <- describe_data(dat_win1_cl,
                               describe_column = 'aoi_left',
                               group_columns = c('target','ID'))

plot(dat_sum_left1)

#summarize samples contributed per trial
paste('sample % contributed for trial =',mean(1-track_dat_win1_cl_participant$TracklossForParticipant))
paste('sample % deviation =',sd(track_dat_win1_cl_participant$TracklossForParticipant))

#summarize number of trials contributed by each participant
summary1 <- describe_data(dat_win1_cl, 'aoi_left', 'ID')
paste('trial avg per participant =',mean(summary1$NumTrials))
paste('sd of trial avg =',sd(summary1$NumTrials))

clean_index1 <- summary$ID[summary1$NumTrials<50 | summary$SD == 0]
clean1 <- dat_win1_cl[!dat_win1_cl$ID %in% clean_index1,]

### Analysis 

dat_win1_cl["aoi_overall"] <- NA
dat_win1_cl$aoi_overall <- ifelse(((dat_win1_cl$target == "left" & dat_win1_cl$aoi_left == TRUE)|(dat_win1_cl$target == "right" & dat_win1_cl$aoi_right == TRUE)), TRUE, FALSE)
seq_win1_cl <- make_time_sequence_data(dat_win1_cl,
                                       time_bin_size = .05,
                                       aois = c('aoi_overall'))

plot(seq_win1_cl)+
  xlab('time until decision')+
  ylab('proportion viewing time')+
  coord_cartesian(ylim = c(0.45,0.7))+
  theme_bw()+
  ggtitle('Likelihood of gaze being directed toward selected face')+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title = element_text(face="bold", size = rel(1), hjust = 0.5),
        axis.line = element_line(color = "black"),
        axis.title.x = element_text(vjust = 0, size = rel(0.9)),
        axis.title.y = element_text(vjust = 1.1, size = rel(0.9)),
        axis.text.x = element_text(margin = margin(10,10,10,10,"pt")),
        axis.text.y = element_text(margin = margin(10,10,10,0,"pt")),
        axis.ticks.length = unit(-2, "mm"),
        text = element_text(size = 14, family = "Times"),
        panel.border = element_blank())


p1 <- mean(seq_win1_cl$Prop[seq_win1_cl$TimeBin == -1|seq_win1_cl$TimeBin == -2], na.rm = TRUE)
n1 <- round(length(seq_win1_cl$Prop[seq_win1_cl$TimeBin == -1|seq_win1_cl$TimeBin == -2 & !is.na(seq_win1_cl$Prop)]))
np1 <- round(n1*p1)

binom.test(np1,n1,0.5,alternative = 'greater')

########################### LENIENT #############################################################################################

time_window <- -1.6
paste('the time window starts at:', time_window, 'seconds')

dat_win2 <- subset_by_window(data_attention_strict, window_start_time = time_window, window_end_time = 0, rezero = FALSE, remove = TRUE)
trackloss <- trackloss_analysis(data = dat_win2)

dat_win2_cl <- clean_by_trackloss(dat_win2,
                                  participant_prop_thresh = 0.5,
                                  window_start_time = -Inf,
                                  window_end_time = 0)

droplevels(dat_win2_cl$ID)
dat_win2_cl$target <- as.factor(ifelse(test = grepl('left', dat_win2_cl$selection_trial),
                                       yes = 'left',
                                       no = 'right'))

# Data Inspection #########################################################################

track_dat_win2_cl <- trackloss_analysis(data = dat_win2_cl)
track_dat_win2_cl_participant <- unique(track_dat_win2_cl[, c('ID','TracklossForParticipant')])

mean_trackloss2 <- mean(track_dat_win2_cl_participant$TracklossForParticipant)
paste(mean_trackloss2, '= average trackloss trials for participants')

dat_sum_left2 <- describe_data(dat_win2_cl,
                               describe_column = 'aoi_left',
                               group_columns = c('target','ID'))

plot(dat_sum_left2)

#summarize samples contributed per trial
paste('sample % contributed for trial =',mean(1-track_dat_win2_cl_participant$TracklossForParticipant))
paste('sample % deviation =',sd(track_dat_win2_cl_participant$TracklossForParticipant))

#summarize number of trials contributed by each participant
summary2 <- describe_data(dat_win2_cl, 'aoi_left', 'ID')
paste('trial avg per participant =',mean(summary2$NumTrials))
paste('sd of trial avg =',sd(summary2$NumTrials))

clean_index2 <- summary$ID[summary2$NumTrials<50 | summary$SD == 0]
clean2 <- dat_win2_cl[!dat_win2_cl$ID %in% clean_index2,]

### Analysis #########################################################################

dat_win2_cl["aoi_overall"] <- NA
dat_win2_cl$aoi_overall <- ifelse(((dat_win2_cl$target == "left" & dat_win2_cl$aoi_left == TRUE)|(dat_win2_cl$target == "right" & dat_win2_cl$aoi_right == TRUE)), TRUE, FALSE)
seq_win2_cl <- make_time_sequence_data(dat_win2_cl,
                                       time_bin_size = .05,
                                       aois = c('aoi_overall'))

plot(seq_win2_cl)+
  xlab('time until decision')+
  ylab('proportion viewing time')+
  coord_cartesian(ylim = c(0.45,0.7))+
  theme_bw()+
  ggtitle('Likelihood of gaze being directed toward selected face')+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title = element_text(face="bold", size = rel(1), hjust = 0.5),
        axis.line = element_line(color = "black"),
        axis.title.x = element_text(vjust = 0, size = rel(0.9)),
        axis.title.y = element_text(vjust = 1.1, size = rel(0.9)),
        axis.text.x = element_text(margin = margin(10,10,10,10,"pt")),
        axis.text.y = element_text(margin = margin(10,10,10,0,"pt")),
        axis.ticks.length = unit(-2, "mm"),
        text = element_text(size = 14, family = "Times"),
        panel.border = element_blank())


p2 <- mean(seq_win2_cl$Prop[seq_win2_cl$TimeBin == -1|seq_win2_cl$TimeBin == -2], na.rm = TRUE)
n2 <- round(length(seq_win2_cl$Prop[seq_win2_cl$TimeBin == -1|seq_win2_cl$TimeBin == -2 & !is.na(seq_win2_cl$Prop)]))
np2 <- round(n2*p2)

binom.test(np2,n2,0.5,alternative = 'greater')





