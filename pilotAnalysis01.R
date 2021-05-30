############################################## ANALYSIS FACE-ATTRACTIVENESS #########################################################

rm(list=ls())
dev.off()

myPackages <- c('dplyr','eyetrackingR','labelled','ggplot2','stringr','data.table','Matrix','lme4','zoo','sur')
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
  subPar <- subset(longData, longData$ID == uniqueID_long[z])
  a <- unique(subPar$pair[which(subPar$trial_time > 30)])
  longData <- longData[!(longData$ID == uniqueID_long[z] & longData$pair %in% a),]
}


for(d in 1:length(uniqueID_long)){
  subPar <- subset(longData, longData$ID == uniqueID_long[d])
  b <- unique(subPar$pair[which(subPar$trial_time[subPar$cond == 'response'] < .5)])
  longData <- longData[!(longData$ID == uniqueID_long[z] & longData$pair %in% b),]
}

paste('check too short:', which(longData$trial_time[longData$cond == 'response'] < .5), 'check too long:', which(longData$trial_time[longData$cond == 'response'] > 30))

################################## FLEXIBLE MIDLINE EXCLUSION AND ADJUSTMENT #######################################################
#trials further than 1/4 of the screen width away from the midline are excluded 
#trials with midline deadzones exceeding 1/2 of the screen are excluded

longData$var_midline <- NA
longData$mid_4 <- longData$res_x/5

#variable midline based on preceeding fixations (tailing 80% to exclude early fluctuations)
for(ff in 1:length(uniqueID_long)){
  cat(round(ff/length(uniqueID_long)*100,2),"%    \r")
  for(gg in 1:length(unique(longData$pair))){
    longData$var_midline[longData$ID == uniqueID_long[ff] & longData$pair == gg] <- median(tail(longData$gaze_x[longData$ID == uniqueID_long[ff] & longData$pair == gg & longData$cond == 'fixation']),round(.8*length(longData$gaze_x[longData$ID == uniqueID_long[ff] & longData$pair == gg & longData$cond == 'fixation'])))
    longData$var_midline_sd[longData$ID == uniqueID_long[ff] & longData$pair == gg] <- sd(tail(longData$gaze_x[longData$ID == uniqueID_long[ff] & longData$pair == gg & longData$cond == 'fixation']),round(.8*length(longData$gaze_x[longData$ID == uniqueID_long[ff] & longData$pair == gg & longData$cond == 'fixation'])))
  }
}

#trial exclusion large midline area
longData$mid_offset <- abs(longData$var_midline - longData$mid_x)
longData$exclude <- ifelse(longData$res_x*.25 <= longData$mid_offset, TRUE, FALSE)
excl_middistance <- percent.table(longData$exclude)
longData <- longData[longData$exclude == FALSE]

#trial exclusion large midline deviation
longData$exclude <- longData$var_midline_sd >= 50
#longData$exclude <- longData$var_midline_sd >= longData$mid_4
excl_midarea <- percent.table(longData$exclude)
longData <- longData[longData$exclude == FALSE]

paste('midline area included %:', excl_midarea[1], 'midline variation included%:', excl_middistance[1])

########################################### ADD AOIS #############################################################################
# add important variables
longData$aoi <- FALSE
longData$aoi_left <- NA
longData$aoi_right <- NA

uniquePair <- unique(longData$pair)
uniqueID <- unique(longData$ID)
longData$Trial <- 1:nrow(longData)
uniqueIDL <- longData$Trial

uniqueBottomL <- rep(0, nrow(longData))
uniqueTopL <- longData$res_y

#below AOIs should be used if fixed AOIs are desired, with a set left right and middle bound for each participant
#uniqueLeftL <- rep(0, nrow(longData))
#uniqueMidL <- longData$mid_x -1 # -1 indicates a margin in the middle that is considered neither right or left
#uniqueMidR <- longData$mid_x +1 # +1 indicates a margin in the middle that is considered neither right or left
#uniqueRightL <- longData$res_x

responsiveMidX <- longData$var_midline - 1/4*longData$var_midline_sd
responsiveMidY <- longData$var_midline + 1/4*longData$var_midline_sd
responsiveLeft <- rep(0, nrow(longData)) + longData$mid_offset
responsiveRight <- longData$res_x + longData$mid_offset

aoiLeft <- data.frame(Trial = uniqueIDL, 
                      Left = responsiveLeft, Right = responsiveMidX,
                      Top = uniqueTopL, Bottom = uniqueBottomL)

aoiRight <- data.frame(Trial = uniqueIDL,
                       Left = responsiveMidY, Right = responsiveRight,
                       Top = uniqueTopL, Bottom = uniqueBottomL)

#below AOI code for fixed bounds if desired
#aoiLeft <- data.frame(Trial = uniqueIDL, 
#                     Left = uniqueLeftL, Right = uniqueMidL,
#                     Top = uniqueTopL, Bottom = uniqueBottomL)

#aoiRight <- data.frame(Trial = uniqueIDL,
#                      Left = uniqueMidR, Right = uniqueRightL,
#                      Top = uniqueTopL, Bottom = uniqueBottomL)

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

datAoi$trackloss <- FALSE

#selection of variables of interest
names(datAoi)
datAoi <- subset(datAoi, select = c("ID","qual_id","trials_ran","completion","consent","webcam","webcam_type",
                                    "cal_x","cal_y","cal_number","mouse_x","mouse_y",
                                    "cal_off_x","cal_off_y","res","res_x","res_y",
                                    "block","pair","aoi","Trial","trial_time",
                                    "cond", "gaze_x","gaze_y","aoi_left","aoi_right",
                                    "selection","selection_trial","trackloss"))

#create reverse time vector that leads up to the response (timepoint = 0)
datAoi$rev_time <- NA

for(dd in 1:length(uniqueID)){
  for(ii in 1:length(unique(datAoi$pair))){
    datAoi$rev_time[datAoi$pair == ii & datAoi$ID == uniqueID[dd] & datAoi$block == 1] <- rev(datAoi$trial_time[datAoi$pair == ii & datAoi$ID == uniqueID[dd] & datAoi$block == 1])
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
paste('percent trackloss =',nrow(data[data$trackloss == TRUE,])/nrow(data))

################################# TRACKLOSS ANALYSIS ###############################################################################
#participants with trackloss >.5 are excluded entirely
#'time-window' determines the time plotted in the following graphs (mean rt - 1 sd)


#time_window <- 0-(abs(mean(data$rev_time[data$cond == 'begin image display'], na.rm = TRUE)) - sd(data$rev_time[data$cond == 'begin image display'], na.rm = TRUE))
time_window <- -1.6

paste('the time window starts at:', time_window, 'seconds')

data_window <- subset_by_window(data, window_start_time = time_window, window_end_time = 0, rezero = FALSE, remove = TRUE)
trackloss <- trackloss_analysis(data = data_window)

data_window_clean <- clean_by_trackloss(data_window,
                                        participant_prop_thresh = 0.5,
                                        window_start_time = -Inf,
                                        window_end_time = 0)

droplevels(data_window_clean$ID)
data_window_clean$target <- as.factor(ifelse(test = grepl('left', data_window_clean$selection_trial),
                                             yes = 'left',
                                             no = 'right'))

################################# INSPECT DATA #####################################################################################

trackloss_data_window_clean <- trackloss_analysis(data = data_window_clean)
trackloss_data_window_participant <- unique(trackloss_data_window_clean[, c('ID','TracklossForParticipant')])

mean_trackloss <- mean(trackloss_data_window_participant$TracklossForParticipant)
paste(mean_trackloss, '= average trackloss trials for participants')

dat_summary_left <- describe_data(data_window_clean,
                                  describe_column = 'aoi_left',
                                  group_columns = c('target','ID'))

plot(dat_summary_left)

#summarize samples contributed per trial
paste('sample % contributed for trial =',mean(1-trackloss_data_window_participant$TracklossForParticipant))
paste('sample % deviation =',sd(trackloss_data_window_participant$TracklossForParticipant))

#summarize number of trials contributed by each participant
summary <- describe_data(data_window_clean, 'aoi_left', 'ID')
paste('trial avg per participant =',mean(summary$NumTrials))
paste('sd of trial avg =',sd(summary$NumTrials))


clean_index <- summary$ID[summary$NumTrials<50 | summary$SD == 0]
clean <- data_window_clean[!data_window_clean$ID %in% clean_index,]

################################# ANALYSES ############################################################################################
#window proportion viewing times
data_window_clean["aoi_overall"] <- NA
data_window_clean$aoi_overall <- ifelse(((data_window_clean$target == "left" & data_window_clean$aoi_left == TRUE)|(data_window_clean$target == "right" & data_window_clean$aoi_right == TRUE)), TRUE, FALSE)
sequence_window_clean <- make_time_sequence_data(data_window_clean,
                                                 time_bin_size = .05,
                                                 aois = c('aoi_overall'))

#same for cleaner dataset
clean['aoi_overall'] <- NA
clean$aoi_overall <- ifelse(((clean$target == "left" & clean$aoi_left == TRUE)|(clean$target == "right" & clean$aoi_right == TRUE)), TRUE, FALSE)
clean_seq <- make_time_sequence_data(clean,
                                     time_bin_size = .05,
                                     aois = c('aoi_overall'))


a <- plot(sequence_window_clean)+
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

b <- plot(clean_seq)+
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

multiplot(a,b, cols =2 )

#compute average proportion over 100ms approaching the decision
mean(sequence_window_clean$Prop[sequence_window_clean$TimeBin == -1|sequence_window_clean$TimeBin == -2], na.rm = TRUE)

#separate display of the viewing proportions
sequence_window_clean_right <- make_time_sequence_data(clean,
                                                       time_bin_size = .05,
                                                       predictor_columns = 'target',
                                                       aois = c('aoi_right'))


sequence_window_clean_left <- make_time_sequence_data(clean,
                                                      time_bin_size = .05,
                                                      predictor_columns = 'target',
                                                      aois = c('aoi_left'))

a <- plot(sequence_window_clean_right[sequence_window_clean_left$target == 'right',], predictor_column = 'target')+
  xlab('time until decision')+
  ylab('proportion viewing time')+
  coord_cartesian(xlim = c(-1.6,0), ylim = c(.45,.7))
b <- plot(sequence_window_clean_left[sequence_window_clean_left$target == 'left',], predictor_column = 'target')+
  xlab('time until decision')+
  ylab('proportion viewing time')+
  coord_cartesian(xlim = c(-1.6,0), ylim = c(.45,.7))

multiplot(a,b, cols = 2)

