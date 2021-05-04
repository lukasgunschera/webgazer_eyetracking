##### ANALYSIS FACE-ATTRACTIVENESS #####

rm(list=ls())

library(dplyr); library(eyetrackingR); library(tidyr); library(labelled); library(ggplot2); library(stringr);
library(data.table); library(Matrix); library(lme4); library(ggplot2); library(zoo)

# Load Data for eyetrackingR

setwd('/Users/lukasgunschera/Documents/UvA/Intern/pilot/Analysis')
load('pilot01.Rda')

longData <- rbindlist(yPilot)
longData <- longData[-(1:22),]

longData <- longData[!is.na(longData$gaze_x)]
longData$Trial <- 1:nrow(longData)
longData$aoi <- FALSE
longData$aoi_left <- logical()
longData$aoi_right <- logical()

uniquePair <- unique(longData$pair)
uniqueID <- unique(longData$ID)
uniqueIDL <- longData$Trial
uniqueLeftL <- rep(0, nrow(longData))
uniqueMidL <- longData$mid_x -1 # -1 indicates a margin in the middle that is considered neither right or left
uniqueMidR <- longData$mid_x +1 # +1 indicates a margin in the middle that is considered neither right or left
uniqueBottomL <- rep(0, nrow(longData))
uniqueTopL <- longData$res_y
uniqueRightL <- longData$res_x


aoiLeft <- data.frame(Trial = uniqueIDL, 
                      Left = uniqueLeftL, Right = uniqueMidL,
                      Top = uniqueTopL, Bottom = uniqueBottomL)

aoiRight <- data.frame(Trial = uniqueIDL,
                       Left = uniqueMidR, Right = uniqueRightL,
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

datAoi$trackloss <- logical()

# add future selection to each individual trial

datAoi$selection_trial <- 0
selection_rev <- rev(datAoi$selection)
rev_selection <- na.locf(selection_rev)
datAoi$selection_trial <- rev(rev_selection)

table(datAoi$selection == datAoi$selection_trial)

var_label(datAoi$selection_trial) <- "selected face on trial"

# reorder

names(datAoi)

datAoi <- subset(datAoi, select = c("ID","qual_id","trials_ran","completion","consent","webcam","webcam_type",
                                    "glasses","browser","cal_x","cal_y","cal_number","mouse_x","mouse_y",
                                    "cal_off_x","cal_off_y","framerate","res","res_x","res_y",
                                    "block","pair","aoi","Trial","trial_time",
                                    "cond", "gaze_x","gaze_y","aoi_left","aoi_right",
                                    "selection","selection_trial","trackloss"))

### create reverse time vector and set response time to 0

datAoi$rev_time <- NA

for(dd in 1:length(uniqueID)){
  for(ii in 1:length(unique(datAoi$pair))){
    datAoi$rev_time[datAoi$pair == ii & datAoi$ID == uniqueID[dd] & datAoi$block == 1] <- rev(datAoi$trial_time[datAoi$pair == ii & datAoi$ID == uniqueID[dd] & datAoi$block == 1])
  }
}

datAoi$rev_time[datAoi$cond == 'response'] <- 0
datAoi$rev_time <- datAoi$rev_time * -1


################## DATA HAVE BEEN LAODED #############################

table(datAoi$aoi_left)
table(datAoi$aoi_right)

length(is.na(datAoi$aoi_left))

data <- make_eyetrackingr_data(datAoi, 
                               participant_column = "ID",
                               trial_column = "Trial",
                               item_columns = "pair",
                               time_column = "rev_time",
                               trackloss_column = "trackloss",
                               aoi_columns = c('aoi_left','aoi_right'),
                               treat_non_aoi_looks_as_missing = TRUE
)

# data that fall outside of the AOIs will be coded as NA, to do otherwise (code them as FALSE, FALSE) change the 
# 'treat_non_aoi_looks_as_missing' argument to FALSE

# compute proportion of data falling outside of AOIs 

sum(is.na(data$aoi_left)) / length(data$aoi_left) * 100

########################## DATA CLEANING ################################

#Remove participants with X variation in eye gaze estimation (may be varied)

uniqueID <- unique(data$ID)

clean_ind <- character()
for (k in 1:length(uniqueID)){
  cleaning <- subset(data, data$ID == uniqueID[k])
  par_sd <- sd(cleaning$gaze_x, na.rm = TRUE)
  
  if(par_sd == 0){
    #change value based on how much variation is set as minimum criterion
    
    clean_ind <- c(clean_ind, as.character(uniqueID[k]))
  }
}

data <- data[!data$ID %in% clean_ind,]

#calculate participant loss percentage 
length(unique(data$ID))/length(uniqueID)

#Remove participants who always select the same response


noResVar <- character()

for(i in 1:length(uniqueID)){
  ind <- uniqueID[i]
  ind <- as.character(ind)
  uniqueResPar <- unique(data$selection_trial[data$ID == ind])
  
  if(length(uniqueResPar) < 2){
    noResVar <- as.character(c(noResVar, ind))
  }
}

data <- data[!data$ID %in% noResVar, ]

#Remove trials with extremely large response times (60 seconds)

max(data$trial_time)

for(z in 1:length(uniqueID)){
  subPar <- subset(data, data$ID == uniqueID[z])
  a <- unique(subPar$pair[which(subPar$trial_time > 60)])
  data <- data[!(data$ID == uniqueID[z] & data$pair %in% a),]
}

data[data$trial_time > 60,]


#Remove trials with extremely low response times (<.5 seconds)

min(data$trial_time[data$cond == 'response'])
median(data$trial_time[data$cond == 'response'])
mean(data$trial_time[data$cond == 'response'])
sd(data$trial_time[data$cond == 'response'])


for(d in 1:length(uniqueID)){
  subPar <- subset(data, data$ID == uniqueID[d])
  b <- unique(subPar$pair[which(subPar$trial_time[subPar$cond == 'response'] < .5)])
  data <- data[!(data$ID == uniqueID[z] & data$pair %in% b),]
}

which(data$trial_time[data$cond == 'response'] < .5)

################################# TRACKLOSS ANALYSIS ###############################################################################
#participants with trackloss >.5 are excluded entirely

data_window <- subset_by_window(data, window_start_time = -2.7, window_end_time = 0, rezero = FALSE, remove = TRUE)
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

dat_summary_left <- describe_data(data_window_clean,
              describe_column = 'aoi_left',
              group_columns = c('target','ID'))

plot(dat_summary_left)

#summarize samples contributed per trial

mean(1-trackloss_data_window_participant$TracklossForParticipant)
sd(trackloss_data_window_participant$TracklossForParticipant)

#summarize number of trials contributed by each participant

summary <- describe_data(data_window_clean, 'aoi_left', 'ID')
mean(summary$NumTrials)
sd(summary$NumTrials)

################################# ANALYSES ############################################################################################
#window proportion viewing times

sequence_window_clean <- make_time_sequence_data(data_window_clean,
                                                 time_bin_size = .25,
                                                 predictor_columns = 'target',
                                                 aois = c('aoi_left','aoi_right'))

plot(sequence_window_clean, predictor_column = 'target')



#time sequence analyses

data_time_sequence <- make_time_sequence_data(data_window_clean,
                                              time_bin_size = .25,
                                              predictor_columns = c('target'),
                                              aois = c('aoi_left','aoi_right'),
                                              summarize_by = 'ID')

plot(data_time_sequence, predictor_column = 'target') + 
  theme_light() +
  coord_cartesian(ylim = c(0.35,0.65))

tb_analysis <- analyze_time_bins(data = data_time_sequence,
                                 predictor_column = 'target',
                                 p_adjust_method = 'holm',
                                 aoi = 'aoi_left',
                                 test = 't.test',
                                 alpha = .05)

plot(tb_analysis, type = "estimate") + theme_light()
summary(tb_analysis)

#bootstrpped smoothed divergence analysis
tb_boot <- analyze_time_bins(data = data_time_sequence, predictor_column = 'target',
                             test = 'boot_splines',
                             within_subj = TRUE,
                             bs_samples = 1000,
                             aoi = 'aoi_left',
                             alpha = .05)

plot(tb_boot) + theme_light()
summary(tb_boot)


#cluster analysis

num_sub <- length(unique((data_time_sequence$ID)))
threshold_dat = qt(p = 1 - .05/2, df = num_sub-1) 

data_time_cluster <- make_time_cluster_data(data_time_sequence, test = 't.test', paired = TRUE,
                                            aoi = 'aoi_left',
                                            predictor_column = 'target', threshold = threshold_dat)

plot(data_time_cluster) +  ylab("T-Statistic") + theme_light()
summary(data_time_cluster)

#bootstrap null distribution of clusters

clust_analysis <- analyze_time_clusters(data_time_cluster, within_subj = TRUE, paired = TRUE,
                                        samples=500)

plot(clust_analysis) + theme_light()
summary(clust_analysis)
