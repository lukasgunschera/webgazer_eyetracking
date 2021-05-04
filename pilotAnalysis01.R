############################################## ANALYSIS FACE-ATTRACTIVENESS #########################################################

rm(list=ls())

library(dplyr); library(eyetrackingR); library(tidyr); library(labelled); library(ggplot2); library(stringr);
library(data.table); library(Matrix); library(lme4); library(ggplot2); library(zoo)

# Load Data for eyetrackingR

setwd('/Users/lukasgunschera/Documents/UvA/Intern/pilot/Analysis')
load('pilot01.Rda')

longData <- rbindlist(yPilot)
longData <- longData[-(1:22),]

longData <- longData[!is.na(longData$gaze_x)]

# add future selection to each individual trial

longData$selection_trial <- 0
selection_rev <- rev(longData$selection)
rev_selection <- na.locf(selection_rev)
longData$selection_trial <- rev(rev_selection)

table(longData$selection == longData$selection_trial)

var_label(longData$selection_trial) <- "selected face on trial"

###################################### DATA CLEANING ###############################################################################

#Remove participants with X variation in eye gaze estimation (may be varied)

uniqueID_long <- unique(longData$ID)

clean_ind <- character()
for (k in 1:length(uniqueID_long)){
  cleaning <- subset(longData, longData$ID == uniqueID_long[k])
  par_sd <- sd(cleaning$gaze_x, na.rm = TRUE)
  
  if(par_sd == 0){
    #change value based on how much variation is set as minimum criterion
    print(uniqueID_long[k])
    clean_ind <- c(clean_ind, as.character(uniqueID_long[k]))
  }
}

longData <- longData[!longData$ID %in% clean_ind,]



#calculate participant loss percentage 
length(unique(longData$ID))/length(uniqueID_long)

#Remove participants who always select the same response

uniqueID_long <- unique(longData$ID)
noResVar <- character()

for(i in 1:length(uniqueID_long)){
  ind <- uniqueID_long[i]
  ind <- as.character(ind)
  uniqueResPar <- unique(longData$selection_trial[longData$ID == ind])
  
  if(length(uniqueResPar) < 2){
    noResVar <- as.character(c(noResVar, ind))
  }
}

longData <- longData[!longData$ID %in% noResVar, ]

length(unique(longData$ID))/length(uniqueID_long)

#Remove trials with extremely large response times (60 seconds)
uniqueID_long <- unique(longData$ID)
max(longData$trial_time)

for(z in 1:length(uniqueID)){
  subPar <- subset(longData, longData$ID == uniqueID_long[z])
  a <- unique(subPar$pair[which(subPar$trial_time > 60)])
  longData <- longData[!(longData$ID == uniqueID_long[z] & longData$pair %in% a),]
}

data[data$trial_time > 60,]

#Remove trials with extremely low response times (<.5 seconds)

min(longData$trial_time[longData$cond == 'response'])
median(longData$trial_time[longData$cond == 'response'])
mean(longData$trial_time[longData$cond == 'response'])
sd(longData$trial_time[longData$cond == 'response'])


for(d in 1:length(uniqueID_long)){
  subPar <- subset(longData, longData$ID == uniqueID_long[d])
  b <- unique(subPar$pair[which(subPar$trial_time[subPar$cond == 'response'] < .5)])
  longData <- longData[!(longData$ID == uniqueID_long[z] & longData$pair %in% b),]
}

which(longData$trial_time[longData$cond == 'response'] < .5)

################################## FLEXIBLE MIDLINE EXCLUSION AND ADJUSTMENT #######################################################
#participants whose midline area exceeds 33% of the screen are excluded
#participants whose center varies more than 25% from the center are excluded

longData$var_midline <- NA
longData$mid_33 <- longData$res_x/3

for(ff in 1:length(uniqueID_long)){
  for(gg in 1:length(unique(longData$pair))){
    longData$var_midline[longData$ID == uniqueID_long[ff] & longData$pair == gg] <- median(longData$gaze_x[longData$ID == uniqueID_long[ff] & longData$pair == gg & longData$cond == 'fixation'])
    longData$var_midline_sd[longData$ID == uniqueID_long[ff] & longData$pair == gg] <- sd(longData$gaze_x[longData$ID == uniqueID_long[ff] & longData$pair == gg & longData$cond == 'fixation'])
  }
}

#exclude trials where the area around the midline is more than a third of the screen size

longData$exclude <- longData$var_midline_sd*2 >= longData$mid_33
table(longData$exclude)

longData <- longData[longData$exclude == FALSE]
table(longData$exclude)

#exclude trials where midline is more than

longData$mid_offset <- longData$var_midline - longData$mid_x
longData$exclude <- ifelse(longData$res_x*.25 <= longData$mid_offset, TRUE, FALSE)

table(longData$exclude)

longData <- longData[longData$exclude == FALSE]


########################################### ADD AOIS #############################################################################
# add important variables

longData$aoi <- FALSE
longData$aoi_left <- NA
longData$aoi_right <- NA

uniquePair <- unique(longData$pair)
uniqueID <- unique(longData$ID)
longData$Trial <- 1:nrow(longData)
uniqueIDL <- longData$Trial

uniqueLeftL <- rep(0, nrow(longData))
uniqueMidL <- longData$mid_x -1 # -1 indicates a margin in the middle that is considered neither right or left
uniqueMidR <- longData$mid_x +1 # +1 indicates a margin in the middle that is considered neither right or left
uniqueBottomL <- rep(0, nrow(longData))
uniqueTopL <- longData$res_y
uniqueRightL <- longData$res_x

responsiveMidX <- longData$var_midline - longData$var_midline_sd
responsiveMidY <- longData$var_midline + longData$var_midline_sd
responsiveLeft <- rep(0, nrow(longData)) + longData$mid_offset
responsiveRight <- longData$res_x + longData$mid_offset


aoiLeft <- data.frame(Trial = uniqueIDL, 
                      Left = responsiveLeft, Right = responsiveMidX,
                      Top = uniqueTopL, Bottom = uniqueBottomL)

aoiRight <- data.frame(Trial = uniqueIDL,
                       Left = responsiveMidY, Right = responsiveRight,
                       Top = uniqueTopL, Bottom = uniqueBottomL)

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


################################# DATA HAVE BEEN LAODED ##############################################################################

table(datAoi$aoi_left)
table(datAoi$aoi_right)
sum(datAoi$aoi_left == FALSE & datAoi$aoi_right == FALSE) #should be equivalent to trackloss data after following function

sum(is.na(datAoi$aoi_left))

data <- make_eyetrackingr_data(datAoi, 
                               participant_column = "ID",
                               trial_column = "Trial",
                               item_columns = "pair",
                               time_column = "rev_time",
                               trackloss_column = "trackloss",
                               aoi_columns = c('aoi_left','aoi_right'),
                               treat_non_aoi_looks_as_missing = TRUE
)

nrow(data[data$trackloss == TRUE,])

# data that fall outside of the AOIs will be coded as NA, to do otherwise (code them as FALSE, FALSE) change the 
# 'treat_non_aoi_looks_as_missing' argument to FALSE

# compute proportion of data falling outside of AOIs 

sum(is.na(data$aoi_left)) / length(data$aoi_left) * 100

################################# DATA CLEANING ####################################################################################

################################# TRACKLOSS ANALYSIS ###############################################################################
#participants with trackloss >.5 are excluded entirely

test <- subset(data, data$trackloss == TRUE)

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

#WINDOW ANALYIS BASIC

data_window_agg <- make_time_window_data(data_window_clean, 
                                                    aois = c('aoi_left','aoi_right'),
                                                    predictor_columns = c('target'),
                                                    summarize_by = "ID")

plot(data_window_agg, predictor_columns = 'target', dv = 'ArcSin')
describe_data(data_window_agg, describe_column = 'ArcSin', group_columns = 'target') #show condition means

#Looking time comparisons

t.test(ArcSin ~ target, data = data_window_agg, paired = TRUE) #simpled paired t-test between total looking times

response_window_agg <- make_time_window_data(data_window_clean, 
                                         aois = c('aoi_left','aoi_right'),
                                         predictor_columns = c('target','webcam','glasses'))

response_window_agg$targetC <- ifelse(response_window_agg$target == 'right', .5, -.5)
response_window_agg$targetC <- as.numeric(scale(response_window_agg$targetC, center = TRUE, scale = FALSE))

model_time_window <- lmer(Elog ~ targetC + (1 + targetC | Trial) + (1 | ID), 
                          data = response_window_agg, REML = FALSE)

######################################## TIME SEQUENCE ANALYSES ###################################################################

data_time_sequence <- make_time_sequence_data(data_window_clean,
                                              time_bin_size = .25,
                                              predictor_columns = c('target'),
                                              aois = c('aoi_left','aoi_right'),
                                              summarize_by = 'ID')

plot(data_time_sequence, predictor_column = 'target') + 
  theme_light() +
  coord_cartesian(ylim = c(0.2,0.8))

tb_analysis <- analyze_time_bins(data = data_time_sequence,
                                 predictor_column = 'target',
                                 p_adjust_method = 'holm',
                                 aoi = 'aoi_left',
                                 test = 't.test',
                                 alpha = .05)

plot(tb_analysis, type = "estimate") + theme_light()
summary(tb_analysis)

#BOOTSTRAPPED SMOOTHED DIVERGENCE ANALYSIS
tb_boot <- analyze_time_bins(data = data_time_sequence, predictor_column = 'target',
                             test = 'boot_splines',
                             within_subj = TRUE,
                             bs_samples = 1000,
                             aoi = 'aoi_left',
                             alpha = .05)

plot(tb_boot) + theme_light()
summary(tb_boot)


#CLUSTER ANALYSIS
#had two advantages over the previous bootstrapping alternative as it does control for false-alarm rate while sacrificing
#little sensitivity and allows for a larger variety of statistical tests (wilcox, lm, lmer, ...)


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








