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


# TRACKLOSS ANALYSIS

data_clean <- clean_by_trackloss(data,
                                 participant_prop_thresh = 0.5,
                                 window_start_time = Inf,
                                 window_end_time = 0)

droplevels(data_clean$ID)


################################ INSPECT DATA #####################################

describe_data <- describe_data(data_clean, 'aoi_left', 'ID')
plot(describe_data)



### Create data subsets

data_window <- subset_by_window(data_clean, window_start_time = -2.7, window_end_time = 0, rezero = FALSE, remove = TRUE)
data_time_window <- make_time_window_data(data_clean, aois = c('aoi_left','aoi_right'), predictor_columns = 'selection_trial', summarize_by = 'ID')
data_time_sequence <- make_time_sequence_data(data_window, time_bin_size = .1, aois = c('aoi_left'), predictor_columns = 'selection_trial', summarize_by = 'ID')

####################### ANALYSES ############################


### TIME SEQUENCE ###
# input are 'make.time.sequence.data' and 'subset.by.window'

time_analysis <- analyze_time_bins(data_time_sequence,
                                   treatment_level = "right",
                                   predictor_column = 'selection_trial',
                                   alpha = .05, test = "lm",
                                   aoi = c("aoi_right")
)

plot(data_time_sequence, predictor_column = c('selection_trial'))


### CLUSTER ###

print(data_time_cluster)
plot(data_time_window, predictor_columns = 'selection_trial', dv = "Prop")


### BOOTSPLINES ###

make_boot_splines_data(data_time_sequence, 
                       predictor_column = 'selection_trial',
                       within_subj = FALSE,
                       aoi = "aoi_right",
                       bs_samples = 500,
                       alpha = .05,
                       smoother = "smooth.spline"
)


### Example

data_window$target <- as.factor(ifelse(test = grepl('(left|right)', data_window$selection_trial),
                                       yes = 'left',
                                       no = 'right'
))

word_time <- make_time_sequence_data(data_window, time_bin_size = .25, 
                                     predictor_columns = 'target', aois = c('aoi_right','aoi_left'))

plot(word_time, predictor_column = 'target')


word_time$targetC <- ifelse(word_time$target == 'right', .5, -.5)
word_time$targetC <- word_time$targetC - mean(word_time$targetC)

library(lme4)
model <- lmer(Elog ~ targetC*(ot1 + ot2 + ot3 + ot4 + ot5) + (1 | Trial) + (1 | ID), data = word_time, REML = FALSE)
broom::tidy(model, effects="fixed")
drop1(model,~.,test="Chi")




