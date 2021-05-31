### PILOT DATA PREPROCESSING ###

rm(list=ls())
library(tidyr); library(dplyr); library(magrittr); library(haven); library(here);
library(stringr); library(purrr); library(labelled); library(data.table); library(R.utils)

# LOAD QUALITRICS

setwd('/Users/lukasgunschera/Documents/UvA/Intern/data')
qualitrics01 <- read.csv('eyeTrackingDemo_May 26, 2021_01.09.csv', header = TRUE)
colnames(qualitrics01)

qualitrics01 %<>% select(c("Progress","Duration..in.seconds.","RecordedDate",
                           "Q3","Q4","Q19","Q5_Browser","Q5_Version",
                           "Q5_Operating.System","Q5_Resolution","Q14",
                           "SC0","PROLIFIC_PID","Q17"))

# LOAD DATAFILES PROLIFIC

setwd('/Users/lukasgunschera/Documents/UvA/Intern/data/dat_clean')
my.folder <- paste0(getwd(), '/', 'dat')
setwd(my.folder)
my.delete.empty.csv = lapply(Filter(function(x) countLines(x)<=25, list.files(pattern='pav.csv')), unlink)
my.model.files <- list.files(my.folder, pattern="^model.*?\\.csv")
my.model.list  <- lapply(paste0(my.folder, my.model.files), read.csv)
datadamy.model.data  <- do.call(rbind, my.model.list)

setwd('/Users/lukasgunschera/Documents/UvA/Intern/data/dat_clean')
allPav <-  list.files(path = '/Users/lukasgunschera/Documents/UvA/Intern/data/dat',
                      pattern = "pav.csv",
                      full.names = TRUE,
                      recursive = TRUE)

# remove unnecessary columns 
yPilot <- list()
yPilot <- lapply(1:length(allPav), function(f){
  
  print(f)
  
  dat01 <-  read.csv(allPav[f], header = TRUE, na.strings=c(""," ","NA"))
  dat02 <- dat01 %>% select(-c("expTrials2.ran", 
                               "expTrials2.thisIndex", 
                               "expTrials2.thisTrialN", 
                               "expTrials2.thisRepN", 
                               "psychopyVersion",
                               "mouse_3.leftButton",
                               "mouse_3.midButton",
                               "mouse_3.rightButton",
                               "date", "expName",
  ))
  
  ## CONDITIONS WHERE LOOP WILL FAIL 
  
  if(!'response_type' %in% colnames(dat02)){
    0
  }else{
    
    
    
    # match prolific and qualitrics
    ind <- 0
    ind <- which(dat01[1, 'participant'] == qualitrics01['PROLIFIC_PID'])
    
    if(length(ind) > 2){
      ind <- ind[ind == max(ind)]
    }
    
    dat02 <- merge(dat01, qualitrics01[ind, ], all = TRUE)
    
    
    # Create dataset & indices 
    
    a01 <-  which(!is.na(dat02$mouse_3.x))
    a02 <- min(a01)
    a03 <- max(a01)
    
    dat03 <- data_frame(
      ID = dat02$participant, qual_id = dat02$PROLIFIC_PID,
      trials_ran = dat02$trials.ran,completion = dat02$Progress, 
      consent = dat02$Q3, webcam = dat02$Q4, webcam_type = dat02$Q19, 
      res = dat02$Q5_Resolution, res_x = dat02$screen_pixels_x, res_y = dat02$screen_pixels_y,
      mid_x = dat02$screen_pixels_x/2, mid_y = dat02$screen_pixels_y/2,
      mid_x_individual = NA,
      
      cal_x = dat02$calibration_x, cal_y = dat02$calibration_y,
      cal_number = c(a02:a03, rep(NA, nrow(dat02)- a03)),
      mouse_x = dat02$mouse_3.x, mouse_y = dat02$mouse_3.y,
      cal_off_x = cal_x - mouse_x, cal_off_y = cal_y - mouse_y,
      
      gaze_x = dat02$eye.gaze.x, gaze_y = dat02$eye.gaze.y,
      cond = dat02$stage, pair = dat02$pair_number, block = dat02$Block,
      trial_time = dat02$trial_time, time_cont = 0,
      selection = dat02$response_type,
      
    )
    
    # Adding direction estimates
    
    dat03 <- dat03 %>% rowwise() %>%
      mutate(est_direction = if_else(gaze_x < mid_x, 'left', 'right'))
    
    
    gaze_x_fixation <- dat03$gaze_x[dat03$cond == 'fixation' | dat03$cond == 'begin fixation']
    
    fixationDisplay <- subset(dat03, dat03$cond == 'fixation' | 
                                dat03$cond == 'begin fixation')
    
    mid_x_indiv <- numeric()
    for(o in 1:max(dat03$pair, na.rm = TRUE)){
      mid_x_indiv <- c(mid_x_indiv, mean(gaze_x_fixation[fixationDisplay$pair == o], na.rm = TRUE))
    }
    
    
    dat03 <- dat03 %>% rowwise() %>%
      mutate(mid_x_individual = case_when (pair == 1 ~ mid_x_indiv[1],
                                           pair == 2 ~ mid_x_indiv[2],
                                           pair == 3 ~ mid_x_indiv[3],
                                           pair == 4 ~ mid_x_indiv[4],
                                           pair == 5 ~ mid_x_indiv[5],
                                           pair == 6 ~ mid_x_indiv[6],
                                           pair == 7 ~ mid_x_indiv[7],
                                           pair == 8 ~ mid_x_indiv[8],
                                           pair == 9 ~ mid_x_indiv[9],
                                           pair == 10 ~ mid_x_indiv[10],
                                           pair == 11 ~ mid_x_indiv[11],
                                           pair == 12 ~ mid_x_indiv[12],
                                           pair == 13 ~ mid_x_indiv[13],
                                           pair == 14 ~ mid_x_indiv[14],
                                           pair == 15 ~ mid_x_indiv[15],
                                           pair == 16 ~ mid_x_indiv[16],
                                           pair == 17 ~ mid_x_indiv[17],
                                           pair == 18 ~ mid_x_indiv[18]))
    
    
    
    
    # Accuracy calculations using the average viewing position across trial
    
    imageDisplay <- subset(dat03, dat03$cond == 'image display' | 
                             dat03$cond == 'begin image display' |
                             dat03$cond == 'response')
    gaze_x_image <- imageDisplay$gaze_x
    
    avg_gaze_x <- numeric()
    for(t in 1:max(dat03$pair, na.rm = TRUE)){
      avg_gaze_x <- c(avg_gaze_x, mean(gaze_x_image[imageDisplay$pair == t], na.rm = TRUE))
    }
    
    dat03 <- dat03 %>% rowwise() %>%
      mutate(est_avg = case_when(pair == 1 ~ avg_gaze_x[1],
                                 pair == 2 ~ avg_gaze_x[2],
                                 pair == 3 ~ avg_gaze_x[3],
                                 pair == 4 ~ avg_gaze_x[4],
                                 pair == 5 ~ avg_gaze_x[5],
                                 pair == 6 ~ avg_gaze_x[6],
                                 pair == 7 ~ avg_gaze_x[7],
                                 pair == 8 ~ avg_gaze_x[8],
                                 pair == 9 ~ avg_gaze_x[9],
                                 pair == 10 ~ avg_gaze_x[10],
                                 pair == 11 ~ avg_gaze_x[11],
                                 pair == 12 ~ avg_gaze_x[12],
                                 pair == 13 ~ avg_gaze_x[13],
                                 pair == 14 ~ avg_gaze_x[14],
                                 pair == 15 ~ avg_gaze_x[15],
                                 pair == 16 ~ avg_gaze_x[16],
                                 pair == 17 ~ avg_gaze_x[17],
                                 pair == 18 ~ avg_gaze_x[18]
      ))
    
    dat03 <- dat03 %>% rowwise() %>%
      mutate(est_dir_avg = case_when(est_avg < mid_x ~ "left",
                                     est_avg > mid_x ~ "right"
      ))
    
    
    
    ### LABELLING
    attributes(dat03)
    
    var_label(dat03$webcam) <- "1 = yes, 2 = no"
    var_label(dat03$webcam_type) <- "1 = built-in, 2 = external, 3 = none"
    var_label(dat03$consent) <- "1 = yes, 0 = no"
    var_label(dat03$res) <- "screen size"
    var_label(dat03$mid_x) <- "screen x-midline"
    var_label(dat03$mid_y) <- "screen y-midline"
    #var_label(dat03$framerate) <- "screen framerate"
    var_label(dat03$selection) <- "selected face"
    var_label(dat03$pair) <- "number of facepair"
    var_label(dat03$qual_id) <- "prolific id obtained from qualitrics"
    var_label(dat03$est_direction) <- "x-estimated direction at given point in time"
    var_label(dat03$est_dir_avg) <- "x-esitmated direction over entire face-pair"
    var_label(dat03$mid_x_individual) <- "midline based on fixations of respective pair"
    var_label(dat03$est_avg) <- "average x-value over face-pair"
    
    dat03
  }
}
)

for(ii in 1:length(yPilot)){
  yPilot[[ii]][['ID']] <- ii
  yPilot[[ii]][['qual_id']] <- ii
}

setwd('/Users/lukasgunschera/Documents/UvA/Intern/data')
save(yPilot, file = "faceTrack.Rda")
