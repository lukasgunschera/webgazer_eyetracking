######################## Exploratory Analyses of Webcam Eyetracking Data ############################

rm(list=ls())
myPackages <- c('dplyr','eyetrackingR','labelled','ggplot2','stringr','data.table','Matrix','lme4','zoo','sur','rlist')
lapply(myPackages, require, character.only = TRUE) 

setwd('/Users/lukasgunschera/Documents/UvA/Intern/data/filedrop')
data_window_clean <- list.load('data_window_clean.rds')
eyetrackingRdat <- list.load('eyetrackingRdat.rds')

#enter non-match values
apply(eyetrackingRdat, 2, function(x) gsub("^$|^ $", NA, x))
eyetrackingRdat$gender_match[is.na(eyetrackingRdat$gender_match)] <- 'nonmatch'
table(eyetrackingRdat$gender_match)

#prepare data for analysis (create time-window and perform trackloss analysis)
time_window <- -1.6
paste('the time window starts at:', time_window, 'seconds')

eyeRdat_window <- subset_by_window(eyetrackingRdat, window_start_time = time_window, 
                                   window_end_time = 0, rezero = FALSE, remove = TRUE)

eyeRdat_window_clean <- clean_by_trackloss(eyeRdat_window,
                                        participant_prop_thresh = 0.5,
                                        window_start_time = -Inf,
                                        window_end_time = 0)

#subset dataset for selection of male and female preference only (excluding bi and asexual participants)
table(eyeRdat_window_clean$attracted)

#create sex-match predictor and th
droplevels(eyeRdat_window_clean$ID)
eyeRdat_window_clean$sex_match <- as.factor(ifelse(test = grepl('nonmatch', eyeRdat_window_clean$gender_match),
                                             yes = 'nonmatch',
                                             no = 'match'))

eyeRdat_window_clean$target <- as.factor(ifelse(test = grepl('left', eyeRdat_window_clean$selection_trial),
                                             yes = 'left',
                                             no = 'right'))

table(eyeRdat_window_clean$sex_match)

#create overall AOI viewing distribution (irrespective of right or left)
eyeRdat_window_clean["aoi_overall"] <- NA

eyeRdat_window_clean$aoi_overall <- ifelse(((eyeRdat_window_clean$target == "left" & eyeRdat_window_clean$aoi_left == TRUE)|(eyeRdat_window_clean$target == "right" & eyeRdat_window_clean$aoi_right == TRUE)), TRUE, FALSE)
eyeRdat_w_c <- eyeRdat_window_clean

eyeRdat_window_clean <- eyeRdat_window_clean %>% filter(attracted == 'females' | attracted == 'males' )
eyeRdat_seq_clean <- make_time_sequence_data(eyeRdat_window_clean,
                                             time_bin_size = .05,
                                             predictor_columns = 'sex_match',
                                             aois = c('aoi_overall'))


plot(eyeRdat_seq_clean, predictor_column = 'sex_match')

plot(eyeRdat_window_clean, predictor_column = 'gender_match')+
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

## Cluster-base permutation analysis 
#runs a test on each time bin that quantifies statistical significance of the effect at each time bin
#the bins are shuffled and the test-then-cluster procedure is performed on each iteration of the data 


eyeRdat_seq <- make_time_sequence_data(eyeRdat_w_c,
                                         time_bin_size = .05, 
                                         predictor_columns = c("target"),
                                         aois = "aoi_overall",
                                         summarize_by = "ID" )


num_sub = length(unique((eyeRdat_seq$ID)))
threshold_t <-  qt(p = 1 - .05/2,df = num_sub-1) # two-tailed


eyeRdat_cluster <- make_time_cluster_data(eyeRdat_seq, 
                       test= "t.test", paired=TRUE,
                       predictor_column = "target", 
                       threshold = threshold_t) 
plot(eyeRdat_cluster) +  ylab("T-Statistic") + theme_light()

setwd('/Users/lukasgunschera/Documents/UvA/Intern/data/filedrop')
list.save(eyeRdat_cluster, 'eyeDat_cluster.rds')
list.save(eyeRdat_seq_clean, 'eyeDat_seq_sex.rds')
list.save(eyeRdat_window_clean, 'eyeDat_window.rds')

