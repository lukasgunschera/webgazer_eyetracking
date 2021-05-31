########## EXPLORATORY ANALYSES ######################

#WINDOW ANALYIS BASIC
data_window_agg <- make_time_window_data(data_window_clean, 
                                         aois = c('aoi_left','aoi_right'),
                                         predictor_columns = c('target'),
                                         summarize_by = "ID")

plot(data_window_agg, predictor_columns = 'target', dv = 'ArcSin')
describe_data(data_window_agg, describe_column = 'ArcSin', group_columns = 'target') #show condition means

#total looking time comparisons // expect no difference in total viewing times
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


#run tests on each time bin // expect large parameter estimates approaching time = 0
tb_analysis <- analyze_time_bins(data = data_time_sequence,
                                 predictor_column = 'target',
                                 p_adjust_method = 'holm',
                                 aoi = 'aoi_left',
                                 test = 't.test',
                                 alpha = .05)

plot(tb_analysis, type = "estimate") + theme_light()
summary(tb_analysis)

#BOOTSTRAPPED SMOOTHED DIVERGENCE ANALYSIS
#bootstrap analysis of time bins // expect large parameter estimates approaching time = 0
tb_boot <- analyze_time_bins(data = data_time_sequence, predictor_column = 'target',
                             test = 'boot_splines',
                             within_subj = TRUE,
                             bs_samples = 1000,
                             aoi = 'aoi_left',
                             alpha = .05)

plot(tb_boot) + theme_light()
summary(tb_boot)

#########################CLUSTER ANALYSIS###########################################################################################3
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

# plotted viewing times for each participant
uniqueID <- unique(data_window_clean$ID)

seq_dat <- list()

for(oo in 1:length(uniqueID)){
  currentID <- uniqueID[oo]
  seq_dat[[oo]] <- make_time_sequence_data(data_window_clean[data_window_clean$ID == currentID,],
                                           time_bin_size = .25,
                                           predictor_columns = 'target',
                                           aois = c('aoi_left','aoi_right'))
}

plot(seq_dat[[17]], predictor_column = 'target')
