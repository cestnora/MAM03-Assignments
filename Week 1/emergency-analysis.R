# Add packages here
packages<- c('xtable', 'ggplot2')

# Install and load packages
lapply(packages, install.packages)
lapply(packages, library, character.only = TRUE)


data <- read.csv('emergency.csv', header = TRUE, sep=",")
print(data[data$'recorded_time'<=5,])

n_patients <- nrow(data)

# Separate the data into patients with completed waiting times & still waiting after 5 hours
compl_wait <- data[data$'recorded_time' < 5,]
still_wait <- data[data$'recorded_time' >= 5,]

percent_compl_wait <- nrow(compl_wait)/nrow(data)*100
percent_still_wait <- nrow(still_wait)/nrow(data)*100

# Create a dataframe to summarize the descriptive statistics
summary_df <- data.frame('description'=c('Total', 'Completed waiting', 'Still waiting after 5h'))

summary_df$'recorded_time' <- list(data$'recorded_time', compl_wait$'recorded_time', still_wait$'recorded_time')
summary_df$'percentage' <- c('', percent_compl_wait, percent_still_wait)
summary_df$'mean_waiting_time' <- sapply(summary_df$'recorded_time', mean)
summary_df$'median_waiting_time' <- sapply(summary_df$'recorded_time', median)
summary_df$'sd' <- sapply(summary_df$'recorded_time', sd)

ranges <- sapply(summary_df$'recorded_time', range)
summary_df$'min_waiting_time' <- ranges[1,]
summary_df$'max_waiting_time' <- ranges[2,]

summary_df$'recorded_time' <- NULL

xtable(summary_df)

# For the histogram we need to separate the incomplete observations by making an additional bar capturing the >5h observations.
hist_data <- data.frame(data)
data_summary <- summary(data$'recorded_time')
hist_data$'recorded_time' <- ifelse(hist_data$'indicator' == 0, hist_data$'recorded_time' + 0.0001,  hist_data$'recorded_time')
hist_data$'indicator' <- ifelse(hist_data$indicator == 0, 'incomplete (>5h)', 'complete')
q1 <- data_summary['1st Qu.'][[1]]
q3 <- data_summary['3rd Qu.'][[1]]

# Create the plot
ggplot(hist_data, aes(x=recorded_time, fill = indicator))+ 
  geom_histogram(
    binwidth=1, 
    boundary=0, 
    color='black', 
    alpha=0.8
    )+
  geom_vline(
    aes(xintercept=data_summary['Median'][[1]]), 
    linetype='dashed'
    )+
  annotate('text', x=3.3, y=9, label='median', angle=90)+
  
  geom_vline(
    xintercept=c(q1,q3),
        linetype='dotted',
        color='darkorange',
        linewidth=1
    )+
  annotate('text', x=q1, y = Inf, label='Q1', vjust=1.5, color='darkorange')+
  annotate('text', x=q3, y = Inf, label='Q3', vjust=1.5, color='darkorange')+
  
  ggtitle('Distribution of waiting time')+
  xlab('Recorded waiting time (hours)')+
  ylab('Count')+
  
  scale_x_continuous(
    breaks = seq(0, 6, by=1), 
    limits = c(0, 6)
    )+
  scale_y_continuous(
    breaks = seq(0, 10, by=1),
    limits = c(0, 10)
  )+
  
  scale_fill_manual(values = c('red', 'blue'))

# Note: Data is right skewed

###############################################################################


