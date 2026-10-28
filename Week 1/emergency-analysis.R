data <- read.csv('emergency.csv', header = TRUE, sep=",")
print(data[data$'recorded_time'<=5,])

n_patients <- nrow(data)

# Separate the data into patients with completed waiting times & still waiting after 5 hours
compl_wait <- data[data$'recorded_time' < 5,]
still_wait <- data[data$'recorded_time' >= 5,]

percent_compl_wait <- nrow(compl_wait)/nrow(data)*100
percent_still_wait <- nrow(still_wait)/nrow(data)*100

# Summarize the descriptive statistics of the (sub)set (mean, min, max)
summary_total <- summary(data$'recorded_time')
summary_compl_wait <- summary(compl_wait$'recorded_time')
summary_still_wait <- summary(still_wait$'recorded_time')

summary_df <- data.frame('description'=c('Total', 'Completed waiting', 'Still waiting after 5h'))

summary_df$'recorded_time' <- list(data$'recorded_time', compl_wait$'recorded_time', still_wait$'recorded_time')
summary_df$'percentage' <- c('', percent_compl_wait, percent_still_wait)
summary_df$'mean waiting time' <- list(summary_total['Mean'], summary_compl_wait['Mean'], summary_still_wait['Mean'])
summary_df$'median waiting time' <- list(summary_total['Median'], summary_compl_wait['Median'], summary_still_wait['Median'])
