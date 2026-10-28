install.packages('xtable')
library(xtable)

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
