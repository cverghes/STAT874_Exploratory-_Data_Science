library(plyr)
dat_all = read.csv("C:/Users/chris/Desktop/MDSAI/STAT847/Midterm/Mind Monitor detailed data 2024-01-21.csv")
dat_summary = ddply(dat_all, "sessionnum", summarise,
                    activity = activity[1],
                    session_time = session_time[1],
                    mean_alpha = mean(total_alpha, na.rm=TRUE),
                    mean_beta = mean(total_beta, na.rm=TRUE),
                    mean_gamma = mean(total_gamma, na.rm=TRUE),
                    mean_delta = mean(total_delta, na.rm=TRUE),
                    mean_theta = mean(total_theta, na.rm=TRUE),
                    var_alpha = var(total_alpha, na.rm=TRUE),
                    var_beta = var(total_beta, na.rm=TRUE),
                    var_gamma = var(total_gamma, na.rm=TRUE),
                    var_delta = var(total_delta, na.rm=TRUE),
                    var_theta = var(total_theta, na.rm=TRUE),
                    blinks_minute = Nblinks[1]/session_time[1]*60,
                    jaws_minute = Njaw[1]/session_time[1]*60,
                    mean_pos_xy = mean(sqrt(Accelerometer_X^2 + Accelerometer_Y^2), na.rm=TRUE),
                    mad_accel = mean(abs(Accelerometer_X^2 + Accelerometer_Y^2 + Accelerometer_Z^2 - 1), na.rm=TRUE),
                    rmse_gyro = mean(sqrt(Gyro_X^2 + Gyro_Y^2 + Gyro_Z^2), na.rm=TRUE)
)
library(tidyverse)

dat_rest = dat_summary %>% filter(activity == 'resting')
dat_active = dat_summary %>% filter(activity != 'resting')
summary(dat_active[c("blinks_minute","jaws_minute")])
summary(dat_rest[c("blinks_minute","jaws_minute")])

t.test(dat_rest$blinks_minute, dat_active$blinks_minute)
t.test(dat_rest$jaws_minute, dat_active$jaws_minute)
# Set up a 2x2 layout for the plots
par(mfrow = c(2, 1))
plot(density(dat_rest$blinks_minute), main = "Kernel Density Estimation for bpm rest", col = "blue")
plot(density(dat_active$blinks_minute), main = "Kernel Density Estimation for bpm active", col = "blue")
plot(density(dat_rest$jaws_minute), main = "Kernel Density Estimation", col = "blue")
plot(density(dat_active$jaws_minute), main = "Kernel Density Estimation", col = "blue")




#####################################################################################################################################################
#Part 2

dat_sum = read.csv("C:/Users/chris/Desktop/MDSAI/STAT847/Midterm/Mind Monitor session summaries 2024-01-21.csv")
dat_rest = dat_sum %>% filter(activity == 'resting')
dat_active = dat_sum %>% filter(activity != 'resting')
# List of features for which you want to perform t-tests
features <- c("mean_alpha", "mean_beta", "mean_gamma", "mean_delta", "mean_theta",
              "var_alpha", "var_beta", "var_gamma", "var_delta", "var_theta")

# Create an empty data frame to store results
p_values_df <- data.frame(Feature = character(), P_Value = numeric(), stringsAsFactors = FALSE)

# Loop through each feature and perform t-test
for (feature in features) {
  p_value <- t.test(dat_rest[[feature]], dat_active[[feature]])$p.value
  p_values_df <- rbind(p_values_df, data.frame(Feature = feature, P_Value = p_value))
}

# Display the table of p-values
print(p_values_df)


################################################################################################################
#Part 3
dat_active = dat_all %>% filter(activity != 'resting')
min_sessionnum <- min(dat_active$sessionnum)
# Subset the data frame for rows where sessionnum is the minimum and activity is resting
isolated_values <- dat_active[dat_active$sessionnum == min_sessionnum, ]
max_time = max(isolated_values$time_in)
first_20 <- isolated_values[isolated_values$time_in < max_time*0.2, ]
last_20 <- isolated_values[isolated_values$time_in > max_time*0.8, ]
t.test(first_20$total_alpha, last_20$total_alpha)$p.value
t.test(first_20$total_beta, last_20$total_beta)$p.value
t.test(first_20$total_gamma, last_20$total_gamma)$p.value
t.test(first_20$total_delta, last_20$total_delta)$p.value
t.test(first_20$total_theta, last_20$total_theta)$p.value
column_names <- c('total_alpha', 'total_beta', 'total_gamma', 'total_delta', 'total_theta')

# Set up the layout for the plot
par(mfrow = c(2, 3), mar = c(4, 4, 2, 1))

# Loop through each column and create overlaid histograms
for (col in column_names) {
  hist(first_20[[col]], col = rgb(0.2, 0.8, 0.2, alpha = 0.5), main = paste("Histogram of", col), xlab = col, ylim = c(0, max(hist(first_20[[col]], plot = FALSE)$counts, hist(last_20[[col]], plot = FALSE)$counts)))
  hist(last_20[[col]], col = rgb(0.8, 0.2, 0.2, alpha = 0.5), add = TRUE)
  legend("topright", legend = c("first_20", "last_20"), fill = c(rgb(0.2, 0.8, 0.2, alpha = 0.5), rgb(0.8, 0.2, 0.2, alpha = 0.5)))
}
################################################################################################################
#Part 4
dat_rest = dat_all %>% filter(activity == 'resting')
min_sessionnum <- min(dat_rest$sessionnum)
# Subset the data frame for rows where sessionnum is the minimum and activity is resting
isolated_values <- dat_rest[dat_rest$sessionnum == min_sessionnum, ]
max_time = max(isolated_values$time_in)
first_20 <- isolated_values[isolated_values$time_in < max_time*0.2, ]
last_20 <- isolated_values[isolated_values$time_in > max_time*0.8, ]
t.test(first_20$total_alpha, last_20$total_alpha)$p.value
t.test(first_20$total_beta, last_20$total_beta)$p.value
t.test(first_20$total_gamma, last_20$total_gamma)$p.value
t.test(first_20$total_delta, last_20$total_delta)$p.value
t.test(first_20$total_theta, last_20$total_theta)$p.value
column_names <- c('total_alpha', 'total_beta', 'total_gamma', 'total_delta', 'total_theta')

# Set up the layout for the plot
par(mfrow = c(2, 3), mar = c(4, 4, 2, 1))

# Loop through each column and create overlaid histograms
for (col in column_names) {
  hist(first_20[[col]], col = rgb(0.2, 0.8, 0.2, alpha = 0.5), main = paste("Histogram of", col), xlab = col, ylim = c(0, max(hist(first_20[[col]], plot = FALSE)$counts, hist(last_20[[col]], plot = FALSE)$counts)))
  hist(last_20[[col]], col = rgb(0.8, 0.2, 0.2, alpha = 0.5), add = TRUE)
  legend("topright", legend = c("first_20", "last_20"), fill = c(rgb(0.2, 0.8, 0.2, alpha = 0.5), rgb(0.8, 0.2, 0.2, alpha = 0.5)))
}

###########################################################################################################
#Part 5
cor_summary <- dat_all %>%
  na.omit() %>%  # Remove rows with NA values
  group_by(sessionnum) %>%
  summarise(
    activity = first(activity),
    cor_value = {
      # Check for zero standard deviations before calculating correlation
      if (sd(Alpha_TP9) != 0 & sd(Alpha_TP10) != 0) {
        cor(Alpha_TP9, Alpha_TP10)
      } else {
        NA  # Set correlation to NA if standard deviation is zero
      }
    }
  )
dat_rest<-cor_summary %>% filter(activity == 'resting')
cor_summary
dat_active = dat_all %>% filter(activity != 'resting')

###########################################################################################################
#Part 6
dat_accelerometer <- dat_all %>%
  na.omit() %>%
  group_by(sessionnum) %>% 
  summarise(
    activity = first(activity),
    mean_X = mean(Accelerometer_X),
    mean_Y = mean(Accelerometer_Y),
    mean_Z = mean(Accelerometer_Z),
    std_X = sd(Accelerometer_X),
    std_Y = sd(Accelerometer_Y),
    std_Z = sd(Accelerometer_Z)
  )
dat_accelerometer
###########################################################################################################
#Part 7
library(ggplot2)
# Subset data for session number 11
dat_11 <- dat_all %>% filter(sessionnum == 11  & !is.na(Delta_TP9))

## Smooth over time
ss_alpha = smooth.spline(dat_11$time_in, dat_11$total_alpha, spar=0.5)
ss_beta = smooth.spline(dat_11$time_in, dat_11$total_beta, spar=0.5)
ss_gamma = smooth.spline(dat_11$time_in, dat_11$total_gamma, spar=0.5)
ss_delta = smooth.spline(dat_11$time_in, dat_11$total_delta, spar=0.5)
ss_theta = smooth.spline(dat_11$time_in, dat_11$total_theta, spar=0.5)
plot(ss_alpha, type='n', col="White", lwd=3, ylim=c(-2,3),
     main="Brainwaves Over Time", xlab = "Time", ylab = "Smoothed Brainwave Value")
lines(ss_alpha, type='l', col="Red", lwd=3)
lines(ss_beta, type='l', col="Blue", lwd=3)
lines(ss_gamma, type='l', col="Black", lwd=3)
lines(ss_delta, type='l', col="Darkgreen", lwd=3)
lines(ss_theta, type='l', col="Grey", lwd=3)
# Add legend
legend("topright", legend = c("Alpha", "Beta", "Gamma", "Delta", "Theta"),
       col = c("Red", "Blue", "Black", "Darkgreen", "Grey"), lty = 1, lwd = 2.5, cex = 0.8)
###########################################################################################################
#Part 8

dat_alpha_means <- dat_all %>%
  na.omit() %>%
  group_by(sessionnum) %>%
  summarise(
    activity = first(activity),
    Total_mean_Alpha = ifelse(is.na(mean(Alpha_AF7[Alpha_AF7 != 0], na.rm = TRUE)),0, mean(Alpha_AF7[Alpha_AF7 != 0], na.rm = TRUE)) +
      ifelse(is.na(mean(Alpha_AF8[Alpha_AF8 != 0], na.rm = TRUE)), 0, mean(Alpha_AF8[Alpha_AF8 != 0], na.rm = TRUE)) +
      ifelse(is.na(mean(Alpha_TP10[Alpha_TP10 != 0], na.rm = TRUE)), 0, mean(Alpha_TP10[Alpha_TP10 != 0], na.rm = TRUE))
  )
print(dat_alpha_means)
###########################################################################################################
#Part 9

library(plyr)
library(tidyverse)
dat_all <- read.csv("C:/Users/chris/Desktop/MDSAI/STAT847/Midterm/Mind Monitor detailed data 2024-01-21.csv")
library(ggplot2)

# Get unique sessionnum values
unique_sessionnums <- unique(dat_all$sessionnum)

# Loop through each sessionnum
for (k in unique_sessionnums) {
  session_data <- dat_all %>% filter(sessionnum == k)
  
  # Create a density plot for N-blinks and N-jaws
  plot <- ggplot(session_data, aes(x = `Nblinks`, fill = "Nblinks")) +
    geom_density(alpha = 0.5, color = "blue") +
    geom_density(aes(x = `Njaw`, fill = "Njaw"), alpha = 0.5, color = "green") +
    labs(title = paste("Density Plot for session: ", k, "  Activity:", unique(session_data$activity)),
         x = "Count",
         y = "Density",
         fill = "Variable") +
    scale_fill_manual(values = c("blue", "green")) +
    theme_minimal()
  
  # Print each plot individually
  print(plot)
}

#################################################################################
#Question9 Part 2
dat_EEG <- dat_all %>%
  na.omit() %>%
  group_by(sessionnum) %>%
  summarise(
    sessionnum = first(sessionnum),
    activity = first(activity),
    mean_eeg = mean(AUX_RIGHT),
    std_eeg = sd(AUX_RIGHT)
  )
ggplot(dat_EEG, aes(x = sessionnum, y = mean_eeg)) +
  geom_line(color = 'green') +
  geom_point(aes(color = ifelse(activity == 'resting', 'red', 'blue'))) +
  labs(title = "Mean EEG Over Sessions",
       x = "Session Number",
       y = "Mean EEG") +
  scale_color_identity() +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(dat_EEG, aes(x = sessionnum, y = std_eeg)) +
  geom_line(color = 'green') +
  geom_point(aes(color = ifelse(activity == 'resting', 'red', 'blue'))) +
  labs(title = "Mean EEG Over Sessions",
       x = "Session Number",
       y = "Mean EEG") +
  scale_color_identity() +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
