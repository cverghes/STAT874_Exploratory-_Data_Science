
# Load the dataset
woodbine_data <- read.csv("HRN woodbine scraped data 2023-12-04.csv")

library(plyr)
library(tidyverse)
#Use ddply to summarize based on racecount and only display information that are 
#consistent per race
woodbine_race <- woodbine_data %>% ddply("racecount", summarize, 
                                         racecount = first(racecount),
                                         meet_location = first(meet_location),
                                         meet_wday = first(meet_wday),
                                         meet_mday = first(meet_mday),
                                         meet_year = first(meet_year),
                                         purse = first(purse),
                                         time_frac1 = first(time_frac1),
                                         time_frac2 = first(time_frac2),
                                         time_frac3 = first(time_frac3),
                                         time_frac4 = first(time_frac4),
                                         time_frac5 = first(time_frac5),
                                         time_final = first(time_final),
                                         track_length = first(track_length),
                                         track_type = first(track_type),
                                         race_class = first(race_class),
                                         dist_frac1 = first(dist_frac1),
                                         dist_frac2 = first(dist_frac2),
                                         dist_frac3 = first(dist_frac3),
                                         dist_frac4 = first(dist_frac4),
                                         dist_frac5 = first(dist_frac5))


# Display the first 3 rows of the new dataset
woodbine_race[1:3,]
##############################################################################################
#Average Race Final times
by(woodbine_race$time_final, woodbine_race$track_length, function(x) round(mean(x), 2)) %>% 
  knitr::kable(col.names = c("Track Length", "Mean Time"))
###############################################################################################
woodbine_data %>% mutate(rounded_odds = round(horse_odds_decimal)) %>% 
  ddply("rounded_odds", summarize,
        Mean_Time = round(mean(horse_place == 2, na.rm = TRUE), 3)) %>% 
  knitr::kable(col.names = c("Odds", "Probability"))

#######################################################################################
sample1 <- woodbine_race %>%
  filter(track_type != "All Weather Track" & track_length == "6F")

# Create sample2
sample2 <- woodbine_race %>%
  filter(track_type == "All Weather Track" & track_length == "6F")

# Perform t-test
t.test(sample1$time_final, sample2$time_final)

#There is no statistically significant difference in average finish times at the 0.05 level.

#############################################################################################################
#Read the Hastings and Assiniboia Race data
hastings_data <- read.csv("HRN hastings scraped data 2023-12-04.csv")
assiniboia_data <- read.csv("HRN assiniboia scraped data 2023-12-04.csv")
#Only use the final time for races where track length is 6F for all three locations
assiniboia_race <- assiniboia_data[assiniboia_data$track_length == "6F", "time_final"]
hastings_race <- hastings_data[hastings_data$track_length == "6F", "time_final"]
woodbine_race <- woodbine_data[woodbine_data$track_length == "6F", "time_final"]
#Create a list containing all three locations
all_data <- list(assiniboia_race, hastings_race, woodbine_race)
#Plot these finish times using a box plot
boxplot(all_data, col = c("red", "blue", "green"), main = "Plot of Final Time by Location",
        xlab = "Location", ylab = "Final Time", names = c("Assiniboia", "Hastings", "Woodbine"))
  #################################################################################
#Create a dataframe consisting of the name of horses and the number of wins
winning_horse <- woodbine_data[woodbine_data$horse_place == 1 
                               & !is.na(woodbine_data$horse_place),] %>% 
  ddply("horse_name",summarize, wins=length(racecount)) %>% 
  arrange(desc(wins))
#Top 5 winning horses need to be added into a table
head(winning_horse, 5) %>%
  knitr::kable(col.names = c("Horse", "Wins"))
#######################################################################################
#Create dataframe with horse information and new column with number of horses
modified_data <- woodbine_data[, c("racecount","horse_name","horse_place","purse")] %>%
  drop_na(horse_place)%>%
  group_by(racecount) %>%
  mutate(num_horses = sum(!horse_place %in% c(1, 2, 3)))

#Add new column (prizes) which calculates each horse's prize money
prizes <- modified_data %>%
  mutate(prize = ifelse(horse_place == 1, 0.6 * purse, 
                        ifelse(horse_place == 2, 0.2 * purse, 
                               ifelse(horse_place == 3, 0.1 * purse,
                                       0.1 * purse/num_horses))) ,)

#Calculate cumulative prizes for each horse and arrange in descending order
complete_prizes <- prizes %>% ddply("horse_name", summarize,Prize_Money = sum(prize)) %>% 
  arrange(desc(Prize_Money))
#Print 5 largest earned prizes in a table
head(complete_prices, 5) %>%
  knitr::kable(col.names = c("Horse", "Prize Money"))
######################################################################################


library(stringr)

convert_to_yards_regex <- function(distance_string) {
  match_result1 <- str_match(distance_string, "^(\\d+)([MF])$")
  match_result2 <- str_match(distance_string, "^(\\d+)\\s(\\d+)/(\\d+)([MF])$")
  match_result3 <- str_match(distance_string, "^(\\d+)([MF])\\s(\\d+)[Y]$")
  if (!is.na(match_result1[,1])) {
    numeric_value <- as.numeric(match_result1[, 2])
    unit <- as.character(match_result1[, 3])
    yards <- 0
  }
  else if (!is.na(match_result2[,1])) {
    # Handle the case with a space, a '/', and a second number
    numerator <- as.numeric(match_result2[, 3])
    denominator <- as.numeric(match_result2[, 4])
    numeric_value <- as.numeric(match_result2[, 2]) + (numerator / denominator)
    unit <- as.character(match_result2[, 5])
    yards <- 0
  }
  else if (!is.na(match_result3[,1])) {
    # Handle the case with a space, a '/', and a second number
    yards <- as.numeric(match_result3[, 4])
    numeric_value <- as.numeric(match_result3[, 2])
    unit <- as.character(match_result3[, 3])
  }
    
    if (unit == "F") {
      # Convert furlongs to yards (1 Furlong = 220 yards)
      return((numeric_value * 220) + yards)
    } 
    else if (unit == "M") {
      # Convert miles to yards (1 Mile = 1760 yards)
      return((numeric_value * 1760)+ yards)
    }
}


woodbine_tracks <- woodbine_data %>%
  rowwise() %>%
  mutate(track_length_yards = convert_to_yards_regex(track_length)) %>%
  ungroup()

woodbine_tracks_filtered <- woodbine_tracks%>%
  filter(!is.na(track_length_yards) & track_length_yards >= 990 & track_length_yards <= 2750)

plot <- ddply(woodbine_tracks_filtered, "track_length_yards",summarise,
               AvgTime = mean(time_frac2, na.rm=T))
cplot <- ggplot(data = plot, aes(x = track_length_yards, y = AvgTime)) +
  geom_line(color = "red", size = 1.5) +
  geom_point(shape = 17, color = "yellow", size = 2) +
  theme_minimal() +
  labs(title = "Broken Line Plot",
       x = "Length [Yards]",
       y = "Time Fraction 2")
print(cplot)
##################################################################################################
#Quadratic Model
model <- lm(AvgTime ~ poly(track_length_yards, degree = 2, raw = TRUE), data = plot)

cat("\n Coefficients:\n", coef(model),
    "\n Residual Standard Error: ", summary(model)$sigma, 
    "\n R-squared: ", summary(model)$r.squared, "\n")
#Summary of model
summary(model)
