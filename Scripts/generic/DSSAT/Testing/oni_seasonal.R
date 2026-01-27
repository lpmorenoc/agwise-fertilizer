if (!requireNamespace("modeest", quietly = TRUE)) install.packages("modeest")

library(modeest)
library(tidyverse)
library(ggplot2)

path_oni <- "/home/jovyan/agwise-datasourcing/dataops/datasourcing/Data/Global_GeoData/Landing/ONI/oni_categorical.txt"
path_prec <- "/home/jovyan/agwise-potentialyield/dataops/potentialyield/Data/useCase_Rwanda_RAB/Maize/result/DSSAT/AOI/useCase_Rwanda_RAB_Maize_AOI_season_rainfall1.RDS"

monthly_prec <- readRDS(path_prec)
oni_cat <- read_delim(path_oni, delim = " ")


monthly_prec_with_oni <- monthly_prec %>%
  left_join(oni_cat, by = c("year" = "year", "month" = "num_month"))%>%
  mutate(
    month.y = factor(month.y, levels = c("January", "February", "March", "April", "May", "June",
                                         "July", "August", "September", "October", "November", "December")),
    oni_category = factor(oni_category, levels = c("nino", "neutral", "nina"))
  )


# Calculate the average monthly rain for each month
monthly_stats <- monthly_prec_with_oni %>%
  group_by(month) %>%
  summarize(monthly_avg_rain = mean(monthly_rain, na.rm = TRUE),
            monthly_sd_rain = sd(monthly_rain, na.rm = TRUE),
            monthly_min_rain = min(monthly_rain, na.rm = TRUE),
            monthly_max_rain = max(monthly_rain, na.rm = TRUE))

# # Calculate the anomaly by joining the average monthly rain and subtracting it from the monthly rain
# monthly_prec_anomaly <- monthly_prec_with_oni %>%
#   left_join(monthly_stats, by = "month") %>%
#   mutate(standardized_anomaly = (monthly_rain - monthly_avg_rain) /(monthly_max_rain - monthly_min_rain))



# ggplot(monthly_prec_anomaly, aes(x = ANOM, y = standardized_anomaly, colour = oni_category))+
#   geom_jitter()


# Create a boxplot of monthly_rain by oni_category and month
ggplot(monthly_prec_with_oni, aes(x = month.y, y = monthly_rain)) +
  geom_boxplot(outlier.size = 0.5, outlier.alpha = 0.5) +
  labs(
    title = "Monthly Rainfall by Month",
    x = " ",
    y = "Monthly Rain (mm)",
    ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 18, face = "bold"),
    axis.title.x = element_text(size = 16),
    axis.title.y = element_text(size = 16),
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12),
    strip.text = element_text(size = 14)
  )

# Create a boxplot of monthly_rain by oni_category and month
ggplot(monthly_prec_with_oni, aes(x = month.y, y = monthly_rain, fill = oni_category)) +
  geom_boxplot(outlier.size = 0.5, outlier.alpha = 0.5) +
  scale_fill_manual(values = c("nino" = "tomato1", "nina" = "royalblue2", "neutral" = "gold")) +
  labs(
    title = "Monthly Rainfall by ONI Category and Month",
    x = " ",
    y = "Monthly Rain (mm)",
    fill = "ONI Category"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 18, face = "bold"),
    axis.title.x = element_text(size = 16),
    axis.title.y = element_text(size = 16),
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12),
    strip.text = element_text(size = 14),
    legend.position = "top",
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 12)
  )

# t = monthly_prec_with_oni %>% ungroup()
# t2 = t%>%  group_by(year,oni_category) %>% summarise(test = sum(monthly_rain))
# monthly_prec_with_oni %>% group_by(oni_category) %>% summarise(test = sum(monthly_rain))
# 
# unique_combinations <- t %>%  select(year, oni_category) %>%  distinct()
# 
# first = function(x) x %>% nest %>% ungroup %>% slice(1) %>% unnest(data)
# 
# tabla_1 = t %>% group_by(year,oni_category) %>% first()


# Define the seasons
monthly_season <- monthly_prec_with_oni %>%
  mutate(season = case_when(
    month %in% c(1, 2) ~ "JF",
    month %in% c(3, 4, 5) ~ "MAM",
    month %in% c(6, 7, 8, 9) ~ "JJAS",
    month %in% c(10, 11, 12) ~ "OND"
  )) %>%mutate(
    season = factor(season, levels = c("JF","MAM","JJAS","OND")))

# Calculate the mean and standard deviation of seasonal rain
get_mode <- function(v) {
  mfv(v)[1]
}

seasonal_stats <- monthly_season %>%
  group_by(LAT,LONG,zone,year,season) %>%
  summarise(
    seasonal_cum_rain = sum(monthly_rain, na.rm = TRUE),
    seasonal_avg_rain = mean(monthly_rain, na.rm = TRUE),
    mode_oni_category = get_mode(oni_category)
  )

# Create a boxplot of monthly_rain by oni_category and month
ggplot(seasonal_stats, aes(x = season, y = seasonal_cum_rain)) +
  geom_boxplot(outlier.size = 0.5, outlier.alpha = 0.5) +
  labs(
    title = "Monthly Rainfall by Season",
    x = " ",
    y = "Monthly Rain (mm)",
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 18, face = "bold"),
    axis.title.x = element_text(size = 16),
    axis.title.y = element_text(size = 16),
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12),
    strip.text = element_text(size = 14)
  )


ggplot(seasonal_stats, aes(x = season, y = seasonal_cum_rain, fill = mode_oni_category)) +
  geom_boxplot(outlier.size = 0.5, outlier.alpha = 0.5) +
  facet_wrap(.~zone,scales = "free_y",nrow = 2) +
  scale_fill_manual(values = c("nino" = "tomato1", "nina" = "royalblue2", "neutral" = "gold")) +
  labs(
    title = "Rainfall by ONI Category and Season",
    x = " ",
    y = "Season total Rain (mm)",
    fill = "ONI Category"
  ) +
  theme_bw(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 18, face = "bold"),
    axis.title.x = element_text(size = 16),
    axis.title.y = element_text(size = 16),
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12),
    strip.text = element_text(size = 14),
    legend.position = "top",
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 12)
  )

