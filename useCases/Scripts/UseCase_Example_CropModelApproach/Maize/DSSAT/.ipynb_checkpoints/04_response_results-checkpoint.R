library(dplyr)
library(ggplot2)
library(stringr)
library(lubridate)

# Simple plot
country <- "Kenya"
useCaseName <- "KALRO_app2"
Crop <- "Maize"
season <- 1

dir_path <- paste0("/home/jovyan/agwise-fertilizer/Data/useCase_", country, "_",
                   useCaseName, "/", Crop, "/result/DSSAT/AOI/")
dssat_data <- readRDS(paste0(dir_path, "useCase_", country, "_", useCaseName,
                             "_", Crop, "_AOI_season_", season, ".RDS"))

long_term_avg_dssat_data <- dssat_data %>%
  mutate(
    HDAT = as.Date(HDAT),
    Year = year(HDAT),
    N_rate = as.numeric(str_extract(TNAM, "^[0-9]+")),
    lat_lon = paste(XLAT, LONG, sep = ", ")) %>%
  group_by(N_rate, lat_lon) %>%
  summarise(HWAH_avg = mean(HWAH), .groups = "drop") %>%
  pivot_wider(
    names_from = N_rate,
    values_from = HWAH_avg,
    names_prefix = "N_"
  )

long_for_plot <- long_term_avg_dssat_data %>%
  pivot_longer(
    cols = starts_with("N_"),
    names_to = "N_rate",
    values_to = "HWAH_avg"
  ) %>%
  mutate(N_rate = as.numeric(gsub("N_", "", N_rate)))


for (location in long_term_avg_dssat_data$lat_lon[1:5]) {
  df_loc <- long_for_plot %>% filter(lat_lon == location)
  
  p <- ggplot(df_loc, aes(x = N_rate, y = HWAH_avg)) +
    geom_line() +
    geom_point() +
    theme_bw() +
    labs(title = paste("Location:", location),
         x = "N rate (kg/ha)",
         y = "Yield (kg/ha)")
  
  print(p)
}


# Separate first and second half of the data
years_sorted <- sort(unique(year(as.Date(dssat_data$HDAT))))
half_n <- floor(length(years_sorted) / 2)
first_half_years <- years_sorted[1:half_n]
second_half_years <- years_sorted[(half_n+1):length(years_sorted)]

first_half_avg_dssat_data <- dssat_data %>%
  mutate(
    HDAT = as.Date(HDAT),
    Year = year(HDAT),
    N_rate = as.numeric(str_extract(TNAM, "^[0-9]+")),
    lat_lon = paste(XLAT, LONG, sep = ", ")
  ) %>%
  filter(Year %in% first_half_years) %>%
  group_by(N_rate, lat_lon) %>%
  summarise(HWAH_avg = mean(HWAH), .groups = "drop") %>%
  pivot_wider(
    names_from = N_rate,
    values_from = HWAH_avg,
    names_prefix = "N_"
  )

second_half_avg_dssat_data <- dssat_data %>%
  mutate(
    HDAT = as.Date(HDAT),
    Year = year(HDAT),
    N_rate = as.numeric(str_extract(TNAM, "^[0-9]+")),
    lat_lon = paste(XLAT, LONG, sep = ", ")
  ) %>%
  filter(Year %in% second_half_years) %>%
  group_by(N_rate, lat_lon) %>%
  summarise(HWAH_avg = mean(HWAH), .groups = "drop") %>%
  pivot_wider(
    names_from = N_rate,
    values_from = HWAH_avg,
    names_prefix = "N_"
  )

# Combine first and second half to plot them together
long_first <- first_half_avg_dssat_data %>%
  pivot_longer(
    cols = starts_with("N_"),
    names_to = "N_rate",
    values_to = "HWAH_avg"
  ) %>%
  mutate(
    N_rate = as.numeric(gsub("N_", "", N_rate)),
    period = "First half"
  )

# Second half
long_second <- second_half_avg_dssat_data %>%
  pivot_longer(
    cols = starts_with("N_"),
    names_to = "N_rate",
    values_to = "HWAH_avg"
  ) %>%
  mutate(
    N_rate = as.numeric(gsub("N_", "", N_rate)),
    period = "Second half"
  )

# Combine them
long_both <- bind_rows(long_first, long_second)

# Plot them
for (location in long_both$lat_lon[1:5]) {
  
  df_loc <- long_both %>% filter(lat_lon == location)
  
  p <- ggplot(df_loc,
              aes(x = N_rate,
                  y = HWAH_avg,
                  color = period,
                  group = period)) +
    geom_line(linewidth = 1.1) +
    geom_point(size = 2) +
    theme_bw() +
    labs(
      title = paste("Location:", location),
      x = "N rate (kg/ha)",
      y = "Yield (kg/ha)",
      color = "Period"
    ) +
    scale_color_manual(values = c("First half" = "blue",
                                  "Second half" = "red"))
  
  print(p)
}