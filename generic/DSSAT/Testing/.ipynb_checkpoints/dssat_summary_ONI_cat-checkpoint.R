install.packages("zoo")
library(zoo)
library(tidyverse)
### 4.2.1. Get the ONI data ####
# Check if the ONI table is already available

url <- "https://www.cpc.ncep.noaa.gov/data/indices/oni.ascii.txt"
destfile <- "/home/jovyan/agwise-datasourcing/dataops/datasourcing/Data/Global_GeoData/Landing/ONI/"

file_name <- "oni.txt"

# Download it if doesn't exist
if (!file.exists(paste(destfile, file_name, sep = ""))){
  download.file(url, paste(destfile, file_name, sep = ""), mode="wb")
} else {
  
  # Update the ONI version with the last version
  unlink(paste(destfile, file_name, sep = ""))
  download.file(url, paste(destfile, file_name, sep = ""), mode="wb")
}

# Open the ONI data
oni <- read_table(paste0(destfile,file_name))

# Reshape the ONI data
month_mapping <- c(
  'DJF'= 'January',
  'JFM'= 'February',
  'FMA'= 'March',
  'MAM'= 'April',
  'AMJ'= 'May',
  'MJJ'= 'June',
  'JJA'= 'July',
  'JAS'= 'August',
  'ASO'= 'September',
  'SON'= 'October',
  'OND'= 'November',
  'NDJ'= 'December'
)
oni$month <- month_mapping[oni$SEAS]
oni$date <- as.Date(paste0(oni$YR, "-", oni$month, "-01"), format="%Y-%B-%d")
oni$year <-year(oni$date)
oni$num_month <-month(oni$date)


oni <- oni %>% arrange(year, num_month)

# # Calculate rolling average of ONI over the previous 5 months
# oni <- oni %>%
#   mutate(Average_ONI_5months = rollapplyr(ANOM, width = 5, FUN = mean, fill = NA, align = "right")) %>%
#   mutate(Average_ONI_5months = ifelse(is.na(Average_ONI_5months), ANOM, Average_ONI_5months))
# 
# 
# # Define thresholds
# nino_threshold <- 0.5
# nina_threshold <- -0.5
# 
# # Classify based on average ONI value
# oni <- oni %>%
#   mutate(Classification = case_when(
#     Average_ONI_5months >= nino_threshold ~ "Niño",
#     Average_ONI_5months <= nina_threshold ~ "Niña",
#     TRUE ~ "Neutral"
#   ))

# Assuming your data frame is named `oni`
# Create a function to determine the category
get_oni_category <- function(anom_values) {
  anom_values <- round(anom_values,1)
  n <- length(anom_values)
  categories <- rep("neutral", n)
  
  i <- 1
  while (i <= n) {
    if (anom_values[i] <= -0.5) {
      count <- 0
      j <- i
      while (j <= n && anom_values[j] <= -0.5) {
        count <- count + 1
        j <- j + 1
      }
      if (count >= 5) {
        categories[i:(i+count-1)] <- "nino"
        i <- i + count
      } else {
        i <- i + 1
      }
    } else if (anom_values[i] >= 0.5) {
      count <- 0
      j <- i
      while (j <= n && anom_values[j] >= 0.5) {
        count <- count + 1
        j <- j + 1
      }
      if (count >= 5) {
        categories[i:(i+count-1)] <- "nina"
        i <- i + count
      } else {
        i <- i + 1
      }
    } else {
      i <- i + 1
    }
  }
  return(categories)
}

# Apply the function to the oni data frame
oni <- oni %>%
  mutate(oni_category = get_oni_category(ANOM))

# Print the data frame to check the result
path_to_save = "/home/jovyan/agwise-datasourcing/dataops/datasourcing/Data/Global_GeoData/Landing/ONI/"
write.table(oni, paste0(path_to_save,"oni_categorical.txt"), row.names = FALSE)

