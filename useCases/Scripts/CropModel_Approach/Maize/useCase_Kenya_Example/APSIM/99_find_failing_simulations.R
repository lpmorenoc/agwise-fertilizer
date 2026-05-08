# Identify failing simulations (treatment/year)

library(arrow)
library(dplyr)
library(tidyr)
library(purrr)
library(tools)

expfile_name <- "MaizeFactorialAugSep.parquet"
# expfile_name <- "MaizePeanutIntercrop.parquet"
# expfile_name <- "MaizePeanutRotation.parquet"

apsim <- arrow::read_parquet(paste0("~/agwise-cropping-innovation/Data/useCase_Rwanda_RAB/Maize/result/APSIM/AOI/", expfile_name))

apsim <- apsim %>%
  dplyr::mutate(
    year = year(Maize.SowingDate),
    pixel_id = paste(zone, basename(file_name), sep="_")
  )

all_years <- 2000:2020  # Intercrop & rotation
# all_years <- 2000:2019  # Monocrop

if (expfile_name == "MaizeFactorialAugSep.parquet"){
  n_simulations_per_loc <- length(all_years) * length(unique(apsim$SowDate))
} else {
  n_simulations_per_loc <- length(all_years) * length(unique(apsim$PSD)) * length(unique(apsim$MSD))
}

failing_files <- apsim %>%
  dplyr::count(pixel_id, file_name) %>%
  filter(n != n_simulations_per_loc)

n_failing_pixels <- dim(failing_files)[1]

cat("Failing pixels:", n_failing_pixels)
cat("Total failing year simulations:", n_failing_pixels * n_simulations_per_loc - sum(failing_files$n), "out of", dim(apsim)[1])


# First: Year failures
failure_by_pixel_years <- apsim %>%
  filter(pixel_id %in% failing_files$pixel_id) %>%
  select(pixel_id, file_name, MSD, PSD, zone, year) %>%
  group_by(pixel_id, file_name, MSD, PSD, zone) %>%
  summarise(
    observed_years = list(sort(unique(year))),
    missing_years = list(setdiff(all_years, observed_years[[1]])),
    n_missing_years = length(missing_years[[1]]),
    .groups = "drop"
  ) %>%
  mutate(
    missing_year_ranges = map_chr(missing_years, ~ {
      yrs <- sort(.x)
      if (length(yrs) == 0) return(NA_character_)
      runs <- split(yrs, cumsum(c(1, diff(yrs) != 1)))
      paste0(sapply(runs, function(r) {
        if (length(r) == 1) as.character(r)
        else paste0(min(r), "-", max(r))
      }), collapse = "; ")
    })
  )

# Second: Treatment failures
failure_by_pixel_treatments <- apsim %>%
  filter(pixel_id %in% failing_files$pixel_id) %>%
  count(pixel_id, file_name, MSD, PSD, zone, name = "n_years") %>%
  group_by(pixel_id, file_name, zone) %>%
  summarise(
    missing_treatments = list(paste0(
      MSD[n_years < 20], " × ", PSD[n_years < 20]
    )),
    n_missing_treatments = length(missing_treatments[[1]]),
    .groups = "drop"
  ) %>%
  mutate(
    missing_treatments = ifelse(
      n_missing_treatments == 0,
      NA,
      paste(missing_treatments[[1]], collapse = "; ")
    )
  )

# Merge Year + Treatment failures
failure_by_pixel <- failure_by_pixel_years %>%
  left_join(failure_by_pixel_treatments,
            by = c("pixel_id", "file_name", "zone")) %>%
  arrange(desc(n_missing_years), desc(n_missing_treatments))

# write_csv(failure_by_pixel, "~/agwise-cropping-innovation/Scripts/APSIM/useCases/UseCase_Rwanda_RAB/Maize/failing_simulation.csv")
