# --------------------------------------------------- #
### CROP MODELLING FERTILIZER RECOMMENDATION SCRIPT ###
# --------------------------------------------------- #

### Settings ###
# 1.a. General Settings (edit as necessary)
country <- "Kenya"
useCaseName <- "KALRO_app2"
Crop <- "Maize"

# 1.b. Paths
filex_temp <- "KEAGFERT.MZX"
fert_file <- "fert_fact_KEN.csv"
geneticfiles <- "MZCER048"
#
path.to.temdata <- paste0("/home/jovyan/agwise-fertilizer/Data/useCase_", 
                          country, "_", useCaseName, "/", Crop, 
                          "/Landing/DSSAT/")

# 1.c. Experimental settings
fertilizer <- FALSE  # All treatments have the same fertilizer 
fert_factorial <- FALSE  # Fertilizer (CSV template) and plant date (RS code) as levels
fert_grid_RS <- TRUE  # Fertilizer (grid) and plant dates (CSV template) as levels 
AOI <- TRUE

season <- 1
pathIn_zone <- TRUE
level2 <- NA
index_soilwat <- 1
ID <- "TLID"

# 1.d. NPK grid
NPK_ranges <- list(N = seq(0, 200, 50),
                   P = seq(0, 50, 50),  # Insufficient soil P data provided.
                   K = seq(0, 0, 0))  # Model K module not implemented for Maize

# 1.e. Read inputs
# countryShp <- geodata::gadm(country, level = 2, path = ".")
# prov <- unique(countryShp$NAME_1)
prov <- c("Kisumu")  # Temporarily set to one province

template_df <- read.csv(paste0(path.to.temdata, fert_file))
varietyids <- unique(template_df$INGENO)

# 1.f. Other inputs
# month-day placeholders; real planting dates come from rs_schedule_df below
Planting_month_date <- "08-01"; Harvest_month_date <- "06-30"
plantingWindow <- 4  # ignored when rs_schedule_df is provided

#########################################################################
### Step 1: Create soil and weather data in DSSAT format for AOI data ###
#########################################################################
source("~/agwise-fertilizer/Script/generic/DSSAT/readGeo_CM_zone.R")

for (i in 1:length(prov)){
  geoData_AOI <- readGeo_CM_zone(
    country = country, useCaseName = useCaseName, Crop = Crop, AOI = AOI, 
    season = season, zone = prov[i], level2 = level2, varietyid = varietyids[1],
    pathIn_zone = pathIn_zone, Depth = c(5, 15, 30, 60, 100, 200)
  )
  message <- paste("Province finished:", i, Sys.time())
}

copy_WTH_SOIL_data_for_variety(
  country = country, useCaseName = useCaseName, Crop = Crop, AOI = AOI, 
  varietyids = varietyids)

##################################################################
### Step 2: Create DSSAT input files and run DSSAT simulations ###
##################################################################
source("~/agwise-fertilizer/Script/generic/DSSAT/dssat_expfile_zone_RS_Dates_fert_factorial.R")

### Create DSSAT input files ###
for (j in seq_along(varietyids)) {
  for (i in seq_along(prov)) {
    invisible(
      dssat.expfile(
        country = country, useCaseName = useCaseName, Crop = Crop, AOI = AOI,
        filex_temp = filex_temp,
        # month-day placeholders; real planting dates come from rs_schedule_df below
        Planting_month_date = Planting_month_date,
        Harvest_month_date  = Harvest_month_date,
        ID = ID, season = season,
        plantingWindow = plantingWindow,  # ignored when rs_schedule_df is provided
        varietyid = varietyids[j],
        zone = prov[i], level2 = level2, fertilizer = fertilizer,
        fert_factorial = fert_factorial, template_df = template_df,
        fert_grid_RS = fert_grid_RS, NPK_ranges = NPK_ranges,
        geneticfiles = geneticfiles, index_soilwat = index_soilwat,
        pathIn_zone = pathIn_zone
      )
    )
  }
}

### Run DSSAT Simulations ###
source("~/agwise-fertilizer/Script/generic/DSSAT/dssat_exec.R")
TRT <- dim(expand.grid(NPK_ranges))[1] * 4  # TODO: Revise planting dates
for (j in seq_along(varietyids)) {
  for (i in seq_along(prov)) {
    execmodel_AOI <- dssat.exec(country = country,  useCaseName = useCaseName, 
                                Crop = Crop, AOI = AOI, TRT = 1:TRT, 
                                varietyid = varietyids[j], zone = prov[i])
  }
}

##########################################################################
### Step 3: Merge DSSAT results, produce response statistics and plots ###
##########################################################################
source("~/agwise-fertilizer/Script/generic/DSSAT/merge_DSSAT_output.R")

### Merge DSSAT outputs ###
merge_DSSAT_output(country = country, useCaseName = useCaseName, Crop = Crop,
                   AOI = TRUE, season = season, varietyids = varietyids,
                   zone_folder = TRUE, level2_folder = FALSE)

### Produce response statistics and plots ###
source("~/agwise-fertilizer/Script/generic/DSSAT/response_plots_for_fert_factorial.R")
response_plots_and_statistics_for_fert_factorial(
  country = country , useCaseName = useCaseName, Crop = Crop, season = season)

