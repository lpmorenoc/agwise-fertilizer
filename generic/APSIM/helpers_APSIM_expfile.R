
select <- dplyr::select
mutate <- dplyr::mutate
rename <- dplyr::rename


################################################################################
# Helper functions
################################################################################

#' fix_start_end_dates
#'
#' Adjusts the APSIM Clock start and end dates based on a user-defined range (`clck`)
#' and a weather file (`met_file`). The function sets the simulation period to the
#' most limiting dates: the later of the requested start and weather start, and
#' the earlier of the requested end and weather end.
#'
#' @param filex_temp Character. Path to the APSIMX experiment file (.apsimx).
#' @param clck Character vector of length 2. User-defined start and end dates 
#'             in APSIMX format: c("YYYY-MM-DDT00:00:00", "YYYY-MM-DDT00:00:00").
#' @param met_file Character. Path to the weather file (.met) used for the experiment.
#' @param pathOUT Character. Directory where the APSIMX file resides and will be written.
#'
#' @return Updates the Clock node in the APSIMX experiment file with the computed
#'         start and end dates. Prints a message confirming the update.
#'
#' @details
#' - Reads the weather file and determines its start and end dates.
#' - Computes the most limiting start and end dates using the intersection of
#'   `clck` and the weather file range.
#' - Uses `apsimx::edit_apsimx()` to update the Clock node.
#' - Ensures the simulation period does not exceed the weather file boundaries.
#'
#' @examples
#' clck <- c("1981-01-01T00:00:00", "2020-12-31T00:00:00")
#' fix_start_end_dates("Base_one.apsimx", clck, "weather.met", pathOUT = "apsim_project")

fix_start_end_dates <- function(filex_temp, clck, met_file, pathOUT) {
  # Validate clck
  if(length(clck) != 2) stop("clck must be a vector of length 2: c(Start, End)")
  
  # Read weather file
  lines <- readLines(met_file)
  header_line <- grep("^year", lines, ignore.case = TRUE)
  if(length(header_line) == 0) stop("Could not find weather header in met file.")
  
  weather <- read.table(text = lines[(header_line+1):length(lines)], header = FALSE)
  names(weather) <- strsplit(lines[header_line], "\\s+")[[1]]
  weather <- weather[-1, ]
  row.names(weather) <- NULL
  
  
  # Remove units row if present
  if(all(grepl("[a-zA-Z]", weather[1, ]))) weather <- weather[-1, ]
  
  # Compute weather start and end dates
  weather$date <- as.Date(as.numeric(weather$day) - 1, origin = paste0(as.numeric(weather$year), "-01-01"))
  weather_start <- paste0(min(weather$date), "T00:00:00")
  weather_end   <- paste0(max(weather$date), "T00:00:00")
  
  # Determine most limiting start and end
  final_start <- max(clck[1], weather_start)
  final_end   <- min(clck[2], weather_end)
  final_clck  <- c(final_start, final_end)
  
  # Update APSIM Clock
  apsimx::edit_apsimx(
    filex_temp,
    src.dir = pathOUT,
    wrt.dir = pathOUT,
    root = c("pd", "Base_one"),  # adjust to your experiment structure
    node = "Clock",
    parm = c("Start", "End"),
    value = final_clck,
    overwrite = TRUE
  )
  
  message("Clock (Start-End) updated to: ", final_clck[1], " → ", final_clck[2])
}

#' adjust_soil_profile
#'
#' Ensures APSIMX soil assumptions are satisfied by adjusting either soil water
#' parameters (LL15, DUL, SAT) or crop lower limits (LLs).
#'
#' @param soil_df Data frame with soil profile (AirDry, LL15, DUL, SAT, BD, Maize.LL, Soybean.LL, Wheat.LL)
#' @param modify "soil" to adjust soil parameters to fit crops, "crop" to adjust crops to fit soil
#' @return Modified soil_df satisfying: AirDry < LL < DUL < SAT, SAT < 1-BD/PD, Crop LLs within AirDry-DUL
#' @details Prints messages for any adjustments made.
#' @examples
#' fixed_soil <- adjust_soil_profile(ex_profile$soil, modify = "soil")
#' fixed_crops <- adjust_soil_profile(ex_profile$soil, modify = "crop")

adjust_soil_profile <- function(soil_df, modify = c("crop", "soil"), epsilon = 0.0001) {
  modify <- match.arg(modify)
  
  PD <- 2.65  # particle density (g/cm3)
  max_sat <- 1 - soil_df$BD / PD
  
  # Loop through layers
  for(i in 1:nrow(soil_df)) {
    
    airdry <- soil_df$AirDry[i]
    ll <- soil_df$LL15[i]
    dul <- soil_df$DUL[i]
    sat <- soil_df$SAT[i]
    
    # Add more if missing
    crops <- c("Maize.LL", "Soybean.LL", "Wheat.LL", "Barley.LL", "Sorghum.LL",
               "Rice.LL", "Peanut.LL")
    
    crops <- crops[crops %in% names(soil_df)]
    
    crop_lls <- sapply(crops, function(c) soil_df[[c]][i])
    min_crop_ll <- min(crop_lls)
    
    # Identify violations
    violate_airdry <- airdry >= ll
    violate_ll_dul <- ll >= dul
    violate_dul_sat <- dul >= sat
    violate_sat_max <- sat > max_sat[i]
    violate_crop <- any(crop_lls <= airdry | crop_lls >= dul | crop_lls <= ll)
    
    any_violation <- violate_airdry || violate_ll_dul || violate_dul_sat || violate_sat_max || violate_crop
    
    if(!any_violation) next
    
    if(modify == "soil") {
      # Adjust SAT < max_sat
      if(sat > max_sat[i]) {
        message(sprintf("Layer %d: SAT adjusted from %.3f to %.3f (max allowed %.3f)", 
                        i, sat, max_sat[i], max_sat[i]))
        sat <- max_sat[i]
        soil_df$SAT[i] <- sat
      }
      
      # Adjust DUL < SAT
      if(dul >= sat) {
        dul <- sat - epsilon
        message(sprintf("Layer %d: DUL adjusted to %.3f to be < SAT %.3f", i, dul, sat))
        soil_df$DUL[i] <- dul
      }
      
      # Adjust DUL: > LL15
      lower_bound <- max(ll, min_crop_ll)
      if(dul <= lower_bound) {
        dul <- (lower_bound + sat)/2  # Adjusted in between SAT and a valid LL
        message(sprintf("Layer %d: DUL adjusted from %.3f to %.3f to be > LL15 %.3f", i, soil_df$DUL[i], dul, ll))
        soil_df$DUL[i] <- dul
      }
      
      # Adjust LL15: < min(DUL, min_crop_LL)
      upper_limit <- min(dul, min_crop_ll)
      if(ll >= upper_limit) {
        ll <- upper_limit - epsilon
        message(sprintf("Layer %d: LL15 adjusted to %.3f to be < min(DUL %.3f, min crop LL %.3f)", 
                        i, ll, dul, min_crop_ll))
        soil_df$LL15[i] <- ll
      }
      
      # Adjust AirDry: > min(LL15, min_crop_LL)
      min_limit <- min(ll, min_crop_ll)
      if(airdry >= min_limit) {
        airdry <- min_limit - epsilon
        message(sprintf("Layer %d: AirDry adjusted from %.3f to %.3f to be < LL15 %.3f", 
                        i, soil_df$AirDry[i], airdry, ll))
        soil_df$AirDry[i] <- airdry
      }
      
    } else if(modify == "crop") {
      # Adjust each crop LL to be > AirDry, < DUL
      for(crop in crops) {
        crop_ll <- soil_df[[crop]][i]
        new_crop_ll <- crop_ll
        if(crop_ll <= airdry) {
          new_crop_ll <- airdry + epsilon
          message(sprintf("Layer %d: %s adjusted from %.3f to %.3f (AirDry %.3f)", 
                          i, crop, crop_ll, new_crop_ll, airdry))
        }
        if(crop_ll >= dul) {
          new_crop_ll <- dul - epsilon
          message(sprintf("Layer %d: %s adjusted to %.3f to be < DUL %.3f", 
                          i, crop, new_crop_ll, dul))
        }
        soil_df[[crop]][i] <- new_crop_ll
      }
      
      # Adjust SAT if needed
      if(sat > max_sat[i]) {
        message(sprintf("Layer %d: SAT adjusted from %.3f to %.3f (max allowed %.3f)", 
                        i, sat, max_sat[i], max_sat[i]))
        soil_df$SAT[i] <- max_sat[i]
      }
    }
  }
  
  return(soil_df)
}




create_rs_schedule <- function(template_df, fc_year = NA) {
  if (!is.na(fc_year)) {
    rs_schedule_df <- template_df %>%
      {
        nm <- names(.)
        if ("lon" %in% nm)  rename(., longitude = lon) else .
      } %>%
      {
        nm <- names(.)
        if ("lat" %in% nm)  rename(., latitude  = lat) else .
      } %>%
      dplyr::select(longitude, latitude, q25, q50, q75) %>%
      mutate(
        longitude = as.numeric(longitude),
        latitude  = as.numeric(latitude)
      ) %>%
      fill(q25, q50, q75, .direction = "downup") %>%
      mutate(
        q25_date = doy_to_date(q25, year = fc_year),
        q75_date = doy_to_date(q75, year = fc_year)
      ) %>%
      rowwise() %>%
      mutate(
        planting_dates = list(seq(q25_date, q75_date, length.out = 4)),
        startingDate   = min(planting_dates) %m-% months(1),
        harvestDate    = max(planting_dates) %m+% months(8)
      ) %>%
      ungroup() %>%
      mutate(
        lon_r = round(longitude, 3),
        lat_r = round(latitude, 3)
      ) %>%
      dplyr::select(-c(q25, q50, q75, q25_date, q75_date))
  } else if (is.na(fc_year)) {
    rs_schedule_df <- template_df %>%
      {
        nm <- names(.)
        if ("lon" %in% nm)  rename(., longitude = lon) else .
      } %>%
      {
        nm <- names(.)
        if ("lat" %in% nm)  rename(., latitude  = lat) else .
      } %>%
      dplyr::select(longitude, latitude, q25, q50, q75) %>%
      mutate(
        longitude = as.numeric(longitude),
        latitude  = as.numeric(latitude)
      ) %>%
      fill(q25, q50, q75, .direction = "downup") %>%
      mutate(
        q25_date = doy_to_date(q25),
        q75_date = doy_to_date(q75)
      ) %>%
      rowwise() %>%
      mutate(
        planting_dates = list(seq(q25_date, q75_date, length.out = 4)),
        startingDate   = min(planting_dates) %m-% months(1),
        harvestDate    = max(planting_dates) %m+% months(8)
      ) %>%
      ungroup() %>%
      mutate(
        lon_r = round(longitude, 3),
        lat_r = round(latitude, 3)
      ) %>%
      dplyr::select(-c(q25, q50, q75, q25_date, q75_date))
  }
  
  return(rs_schedule_df)
}




edit_perm_sowdate <- function(file_in, file_out = file_in, new_dates) {
  x <- jsonlite::fromJSON(file_in, simplifyVector = FALSE)
  
  # Find the Experiment called "pd"
  exp_i <- which(vapply(x$Children, \(n) identical(n$Name, "pd"), logical(1)))
  if (length(exp_i) != 1) stop("Could not uniquely find Experiment named 'pd'.")
  
  pd <- x$Children[[exp_i]]
  
  # Find Factors
  fac_i <- which(vapply(pd$Children, \(n) identical(n$Name, "Factors"), logical(1)))
  if (length(fac_i) != 1) stop("Could not uniquely find 'Factors' under 'pd'.")
  
  factors <- pd$Children[[fac_i]]
  
  # Find Permutation
  perm_i <- which(vapply(factors$Children, \(n) identical(n$Name, "Permutation"), logical(1)))
  if (length(perm_i) != 1) stop("Could not uniquely find 'Permutation' under 'Factors'.")
  
  perm <- factors$Children[[perm_i]]
  
  # Find Factor named SowDate
  sd_i <- which(vapply(perm$Children, \(n) identical(n$Name, "SowDate"), logical(1)))
  if (length(sd_i) != 1) stop("Could not uniquely find Factor named 'SowDate' under 'Permutation'.")
  
  # Edit Specification (keep the left-hand side exactly, replace right-hand side)
  old_spec <- perm$Children[[sd_i]]$Specification
  
  # If old_spec is somehow a vector itself, grab the first element just in case
  if (length(old_spec) > 1) old_spec <- old_spec[1]
  
  if (!grepl("=", old_spec, fixed = TRUE)) stop("Unexpected: SowDate$Specification does not contain '='.")
  
  lhs <- sub("=.*$", "", old_spec)              # everything before '='
  
  # FIX: Collapse new_dates into a single string separated by commas
  collapsed_dates <- paste(new_dates, collapse = ", ")
  
  # Combine LHS with the collapsed dates
  perm$Children[[sd_i]]$Specification <- paste0(lhs, "=", collapsed_dates)
  
  # Put back and write
  factors$Children[[perm_i]] <- perm
  pd$Children[[fac_i]] <- factors
  x$Children[[exp_i]] <- pd
  
  writeLines(jsonlite::toJSON(x, auto_unbox = TRUE, pretty = TRUE, null = "null"), file_out)
  invisible(file_out)
}

edit_perm_fertilise <- function(file_in, file_out = file_in, new_max, new_step) {
  x <- jsonlite::fromJSON(file_in, simplifyVector = FALSE)
  
  # Find the Experiment called "pd"
  exp_i <- which(vapply(x$Children, \(n) identical(n$Name, "pd"), logical(1)))
  if (length(exp_i) != 1) stop("Could not uniquely find Experiment named 'pd'.")
  
  pd <- x$Children[[exp_i]]
  
  # Find Factors
  fac_i <- which(vapply(pd$Children, \(n) identical(n$Name, "Factors"), logical(1)))
  if (length(fac_i) != 1) stop("Could not uniquely find 'Factors' under 'pd'.")
  
  factors <- pd$Children[[fac_i]]
  
  # Find Permutation
  perm_i <- which(vapply(factors$Children, \(n) identical(n$Name, "Permutation"), logical(1)))
  if (length(perm_i) != 1) stop("Could not uniquely find 'Permutation' under 'Factors'.")
  
  perm <- factors$Children[[perm_i]]
  
  # Find Factor named FertiliseFactor
  ff_i <- which(vapply(perm$Children, \(n) identical(n$Name, "FertiliseFactor"), logical(1)))
  if (length(ff_i) != 1) stop("Could not uniquely find Factor named 'FertiliseFactor' under 'Permutation'.")
  
  # Edit Specification
  old_spec <- perm$Children[[ff_i]]$Specification
  if (!grepl("=", old_spec, fixed = TRUE)) stop("Unexpected: FertiliseFactor$Specification does not contain '='.")
  
  lhs <- sub("=.*$", "", old_spec)  # everything before '='
  new_rhs <- paste0("0 to ", new_max, " step ", new_step)
  perm$Children[[ff_i]]$Specification <- paste0(lhs, "=", new_rhs)
  
  # Put back and write
  factors$Children[[perm_i]] <- perm
  pd$Children[[fac_i]] <- factors
  x$Children[[exp_i]] <- pd
  
  writeLines(jsonlite::toJSON(x, auto_unbox = TRUE, pretty = TRUE), file_out)
  invisible(file_out)
}



  

# DOY -> Date (year is arbitrary; DSSAT uses mm-dd derived from these Dates)
doy_to_date <- function(x, year = 2001L) {
  x <- suppressWarnings(as.integer(x))
  as.Date(x - 1L, origin = paste0(year, "-01-01"))
}


# Helper function to extract FAMN amounts based on Application (F) and Treatment (FERNAME)
get_amount <- function(fert_pattern, f_val,site_df) {
  val <- site_df %>%
    filter(str_detect(FERNAME, fert_pattern) & F == f_val) %>%
    pull(FAMN)
  
  if(length(val) == 0) return(0) else return(val[1])
}

# Recursive function to find and modify the FertiliserRule Manager
update_manager <- function(node,basal_1,top_1,basal_2,top_2,basal_3,top_3,dap) {
  if (is.list(node)) {
    
    # Check if this node is the correct FertiliserRule manager containing the CodeArray
    if (!is.null(node$`$type`) && 
        node$`$type` == "Models.Manager, Models" && 
        !is.null(node$Name) && 
        node$Name == "FertiliserRule") {
      
      # Verify it contains the specific C# script managing the Treatment property
      if (!is.null(node$CodeArray) && any(str_detect(unlist(node$CodeArray), "Treatment\\.ToLower"))) {
        
        code <- node$CodeArray
        
        # Iterate through the lines of C# code and substitute values
        for (i in seq_along(code)) {
          
          # Update 'First' Treatment block
          if (str_detect(code[[i]], "if \\(Treatment\\.ToLower\\(\\) == \"first\"\\)")) {
            code[[i+2]] <- sprintf("                basalAmount = %s;", basal_1)
            code[[i+3]] <- sprintf("                topDressAmount = %s;", top_1)
          } 
          # Update 'Second' Treatment block
          else if (str_detect(code[[i]], "else if \\(Treatment\\.ToLower\\(\\) == \"second\"\\)")) {
            code[[i+2]] <- sprintf("                basalAmount = %s;", basal_2)
            code[[i+3]] <- sprintf("                topDressAmount = %s;", top_2)
          } 
          # Update 'Third' Treatment block
          else if (str_detect(code[[i]], "else if \\(Treatment\\.ToLower\\(\\) == \"third\"\\)")) {
            code[[i+2]] <- sprintf("                basalAmount = %s;", basal_3)
            code[[i+3]] <- sprintf("                topDressAmount = %s;", top_3)
          } 
          # Update DAP logic inside the OnDoManagement method
          else if (str_detect(code[[i]], "if \\(Clock\\.Today == sowingDate\\.AddDays\\(")) {
            code[[i]] <- sprintf("            if (Clock.Today == sowingDate.AddDays(%s))", dap)
          }
        }
        
        node$CodeArray <- code
      }
    }
    
    # Recursively apply to children nodes
    if (!is.null(node$Children)) {
      # 2. Pass the fertilizer variables down through lapply
      node$Children <- lapply(node$Children, update_manager, 
                              basal_1 = basal_1, top_1 = top_1, 
                              basal_2 = basal_2, top_2 = top_2, 
                              basal_3 = basal_3, top_3 = top_3, 
                              dap = dap)
    }
  }
  return(node)
}


#' Modify APSIMX Fertilizer Amounts and Timings
#'
#' @param apsimx_file String. Path to the input .apsimx file.
#' @param template_df String. Path to the fertilizer recommendation CSV file.
#' @param target_lon Numeric. The longitude of the target location.
#' @param target_lat Numeric. The latitude of the target location.
#' @param output_file String. Path for the output .apsimx file.
modify_apsim_fertilizer <- function(apsimx_file, template_df, target_lon, target_lat, output_file) {
  
  # 1. Load the CSV data
  df <- template_df
  
  # 2. Filter for the specific location 
  site_df <- df %>% filter(lon == target_lon, lat == target_lat)
  
  if (nrow(site_df) == 0) {
    stop("The specified longitude and latitude were not found in the CSV.")
  }
  

  
  # 3. Extract the targeted values from the CSV
  # 1st Treatment
  basal_1 <- get_amount("1st fert", 1,site_df)
  top_1   <- get_amount("1st fert", 2,site_df)
  dap     <- site_df %>% filter(str_detect(FERNAME, "1st fert") & F == 2) %>% pull(F.dap) %>% .[1]
  
  # 2nd Treatment
  basal_2 <- get_amount("2nd fert", 1,site_df)
  top_2   <- get_amount("2nd fert", 2,site_df)
  
  # 3rd Treatment
  basal_3 <- get_amount("3rd fert", 1,site_df)
  top_3   <- get_amount("3rd fert", 2,site_df)
  
  # 4. Read the APSIMX JSON file
  apsim <- fromJSON(apsimx_file, simplifyVector = FALSE)
  

  
  #5.  Apply modifications
  apsim_updated <- update_manager(apsim,basal_1,top_1,basal_2,top_2,basal_3,top_3,dap)
  
  # 6. Save the updated JSON safely
  write_json(apsim_updated, output_file, auto_unbox = TRUE, pretty = TRUE, null = "null")
  message("APSIM file successfully updated and saved to: ", output_file)
}





# Produce a dataframe with coords of available soil data and RS planting dates
get_zone_coords_pdates <- function(
    country, useCaseName, Crop, zone, Soil_source, rs_schedule_df,
    datasourcing_path = "~/agwise-datasourcing/dataops/datasourcing") {
  
  if (Soil_source == "ISDA") soil_path <- paste0(datasourcing_path, "/Data/useCase_", country, "_", useCaseName, "/", Crop, "/result/geo_4cropModel/", zone, "/ISDA_SoilDEM_PointData_AOI_profile.RDS")
  if (Soil_source == "ISRIC") soil_path <- paste0(datasourcing_path, "/Data/useCase_", country, "_", useCaseName, "/", Crop, "/result/geo_4cropModel/", zone, "/SoilDEM_PointData_AOI_profile.RDS")
  Soil <- readRDS(soil_path)
  Soil <- na.omit(Soil) %>%
    rename(longitude = lon,
           latitude = lat)
  
  new_coords <- Soil %>% dplyr::select("longitude", "latitude", "NAME_1", "NAME_2") %>%
    mutate(longitude = round(longitude, 3),
           latitude = round(latitude, 3))
  
  new_coords <- new_coords %>% mutate(lat_lon = paste(latitude, longitude))
  
  rs_schedule_df <- rs_schedule_df %>% 
    mutate(longitude = round(longitude, 3),
           latitude = round(latitude, 3))
  
  merged <- inner_join(new_coords, rs_schedule_df, 
                       by = c("latitude", "longitude")) %>%
    unique()
  
  row.names(merged) <- NULL
  
  return(merged)
}






# Cardinal to ordinal for naming
ordinal <- function(x) {
  s <- ifelse(x %% 100 %in% 11:13, "th",
              ifelse(x %% 10 == 1, "st",
                     ifelse(x %% 10 == 2, "nd",
                            ifelse(x %% 10 == 3, "rd", "th"))))
  paste0(x, s)
}


# Produce Initial Conditions df that is common for all DSSAT experiment design approaches
get_filex_initial_conditions <- function(ex_profile, crop_code, plant_dates, file_x) {
  plant_dates <- sort(as.Date(plant_dates))
  n_pd <- length(plant_dates)
  
  ### Ensure ex_profile is ordered by SLB
  slb <- ex_profile$SLB[[1]]
  ord <- order(slb)
  
  ex_profile_sorted <- ex_profile
  
  list_cols <- sapply(ex_profile, is.list)
  
  ex_profile_sorted[list_cols] <- lapply(
    ex_profile[list_cols],
    function(col) list(col[[1]][ord])
  )
  
  n_layers <- length(slb)
  # TODO: So far, we have one IC for each PD
  n_ic <- n_pd
  
  fixed_SNH4 <- 0.6
  fixed_SNO3 <- 3
  
  ic_df <- file_x$`INITIAL CONDITIONS`
  
  ic_df <- ic_df[rep(1, n_ic), ]
  
  ic_df <- ic_df %>% 
    mutate(
      PCR = crop_code
    )
  ic_df$ICBL <- rep(list(sort(slb)), nrow(ic_df))
  ic_df$C <- 1:n_ic
  ic_df$ICDAT <- as.POSIXct(plant_dates %m-% months(1))
  ic_df$SNH4 <- rep(list(rep(fixed_SNH4, n_layers)), nrow(ic_df))
  ic_df$SNO3 <- rep(list(rep(fixed_SNO3, n_layers)), nrow(ic_df))
  ic_df$SH2O <- mapply(function(sdul, slll, index) {
    slll + ((sdul - slll) * index)
  }, ex_profile_sorted$SDUL, ex_profile_sorted$SLLL, MoreArgs = list(index = index_soilwat),
  SIMPLIFY = FALSE)
  
  ic_df
}


# Produce General df that is common for all DSSAT experiment design approaches
get_filex_general <- function(ex_profile, file_x) {
  gen_df <- file_x$GENERAL
  gen_df <- gen_df %>%
    mutate(
      SITE = ex_profile$SITE
    )
  
  gen_df
}


# Produce Fields df that is common for all DSSAT experiment design approaches
get_filex_fields <- function(ex_profile, file_x, i, wsta_prefix) {
  fields_df <- file_x$FIELDS
  fields_df <- fields_df %>%
    mutate(
      WSTA = paste0(wsta_prefix, formatC(
        width = 4, as.integer((i)), flag = "0")),
      ID_SOIL = paste0('TRAN', formatC(
        width = 5, as.integer((i)), flag = "0")),
      XCRD = ex_profile$LONG,
      YCRD = ex_profile$LAT
    )
  
  fields_df
}


# Produce Cultivars df that is common for all DSSAT experiment design approaches
get_filex_cultivars <- function(file_x, crop_code, varietyid, path.to.temdata, geneticfiles) {
  cname <- DSSAT::read_cul(file.path(path.to.temdata, paste0(geneticfiles, '.CUL'))) %>%
    filter(`VAR#` == varietyid)
  cultivars_df <- file_x$CULTIVARS
  cultivars_df <- cultivars_df %>%
    mutate(
      CR = crop_code,
      INGENO = varietyid,
      CNAME = cname$VRNAME
    )
  
  cultivars_df
}


# Produce Planting Details df that is common for all DSSAT experiment design approaches
get_filex_plantdetails <- function(file_x, plant_dates) {
  plant_dates <- sort(as.Date(plant_dates))
  n_pd <- length(plant_dates)
  
  pd_df <- file_x$`PLANTING DETAILS`
  
  pd_df <- pd_df[rep(1, n_pd), ]
  
  pd_df$P <- 1:n_pd
  pd_df$PDATE <- as.POSIXct(plant_dates)
  pd_df$PLNAME <- paste(ordinal(1:n_pd), "plant date")
  
  pd_df
}


# Produce Harvest Details df that is common for all DSSAT experiment design approaches
get_filex_harvestdetails <- function(file_x, plant_dates) {
  plant_dates <- sort(as.Date(plant_dates))
  n_pd <- length(plant_dates)
  n_hd <- n_pd
  
  hd_df <- file_x$`HARVEST DETAILS`
  
  hd_df <- hd_df[rep(1, n_hd), ]
  
  hd_df$H <- 1:n_hd
  hd_df$HDATE <- as.POSIXct(max(plant_dates) %m+% months(8))
  
  hd_df
}


# Produce Simulation Controls df that is common for all DSSAT experiment design approaches
# fert_list: Y/N for nutrients. R/N for "FERTI". Check DSSAT R repo
get_filex_simulationcontrols <- function(
    file_x, plant_dates, number_years,
    fert_list = list(NITRO = "N", PHOSP = "N", POTAS = "N", FERTI = "R")) 
{
  plant_dates <- sort(as.Date(plant_dates))
  n_pd <- length(plant_dates)
  # TODO: So far, we have one SC for each PD
  n_sc <- n_pd
  
  sc_df <- file_x$`SIMULATION CONTROLS`
  
  sc_df <- sc_df[rep(1, n_sc), ]
  
  sc_df$N <- 1:n_sc
  sc_df$NYERS <- number_years
  sc_df$SDATE <- as.POSIXct(plant_dates %m-% months(1))
  sc_df$SNAME <- paste(ordinal(1:n_pd), "plant date")
  sc_df$FMOPT <- NULL
  sc_df$HFRST <- -99
  sc_df$NITRO <- fert_list$NITRO
  sc_df$PHOSP <- fert_list$PHOSP
  sc_df$POTAS <- fert_list$POTAS
  sc_df$FERTI <- fert_list$FERTI
  
  sc_df
}


# Produce Fertilizers Inorganic df that is common for all DSSAT experiment design approaches
# If no Fertilizers return NULL so there is no addition
get_filex_fertilizersinorganic <- function(
    file_x, plant_dates, template_df, NPK_ranges, longitude, latitude, varietyid,
    fert_list = NULL) {
  fi_df <- file_x$`FERTILIZERS (INORGANIC)`
  
  # TODO: template_df seems to be wrong for this approach. Need to revisit with Siya or modify this
  # TODO: Checks: 1) F from template_df?, F is incorrect currently, PDAT from template_df?
  # Path: fertilizer from template file
  if(!is.null(template_df$FAMN)) {
    fi_df <- template_df %>%
      filter(lon == longitude,
             lat == latitude)  # INGENO has been removed from template_df,
             # INGENO == varietyid)
    n_split_applications <- length(unique(fi_df$F.dap))
    
    # TODO: if plant_dates from template_df 
    # TODO: NEED TO REVISIT THIS!! FDATE WRONG? F WRONG?
    # n_fi <- dim(fi_df)[1]
    fi_df <- fi_df[rep(1:nrow(fi_df), times = length(plant_dates)), ]
    row.names(fi_df) <- NULL
    
    n0 <- nrow(fi_df) / length(plant_dates)
    # fi_df$FDATE <- as.POSIXct(fi_df$F.dap + rep(plant_dates, each = n0))
    fi_df$FDATE_date <- as.Date(fi_df$F.dap + rep(plant_dates, each = n0))
    fi_df$FDATE <- as.integer(
      format(fi_df$FDATE_date, "%y")) * 1000 +
        as.integer(format(fi_df$FDATE_date, "%j"))
    
    fi_df <- fi_df %>% dplyr::select(-FDATE_date)
    
    fi_df$F <- rep(seq_len(dim(fi_df)[1]/n_split_applications), each = n_split_applications)
    
    fi_df <- fi_df %>% 
      dplyr::select(any_of(
        c("F", "FDATE", "FMCD", "FACD", "FDEP", "FAMN", "FAMP", "FAMK", "FAMC",
          "FAMO", "FOCD", "FERNAME")))
    return(fi_df)
    
  } else
    # Path: fertilizer from NPK_ranges
    if (exists("NPK_ranges") && !is.null(NPK_ranges)) {
      n_split_applications <- NPK_ranges$n_split_applications
      
      fi_df <- expand.grid(
        FAMN = NPK_ranges$N,
        FAMP = NPK_ranges$P,
        FAMK = NPK_ranges$K,
        plant_date = plant_dates,
        n_split = n_split_applications
      ) %>% 
        mutate(F = row_number())
      
      fi_expanded <- fi_df %>%
        mutate(
          FAMN = FAMN / n_split,
          FAMP = FAMP / n_split,
          FAMK = FAMK / n_split,
          pd_index = match(plant_date, plant_dates)
        ) %>%
        uncount(n_split, .id = "application_index") %>%
        mutate(
          F.dap = NPK_ranges$F.dap[application_index],
          FDATE = as.POSIXct(plant_date + F.dap),
          FERNAME = paste(
            ordinal(F), "fert",
            ordinal(application_index), "app",
            ordinal(pd_index), "pd"
          ),
          FMCD = NPK_ranges$FMCD,
          FACD = NPK_ranges$FACD,
          FDEP = NPK_ranges$FDEP,
          FAMC = NPK_ranges$FAMC,
          FAMO = NPK_ranges$FAMO,
          FOCD = NPK_ranges$FOCD
        ) %>%
        select(-c(application_index, plant_date, pd_index, F.dap)) %>%
        select(c(F, FDATE, FMCD, FACD, FDEP, FAMN, FAMP, FAMK, FAMC, FAMO, FOCD,
                 FERNAME))
      
      return(fi_expanded)
    } else {
      # No fertilizer in the experiment
      message("Continuing without modifying fertilizer tab in DSSAT template.")
      NULL
    }
}


# Produce Treatments df that is common for all DSSAT experiment design approaches
get_filex_treatments <- function(file_x, fert_list = NULL) {
  treatments_df <- file_x$`TREATMENTS                        -------------FACTOR LEVELS------------`
  fi_df <- file_x$`FERTILIZERS (INORGANIC)`
  sc_df <- file_x$`SIMULATION CONTROLS`
  pd_df <- file_x$`PLANTING DETAILS`
  ic_df <- file_x$`INITIAL CONDITIONS`
  plant_dates <- pd_df$PDATE
  number_years <- unique(sc_df$NYERS)
  
  if (!is.null(fi_df)) {
    # Fertilizer levels already consider planting dates levels
    n_t <- max(fi_df$F)
    n_sc <- max(sc_df$N)
    treatments_df <- treatments_df[rep(1, n_t), ]
    
    treatments_df$N <- 1:n_t
    treatments_df$MF <- 1:n_t
    n_pd <- max(pd_df$P)
    fert_levels <- unique(gsub("\\s*\\d+(st|nd|rd|th) application", "", fi_df$FERNAME))
    pd_levels <- paste0(1:n_pd, c("st","nd","rd","th")[1:n_pd], " pd")
    
    treat_names <- expand.grid(
      fert = fert_levels,
      pd   = pd_levels,
      stringsAsFactors = FALSE
    )
    treat_names$TNAME <- paste(treat_names$fert, treat_names$pd)
    
    treatments_df$TNAME <- treat_names$TNAME

    # IC, MP, SM and MH are the same
    pd_index <- as.integer(sub(".*\\b(\\d+)(st|nd|rd|th) pd.*", "\\1",
                               treatments_df$TNAME))
    treatments_df$IC <- pd_index
    treatments_df$MP <- pd_index
    treatments_df$MH <- pd_index
    treatments_df$SM <- pd_index
    
    sc_df <- sc_df[rep(1, n_sc), ]
    sc_df$N <- 1:n_sc
    sc_df$NYERS <- number_years
    sc_df$SDATE <- as.POSIXct(plant_dates %m-% months(1))
    sc_df$SNAME <- paste(ordinal(1:n_pd), "plant date")
    sc_df$FMOPT <- NULL
    sc_df$HFRST <- -99
    sc_df$NITRO <- fert_list$NITRO
    sc_df$PHOSP <- fert_list$PHOSP
    sc_df$POTAS <- fert_list$POTAS
    sc_df$FERTI <- fert_list$FERTI
    
    return(treatments_df)
  } else if (is.null(fi_df)) {
    # Only Planting date is a treatment
    n_pd <-max(pd_df$P)
    n_t <- n_pd
    treatments_df <- treatments_df[rep(1, n_t), ]
    
    treatments_df$TNAME <- pd_df$PLNAME
    treatments_df$N <- 1:n_t
    treatments_df$IC <- 1:n_t
    treatments_df$MP <- 1:n_t
    treatments_df$MH <- 1:n_t
    return(treatments_df)
  }
}


# Ensure layers are correctly ordered
check_layers_order <- function(ex_profile) {
  ex_profile <- ex_profile %>%
    unnest(cols = everything()) %>%   # unnest all list-columns
    arrange(SLB)
  return(ex_profile)
}


# Get range of treatments
get_n_treatments <- function(
    template_df, Forecast, fertilizer, fert_factorial, fert_grid_RS) {
  
  if (fert_factorial) {
    n_applications <- length(unique(template_df$F.dap))
    
    one_location_levels <- template_df %>%
      filter(lon == template_df$lon[1],
             lat == template_df$lat[1])
    n_fert <- dim(one_location_levels)[1] / n_applications
    # Columns that define the RS planting date
    n_pd <- length(colnames(template_df)[grep("^q", colnames(template_df))]) + 1
    TRT <- 1:(n_fert * n_pd)
  } else {
    message("get_n_treatments() not implemented for your type of experiment")
  }
  
  return(TRT)
}

