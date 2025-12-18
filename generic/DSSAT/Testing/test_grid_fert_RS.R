create_grid_factorial_design <- function(file_x, ex_profile,
                                         template_df, NPK_ranges,
                                         plant_dates, FMCD = "FE027", 
                                         FACD = "AP004", FDEP = 5, F.dap = 42,
                                         FAMC = -99, FAMO = -99, FOCD = -99
                                         ) {
  
  fert_x <- dplyr::slice(file_x$`FERTILIZERS (INORGANIC)`, 0)
  
  fert_factorial_df <- expand.grid(
    FAMN = NPK_ranges$N,
    FAMP = NPK_ranges$P,
    FAMK = NPK_ranges$K
  ) %>%
    mutate(FMCD = FMCD,
           FACD = FACD,
           FDEP = FDEP,
           FAMC = FAMC,
           FAMO = FAMO,
           FOCD = FOCD,
           FERNAME = paste0(FAMN, "N.", FAMP, "P.", FAMK, "K"),
           F = NA,
           FDATE = NA) %>%
    select(all_of(colnames(fert_x)))
  
  # template_df <- template_df_ori
  
  template_df <- template_df %>%
    filter(lat == ex_profile$LAT & lon == ex_profile$LON)
  
  split_app <- unique(template_df$split_application)
  
  file_x$CULTIVARS$CNAME <- unique(template_df$CNAME)
  
  template_df <- template_df %>%
    select(-c(CNAME, INGENO))  # Remove non-DSSAT columns
  
  # Populate fertilizer levels
  fert_x <- dplyr::slice(file_x$`FERTILIZERS (INORGANIC)`, 0)
  for (i in seq_along(plant_dates)){
    # Split application for Nitrogen
    if (split_app %in% c("Yes", T)){
      # Half N and all P and K applied at planting
      first_application_df <- fert_factorial_df %>%
        mutate(F = as.numeric(row.names(fert_factorial_df)) + 
                 (i - 1) * dim(fert_factorial_df)[1],
               FDATE = plant_dates[i],
               FAMN = FAMN/2)
      # Half N and zero P and K applied F.dap days after planting
      second_application_df <- fert_factorial_df %>%
        mutate(F = as.numeric(row.names(fert_factorial_df)) + 
                 (i - 1) * dim(fert_factorial_df)[1],
               FDATE = plant_dates[i] + F.dap,
               FAMN = FAMN/2,
               FAMP = 0,
               FAMK = 0)
      # Interleave rows
      fert_x_ij <- bind_rows(
        first_application_df %>% mutate(.idx = row_number(), .src = 1),
        second_application_df %>% mutate(.idx = row_number(), .src = 2)
      ) %>%
        arrange(.idx, .src) %>%
        select(-.idx, -.src)
      
    } else {
    # All fertilizer applied at planting
      fert_x_ij <- fert_factorial_df %>%
        mutate(F = as.numeric(row.names(fert_factorial_df)) + 
                 (i - 1) * dim(fert_factorial_df)[1],
               FDATE = plant_dates[i])
    }
    
    fert_x <- bind_rows(fert_x, fert_x_ij)
  }
  file_x$`FERTILIZERS (INORGANIC)` <- fert_x
  
  
  file_x$FIELDS$ID_FIELD <- unique(template_df$NAME_2)
  file_x$FIELDS$XCRD <- unique(template_df$lon)
  file_x$FIELDS$YCRD <- unique(template_df$lat)
  
  planting_details_df <- file_x$`PLANTING DETAILS`[rep(seq_len(nrow(
    file_x$`PLANTING DETAILS`)), 4), ] %>%
    mutate(P = 1:length(plant_dates),
           PDATE = as.POSIXct(plant_dates))
  file_x$`PLANTING DETAILS` <- planting_details_df
  
  initial_conditions_df <- file_x$`INITIAL CONDITIONS`[rep(seq_len(nrow(
    file_x$`INITIAL CONDITIONS`)), 4), ] %>%
    mutate(C = 1:length(plant_dates),
           ICDAT = as.POSIXct(plant_dates %m-% months(1)))
  file_x$`INITIAL CONDITIONS` <- initial_conditions_df
  
  harvest_details_df <- file_x$`HARVEST DETAILS`[rep(seq_len(nrow(
    file_x$`HARVEST DETAILS`)), 4), ] %>%
    mutate(H = 1:length(plant_dates),
           HDATE = as.POSIXct(plant_dates %m+% months(8)))
  file_x$`HARVEST DETAILS` <- harvest_details_df
  
  sim_controls_df <- file_x$`SIMULATION CONTROLS`[rep(seq_len(nrow(
    file_x$`SIMULATION CONTROLS`)), 4), ] %>%
    mutate(N = 1:length(plant_dates),
           SDATE = as.POSIXct(plant_dates %m-% months(1)))
  if (AOI) sim_controls_df$NYERS <- number_years
  file_x$`SIMULATION CONTROLS` <- sim_controls_df
  
  # TODO CONTINUE HERE
  
  trt_x_original <- file_x$`TREATMENTS                        -------------FACTOR LEVELS------------`
  trt_x <- dplyr::slice(trt_x_original, 0)
  trt_ij <- 0
  
  for (i in seq_along(plant_dates)){
    for (j in 1:length(unique(fert_x$FERNAME))){
      trt_ij <- trt_ij + 1
      trt_x_ij <- trt_x_original
      
      Tname <- fert_x %>%
        filter(F == j) %>%
        select(FERNAME) %>%
        mutate(FERNAME = substr(FERNAME, 1, 12)) %>%
        unique()
      
      trt_x_ij <- trt_x_ij %>%
        mutate(
          N = trt_ij,
          TNAME = case_when(
            length(plant_dates) == 1 ~ Tname[[1, 1]],
            TRUE ~ paste0(Tname[1, ], "@", format(plant_dates[i], "%m-%d"))
          ),
          MF = trt_ij,
          across(c(MH, MP, IC, SM), ~ i)
        )
      
      trt_x <- bind_rows(trt_x, trt_x_ij)
    }
  }
  file_x$`TREATMENTS                        -------------FACTOR LEVELS------------` <- trt_x
  
  return(file_x)
}