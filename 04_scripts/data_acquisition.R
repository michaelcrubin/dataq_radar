### ********************************************************
### THIS SCRIPT MANAGE SWEATHER DATA AQUISITION FOR THE PROJECT
### ********************************************************


## --------------- LIBRARIES ------------------

library(here)
source(here("scripts", "helpers.R"))
source(here("scripts", "parameters.R"))

## passwords to be deleted
Sys.setenv("mm_user" = "schweizer_hagel")
Sys.setenv("mm_password" = "RVvI525YLI")
Sys.setenv("mb_password" = "GeXbpCwuclVIvSYh")


###----------------------------- HELPER FUNCTIONS ---------------------------------


expected_n_data <- function(timeres){
  list(P1D = 365, PT15M = 365*24*4, PT1H = 365*24)[[timeres]]
}


# i need to shift 1 unit forth because otherwise it will access the data of the pevious period (i.e. 1.1.24 will sum all hours form 31.12.23)
# hence, what I want is the value for 1.2.24, but give it the name of 1.1.24
# therefore, i have to shift in parsetime 1 unit forth, then i have to shift after 1 unit back
# needed to force the start in locla time / api expect always UTC
# additonally we shift 7.5 min back to have the inst value at the mid point of the 15mins
parse_time <- function(date_string, timeres){
  if (timeres == "P1D"){
    lubridate::ymd_hms(paste(date_string, "00:00:00")) %>%
      lubridate::with_tz("UTC") %>%
      magrittr::add(lubridate::days(1)) %>% 
      format("%Y-%m-%dT%H:%M:%SZ")
    
  } else if (timeres == "PT1H") {
    lubridate::ymd_hms(paste(date_string, "00:00:00")) %>%
      lubridate::with_tz("UTC") %>%
      magrittr::add(lubridate::hours(1)) %>% 
      format("%Y-%m-%dT%H:%M:%SZ")
    
  } else if (timeres == "PT15M") {
    lubridate::ymd_hms(paste(date_string, "00:00:00")) %>%
      lubridate::with_tz("UTC") %>%
      # magrittr::add(lubridate::seconds(7.5*)) %>% 
      format("%Y-%m-%dT%H:%M:%SZ")
    
  } else {
    lubridate::ymd_hms(paste(date_string, "00:00:00")) %>%
      lubridate::with_tz("UTC") %>%
      format("%Y-%m-%dT%H:%M:%SZ")
  }
}

shift_time_back <- function(X, timeres){
  if (timeres == "P1D"){
    X %>% dplyr::mutate(timestamp = as.Date(timestamp - lubridate::days(1)))
  } 
  else if (timeres == "PT1H") {
    X %>% dplyr::mutate(timestamp = as.POSIXct(timestamp - lubridate::hours(1)))
  }
  else if (timeres == "PT15M") {
    X
  } 
  else {
    X
  }
  
}


###----------------------------- METADATA TABLE FUNCTIONS ---------------------------------


# Calls tables for a certain 
call_station_table_MM <- function(country, parameter, ...){
  url <- glue("https://{Sys.getenv('mm_user')}:{Sys.getenv('mm_password')}@api.meteomatics.com/find_station?location={country}&parameters={parameter}")
  httr::GET(url) %>%
    httr::content( type = "text", encoding="UTF-8") %>%
    data.table::fread(sep = ";") %>%
    as.data.frame() %>%
    tidyr::separate(`Location Lat,Lon`, into = c("lat", "lon"), sep = ",", convert = TRUE) %>%
    dplyr::mutate(date = `Start Date`, end = `End Date`) %>%
    dplyr::select(all_of(c("ID Hash", "wmo_id" ="WMO ID", "alt_id" = "Alternative IDs","Name", "lon", "lat", "date", "end")))
}


# obtains meta data for all available statons for one country and one var
CREATE_meta_data <- function(var_id, country, var_list, ...){
  # import the country polygons
  geo <- readRDS(here("DB","raw_data",  "countries.rds")) %>%
    filter(tolower(country_name) == country)
  
  # call all available stations
  call_station_table_MM(country, var_list[["station"]][["parameter"]]) %>%
    
    # check if in country (mm param unreliable)
    dplyr::mutate(lon1 = lon,lat1 = lat) %>%
    sf::st_as_sf(coords = c("lon1", "lat1"), crs = 4326) %>%
    sf::st_join( geo, join = st_within) %>% 
    tidyr::drop_na(country_id) %>%
    sf::st_drop_geometry() %>%
    
    # add meta data
    dplyr::mutate(
      loc_id = paste0(var_id,"_mm_", tolower(country_id),"_", row_number()),
      loc_name = Name,
      var_id = var_id,
      parameter = var_list[["station"]][["parameter"]],
      timeres = var_list[["station"]][["timeres"]], 
      min_year = lubridate::year(date),
      max_year = lubridate::year(end)
    )
}

# obtains meta data for all available statons for one country and one var
CREATE_BMN_meta_data <- function(var_id, var_list, ...){
  
  country_id <- "CHE"
  country <- "Switzerland"
  bmn_meta <- readRDS(here("DB", "raw_data", "meta_bmn.rds"))
  bmn_data <- readRDS(here("DB", "raw_data", "data_bmn.rds"))%>% dplyr::mutate(date = as.Date(date))
  X <- bmn_data %>% 
    left_join(bmn_meta, by = join_by(loc_id)) %>%
    dplyr::mutate(year = lubridate::year(date)) %>%
    group_by(bdm_loc_id) %>%
    summarise(
      loc_name = dplyr::first(loc_name),
      date = min(date),
      end = max(date),
      min_year = min(year),
      max_year = max(year),
      years = list(unique(year)),
      n_year = length(unique(year)),
      lon = dplyr::first(lon),
      lat = dplyr::first(lat),
      data_source = dplyr::first(data_source),
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      loc_id = paste0(var_id,"_bmn_", tolower("che"),"_", bdm_loc_id),
      #loc_name = Name,
      var_id = var_id,
      country_id = country_id,
      country_name = country,
      parameter = var_list[["station"]][["parameter"]],
      timeres = var_list[["station"]][["timeres"]]
      
    )
  return(X)
  
}

###----------------------------- GROUND TRUTH FUNCTIONS ---------------------------------


join_dublicates <- function(X, y, ...){
  if (nrow(X) == 1){return(X)}
  
  # join the data sets
  data <- X %>% dplyr::select(station_data) %>% tidyr::unnest(cols = c(station_data)) %>%
    dplyr::group_by(year) %>%
    dplyr::slice_max(order_by = n_clean_data_point, n=1, with_ties = F) %>%
    ungroup()
  
  # add to meta data
  Y <- X %>% 
    dplyr::slice_max(order_by = tot_clean_data_point, n=1, with_ties = F) %>%
    dplyr::mutate(
      n_year = nrow(data),
      years = list(data$year),
      tot_data_point = sum(data$n_data_point),
      tot_clean_data_point = sum(data$n_clean_data_point),
      station_data = list(data)
    )
  return(Y)
}


fetch_station_data_chunk <- function(var_id,  alt_id, wmo_id, year, timeres, parameter, completness_criteria = 0.7, ...) {

  if (is.na(alt_id)){
    id <- paste0("wmo_", wmo_id)
  } else {
    id <- paste0("id_", alt_id)
  }
  
  date <- as.Date(paste0(year, "-01-01")) %>% parse_time(timeres)
  end <- as.Date(paste0(year, "-12-31")) %>% parse_time(timeres)
  url <- glue("https://{Sys.getenv('mm_user')}:{Sys.getenv('mm_password')}@api.meteomatics.com/{date}--{end}:{timeres}/{parameter}/{id}/csv?source=mix-obs&on_invalid=fill_with_invalid")
  
  ok <- tryCatch({
    data <- httr::GET(url) %>% 
      httr::content(type = "text", encoding = "UTF-8") %>% 
      data.table::fread(sep = ";") %>% 
      as.data.frame() %>% 
      dplyr::rename_with(.cols = dplyr::everything(), ~ c("timestamp", var_id))
    TRUE
  }, error = function(e) {
    FALSE
  })
  
  # data completeness check 
  if (!isTRUE(ok)){
    print(paste(id, year, "call failed"))
    return(NULL)
    # return(list(year = year, success = FALSE, n_data_point = NA, n_clean_data_point = NA, data = list(NULL)))
  }
  # criteria: 70% need to be real data
  
  all <- data %>% pull()
  clean <- all[all != -999]
  
  if (length(clean) < ceiling(expected_n_data(timeres) * completness_criteria)){
    print(paste(id, year, "not enough data or too many missing"))
    return(NULL)
    # return(list(year = year, success = FALSE, n_data_point = length(all), n_clean_data_point = length(clean), data = list(NULL)))
  }

  # err <- data %>% pull()
  # if (sum(err != -999) / length(err) < 0.7){ 
  #   print(paste(id, year, "too many missing"))
  #   return(list(year = year, success = FALSE, data = list(NULL)))
  # }
  ret <- list(
    year = year,
    # success = TRUE,
    n_data_point = length(all),
    n_clean_data_point = length(clean),
    data = list(data)
  )
  return(ret)
  
}


call_station_data <- function(var_id,  alt_id, wmo_id, min_year, max_year, timeres, parameter, ...){
  
  years <- c(min_year:max_year)
  data <- purrr::map_dfr(years, ~fetch_station_data_chunk(var_id,  alt_id, wmo_id, year = .x, timeres, parameter)) 

  ret <- list(
    n_year = nrow(data),
    success = (nrow(data) > 0),
    years = list(data$year),
    tot_data_point = sum(data$n_data_point),
    tot_clean_data_point = sum(data$n_clean_data_point),
    station_data = list(data)
  )
  return(ret)
}


prepare_bmn_station_data<- function(var_id, ...){
  
  # get
  readRDS(here("DB", "raw_data", "data_bmn.rds")) %>% 
    dplyr::mutate(date = as.Date(date)) %>%
    mutate(
      year = lubridate::year(date)
    ) %>%
    group_by(bdm_loc_id, year ) %>%
    mutate(
      n_data_point = n(),
      n_clean_data_point = n_data_point) %>%
    ungroup()%>%
    dplyr::select(bdm_loc_id, timestamp = date, year, value, n_data_point, n_clean_data_point) %>%
    dplyr::rename_with(.cols = c("value"), ~ c(var_id)) %>%
    tidyr::nest(data = c("timestamp", var_id))%>%
    tidyr::nest(station_data = c("year", "n_data_point","n_clean_data_point","data"))
}

###----------------------------- MODEL FUNCTIONS ---------------------------------


# fetches and parses the meteomatics api data
fetch_mm_model_data <- function(var_id, lon, lat, year, timeres, parameter, model, modelparam, ...) {

  print(paste("************************************"))
  date <- as.Date(paste0(year, "-01-01")) %>% parse_time(timeres)
  end <- as.Date(paste0(year, "-12-31")) %>% parse_time(timeres)
  url <- glue("https://{Sys.getenv('mm_user')}:{Sys.getenv('mm_password')}@api.meteomatics.com/{date}--{end}:{timeres}/{parameter}/{lat},{lon}/csv?{modelparam}")
 # print(url)
  # url <- "https://schweizer_hagel:RVvI525YLI@api.meteomatics.com/2020-01-01T00:00:00.000+01:00--2020-12-31T00:00:00.000+01:00:PT1H/precip_1h:mm/47.3744489,8.5410422/csv?model=ecmwf-era5"
  # httr::GET(url) %>% 
  #   httr::content(type = "text", encoding = "UTF-8")
  data <- tryCatch({
    x <- httr::GET(url) %>% 
      httr::content(type = "text", encoding = "UTF-8") %>% 
      data.table::fread(sep = ";") %>% 
      as.data.frame() %>%
      dplyr::rename_with(.cols = dplyr::everything(), ~ c("timestamp", var_id)) %>%
      shift_time_back(timeres) %>%
      dplyr::mutate(model = !!model)
    print(paste("SUCCESS", var_id, year, model))
    x
  }, error = function(e) {
    print(paste("failed", var_id, year, model))
    NULL
  })
}


# fetches and parses the meteoblue api data
fetch_mb_model_data <- function(var_id, lon, lat, year, timeres, parameter, model, modelparam, ...) {
  
  # year <- 2024
  print("************************************")
  print(paste("Meteoblue:", var_id, year, model))
  
  date <- as.Date(paste0(year, "-01-01")) %>% parse_time(timeres)
  end  <- as.Date(paste0(year, "-12-31")) %>% parse_time(timeres)
  
  body <- list(
    units = list(
      temperature = "C",
      velocity = "km/h",
      length = "metric",
      energy = "watts"
    ),
    geometry = list(
      type = "MultiPoint",
      coordinates = list(c(lon, lat)),
      mode = "preferLandWithMatchingElevation"
    ),
    format = "json",
    timeIntervals = list(paste0(substr(date, 1, 10), "T+00:00/", substr(end, 1, 10), "T+00:00")),
    timeIntervalsAlignment = "none",
    queries = list(list(
      domain = gsub("domain=", "", modelparam),
      timeResolution = timeres,
      codes = list(list(
        code = parameter,
        level = "sfc"
      ))
    ))
  )
  response <- httr::POST(
    url = "https://my.meteoblue.com/dataset/query",
    query = list(apikey = Sys.getenv("mb_password")),
    body = body,
    encode = "json",
    httr::content_type_json()
  )
  
  if (httr::status_code(response) != 200) {
    print(paste("FAILED", var_id, year, model, "status:", httr::status_code(response)))
    return(NULL)
  }
  
  result <- httr::content(response, as = "parsed", simplifyVector = TRUE)
  times <- result$timeIntervals[[1]]
  values <-result$codes[[1]][["dataPerTimeInterval"]][[1]][["data"]][[1]]
  
  df <- tibble::tibble(
    timestamp = lubridate::ymd_hm(times, tz = "UTC"),
    !!var_id := as.numeric(values),
    model = model
  )  %>%
    shift_time_back(timeres)
  
  print(paste("SUCCESS", var_id, year, model))
  return(df)
}


# # fetches 1 year all models
get_model_data_year <- function(var_id, lon, lat, year,  timeres, parameter, var_list){
  # browser()
  # here get the mm data
  mm_list <-var_list[["mm"]]
  mm_data <- purrr::imap_dfr(mm_list$models, ~fetch_mm_model_data(var_id, lon, lat, year,  timeres = mm_list$timeres, parameter = mm_list$parameter, model = .y, modelparam = .x))
  
  
  # here add meteoblue data
  mb_list <-var_list[["mb"]]
  mb_data <- purrr::imap_dfr(mb_list$models, ~fetch_mb_model_data(var_id, lon, lat, year,  timeres = mb_list$timeres, parameter = mb_list$parameter, model = .y, modelparam = .x))

  data <- rbind(mm_data, mb_data)
  
  if (nrow(data) > 0){
    ret <- list(
      year = year,
      models = list(unique(data$model)),
      data = list(data)
    )
  } else {
    ret<-NULL
  }

  return(ret)
}


# takes a year, lon, lat, var, timeres and a model list and returns a joined df with all available models
extract_model_data <- function(var_id, lon, lat, years, timeres, parameter, var_list, ...){
  
  data <- purrr::map_dfr(years, ~get_model_data_year(var_id, lon, lat, year = .x,  timeres, parameter, var_list))
  
  ret <- list(
    all_models = list(unique(unlist(data$models, use.names = F))),
    model_data = list(data)
  )
  return(ret)
  
}

###----------------------------- RUNNER VALIDATE DATA ---------------------------------

RUN_data_extraction <- function(var_id, country, suffix = "X", renew_meta = FALSE, parameter_list){

  var_list <- parameter_list[[var_id]]

  
  if (renew_meta){
    X <- CREATE_meta_data(var_id, country, var_list)

    Y <- X %>%
      #slice(1:10)%>%
      #filter(alt_id == "STG") %>%
      dplyr::mutate(purrr::pmap_dfr(., .f = call_station_data)) %>%
      dplyr::filter(success) %>% dplyr::select(-success) 
    
    Z <- Y %>% 
      dplyr::group_by(alt_id) %>%
      group_modify(~join_dublicates(.x, .y)) %>%
      ungroup()
    
    saveRDS(Z, here("DB","meta_data",  paste0(var_id, "_", country,"_",suffix,".rds")))
  }

  Y <- readRDS(here("DB","meta_data",  paste0(var_id, "_", country,"_",suffix,".rds")))# %>% slice(33:145)

  browser()
  # add model data and nest it
  ## normal
  Z <- Y %>%
    slice(1:2)%>%
    dplyr::mutate(purrr::pmap_dfr(., .f = extract_model_data, var_list = var_list))

  ## paralellized
  future::plan("multisession", workers = 30)  # Use n_cores
  Z <- Y %>% dplyr::mutate(furrr::future_pmap_dfr(., .f = extract_model_data, var_list = var_list))
  future::plan("sequential")
  
  meta_cols <- c("loc_name","wmo_id", "alt_id", "date", "end", "timeres", "parameter", "years","min_year", "max_year", "country_id", "country_name", "lon", "lat")
  RES <- Z %>%
    tidyr::nest(meta_data = all_of(meta_cols)) %>%
    dplyr::select(loc_id, var_id, n_year,  all_models, tot_data_point, tot_clean_data_point, station_data, model_data, meta_data)

  #arrow::write_feather(RES, here("temp_data",  paste0(var_id, "_", country,"_",suffix,".feather")))
  saveRDS(RES, here("temp_data",  paste0(var_id, "_", country,"_",suffix,".rds")))
  print(paste("SUCCESS", var_id, country))
  return(TRUE)
  
}


###----------------------------- RUNNER TEST DATA ---------------------------------

RUN_bmn_data_extraction <- function(var_id, country, suffix = "X", parameter_list){
  var_list <- parameter_list[[var_id]]
  X <- CREATE_BMN_meta_data(var_id, var_list)
  station_data <- prepare_bmn_station_data(var_id)

  # Y <- X %>%
  #   slice(1:2)%>%
  #   dplyr::mutate(purrr::pmap_dfr(., .f = extract_model_data, var_list = var_list))
  # 
  ## paralellized
  future::plan("multisession", workers = 4)  # Use n_cores
  Y <- X %>% dplyr::mutate(furrr::future_pmap_dfr(., .f = extract_model_data, var_list = var_list))
  future::plan("sequential")
  
  Z <- Y %>% left_join(station_data, by = join_by(bdm_loc_id))

  meta_cols <- c("loc_name","bdm_loc_id", "date", "end", "timeres", "parameter", "years","min_year", "max_year", "country_id", "country_name","data_source", "lon", "lat")
  RES <- Z %>%
    tidyr::nest(meta_data = all_of(meta_cols)) %>%
    dplyr::select(loc_id, var_id, n_year,  all_models, station_data, model_data, meta_data)
  
  saveRDS(RES, here("temp_data",  paste0(var_id, "_", country,"_",suffix,".rds")))
  print(paste("SUCCESS", var_id, country))
  return(TRUE)
  
}



###----------------------------- TOP LINE -----------------------------

var_id <- "rain_trend"
country <- "switzerland"
suffix <- "trend"

parameter_list <- GET_parameters()
# RUN_bmn_data_extraction(var_id, country, suffix, parameter_list)
# 


res <- RUN_data_extraction("rain_trend", "switzerland", "trend",  renew_meta = F, parameter_list = parameter_list)
#res <- RUN_data_extraction(var_id, country, suffix, renew_meta = F, parameter_list = parameter_list)
#res <- RUN_data_extraction("rain_day", "switzerland", "cal", renew_meta = )
#res <- RUN_data_extraction("rain_hour", "switzerland", "cal", renew_meta = FALSE)


split_store_dataset( var_id, country, suffix)

available_locs(var_id, country, suffix)
data <- open_data(var_id, country, suffix)



# Y<-open_station_data(var_id, country, suffix, years = c(2022:2024))
# X<-open_station_data(var_id, country, suffix, loc_id = NA, slice = NA, years = NA)
# X<-open_station_data(var_id, country, suffix, loc_id = NA, slice = NA, years = NA)