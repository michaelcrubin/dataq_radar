### MAIN PROJECT FILE // WORK HERE
# Auto Generated: 2026-03-03 09:22:02.891928
# Start working here...

##-------------------------- IMPORT DEPENDENCIES -------------------------- 
# ....

library(here)
source(here('04_scripts', 'global.R'))
source(here('04_scripts', 'helpers.R'))
##-------------------------- FUNCTIONS -------------------------- 

# variable <- "rain"
# country <- "CHE"
# date <- "2026-02-16"
# end <- "2026-02-16"
# properties <- GET_query_geodims(country, base_res = c(0.0131, 0.009), max_cells = 100000, crs = 4326)
# 
# QUERY_grid <- map_query_grid(variable, country, date, end)%>%
#   dplyr::mutate(
#     lon = paste(properties$x_min, properties$x_max, sep = "_"),
#     lat = paste(properties$y_min, properties$y_max, sep = "_"),
#     res = paste(properties$x_res, properties$y_res, sep = "_")
#   ) %>%
#   dplyr::select(var_id, parameter, country_id, grid_res, lon, lat, res) %>%
#   saveRDS(here("01_database", "grid_info_ch.RDS"))


params <- GET_mm_variable()


cutoff_admin_boundary <- function(map, country, level, key = NA, buffer = NA){
  
  poly <- GET_geography(country_id = country, level = level, key = key, buffer = buffer)
  
  if (any(class(map) %in% c("RasterBrick","RasterLayer","RasterStack"))){
    # clipping a raster
    map_cut <- terra::mask(map, as(poly, "Spatial"))
  } else if (c("sf") %in% class(map)) {
    # clipping a sf
    sf::st_within()
    map_cut <- map %>%
      dplyr::mutate(inside = as.vector(sf::st_within(x = geom, y = poly, sparse = FALSE))) %>%
      dplyr::filter(inside)
  } else {
    map_cut <- map
  }
  return(map_cut)
}


api_parse_raster_geostore <- function(answer, variable, raw = FALSE, clean = FALSE, country_id = NA, border_clip = FALSE, buffer = NA){
  
  # img <-  httr::content(answer)[[1]][["file"]] %>%
  #   stringr::str_replace_all("(?<!\\\\)\\\\{1}(?!\\\\)", "/") %>%
  #   raster::brick()
  # answer <- httr::content(answer)
  files <-  httr::content(answer) %>%
    purrr::map_chr("file") %>%
    stringr::str_replace_all("(?<!\\\\)\\\\{1}(?!\\\\)", "/")
  
  img <- files %>%
    purrr::map(raster::brick) %>%
    purrr::reduce(raster::merge)
  
  # clips or not the image
  if (isTRUE(border_clip) & !is.na(country_id)){
    img <- cutoff_admin_boundary(img, country = country_id, level = "country", key = NA, buffer = buffer)
  }
  # raw then takes just vals ow list
  if (isTRUE(raw)){
    
    ret <- as.numeric(raster::values(img))
    if (isTRUE(clean)){
      ret <- stats::na.omit(ret)
    }
    return(ret)
    
  } 
  
  else {
    date <- httr::content(answer)[[1]][["datum"]] %>% as.Date()
    ret <- list(
      variable = variable,
      date = date,
      border_clip = border_clip,
      clip_buffer = ifelse(border_clip, buffer, NA),
      raster = list(img)
    )
    return(ret)
  }
}


api_parse_raster_temp <- function(answer, variable, raw = FALSE, clean = FALSE, country_id = NA, border_clip = FALSE, buffer = NA){
  
  
  # Write response to temp file
  nc_file <- tempfile(fileext = ".nc")
  content <- httr::content(answer)
  
  writeBin(httr::content(answer, "raw"), nc_file)
  img <- raster::brick(nc_file)  # reads multi-layer NetCDF
  img <- raster::readAll(img)
  unlink(nc_file)  # delete the temp file if done
  # Inspect structure
  
  # clips or not the image
  if (isTRUE(border_clip) & !is.na(country_id)){
    img <- cutoff_admin_boundary(img, country = country_id, level = "country", key = NA, buffer = buffer)
  }
  # raw then takes just vals ow list
  if (isTRUE(raw)){
    
    ret <- as.numeric(raster::values(img))
    if (isTRUE(clean)){
      ret <- stats::na.omit(ret)
    }
    return(ret)
    
  } 
  
  else {
    
    date <- as.Date(raster::getZ(img))
    ret <- list(
      variable = variable,
      date = date,
      border_clip = border_clip,
      clip_buffer = ifelse(border_clip, buffer, NA),
      raster = list(img)
    )
    return(ret)
  }
}


api_parse_csv <- function(answer, variable, parameter, source, lon, lat,raw = FALSE){
  
  if (isTRUE(raw)){
    a<- httr::content(answer, type = "text", encoding="UTF-8") %>%
      data.table::fread(sep = ";") %>% as.data.frame() %>%
      dplyr::select(-any_of(c("date", "lon", "lat")))%>% dplyr::pull()
  } 
  # workhole parser
  else if (Sys.getenv("mm_res") == "h"){
    clnms <- strsplit( gsub('24h', '1h', parameter) , ",")[[1]]
    A <- httr::content(answer, type = "text", encoding="UTF-8") %>%
      data.table::fread(sep = ";") %>%
      as.data.frame() %>%
      dplyr::rename_with(.cols = tidyselect::all_of(clnms), ~split_var_cols(variable)) %>%
      dplyr::rename_with(.cols = tidyselect::any_of(c("date", "validdate", "Date")), ~"date") %>%
      dplyr::mutate(
        date = as.POSIXct(lubridate::round_date(date, unit = "hour")),
        lon = lon,
        lat = lat
      )
  }
  else {
    clnms <- strsplit(parameter, ",")[[1]]
    A <- httr::content(answer, type = "text", encoding="UTF-8") %>%
      data.table::fread(sep = ";") %>%
      as.data.frame() %>%
      dplyr::rename_with(.cols = tidyselect::all_of(clnms), ~split_var_cols(variable)) %>%
      dplyr::rename_with(.cols = tidyselect::any_of(c("date", "validdate", "Date")), ~"date") %>%
      dplyr::mutate(
        date = dplyr::case_when(source == 1 ~as.Date(date) - lubridate::days(1),
                                TRUE ~ as.Date(date)),
        lon = lon,
        lat = lat
      )
  }
}


parse_response <- function(answer, variable, parameter, source, lon, lat, raw, clean = FALSE, country_id = NA, border_clip = FALSE, buffer = NA){
  status <- answer[["status_code"]]
  
  if (status != 200){return(FALSE)}# If we obtain a status other than 200, abort and return FALSE
  
  # Parse actual data based on content type
  contentType <- httr::headers(answer)[["content-type"]]
  data <- switch(contentType,
                 
                 "text/csv" = {api_parse_csv(answer, variable, parameter, source, lon, lat, raw)},
                 
                 "text/csv; charset=utf-8" = {api_parse_csv(answer, variable, parameter, source,lon, lat, raw)},
                 
                 "application/json; charset=utf-8" = {api_parse_raster_geostore(answer, variable, raw, clean, country_id, border_clip, buffer)},
                 
                 "application/netcdf" = {api_parse_raster_temp(answer, variable, raw, clean, country_id, border_clip, buffer)},
                 
                 {NA}
                 
  )
  return(data)
}


create_api_url <- function(source, type, parameter, date, end = NULL, lon = NULL, lat = NULL, res = NULL, station_id = NULL, model = "model=mix", ...){
  parse_time <- function(date_string){
    lubridate::ymd(date_string) %>% paste("23:59:59Z", sep = "T")
  }
  
  left <- function(x){gsub("_.*", "", x)}
  right <- function(x){gsub(".*?_", "", x)}


  queries <- list(
    "2-time_series" = "http://192.168.111.206:42007/api/data/{date}_{end}/{parameter}/{lat},{lon}/csv",
    "2-map" = "http://192.168.111.206:42007/api/data/{date}/{parameter}?getInfo=true",
    "1-time_series" = "https://{Sys.getenv('mm_user')}:{Sys.getenv('mm_password')}@api.meteomatics.com/{parse_time(date)}--{parse_time(end)}:P1D/{parameter}/{lat},{lon}/csv?{model}",
    "1-map" = "https://{Sys.getenv('mm_user')}:{Sys.getenv('mm_password')}@api.meteomatics.com/{parse_time(date)}--{parse_time(date)}:P1D/{parameter}/{right(lat)},{left(lon)}_{left(lat)},{right(lon)}:{left(res)},{right(res)}/netcdf?{model}",
    "9-time_series" = "https://{Sys.getenv('mm_user')}:{Sys.getenv('mm_password')}@api.meteomatics.com/{parse_time(date)}--{parse_time(end)}:P1D/{parameter}/id_{station_id}/csv?source=mix-obs&on_invalid=fill_with_invalid",
    
    # wormhole to different resolutions and models
    "1hmix-time_series" = "https://{Sys.getenv('mm_user')}:{Sys.getenv('mm_password')}@api.meteomatics.com/{parse_time(date)}--{parse_time(end)}:PT1H/{gsub('24h', '1h', parameter)}/{lat},{lon}/csv?{model}",
    "9hmix-time_series" = "https://{Sys.getenv('mm_user')}:{Sys.getenv('mm_password')}@api.meteomatics.com/{parse_time(date)}--{parse_time(end)}:PT1H/{gsub('24h', '1h', parameter)}/id_{station_id}/csv?source=mix-obs&on_invalid=fill_with_invalid",
    "2hmix-time_series" = "https://{Sys.getenv('mm_user')}:{Sys.getenv('mm_password')}@api.meteomatics.com/{parse_time(date)}--{parse_time(end)}:PT1H/{gsub('24h', '1h', parameter)}/{lat},{lon}/csv?{model}"
    
  )
  
  queries[[paste(paste0(source, Sys.getenv("mm_res"),Sys.getenv("mm_mod")), type, sep = "-")]] %>%
    glue::glue()
}


GET_meteo_data1 <- function(source, model, type, parameter, date, variable, country_id = NA, end = NA, lat = NA, lon = NA, res = NA, station_id = NULL, raw = FALSE, clean = FALSE, border_clip = FALSE, buffer = NA, ...){
  
  a <- create_api_url(source = source, type = type, parameter = parameter, date = date, end = end, lon = lon, lat = lat, res = res, station_id = station_id, model = model)
  print(a)
  b <- httr::GET(a)
  d <- b %>% parse_response(variable, parameter, source, lon, lat, raw, clean, country_id, border_clip, buffer)
  
  
}




EXTRACT_data <- function(date, grid_info, model1 = "model=mix", model2 = "model=mch-radar", ...){
  
  r1 <- GET_meteo_data1(source = 1, model = model1, type = "map", parameter = grid_info$parameter, date = date, variable = grid_info$var_id, 
                        country_id = grid_info$country_id, end = date, lat = grid_info$lat, lon = grid_info$lon, res = grid_info$res, 
                        raw = FALSE, clean = FALSE, border_clip = TRUE, buffer = NA)
  r1 <- r1[["raster"]][[1]]
  
  r2 <- GET_meteo_data1(source = 1, model = model2, type = "map", parameter = grid_info$parameter, date = date, variable = grid_info$var_id, 
                        country_id = grid_info$country_id, end = date, lat = grid_info$lat, lon = grid_info$lon, res = grid_info$res, 
                        raw = FALSE, clean = FALSE, border_clip = TRUE, buffer = NA)
  
  r2 <- r2[["raster"]][[1]]
  
  
  df1 <- as.data.frame(r1, xy = TRUE)
  colnames(df1) <- c("lon", "lat", "value")
  df2 <- as.data.frame(r2, xy = TRUE)
  colnames(df2) <- c("lon", "lat", "value")
  
  return(list(df1 = df1, df2 = df2))
  
}


MKE_plot <- function(lat_line, date, df1, df2, row1, row2, top1, top2, model1, model2){
  

  # colors
  col_m1 <- "red"
  col_m2 <- "blue"
  lims <- range(c(df1$value, df1$value), na.rm = TRUE)
  
  
  
  p3 <- bind_rows(row1, row2) %>%
    ggplot(aes(lon, value, color = model)) +
    geom_line(linewidth = 0.6) +
    geom_vline(xintercept = top1$lon, color = col_m1, linewidth = 0.5) +
    geom_vline(xintercept = top2$lon, color = col_m2, linewidth = 0.5) +
    scale_color_manual(values = setNames(c(col_m1, col_m2), c(model1, model2)),guide  = "none") +
    theme_minimal() +
    theme(axis.title = element_blank()) +
    labs(title = paste("Cross-Section Lat", lat_line, "(Pix Values)"))
  
  
  p1 <- ggplot(df1, aes(x = lon, y = lat, fill = df1$value)) +
    geom_raster() +
    geom_hline(yintercept = lat_line, color = col_m1, linewidth = 1, linetype = "dashed") +
    geom_vline(xintercept = top1$lon, color = col_m1, linewidth = 0.5) +
    coord_equal() +
    scale_fill_viridis_c(limits = lims, guide = "none", na.value = NA) +
    labs(title = model1) +
    theme_minimal() +
    theme(
      axis.title = element_blank(),
      legend.position = "none",
      plot.title = element_text(color = col_m1)
    )
  
  p2 <- ggplot(df2, aes(x = lon, y = lat, fill = df2$value)) +
    geom_raster() +
    geom_hline(yintercept = lat_line, color = col_m2, linewidth = 1, linetype = "dashed") +
    geom_vline(xintercept = top2$lon, color = col_m2, linewidth = 0.5) +
    coord_equal() +
    scale_fill_viridis_c(limits = lims, guide = "none", na.value = NA) +
    labs(title = model2) +
    theme_minimal() +
    theme(
      axis.title = element_blank(),
      legend.position = "none",
      plot.title = element_text(color = col_m2)
    )
  
  p_combi <- p3 / (p1 + p2) +
    plot_annotation(title = paste0("Rain - ", date, " | Cliff at Lat", round(lat_line, 3))) &
    theme(plot.title = element_text(hjust = 0.5))
  
  if (nrow(top1) > 0 | nrow(top2) > 0){
    flag <- "drop_"
  } else {
    flag <- ""
  }
  ggsave(filename = here("01_database", "plots", paste0(flag, date, ".jpeg")), plot = p_combi, dpi = 300, width = 15, height = 13, units = "in")
  
  return(T)
  
}

score_drop <- function(row, k, w){
  
  row2 <- row %>%
    arrange(lon) %>%
    mutate(idx = row_number())
  
  good <- !is.na(row2$value)
  
  r <- rle(good)
  run_id <- which.max(ifelse(r$values, r$lengths, 0))
  start_idx <- sum(r$lengths[seq_len(run_id - 1)]) + 1
  end_idx   <- start_idx + r$lengths[run_id] - 1
  
  row_clean <- row2[start_idx:end_idx, ]
  n <- nrow(row_clean)
  
  row_clean %>%
    mutate(
      base_prev = zoo::rollapply(value, width = w, FUN = median,
                                 fill = NA, align = "right"),
      drop_here = base_prev - value,
      rel_drop  = if_else(!is.na(base_prev) & base_prev > 0,
                          drop_here / base_prev, NA_real_),
      idx2 = row_number()
    ) %>%
    filter(
      idx2 > w,
      idx2 <= n - w,
      is.finite(rel_drop)
    )
}


# expects: df has columns lon, lat, value
# score_drop() as you defined (returns df with rel_drop, value, lon, lat, ...)
scan_lat_cliffs <- function(df1, df2, model1, model2, lat_from = 46.6, lat_to = 47.4, lat_step = 0.1, k = 3, w = 10, min_val = 2, thr_rel = 0.33){
  
  lat_targets <- seq(lat_from, lat_to, by = lat_step)
  
  # helper: get best point and max score from a scored row
  best_from_row <- function(row_scored){
    if (nrow(row_scored) == 0) {
      return(list(top = tibble(), max_primary = NA_real_, max_fallback = NA_real_))
    }
    
    # primary = meets both criteria
    primary <- row_scored %>%
      filter(rel_drop > thr_rel, value > min_val) %>%
      arrange(desc(rel_drop)) %>%
      slice(1)
    
    max_primary <- if (nrow(primary) == 0) NA_real_ else primary$rel_drop[[1]]
    
    # fallback = sharpest regardless of thr_rel, but still require value > min_val
    fallback <- row_scored %>%
      filter(value > min_val) %>%
      arrange(desc(rel_drop)) %>%
      slice(1)
    
    max_fallback <- if (nrow(fallback) == 0) NA_real_ else fallback$rel_drop[[1]]
    
    top <- if (nrow(primary) > 0) primary else fallback
    
    list(top = top, max_primary = max_primary, max_fallback = max_fallback)
  }
  
  results <- lapply(lat_targets, function(lat_line){
    
    line_1 <- df1 %>% summarise(lat0 = lat[which.min(abs(lat - lat_line))]) %>% pull(lat0)
    line_2 <- df2 %>% summarise(lat0 = lat[which.min(abs(lat - lat_line))]) %>% pull(lat0)
    
    row1_raw <- df1 %>% filter(lat == line_1) %>% arrange(lon) %>% mutate(model = as.character(model1))
    row2_raw <- df2 %>% filter(lat == line_2) %>% arrange(lon) %>% mutate(model = as.character(model2))
    
    row1_scored <- if (nrow(row1_raw) > 0) score_drop(row1_raw, k = k, w = w) else row1_raw
    row2_scored <- if (nrow(row2_raw) > 0) score_drop(row2_raw, k = k, w = w) else row2_raw
    
    b1 <- best_from_row(row1_scored)
    b2 <- best_from_row(row2_scored)
    
    list(
      lat_target = lat_line,
      lat_used_1 = line_1,
      lat_used_2 = line_2,
      row1 = row1_scored,
      row2 = row2_scored,
      top1 = b1$top,
      top2 = b2$top,
      max1_primary = b1$max_primary,
      max1_fallback = b1$max_fallback,
      max2_primary = b2$max_primary,
      max2_fallback = b2$max_fallback
    )
  })
  
  # choose latitude by model1: prefer primary, else fallback
  scores <- bind_rows(lapply(results, function(x){
    tibble(
      lat_target = x$lat_target,
      lat_used_1 = x$lat_used_1,
      lat_used_2 = x$lat_used_2,
      max1_primary = x$max1_primary,
      max1_fallback = x$max1_fallback,
      max2_primary = x$max2_primary,
      max2_fallback = x$max2_fallback
    )
  }))
  
  any_primary <- any(is.finite(scores$max1_primary))
  pick_idx <- if (any_primary) {
    which.max(replace(scores$max1_primary, !is.finite(scores$max1_primary), -Inf))
  } else {
    which.max(replace(scores$max1_fallback, !is.finite(scores$max1_fallback), -Inf))
  }
  
  best <- results[[pick_idx]]
  
  list(
    best_lat_target = best$lat_target,
    best_lat_used_1 = best$lat_used_1,
    best_lat_used_2 = best$lat_used_2,
    row1 = best$row1,
    row2 = best$row2,
    top1 = best$top1,
    top2 = best$top2,
    scores = scores
  )
}


RUN_day_analysis <- function(date, grid_info, model1 = "model=mix", model2 = "model=mch-radar", lat_line = 46.8, k = 5, w = 20){


  dfs <- EXTRACT_data(date, grid_info, model1 = "model=mix", model2 = "model=mch-radar")

  # # saveRDS(dfs, here("temp.rds"))
  # dfs <- readRDS( here("temp.rds"))
  df1 <- dfs$df1
  df2 <- dfs$df2

  # model1 <- as.character(model1)
  # model2 <- as.character(model2)
  
  RES <- scan_lat_cliffs(df1, df2, model1, model2, lat_from = 46.6, lat_to = 47.3, lat_step = 0.1, k = 3, w = 10, min_val = 4, thr_rel = 0.33)

  MKE_plot(lat_line = RES$best_lat_used_1, date = date, 
           df1 = df1, df2 = df2, row1 = RES$row1, row2 = RES$row2, top1 = RES$top1, top2 = RES$top2, 
           model1 = model1, model2 = model2)
  
  
  
  return(RES$top1)
 

}


grid_info <- readRDS(here("01_database", "grid_info_ch.RDS"))
dates <- data.frame(date = seq.Date(as.Date("2025-01-01"), as.Date("2026-02-28")))
date <- "2026-02-16"
RES <- dates %>% 
  # filter(date %in% c("2026-02-16", "2025-10-06")) %>%
  slice_sample(n=10) %>%
  dplyr::mutate(purrr::pmap_dfr(., .f = RUN_day_analysis, grid_info = grid_info, model1 = "model=mix", model2 = "model=mch-radar", lat_line = 46.8, k = 5, w = 20))















# 
# 
# p <- RUN_day_analysis(date, grid_info, model1 = "model=mix", model2 = "model=mch-radar", lat_line = 46.8, k = 5, w = 20)
# p  
# 
# date <- "2026-02-16"
# model1 <- "model=mix"
# model2 <- "model=mch-radar"
# lat_line <- 46.9
# k <- 5
# w <- 20
# 
# r1 <- GET_meteo_data1(source = 1, model = model1, type = "map", parameter = grid_info$parameter, date = date, variable = grid_info$var_id, 
#                        country_id = grid_info$country_id, end = date, lat = grid_info$lat, lon = grid_info$lon, res = grid_info$res, 
#                        raw = FALSE, clean = FALSE, border_clip = TRUE, buffer = NA)
# r1 <- r1[["raster"]][[1]]
# 
# r2 <- GET_meteo_data1(source = 1, model = model2, type = "map", parameter = grid_info$parameter, date = date, variable = grid_info$var_id, 
#                           country_id = grid_info$country_id, end = date, lat = grid_info$lat, lon = grid_info$lon, res = grid_info$res, 
#                           raw = FALSE, clean = FALSE, border_clip = TRUE, buffer = NA)
# 
# r2 <- r2[["raster"]][[1]]
# 
# 
# 
# 
# 
# 
# 
# 
# library(dplyr)
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# min_val <- 2
# 
# line_1 <- df1 %>% summarise(lat0 = lat[which.min(abs(lat - lat_line))]) %>% pull(lat0)
# line_2 <- df2 %>% summarise(lat0 = lat[which.min(abs(lat - lat_line))]) %>% pull(lat0)
# 
# 
# row1 <- df1 %>%
#   filter(lat == line_1) %>%
#   mutate(model = model1) %>%
#   arrange(lon) %>%
#   score_drop(k =k, w=w)
# 
# row2 <- df2 %>%
#   filter(lat == line_2) %>%
#   mutate(model = model2) %>%
#   arrange(lon) %>%
#   score_drop(k =k, w=w)
# 
# 
# top1 <- row1 %>% arrange(desc(rel_drop)) %>% dplyr::filter(rel_drop > 0.33 & value > min_val) %>% slice(1) %>% dplyr::select(lon, value, rel_drop)
# top2 <- row2 %>% arrange(desc(rel_drop)) %>% dplyr::filter(rel_drop > 0.33 & value > min_val) %>% slice(1) %>% dplyr::select(lon, value, rel_drop)
# 





##-------------------------- TOP LINE -------------------------- 
# Run this to Sync data with Z backup
BACKUP_data()

# Run this to Deliver Result to Z output
DELIVER_output()
