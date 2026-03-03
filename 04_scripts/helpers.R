

### --------------------- STORAGE AND CACHING ----------------------

deliver_data <- function(local_dir, mirror_dir, ...) {
  if (dir.exists(mirror_dir)) {
    rel  <- list.files(local_dir, recursive = TRUE, full.names = FALSE, include.dirs = FALSE)
    from <- file.path(local_dir,  rel)
    to   <- file.path(mirror_dir, rel)
    
    if (length(rel) > 0) {
      ok <- logical(length(to))
      for (i in seq_along(to)) {
        d <- dirname(to[i])
        if (!dir.exists(d) && d != "" && d != ".") {
          dir.create(d, recursive = TRUE, showWarnings = FALSE)
        }
        ok[i] <- file.copy(from = from[i], to = to[i], overwrite = TRUE)
      }
      if (all(ok)) print("all files copied") else print("some files failed")
      return(ok)
      
    } else {
      print("No new Files")
      return(TRUE)
    }
  } else {
    print("mirror does not exist")
    return(FALSE)
  }
}




get_storage_mirror <- function(){
  dirs <- list.dirs(here(), full.names = FALSE, recursive = FALSE)
  script_dir <- dirs[grepl("script", dirs, ignore.case = TRUE)][1]
  storage_mirror <- readRDS(here(script_dir, "store_mirror.rds"))
  
  dirs <- list.dirs(here(), full.names = FALSE, recursive = FALSE)
  script_dir <- dirs[grepl("output", dirs, ignore.case = TRUE)][1]
  
  list(
    storage_mirror = storage_mirror,
    local_outdir = storage_mirror %>% slice(1) %>% pull(local_dir) %>% sub("([^/]+)$", script_dir, .),
    mirror_outdir = storage_mirror %>% slice(1) %>% pull(mirror_dir) %>% sub("([^/]+)$", script_dir, .)
  )
}


BACKUP_data <- function(doc_dir = here("06_docs")){
  # first delivery from db to docs
  res1 <- deliver_data(local_dir = file.path(Sys.getenv("output_path"), "delivery"), mirror_dir = doc_dir)
  dirs <- get_storage_mirror()
  res2 <- dirs$storage_mirror %>% mutate(res = purrr::pmap(., .f = deliver_data))
  return(res2)
}



DELIVER_output <- function(doc_dir = here("06_docs")){
  res1 <- deliver_data(local_dir = file.path(Sys.getenv("output_path"), "delivery"), mirror_dir = doc_dir)
  dirs <- get_storage_mirror()
  res <- deliver_data(local_dir = dirs$local_outdir, mirror_dir = dirs$mirror_outdir)
  return(res)
}

### --------------------- ... ----------------------



## --------------- DATA STORAGE ------------------


check_make_dir <- function(base, target, ...) {
  path <- do.call(file.path, as.list(c(base, target, list(...))))
  if (!dir.exists(path)) {
    dir.create(path, recursive = TRUE)
  }
  return(path)
}


split_store_dataset <- function(var_id, country, suffix = "X", base = here("DB", "data")){
  
  data <- readRDS(here("temp_data", paste0(var_id, "_", country,"_",suffix,".rds")))
  path <- check_make_dir(base, c(country, var_id, suffix))
  all_locs <- data$loc_id
  purrr::walk(all_locs, ~{data %>% dplyr::filter(loc_id %in% .x) %>% saveRDS(file.path(path, paste0(.x, ".rds")))})
  rm(data)
  gc()
  
  if (all(list.files(path) %in% paste0(all_locs, ".rds"))){
    print("SUCCESS")
    return(TRUE)
  } else {
    print("FAIL")
    return(FALSE)
  }
}


Extract_geostore_data <- function(){
  
  
  path <- here("DB", "data", "switzerland", "rain_day", "geostore")
  
  swissgrid <- RnData::GET_country_grid("CHE", level = "country")
  meta <- readRDS(here("DB", "meta_data", "rain_hour_switzerland_w_mb.rds")) %>%
    dplyr::select(loc_id, loc_name, lat, lon) %>%
    mutate(data_id = gsub("^rain_hour_mm", "rain_day_geostore", loc_id)) %>%
    RnData::CALC_closest_pixel( swissgrid)
  
  X <-GET_meteo_geotimegrid(
    GEO_grid = meta,
    variable = "rain",
    country = "CHE",
    date = "1993-01-01", end = "2024-12-31",
    return = "dataframe",
  )  %>% 
    tidyr::nest(model_data = c("date", "rain"))
  
  data <- meta %>% 
    select(loc_id, data_id, pix_id) %>% 
    left_join(X, by = join_by(pix_id))
  
  all_dtid <- data$data_id
  purrr::walk(all_dtid, ~{data %>% dplyr::filter(data_id %in% .x) %>% saveRDS(file.path(path, paste0(.x, ".rds")))})
  rm(data)
  gc()
  
  
  
}


## --------------- DATA IMPORTERS ------------------


GET_parameters <- function(){
  file <- file.path(here("DB", "meta_data","parameters_model_vars.json"))
  parameters <- fromJSON(file)
  return(parameters)
}


available_locs <- function(var_id, country, suffix, base = here("DB", "data")){
  list.files(file.path(base, country, var_id, suffix)) %>% gsub(".rds", "", .)
}

read_data_set <- function(var_id, country, suffix, loc_id, base = here("DB", "data")){
  path <- file.path(base, country, var_id, suffix, paste0(loc_id, ".rds"))
  purrr::map_dfr(path, ~readRDS(.x))
}


# opens the data of desired locs in collapsed form
open_collapsed_data <- function(var_id, country, suffix, loc_id = NA, slice = NA, base = here("DB", "data")){
  
  # if not loc provided, i take all
  if (all(is.na(loc_id))){
    loc_id <- available_locs(var_id, country, suffix, base)
  }
  
  # import data collapsed for all loc
  X <- read_data_set(var_id, country, suffix, loc_id, base)
  
  # if you want to slice...
  if (!is.na(slice)){
    X <- X %>% dplyr::slice(slice)
  }
  return(X)
  
}


# expand data and select station
unnest_station <- function(X, var_id, ...){
  X %>% 
    dplyr::select(loc_id, station_data) %>% tidyr::unnest(cols = c(station_data))  %>% 
    dplyr::select(loc_id, data) %>% tidyr::unnest(cols = c(data)) %>%
    rename(station = all_of(var_id))
}

# expand data and select model
unnest_model <- function(X, var_id, ...){
  X %>% 
    dplyr::select(loc_id, model_data) %>% tidyr::unnest(cols = c(model_data)) %>% 
    dplyr::select(loc_id, data) %>% tidyr::unnest(cols = c(data)) %>%
    tidyr::pivot_wider(names_from = "model", values_from = all_of(var_id))
}



filter_year <- function(X, years, ...){
  if (!all(is.na(years))){
    X %>% dplyr::filter(lubridate::year(timestamp) %in% !!years)
  } else {
    X
  }
}


# open only expanded station data
open_station_data <- function(var_id, country, suffix, loc_id = NA, slice = NA, years = NA, base = here("DB", "data")){
  
  open_collapsed_data(var_id, country, suffix, loc_id, slice, base) %>% 
    unnest_station(var_id) %>%
    filter_year(years)
}


# open only expanded model data
open_model_data <- function(var_id, country, suffix, loc_id = NA, slice = NA, years = NA, base = here("DB", "data")){
  
  open_collapsed_data(var_id, country, suffix, loc_id, slice, base) %>% 
    unnest_model(var_id) %>%
    filter_year(years)
}

# open all data expanded
open_data <- function(var_id, country, suffix, loc_id = NA, slice = NA, years = NA, base = here("DB", "data")){
  
  
  X <- open_collapsed_data(var_id, country, suffix, loc_id, slice, base)
  
  model_data <-  X %>% unnest_model(var_id) %>% filter_year(years)
  
  X %>% unnest_station(var_id) %>% 
    filter_year(years) %>% 
    dplyr::left_join(model_data, by = dplyr::join_by(loc_id, timestamp))
}


data_availability_table2 <- function(var_id, country, suffix ) {
  
  X <- open_data(var_id, country, suffix) %>%
    dplyr::mutate(year = as.character(lubridate::year(timestamp))) %>% 
    dplyr::mutate(across(where(is.numeric), ~ ifelse(. == -999, NA, .))) %>%
    dplyr::group_by(loc_id, year) %>%
    dplyr::summarise(across(where(is.numeric), ~ round(sum(!is.na(.))/8760, 2))) %>%
    dplyr::group_by(year) %>%
    dplyr::summarise(across(where(is.numeric), ~ round(sum(.), 0))) %>%
    dplyr::ungroup()
  
  return(X)
}



GET_error_metrics <- function(var_id, country, suffix, clean_zero = TRUE){
  
  X <- readRDS(here("DB", "analysis", paste0("raw_metrics_",var_id, "_", country, "_",suffix,".rds")))
  
  if (isTRUE(clean_zero)) {
    X <- X %>% dplyr::filter(sum_model > 0)
    
  }
  return(X)
  
}


## --------------- DATA PREPROCESS ------------------


interpolate_station_gaps <- function(X, maxgap = 2){
  X %>%
    dplyr::mutate(station = ifelse(station == -999, NA_real_, station))%>%
    dplyr::arrange(timestamp) %>%
    dplyr::mutate(station = zoo::na.approx(station, maxgap = !!maxgap, na.rm = FALSE))
}



data_complete <- function(X, maxgap = 2){
  
  Y <- X %>% 
    interpolate_station_gaps(maxgap = maxgap) %>%
    drop_na()
  
  z <- Y %>% dplyr::arrange(timestamp) %>% mutate(dis = as.numeric(difftime(timestamp, lag(timestamp),units =  "days"))) %>% pull(dis)
  
  if (!any(z > 1, na.rm = TRUE)){
    return(Y)
  } else {
    
    print(paste("Dataset", Y$loc_id[1], "discarded because it's incomplete"))
    return(NULL)
  }
  
}


add_time_meta <- function(X){
  X %>%
    dplyr::mutate(
      hour = lubridate::hour(timestamp),
      date = as.Date(timestamp),
      yday = lubridate::yday(timestamp),
      month = lubridate::month(timestamp),
      year = lubridate::year(timestamp)
    )
}

## --------------- DATA PREPROCESS ------------------


## --------------- PLOTTERS ------------------

## --------------- Evaluation plots ------------------

get_color_palette <- function(plot_cols){
  
  # make color palettes considering the limitd number of some palettes.
  colors <- brewer.pal(n = length(plot_cols), name = "Dark2")
  if (length(plot_cols) > length(colors)){
    colors <- c(colors, brewer.pal(n = length(plot_cols) - length(colors), name = "Accent"))
  }
  return(colors)
  
}

# timeseries plot of all models
plot_spaghetti <- function(X, loc_id = "any", year = "any"){
  
  plot_cols <- setdiff(colnames(X), c("timestamp", "loc_id", "event_id"))
  plot_cols <- plot_cols[!grepl("^label_", plot_cols)]
  
  colors <- get_color_palette(plot_cols)
  
  
  xt <- xts::xts(X[, plot_cols], order.by = X$timestamp)
  
  # # Create dygraph
  dy <- dygraph(xt, main = paste(loc_id, year, sep = " - ")) %>%
    dyRangeSelector() %>%
    dyOptions(useDataTimezone = TRUE, strokeWidth =0.8, colors = colors, stepPlot = TRUE) %>%
    dySeries("station", color = "red", strokeWidth = 2.5, strokePattern = "dashed") %>%
    #dySeries("station", color = colors["station"], strokeWidth = 1.6) %>%
    
    dyHighlight(
      highlightCircleSize = 4,
      highlightSeriesBackgroundAlpha = 0.3,
      highlightSeriesOpts = list(strokeWidth = 2.5),
      hideOnMouseOut = TRUE
    ) %>%
    dyLegend(width = 900)
  
  # Extract event start and end dates
  event_ranges <- X %>%
    filter(event_id > 0) %>%
    group_by(event_id) %>%
    summarise(from = min(timestamp), to = max(timestamp), .groups = "drop")
  
  # Add shading for each rain event
  for (i in 1:nrow(event_ranges)) {
    if (i %% 2 == 0){
      dy <- dy %>% dyShading(from = event_ranges$from[i], to = event_ranges$to[i], color = "#CCCCCC50")
    } else {
      dy <- dy %>% dyShading(from = event_ranges$from[i], to = event_ranges$to[i], color = "#0000FF14")
      
    }
  }
  
  return(dy)
}


# bar plot of tau by loc and year
plot_bar_tau <- function(result){
  
  plot_ly(
    data = result,
    x = ~factor(year),
    y = ~result,
    color = ~loc_id,
    opacity = 0.6,
    colors = "Dark2",
    type = 'bar',
    showlegend = FALSE
    
  ) %>%
    layout(
      barmode = "group",
      title = "Tau (Memory Decay) per Station per Year",
      xaxis = list(
        title = "Year",
        tickangle = -45
      ),
      yaxis = list(
        title = "Tau (h)"
      ),
      legend = list(
        title = list(text = ""),
        orientation = "h",
        x = 0.5,
        xanchor = "center",
        y = -0.2
      )
    )
  
}



## --------------- Analysis time agg plots ------------------


add_p_trace <- function(fig, probabilities, name, vis, style, h ){
  # density
  
  txt <- paste0(
    "Bias: ", round(probabilities$mean_x - 0, 2), "\n",
    "kurtosis: ", round(probabilities$kurtosis_x, 2), "\n"
  )
  
  fig %>%
    add_lines(
      x = probabilities$dens$x,
      y = probabilities$dens$y,
      name = paste(name),
      line = list(color = style$col, width = style$w,  dash = "dot"),
      opacity = style$op,
      visible = vis,
      hoverinfo = "x",
      showlegend = T
    ) %>%
    # mean line
    add_lines(
      x = c(probabilities$mean_x, probabilities$mean_x),
      y = c(0, probabilities$ymax),
      name = paste("Mean", name),
      hoverinfo = "name+x",
      line = list(color = style$col, dash = style$line, width = style$w),
      opacity = style$op,
      visible = vis,
      showlegend = F
    ) %>%
    add_trace(
      x = 1,
      y = h,
      type = "scatter",
      mode = "text",
      text = txt,
      textfont = list(color = style$col, size = 14),
      showlegend = FALSE,
      visible = vis
    )
  
}

time_agg_plot <- function(X, metric, view_limit = c(-3, 3)){
  
  library(plotly)
  library(RColorBrewer)
  
  # X <- event_metrics %>% filter(model_name %in% c("mb_nems"))
  by_event <- X %>% dplyr::select(any_of(c("model_name", metric))) %>% tidyr::drop_na() %>% mutate(model_name = paste0(model_name, "_1_event"))
  by_year <- X %>% SUMMARIZE_evaluation(c("model_name", "year", "loc_id")) %>% dplyr::select(any_of(c("model_name", metric))) %>% tidyr::drop_na()%>% mutate(model_name = paste0(model_name, "_2_year"))
  by_loc <- X %>% SUMMARIZE_evaluation(c("model_name", "loc_id")) %>% dplyr::select(any_of(c("model_name", metric))) %>%tidyr::drop_na()%>% mutate(model_name = paste0(model_name, "_3_loc"))
  
  Y <- rbind(by_event, by_year) %>% rbind(by_loc)
  
  models <- sort(unique(X$model_name))
  colors <- brewer.pal(n = 3, name = "Dark2")
  ideal <- get_metric_ideals(X, metric)
  
  general_probs <- error_dist_probabilities(Y[[metric]], ideal, view_limit = 0.999, P_avg = 0.9)
  
  fig <- plot_ly()
  
  for (i in seq_along(models)) {
    #i<-1
    # get data and orientations
    m <- models[i]
    p_event <- Y %>% filter(model_name == paste0(m, "_1_event")) %>% pull(metric) %>% error_dist_probabilities(ideal, view_limit = 0.999,  P_avg = 0.9)
    p_year <- Y %>% filter(model_name == paste0(m, "_2_year")) %>% pull(metric) %>% error_dist_probabilities(ideal, view_limit = 0.999,  P_avg = 0.9)
    p_loc <- Y %>% filter(model_name == paste0(m, "_3_loc")) %>% pull(metric) %>% error_dist_probabilities(ideal, view_limit = 0.999,  P_avg = 0.9)
    vis <- i == 1
    fig <- fig %>%
      add_ideal_range(general_probs$ymax, ideal, vis) %>%
      add_p_trace(p_event, "by Event", vis, style = list(col = colors[1], line = "solid", w = 2, op = 0.9), 1*general_probs$ymax) %>%
      add_p_trace(p_year, "by Year", vis, style = list(col = colors[2], line = "solid", w = 2, op = 0.65), 1.5*general_probs$ymax) %>%
      add_p_trace(p_loc, "by Location", vis, style = list(col = colors[3], line = "solid", w = 2, op = 0.4),2*general_probs$ymax)
  }
  
  n_traces_per_model <- 11
  # fig_built <- plotly_build(fig)
  # length(fig_built$x$data)
  # Dropdown buttons: combine restyle and relayout
  buttons <- lapply(seq_along(models), function(i) {
    vis <- rep(FALSE, length(models) * n_traces_per_model)
    vis[((i - 1) * n_traces_per_model + 1):(i * n_traces_per_model)] <- TRUE
    list(
      args = list(
        list(visible = vis),
        list(title = list(text = paste0("Time aggregation for ",metric, " | ", models[i])))
      ),
      label = models[i],
      method = "update"
    )
  })
  
  
  fig <- fig %>%
    layout(
      title = list(text = paste0("Time aggregation for ", metric, " | ", models[1])),
      barmode = "overlay",
      #xaxis = list(title = "Score", range = c(general_probs$xmin, general_probs$xmax)),
      xaxis = list(title = "Score", range = view_limit),
      
      yaxis = list(title = "Density"),
      updatemenus = list(list(
        buttons = buttons,
        direction = "down",
        x = 0.1,
        y = 1.1,
        showactive = TRUE
      ))
    )
  fig
  return(fig)
  
  
}


## --------------- compare dist plot Plot ------------------


create_metric_chart <- function(fig, X, metric, ideal, general_probs, vis = TRUE, ...) {
  
  models <- sort(unique(X$model_name))
  colors <- setNames(brewer.pal(n = length(models), name = "Dark2"), models)
  
  fig <- fig %>%
    add_ideal_range(general_probs$ymax, ideal, vis = vis)
  
  for (i in seq_along(models)) {
    m <- models[i]
    col <- colors[[m]]
    x <- X %>% filter(model_name == m) %>% pull(metric)
    
    probabilities <- error_dist_probabilities(x, ideal, view_limit = 0.99, P_avg = 0.9)
    
    # Add traces to fig
    fig <- fig %>%
      # add_ideal_range(probabilities$ymax, ideal)%>%
      add_lines(
        x = probabilities$dens$x,
        y = probabilities$dens$y,
        name =m,
        line = list(color = col, width = 2, dash = "solid"),
        opacity = 0.5,
        visible = vis,
        hoverinfo = "name+y",
        showlegend = T
      ) 
    
    
  }
  return(fig)
}

compare_density_plot <- function(X, models, metric_list, ...){
  
  
  X <- X %>%
    filter(model_name %in% models)%>%
    dplyr::select(any_of(c("model_name", metric_list))) %>% 
    tidyr::drop_na()
  
  
  # Add all metric traces with correct visibility
  fig <- plot_ly()
  gen_probs <- list()
  
  for (j in seq_along(metric_list)) {
    
    metric <- metric_list[j]
    ideal <- get_metric_ideals(X, metric)
    
    y <- error_dist_probabilities(X[[metric]], ideal = ideal, view_limit = 0.99, P_avg = 0.9)
    gen_probs <- c(gen_probs,setNames(list(y), metric))
    vis <- j == 1
    fig <- fig %>% create_metric_chart(X, metric, ideal = ideal, general_probs = y, vis = vis)
  }
  
  n_support_traces <- 2
  visibility_masks <- lapply(seq(metric_list), function(i) {
    vis <- rep(FALSE, length(metric_list) * (n_support_traces + length(unique(X$model_name))))
    vis[((i - 1) * (n_support_traces + length(unique(X$model_name))) + 1):(i * (n_support_traces + length(unique(X$model_name))))] <- TRUE
    vis
  })
  # Create dropdown buttons
  buttons <- lapply(seq_along(metric_list), function(i) {
    list(
      method = "update",
      label = metric_list[i],
      args = list(
        list(visible = visibility_masks[[i]]),
        list(
          title = paste("Error Distribution for:", metric_list[i]),
          xaxis = list(title = metric_list[i], range = c(gen_probs[[i]][["xmin"]], gen_probs[[i]][["xmax"]]))
        )
      )
    )
  })
  # Final layout
  fig <- fig %>%
    layout(
      title = list(text = paste("Error Distribution for:", metric_list[1])),
      barmode = "overlay",
      yaxis = list(title = "Density"),
      xaxis = list(title = metric_list[1], range = c(gen_probs[[1]][["xmin"]], gen_probs[[1]][["xmax"]])),
      updatemenus = list(list(
        buttons = buttons,
        direction = "down",
        x = 0.1,
        y = 1.1,
        showactive = TRUE
      ))
    )
  
  ret <- list(
    fig = fig,
    gen_probs = gen_probs
  )
  
  return(ret)
  
}

## --------------- Vector Plot ------------------


vector_plot <- function(X){
  
  # Build the line segments for vectors
  X <- X %>%
    rowwise() %>%
    do(data.frame(
      model_name = .$model_name,
      bias_dir = .$bias_dir,
      x = c(0, .$bias),
      y = c(0, .$precision),
      z = c(0, .$timing)
    ))
  
  # Plot
  p <- plot_ly() %>%
    add_trace(
      data = X,
      x = ~x, y = ~y, z = ~z,
      type = 'scatter3d',
      mode = 'lines+markers',
      split = ~model_name,
      color = ~model_name,
      colors = "Dark2",
      line = list(width = 3, opacity = 0.5),
      marker = list(size = 6, opacity = 0.5),
      showlegend = T 
    ) %>%
    layout(
      scene = list(
        xaxis = list(title = "Bias (Log)"),
        yaxis = list(title = "Precision (nRMSE)"),
        zaxis = list(title = "Timing (hours)"),
        camera = list(eye = list(x = 1.8, y = 1.8, z = 1.2))
      ),
      showlegend = T
      
    )
  return(p)
  
}


## --------------- Point Plots ------------------

mk_range <- function(x, nsd){
  
  c(
    round(max(mean(x, na.rm = T) - nsd * sd(x, na.rm = T), min(x, na.rm = T), na.rm = T),1),
    round(min(mean(x, na.rm = T) + nsd * sd(x, na.rm = T),max(x, na.rm = T), na.rm = T),1)
  )
}

point_plot <- function(X, nmax = 10000, nsd = 2){
  
  library(plotly)
  
  if (nrow(X) > nmax){
    X <- X %>% dplyr::slice_sample(n = nmax)
  }
  
  p <- plot_ly(
    data = X,
    x = ~bias,
    y = ~precision,
    z = ~timing,
    type = 'scatter3d',
    mode = 'markers',
    color = ~model_name,
    colors = "Dark2",
    marker = list(size = 2, opacity = 0.7),
    showlegend = T 
  ) %>%
    layout(
      title = paste("Axis show only ", nsd, "Standard Deviations"),
      scene = list(
        xaxis = list(title = "Bias (Log)", range = mk_range(X$bias, nsd)),
        yaxis = list(title = "Precision (nRMSE)", range = mk_range(X$precision, nsd)),
        zaxis = list(title = "Timing (hours)", range = mk_range(X$timing, nsd)),
        camera = list(eye = list(x = 1.8, y = 1.8, z = 1.2))
      ),
      showlegend = T
    )
  
  return(p)
}


selection_plot <- function(X, nmax = 50000, nsd = 2){
  
  library(plotly)
  X <- X %>% tidyr::drop_na()
  
  if (nrow(X) > nmax){
    X <- X %>% dplyr::slice_sample(n = nmax) 
  }
  
  models <- unique(X$model_name)
  colors <- setNames(brewer.pal(n = length(models), name = "Dark2"), models)
  
  # Add dropdown menu
  buttons <- lapply(seq_along(models), function(i) {
    vis <- rep(FALSE, length(models))
    vis[i] <- TRUE
    list(method = "restyle",
         args = list("visible", vis),
         label = models[i])
  })
  
  traces <- lapply(models, function(m) {
    Y <- X %>% filter(model_name == m)
    col <- colors[[m]]
    
    Y_red <- tibble(bias = 0, precision = 0, timing = 0, model_name = "origin")
    Y_mean <- tibble(bias = mean(Y$bias), precision = mean(Y$precision), timing = mean(Y$timing), model_name = "gravity")
    
    Y_combined <- bind_rows(Y, Y_red, Y_mean)
    
    sz <- max(ceiling(2000/nrow(Y)), 5)
    
    color_vector <- c(rep(col, nrow(Y)), "red", col)
    size_vector <- c(rep(1.5, nrow(Y)), 10, 20)
    opacity_vector <- c(rep(0.7, nrow(Y)), 1, 0.5)
    
    plot_ly(
      data = Y_combined,
      x = ~bias,
      y = ~precision,
      z = ~timing,
      type = 'scatter3d',
      mode = 'markers',
      marker = list(
        size = size_vector,
        opacity = opacity_vector,
        color = color_vector,
        line = list(width = 0)
      ),
      name = m,
      visible = ifelse(m == models[1], TRUE, FALSE)
    )
  })
  
  p <- subplot(traces, shareX = TRUE, shareY = TRUE) %>%
    layout(
      
      title = paste("Axis show only ", nsd, "Standard Deviations"),
      scene = list(
        xaxis = list(title = "Bias (Log)", range = mk_range(X$bias, nsd)),
        yaxis = list(title = "Precision (nRMSE)", range = mk_range(X$precision, nsd)),
        zaxis = list(title = "Timing (hours)", range = mk_range(X$timing, nsd)),
        camera = list(eye = list(x = 1.8, y = 1.8, z = 1.2))
      ),
      showlegend = FALSE,
      
      updatemenus = list(list(
        buttons = buttons,
        direction = "down",
        x = 0.1,
        y = 1.1,
        showactive = TRUE
      ))
    )
  
  
  return(p)
  
  
}






## --------------- TABLES ------------------

mke_DT <- function(X){
  
  X <- X %>%
    dplyr::mutate(dplyr::across(where(is.numeric), ~ round(.x, 3)))
  DT::datatable(
    X,
    options = list(
      paging = FALSE,
      scrollX = TRUE,
      scrollCollapse = TRUE,
      dom = 't',
      fixedColumns = TRUE
    ),
    rownames = FALSE,
    class = 'compact'
  )
}


## --------------- Dist Selection Plot ------------------


# defines the logic how the ideals are calculated and defined
get_metric_ideals <- function(X, metric, ...){
  
  ## metric is always the 90 percent
  metric_ideals <- list(
    
    # ideal: 1 (perfect match), reasonable tolerance [0.5x … 2x]
    sum_ratio = list(ideal_x = 1,
                     type = "center",
                     lwr = quantile(X$sum_ratio, probs = c(0.05), na.rm = TRUE)%>%as.numeric(),
                     upr = quantile(X$sum_ratio, probs = c(0.95), na.rm = TRUE)%>%as.numeric()
    ),
    
    # ideal: 0 (no bias), reasonable tolerance ±1
    bias = list(ideal_x = 0, 
                type = "center",
                lwr = quantile(X$bias, probs = c(0.05), na.rm = TRUE)%>%as.numeric(),
                upr = quantile(X$bias, probs = c(0.95), na.rm = TRUE)%>%as.numeric()
    ),
    log_bias = list(
      ideal_x = 0, 
      type = "center", 
      lwr = quantile(X$log_bias, probs = 0.05, na.rm = TRUE) %>% as.numeric(), 
      upr = quantile(X$log_bias, probs = 0.95, na.rm = TRUE) %>% as.numeric()
    ),
    # ideal: 0 (perfect), use quantile-based or 0–0.1 as tolerance
    nrmse = list(ideal_x = 0, 
                 type = "left",
                 lwr = 0, 
                 upr = quantile(X$nrmse, probs = c(0.90), na.rm = TRUE)%>%as.numeric()
    ),
    
    # same for MAE
    mae = list(ideal_x = 0,
               type = "left",
               lwr = 0,
               upr = quantile(X$mae, probs = c(0.90), na.rm = TRUE)%>%as.numeric()
    ),
    
    # lag ideally 0, tolerance ±5h
    centroid_lag = list(ideal_x = 0,
                        type = "center",
                        lwr = quantile(X$centroid_lag, probs = c(0.05), na.rm = TRUE)%>%as.numeric(),
                        upr = quantile(X$centroid_lag, probs = c(0.95), na.rm = TRUE)%>%as.numeric()
    ),
    
    # sup ideally 0, tolerance ±5h
    sup = list(ideal_x = 0,
               type = "left",
               lwr = 0,
               upr = quantile(X$sup, probs = c(0.90), na.rm = TRUE)%>%as.numeric()
    ),
    
    # emd is a distance, ideal 0, tolerance [0 … 0.15]
    emd = list(ideal_x = 0,
               type = "left",
               lwr = 0,
               upr = quantile(X$emd, probs = c(0.90), na.rm = TRUE)%>%as.numeric()
    )
    
  )
  
  
  return(metric_ideals[[metric]])
  
}


add_ideal_range <- function(fig, ymax, ideal, vis = TRUE){
  ## ideal range
  fig %>%
    add_lines(
      x = c(ideal$ideal_x, ideal$ideal_x),
      y = c(0, ymax),
      name = "Ideal",
      hoverinfo = "name",
      hoverinfo = "skip",
      line = list(color = "black", dash = "solid", width = 2),
      visible = vis,
      showlegend = F
    ) %>%
    ## tolerance range
    add_trace(
      x = c(ideal$lwr, ideal$upr, ideal$upr, ideal$lwr),
      y = c(0, 0, ymax, ymax),
      type = "scatter",
      mode = "none",
      fill = "toself",
      fillcolor = "rgba(150,150,150,0.2)",
      line = list(width = 0),
      hoverinfo = "skip",
      name = "tolerance",
      showlegend = FALSE,
      visible = vis
    )
  
  
  
}


dens_selection_plot <- function(X, metric,  nmax = 5000000, view_limit = 0.99){
  
  #metric <- "centroid_lag"
  library(plotly)
  library(RColorBrewer)
  metric_list <- c("model_name","sum_ratio", "bias","log_bias", "nrmse", "mae", "centroid_lag", "sup", "emd")
  # browser()
  X <- X %>% dplyr::select(any_of(metric_list))  %>%tidyr::drop_na()
  if (nrow(X) > nmax){
    X <- X %>% dplyr::slice_sample(n = nmax)
  }
  
  
  models <- sort(unique(X$model_name))
  colors <- setNames(brewer.pal(n = length(models), name = "Dark2"), models)
  ideal <- get_metric_ideals(X, metric)
  
  general_probs <- error_dist_probabilities(X[[metric]], ideal, view_limit = view_limit, P_avg = 0.9)
  
  
  fig <- plot_ly()
  n_traces_per_model <- 8  # note need for sleection number of traces ow screws
  
  for (i in seq_along(models)) {
    
    # get data and orientations
    m <- models[i]
    col <- colors[[m]]
    x <- X %>% filter(model_name == m) %>% pull(metric)
    vis <- i == 1
    probabilities <- error_dist_probabilities(x, ideal, view_limit = view_limit,  P_avg = 0.9)
    
    t2 <- ifelse(probabilities$P_relative > 0,
                 paste0(round(abs(probabilities$P_relative), 1),"% better than average" ),
                 paste0(round(abs(probabilities$P_relative), 1),"% worse than average" ))
    
    txt <- paste0(
      "ideal: ", ideal$ideal_x, "\n",
      "mean: ", round(probabilities$mean_x, 2), "\n",
      "Kurtosis: ", round(probabilities$kurtosis_x, 2), "\n",
      "Within tolerance: ", round(probabilities$P_model, 1), "%\n",
      t2
    )
    
    fig <- fig %>%
      add_ideal_range(probabilities$ymax, ideal, vis) %>%
      ## tolerance text
      add_trace(
        x = ideal$upr*1.5,
        y = probabilities$ymax * 0.9,
        type = "scatter",
        mode = "text",
        text = txt,
        textfont = list(color = "gray20", size = 14),
        showlegend = FALSE,
        visible = vis
      ) %>%
      
      # historgram
      add_histogram(
        x = x,
        histnorm = "probability density",
        name = paste(m, "- Histogram"),
        marker = list(color = col),
        opacity = 0.3,
        nbinsx = 800,
        hoverinfo = "skip",
        visible = vis,
        showlegend = F
      ) %>%
      # density
      add_lines(
        x = probabilities$dens$x,
        y = probabilities$dens$y,
        name = paste("Density"),
        line = list(color = col, width = 2, dash = "dot"),
        visible = vis,
        hoverinfo = "x",
        showlegend = F
      ) %>%
      # mean line
      add_lines(
        x = c(probabilities$mean_x, probabilities$mean_x),
        y = c(0, probabilities$ymax),
        name = "Mean",
        hoverinfo = "name+x",
        line = list(color = col, dash = "solid", width = 3),
        visible = vis,
        showlegend = F
      ) %>%
      # mean line
      add_lines(
        x = c(probabilities$q_lo, probabilities$q_lo),
        y = c(0, probabilities$ymax),
        name = "Lower-90%Mass",
        hoverinfo = "name+x",
        line = list(color = col, dash = "dot", width = 1),
        visible = vis,
        showlegend = F
      ) %>%
      # mean line
      add_lines(
        x = c(probabilities$q_hi, probabilities$q_hi),
        y = c(0, probabilities$ymax),
        name = "Upper-90%Mass",
        hoverinfo = "name+x",
        line = list(color = col, dash = "dot", width = 1),
        visible = vis,
        showlegend = F
      )
    
  }
  
  # Dropdown buttons: combine restyle and relayout
  buttons <- lapply(seq_along(models), function(i) {
    vis <- rep(FALSE, length(models) * n_traces_per_model)
    vis[((i - 1) * n_traces_per_model + 1):(i * n_traces_per_model)] <- TRUE
    list(
      args = list(
        list(visible = vis)
        # list(title = list(text = paste(metric, "for", models[1])))
      ),
      label = models[i],
      method = "update"
    )
  })
  
  fig <- fig %>%
    layout(
      title = list(text = paste("Error Distribution for Metric:", metric)),
      barmode = "overlay",
      xaxis = list(title = "Score", range = c(general_probs$xmin, general_probs$xmax)),
      yaxis = list(title = "Density"),
      updatemenus = list(list(
        buttons = buttons,
        direction = "down",
        x = 0.1,
        y = 1.1,
        showactive = TRUE
      ))
    )
  
  return(fig)
  
  
}
# 
# ### ------------- aggregation mc plots -----------------
plot_converge <- function(X, var_y, var_color, y_min = NA, y_max  = NA, x_max = NA, ...){
  
  
  if (any(is.na(c(y_min, y_max)))){
    fig <- plot_ly()
  } else {
    browser()
    x_min <- 0
    x_max <- ifelse(!is.na(x_max), x_max, max(X$n_event_agg))
    
    fig <- plot_ly()
    fig <- fig %>%
      add_trace(
        x = c(x_min, x_max, x_max, x_min),
        y = c(y_min, y_min, y_max, y_max),
        type = "scatter",
        mode = "none",
        fill = "toself",
        fillcolor = "rgba(150,150,150,0.2)",  # transparent gray
        line = list(width = 0),
        hoverinfo = "skip",
        name = "Toleranzbereich",
        showlegend = FALSE
      )
    
  }
  fig <- fig %>%
    add_trace(
      data = X,
      y = ~get(var_y),
      color = ~get(var_color),
      colors = "Dark2",
      type = 'scatter',
      mode = 'lines+markers',
      showlegend = F
    ) %>%
    layout(
      title = paste("Effect of aggretation on", var_y),
      xaxis = list(title = "Aggregierte Events"),
      yaxis = list(title = var_y),
      legend = list(title = list(text = "Modell")),
      hovermode = "x unified"
    )
  fig
  return(fig)
  
}


plot_converge_days <- function(X, var_y, var_color, y_min = NA, y_max = NA, x_max = NA) {
  
  # ggf. X auf x_max beschränken (optional, aber oft sinnvoll)
  if (!is.na(x_max)) {
    X <- dplyr::filter(X, n_event_agg <= x_max)
  }
  x_min <- 0
  if (is.na(x_max)) x_max <- max(X$days_mean, na.rm = TRUE)
  
  fig <- plotly::plot_ly()
  
  # Toleranzbereich nur wenn y_min/y_max gesetzt
  if (!any(is.na(c(y_min, y_max)))) {
    fig <- fig %>%
      plotly::add_trace(
        x = c(x_min, x_max, x_max, x_min),
        y = c(y_min, y_min, y_max, y_max),
        type = "scatter",
        mode = "none",
        fill = "toself",
        fillcolor = "rgba(150,150,150,0.2)",
        line = list(width = 0),
        hoverinfo = "skip",
        name = "Toleranzbereich",
        showlegend = FALSE
      )
  }
  fig <- fig %>%
    plotly::add_trace(
      data = X,
      x = ~days_mean,
      y = ~get(var_y),
      color = ~get(var_color), 
      colors = "Dark2",
      type = "scatter",
      mode = "lines+markers",
      showlegend = FALSE
    ) %>%
    plotly::layout(
      title = paste("Effect of aggregation on", var_y),
      xaxis = list(title = "Days (mean)", range = c(x_min, x_max)),
      yaxis = list(title = var_y),
      hovermode = "x unified"
    )
  
  fig
}


test_validate_plot <- function(X, model_name, metric, ...){
  test_data <- X %>%
    filter(model_name == !!model_name & set == "test") #%>% slice_sample(n=100000)
  val_data <- X %>%
    filter(model_name == !!model_name & set == "validate") #%>%slice_sample(n=100000)
  
  
  # Calculate statistics
  mean_test <- mean(test_data[[metric]], na.rm = TRUE)
  mean_val  <- mean(val_data[[metric]], na.rm = TRUE)
  sd_test   <- sd(test_data[[metric]], na.rm = TRUE)
  sd_val    <- sd(val_data[[metric]], na.rm = TRUE)
  kurt_test <- excess_kurtosis(test_data[[metric]])
  kurt_val  <- excess_kurtosis(val_data[[metric]])
  
  # sdall <- mean(c(sd_val, sd_test))
  # meanall <- mean(c(mean_val, mean_test))
  
  comparison_text <- paste0(
    "mean = ", sprintf("+(%.2f", mean_test - mean_val),")\n",
    #" (validate = ", sprintf("%.2f", mean_val), ", test = ", sprintf("%.2f", mean_test), ")\n",
    "sd = ", sprintf("+(%.2f", sd_test - sd_val),")\n",
    "kurtosis = ", sprintf("+(%.2f", kurt_test - kurt_val),")"
  )
  # 
  x_test <- test_data[[metric]]
  # x_test <- pmax(pmin(x_test, (meanall + 3 * sdall)), (meanall - 3 * sdall))
  # 
  x_val <- val_data[[metric]]
  # x_val <- pmax(pmin(x_val, (meanall + 3 * sdall)), (meanall - 3 * sdall))
  # 
  # 
  p <- plot_ly() %>%
    
    add_histogram(
      x = x_val,
      histnorm = "probability density",
      name = "Validate",
      opacity = 0.3,
      nbinsx = 300,
      hoverinfo = "skip",
      marker = list(color = "darkblue"),
      showlegend = T
    ) %>%
    
    add_histogram(
      x = x_test,
      histnorm = "probability density",
      name = "Test",
      opacity = 0.3,
      nbinsx = 500,
      hoverinfo = "skip",
      marker = list(color = "darkred"),
      showlegend = T
    ) %>%
    
    # Vertical line for validate mean (blue)
    # add_trace(
    #   x = c(mean_val, mean_val),
    #   y = c(0, 1),
    #   type = "scatter",
    #   mode = "lines",
    #   name = "Validate Mean",
    #   line = list(color = "darkblue", dash = "dot", width = 2),
    #   showlegend = TRUE
    # ) %>%
    # add_trace(
    #   x = c(mean_test, mean_test),
    #   y = c(0, 1),  # dummy y-range, will auto-rescale
    #   type = "scatter",
    #   mode = "lines",
    #   name = "Test Mean",
    #   line = list(color = "darkred", dash = "dot", width = 2),
    #   showlegend = TRUE
    # ) %>%
    layout(
      title = paste0("Generalization: ",model_name," – ", metric),
      xaxis = list(title = paste(metric, "(cutt off at 5SD)")),
      yaxis = list(title = "p"),
      barmode = "overlay",
      shapes = list(
        list(
          type = "line",
          x0 = mean_val,
          x1 = mean_val,
          y0 = 0,
          y1 = 1,
          xref = "x",
          yref = "paper",
          line = list(color = "darkblue", dash = "dot", width = 2)
        ),
        list(
          type = "line",
          x0 = mean_test,
          x1 = mean_test,
          y0 = 0,
          y1 = 1,
          xref = "x",
          yref = "paper",
          line = list(color = "darkred", dash = "dot", width = 2)
        )
      ),
      annotations = list(
        list(
          x = 0.01,
          y = 0.99,
          xref = "paper",
          yref = "paper",
          showarrow = FALSE,
          align = "left",
          text = paste("<b>Change Distribution:</b><br>", gsub("\n", "<br>", comparison_text)),
          font = list(size = 11),
          xanchor = "left",
          yanchor = "top",
          bgcolor = "rgba(255,255,255,0.8)",
          bordercolor = "gray",
          borderwidth = 1
        )
      ),
      legend = list(
        orientation = "v",
        x = 0.8,
        y = 0.99,
        xanchor = "left",
        bgcolor = "rgba(255,255,255,0.7)",
        bordercolor = "gray",
        borderwidth = 1
      )
    )
  return(p)
}

