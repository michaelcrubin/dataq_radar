

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