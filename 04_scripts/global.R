### PROJECT SETUP FILE
# Auto Generated: 2026-03-03 09:22:02.879908

# FILES
library(here)
library(jsonlite)
library(readr)
library(utils)

# DATA
library(magrittr)
library(dplyr)
library(purrr)
library(lubridate)
library(glue)
library(tidyr)
library(stats)
library(tidyselect)
library(stringr)
library(tibble)

# GEO
library(sf)
library(gstat)
library(raster)
library(terra)

# GRAPH
library(ggplot2)
library(RColorBrewer)
library(patchwork)
library(scales)
library(ggExtra)
library(ggnewscale)
library(colorspace)
library(viridisLite)

# TABLE
library(flextable)

# DATABASE
library(duckdb)
library(DBI)
library(RPostgreSQL)
library(RPostgres)
library(RnData)
library(zoo)


library(terra)
library(ggplot2)
library(patchwork)

### ENVIRONMENT MANAGEMENT
# Uncomment and run manually if needed

# Manage Env
# renv::status()
# renv::init()
# renv::restore()
# renv::snapshot()

# Install manually
# install.packages('remotes')
#remotes::install_git('http://gitlab11.hagel.local/rnd/rnd_packages/RnData.git')
# remotes::install_git('http://gitlab11.hagel.local/rnd/rnd_packages/RnData.git', upgrade = 'never')

# Delete from lockfile
# renv::record(list(RnData = NULL))

# Sys.setenv("mm_user" = "schweizer_hagel")
# Sys.setenv("mm_password" = "RVvI525YLI")
