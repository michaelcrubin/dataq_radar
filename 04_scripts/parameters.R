## How it works
# This is a main script, which executes the final step of the tarif calculations.
# It depends on previous workflows already being completed.

## --------------- LIBRARIES ------------------
library(here)
library(jsonlite)

## ---------------CREATE LIST ------------------

parameters <- list(
  
  
  rain_hour = list(
    
    station = list(parameter = "precip_1h:mm",  timeres = "PT1H"),
    
    
    mm = list(
      models = list(
        mm_mix        = "model=mix",
      #  mm_mixcal     = "model=mix&calibrated=true",
        mm_era5       = "model=ecmwf-era5",
     #   mm_chirps     = "model=chc-chirps2",
     #   mm_icondach   = "model=dwd-icon-d2",
        mm_iconeu     = "model=dwd-icon-eu",
        mm_swiss1k_hc = "model=mm-swiss1k-hindcast",
        mm_swiss1k    = "model=mm-swiss1k"
      ),
      parameter = "precip_1h:mm",
      timeres = "PT1H"
    ),
    mb = list(
      models = list(
        mb_iconeu = "ICONEU",
        mb_nmm    = "NMM4",
        mb_nems   = "NEMS4"
      ),
      parameter = 61,
      timeres = "hourly"
    )
  ),
  

  rain_day = list(
    
    station = list(parameter = "precip_24h:mm",  timeres = "P1D"),
    
    
    mm = list(
      models = list(
        mm_mix        = "model=mix",
        #  mm_mixcal     = "model=mix&calibrated=true",
        mm_era5       = "model=ecmwf-era5",
        #   mm_chirps     = "model=chc-chirps2",
        #   mm_icondach   = "model=dwd-icon-d2",
        mm_iconeu     = "model=dwd-icon-eu",
        mm_swiss1k_hc = "model=mm-swiss1k-hindcast",
        mm_swiss1k    = "model=mm-swiss1k"
      ),
      parameter = "precip_24h:mm",
      timeres = "P1D"
    ),
    mb = list(
      models = list(
        mb_iconeu = "ICONEU",
        mb_nmm    = "NMM4",
        mb_nems   = "NEMS4"
      ),
      parameter = 61,
      timeres = "daily"
    )
  ),
  
  
  tmean_day = list(
    
    station = list(parameter = "t_mean_2m_1h:C", timeres = "P1D"),
    
    
    mm = list(
      models = list(
        mm_mix        = "model=mix",
        mm_mixcal     = "model=mix&calibrated=true",
        mm_era5       = "model=ecmwf-era5",
        mm_chirps     = "model=chc-chirps2",
        mm_icondach   = "model=dwd-icon-d2",
        mm_iconeu     = "model=dwd-icon-eu",
        mm_swiss1k_hc = "model=mm-swiss1k-hindcast",
        mm_swiss1k    = "model=mm-swiss1k"
      ),
      parameter = "t_mean_2m_1h:C",
      timeres = "P1D"
    ),
    mb = list(
      models = list(),  # not yet defined
      parameter = 11,
      timeres = "daily"
    )
  ),
  
  rain_trend = list(
    
    station = list(parameter = "precip_24h:mm",  timeres = "P1D"),
    
    mm = list(
      models = list(
        mm_mix        = "model=mix",
        mm_mixcal     = "model=mix&calibrated=true"
      ),
      parameter = "precip_24h:mm",
      timeres = "P1D"
    )
  )
  
)


## ---------------STORE LIST ------------------

#json_data <- jsonlite::toJSON(parameters, pretty = TRUE)
jsonlite::write_json(parameters, here("DB", "meta_data","parameters_model_vars.json"))














