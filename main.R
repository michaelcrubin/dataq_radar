### MAIN PROJECT FILE // WORK HERE
# Auto Generated: 2026-03-03 09:22:02.891928
# Start working here...

##-------------------------- IMPORT DEPENDENCIES -------------------------- 
# ....

library(here)
source(here('04_scripts', 'global.R'))
source(here('04_scripts', 'helpers.R'))
##-------------------------- FUNCTIONS -------------------------- 


##-------------------------- TOP LINE -------------------------- 
# Run this to Sync data with Z backup
BACKUP_data()

# Run this to Deliver Result to Z output
DELIVER_output()
