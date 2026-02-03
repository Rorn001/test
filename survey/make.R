rm(list = ls())
source("SetGlobals.R")

#Change to the current working directory
setwd(paste0(ROOT, '/survey'))

# Run all scripts
source('code/survey_monitor.r')

# Change to the root
setwd(ROOT)
