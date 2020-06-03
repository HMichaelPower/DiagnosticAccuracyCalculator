###  a ShinyApp to calculate diagnostic accuracy measures
###  
# 
#if( !exists("LoadPackages", mode="function"))
source("FunctionsUsedByTestAccuracyApps.R")

LoadPackages()
enableBookmarking("url")
options(shiny.error = browser)
options(shiny.reactlog = TRUE) # True for debugging

