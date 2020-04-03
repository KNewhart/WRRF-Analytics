setwd("..")
source("R output/Ammonia Forecast v2 - 0.R") # Load variables
print(paste("Forecasting:", predictor.variable))
print(paste("Forecasted Tag:", predictor.tag))
print(paste("Forecast Horizon:", forecast.horizon))


##### Load libraries #####
# Function to install and load libraries
packageLoad <- function(packName){ #packName - package name as a character string, e.g. "quantmod"
  if(!require(packName,character.only = TRUE)){ #If the package is not available, install it
    tryCatch(install.packages(packName,dependencies=TRUE,repos="http://cran.r-project.org")
             , error=print(paste(packName,"could not be installed"))
             , finally = print(paste(packName,"installed"))
    )
  } else {
    print(paste(packName,"installed"))
  }
  library(packName, character.only = TRUE) # load package
  if(length(which(search() == paste0("package:", packName))) > 0) print(paste(packName,"loaded"))
}

# Load required libraries
# TO DO

# Load forecast
all.model.fit <- read.csv(file=paste0(historian.import.path,"ModelResults.csv"))
all.model.fit <- taRifx::remove.factors(all.model.fit)
# true.ammonia <- read.csv(file=paste0(historian.export.path,"WWSCADA2.NIA33391_AB3_AMMONIA.F_CV.CSV"))

x <- as.POSIXct(all.model.fit[1,1])
which(as.POSIXct(all.model.fit[,1])<=(x+all.model.fit[1,5]*60))
