##### Variables #####
setwd("C:/Users/newhartk/Desktop/LIFT_2019")
# setwd(file.path(Sys.getenv("USERPROFILE"),"Desktop", "LIFT_2019"))
# setwd(file.path(Sys.getenv("USERPROFILE"),"Dropbox", "Code", "WRRF-Analytics"))
historian.export.path <- paste0(getwd(),"/Raw data/")
historian.import.path <- paste0(getwd(),"/R output/")
predictor.variable <- "NIA33391_AB3_AMMONIA"
print(paste("Forecasting:", predictor.variable))
predictor.tag <- "NIA33391_AB3_PREDICTED_AMMONIA.F_CV"
print(paste("Forecasted Tag:", predictor.tag))
forecast.horizon <- 50 # In minutes
print(paste("Forecast Horizon:", forecast.horizon))
data.interval <- 5*60 # In minutes
training.window <- 5+forecast.horizon/60/60 # in days

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
sapply(c("readr",
         "xts",
         "glmnet",
         "taRifx",
         "doParallel",
         "foreach",
	   "tensorflow",
         "keras"), packageLoad)

##### Compile data #####
start <- Sys.time()-training.window*24*60*60
end <- Sys.time()
# Import old data
update.all.data <- FALSE
if("data_save_01.RData" %in% list.files(path=historian.import.path)) {
   load(file = paste0(historian.import.path,"data_save_01.RData"))
   if(start > index(all.data)[1]) { # If the start data has already been compiled, 
	all.data <- all.data[paste0(start,"/")] # Crop the already compiled data
	start <- tail(index(all.data),n=1) # start will be the end of the already compiled data
      update.all.data <-TRUE
   }
}

files.to.import <- list.files(path=historian.export.path, "[.]CSV", full.names = TRUE) # All "CSV"s in the folder
files.to.import <- c(files.to.import, list.files(path=historian.export.path, "[.]csv", full.names = TRUE)) # All "csv"s in the folder
if(length(files.to.import) > 0) sapply(files.to.import, file.remove)

# VBS CALL
pathofvbscript <- normalizePath("Raw data/Historian Data Export CSV - Copy.vbs")
# Pull in VBS code
vbs_lines <- readLines(con = pathofvbscript)
# Ammend first line with the number of days to compile
vbs_lines[1] <- paste("Const n =",as.numeric(difftime(Sys.time(),start,units="days")))
# Write back to VBS code
writeLines(text = vbs_lines,
           con = pathofvbscript)
print("Run Historian Export...")
# Run VBS code
# shell(shQuote(normalizePath("01 Historian Data Export CSV - Copy.bat")), wait=TRUE)
