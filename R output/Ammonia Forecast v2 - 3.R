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
sapply(c("taRifx", "zoo"), packageLoad)

# Load forecast, persistence, and actual future values
all.model.fit <- read.csv(file=paste0(historian.import.path,"ModelResults.csv"))
all.model.fit <- remove.factors(all.model.fit)
all.model.fit[,1] <- as.POSIXct(all.model.fit[,1])
true.ammonia <- read.csv(file=paste0(historian.import.path,"WWSCADA2.NIA33391_AB3_AMMONIA.F_CV.CSV"))
true.ammonia <- remove.factors(true.ammonia)
true.ammonia[,2] <- as.POSIXct(true.ammonia[,2], format="%m/%d/%Y %H:%M:%S")
future.index <- apply(all.model.fit,1,function(x) (which(true.ammonia[,2]>(as.POSIXct(x[1])+as.numeric(x[5])*60))[1]-1))
all.model.fit <- cbind(all.model.fit, "Future"=true.ammonia[future.index,3])
all.model.fit <- all.model.fit[(!apply(all.model.fit,1,anyNA)),]
all.model.fit <- all.model.fit[which(round(all.model.fit$Training.Window)==2),]

se.persistence <- apply(all.model.fit,1,function(x) (as.numeric(x[4])-as.numeric(x[7]))^2)
se.actual <- apply(all.model.fit,1,function(x) (as.numeric(x[3])-as.numeric(x[7]))^2)
rmse.persistence <- mean(se.persistence)^0.5
rmse.actual <- mean(se.actual)^0.5
rmse.persistence
rmse.actual

par(mar=c(2.5,4,1,1))
plot(zoo(x=all.model.fit[,4], order.by=all.model.fit[,1]), xlab="", ylab="Zone 7 Ammonia (mg/L)")
lines(zoo(x=all.model.fit[,3], order.by=all.model.fit[,1]), col="red")
lines(zoo(x=all.model.fit[,7], order.by=all.model.fit[,1]), col="blue")
legend("topright", legend=c("Persistence", "Forecast", "Future"), col=c("black", "red", "blue"), lwd=1)

