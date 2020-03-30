historian.export.path <- paste0(getwd(),"/Raw data/")
historian.import.path <- paste0(getwd(),"/R output/")
predictor.variable <- "NIA33391_AB3_AMMONIA"
predictor.tag <- "NIA33391_AB3_PREDICTED_AMMONIA.F_CV"
forecast.horizon <- 50 # In minutes
data.interval <- 5*60 # In minutes
training.window <- 5+forecast.horizon/60/60 # in days