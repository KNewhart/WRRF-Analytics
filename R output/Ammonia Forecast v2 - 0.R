historian.export.path <- paste0(getwd(),"/Raw data/")
historian.import.path <- paste0(getwd(),"/R output/")
predictor.variable <- "NIA33391_AB3_AMMONIA"
predictor.tag <- "NIA33391_AB3_PREDICTED_AMMONIA.F_CV"
forecast.horizon <- 50 # In minutes
data.interval <- 60 # In seconds
training.window <- 3+(forecast.horizon+5)/60/60 # in days, additional 5 min added for compiling time