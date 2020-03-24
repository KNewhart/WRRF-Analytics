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
         "keras"), packageLoad)

##### Compile data #####
# files <- list.files(historian.export.path, pattern="[.]CSV", full.names = TRUE)
# data <- read.csv(files[1], stringsAsFactors=FALSE)
# date.range <- c(data[1,2], data[nrow(data),2])
#write.csv(date.range, file=file.path(historian.export.path, "file_date_range.csv"),
#		row.names = FALSE,
#            col.names = FALSE)

# Import CSVs
print("Importing files...")
files.to.import <- list.files(path=historian.export.path, "[.]CSV") # All "CSV"s in the folder
files.to.import <- c(files.to.import, list.files(path=historian.export.path, "[.]csv")) # All "csv"s in the folder
if(length(files.to.import) == 0) print("No files to import. Check working directory.")

# Import all process data
obj.list <- lapply(files.to.import, function(file) {
  imported.file <- read_csv(paste0(historian.export.path,"/",file), 
                            col_types = cols(TimeStamp = col_datetime(format = "%m/%d/%Y %H:%M:%S")))
  time <- as.POSIXlt(data.frame(imported.file)[,2])
  attr(time, "tzone") <- Sys.timezone()
  time.na <- which(!is.na(time))
  obj.data <- as.numeric(unlist(imported.file[time.na,3]))
  obj.data <- xts(obj.data, order.by=time[time.na])
  colnames(obj.data) <- file
  return(obj.data)
})

# Merge all process data
print("Merging files...")
all.data <- do.call("cbind", obj.list)
colnames(all.data) <- sapply(colnames(all.data), function(x) strsplit(x, "[.]")[[1]][2])

# Save compiled process data
# save(all.data, file = paste0(historian.import.path,"data_save_01.RData"))
write.csv(all.data, file=paste0(historian.import.path,"data_save_01.csv"))

# load(file = paste0(historian.import.path,"data_save_01.RData"))

##### FOR TESTING  #####
# all.data <- all.data[paste0("/",last(index(all.data))-5*60)] # First round of testing
# all.data <- all.data[paste0(index(all.data)[1]+5*60,"/")]

# Timestamps to compile
# if("data_save_02.RData" %in% list.files(path=historian.import.path)) {
#   # Import five.min.data
#   load(paste0(historian.import.path,"data_save_02.RData"))
#   # Pull timestamps
#   five.min.timestamps <- seq(as.numeric(index(five.min.data)[1]), as.numeric(last(index(all.data))), data.interval)
#   # five.min.timestamps <- round(as.POSIXct(five.min.timestamps, origin="1970-01-01"), "mins")
#   five.min.timestamps <- five.min.timestamps[which(five.min.timestamps > as.numeric(index(all.data)[1]))]
# } else {
five.min.timestamps <- seq(as.numeric(index(all.data)[1]), as.numeric(last(index(all.data))), data.interval)
  # five.min.timestamps <- round(as.POSIXct(five.min.timestamps, origin="1970-01-01"), "mins")
# }
print("Finding timestamps...")
index.all.data <- as.numeric(index(all.data))
row.index <- lapply(five.min.timestamps, function(start) {
  intersect(which(index.all.data >= start), 
            which(index.all.data < (start+data.interval)))
})

print("Averaging to timestamps...")
mean.data <- do.call("rbind", lapply(row.index, function(r) {colMeans(all.data[r,], na.rm=TRUE)}))
mean.data <- na.locf(mean.data)
mean.data <- na.locf(mean.data, fromLast=TRUE)
mean.data <- xts(mean.data, order.by=as.POSIXct(five.min.timestamps, origin="1970-01-01"))

# Add sine/cosine pairs
# Convert timestamps to runtime and project onto a unit circle 
print("Setting up training data...")
time.stamps <- difftime(index(mean.data), index(mean.data)[1], units = "mins")
time.stamps <- as.numeric(time.stamps) %% 1440 # Cycles of 1 day are constructed (1440 min/day)
time.stamps <- (time.stamps*360/1440)*pi/180 # Cycles of minutes are converted to radians
diurnal.x <- do.call("cbind", lapply(1:6, function(n) {
  eval(parse(text=paste0("sin(",n,"*time.stamps)","+cos(",n,"*time.stamps)", collapse="+")))
}))
mean.data <- cbind(mean.data, diurnal.x)
write.csv(mean.data, file=paste0(historian.import.path,"data_save_02.csv"))

rows <- which(round(difftime(index(mean.data), index(mean.data)[1], units = "mins"),0) == forecast.horizon)
rows <- rows[1]
predictor.col <- which(colnames(mean.data)==predictor.variable)
if(length(predictor.col) == 0) {
  print(paste("Predictor",predictor.variable, "not found"))
  # quit(save="no")
}
training.data <- cbind(mean.data[1:(nrow(mean.data)-rows+1),], as.numeric(mean.data[rows:nrow(mean.data),predictor.variable]))
colnames(training.data)[ncol(training.data)] <- paste0(predictor.variable,"_FUTURE")
testing.data <- mean.data[nrow(mean.data),]

train.mean <- apply(training.data,2,mean)
train.sd <- apply(training.data,2,sd)
forecast.col <- which(colnames(training.data) == paste0(predictor.variable,"_FUTURE"))
train.x <- scale(training.data, center=train.mean, scale=train.sd)[,-forecast.col]
train.x <- matrix(train.x, nrow = nrow(train.x))
train.y <- scale(training.data, center=train.mean, scale=train.sd)[,forecast.col]
train.y <- matrix(train.y, nrow = nrow(train.y))
test.x <- scale(testing.data, center=train.mean[-forecast.col], scale=train.sd[-forecast.col])
test.x <- matrix(test.x, nrow = nrow(test.x))



print("Train diurnal-linear model...")
# Train model when lambda=0 (initial parameter estimate)
mod.ridge <- cv.glmnet(train.x,train.y,alpha=0)
predict.mod.ridge <- predict(mod.ridge, newx=train.x)
# Adaptive lasso
w3 <- 1/abs(matrix(coef(mod.ridge, s=mod.ridge$lambda.min)[, 1][-1]))^1
set.seed(Sys.time())
mod.adaptive <- cv.glmnet(train.x,train.y,alpha=1,penalty.factor=w3)
pred.adapt <- predict(mod.adaptive,newx=train.x, s='lambda.1se')

train.x <- cbind(train.x, pred.adapt)

write.csv(train.x, file=paste0(historian.import.path,"data_save_03.csv"))

# print(paste("Is keras available?", is_keras_available()))

# print(Sys.getenv())

# if(!is_keras_available()) install_keras()
tryCatch({install_keras()})


print("Train neural network...")

while(!("model" %in% ls())) {
  model <- tryCatch({keras_model_sequential() %>%
    layer_dense(units=round(ncol(train.x)*2/3), input_shape=c(NULL, ncol(train.x))) %>%
    layer_dense(units=ncol(train.x)) %>%
    layer_dense(units = 1)})
}
model %>% compile(
  optimizer = optimizer_rmsprop(),
  loss = "mean_squared_error"
)
history <- model %>% fit(
  x=train.x,
  y=train.y,
  batch_size=1,
  epochs=20,
 verbose=0
)
validation <- model %>% predict(
  x=train.x,
  batch_size=1
)
r2 <- cor(validation, train.y)^2;r2

print("Forecast...")
# Forecast
pred.adapt <- predict(mod.adaptive,newx=test.x, s='lambda.1se')
test.x <- cbind(test.x, pred.adapt)

prediction <- model %>% predict(
  x=test.x,
  batch_size=1
)

forecast <- prediction*train.sd[forecast.col]+train.mean[forecast.col]
if(forecast < 0) forecast <- 0
forecast
print("Saving results...")
##### Write data #####
write.data <- data.frame(predictor.tag)
write.data <- cbind(write.data, as.character(format(index(testing.data), "%m/%d/%Y %H:%M:%S")))
write.data <- cbind(write.data, data.frame(round(forecast,3)))
colnames(write.data) <- c("Tagname","TimeStamp","Value")
write.csv(write.data, file=paste0(historian.import.path,"HistorianDataImport.csv"),
          row.names = FALSE, quote = FALSE)
write.data

# Save results
model.fit <- data.frame("Time" = as.character(index(testing.data)),"Rsqu" = r2, "Forecast" = forecast, stringsAsFactors = FALSE)
if(!("ModelResults.csv" %in% list.files(path=historian.import.path))) {
  write.csv(model.fit, file=paste0(historian.import.path,"ModelResults.csv"), row.names = FALSE)
} else {
  all.model.fit <- read.csv(file=paste0(historian.import.path,"ModelResults.csv"))
  all.model.fit <- taRifx::remove.factors(all.model.fit)
  all.model.fit <- rbind(data.frame(all.model.fit, stringsAsFactors = FALSE), model.fit)
  write.csv(all.model.fit, file=paste0(historian.import.path,"ModelResults.csv"), row.names = FALSE)
}
