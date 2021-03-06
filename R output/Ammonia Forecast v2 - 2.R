##### Variables #####
setwd("C:/Users/newhartk/Desktop/LIFT_2019")
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
   print("Old data found.")
   obj <- load(file = paste0(historian.import.path,"data_save_01.RData"))
   all.data <- get(obj)
   if(start > index(all.data)[1]) { # If the start data has already been compiled, 
	all.data <- all.data[paste0(start,"/")] # Crop the already compiled data
	start <- tail(index(all.data),n=1) # start will be the end of the already compiled data
      update.all.data <-TRUE
   }
}

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
  if(length(obj.data)==0) obj.data <- xts(NA, order.by=end)
  colnames(obj.data) <- file
  return(obj.data)
})

new.data <- do.call("cbind", obj.list)
colnames(new.data) <- sapply(colnames(new.data), function(x) strsplit(x, "[.]")[[1]][2])
names(colnames(new.data)) <- NULL

# Merge all process data
print("Merging files...")
if(update.all.data) {
   if(ncol(all.data) == ncol(new.data)) {
	 all.data <- rbind(all.data, new.data)
	 save(all.data, file = paste0(historian.import.path,"data_save_01.RData"))
   } else {
	print("Error in data pull. Different number of variables in old vs new data.")
	all.data <- rbind(all.data[, which(colnames(all.data) %in% colnames(new.data))],
				new.data[, which(colnames(new.data) %in% colnames(all.data))])
	file.remove(paste0(historian.import.path,"data_save_01.RData")) # Do a complete recompile next time
	quit(save="no")
   }
} else {
   all.data <- new.data
   save(all.data, file = paste0(historian.import.path,"data_save_01.RData"))
}


#write.csv(all.data, file=paste0(historian.import.path,"data_save_01.csv"))


##### FOR TESTING  #####
print("Finding timestamps...")
rounded.timestamps <- seq(as.numeric(index(all.data)[1]), as.numeric(last(index(all.data))), data.interval)
index.all.data <- as.numeric(index(all.data))
row.index <- lapply(rounded.timestamps, function(start) {
  intersect(which(index.all.data >= start), 
            which(index.all.data < (start+data.interval)))
})

print("Averaging to timestamps...")
mean.data <- do.call("rbind", lapply(row.index, function(r) {colMeans(all.data[r,], na.rm=TRUE)}))
mean.data <- na.locf(mean.data)
mean.data <- na.locf(mean.data, fromLast=TRUE)
mean.data <- xts(mean.data, order.by=as.POSIXct(rounded.timestamps, origin="1970-01-01"))

# Add sine/cosine pairs
# Convert timestamps to runtime and project onto a unit circle 
print("Setting up training data...")
time.stamps <- difftime(index(mean.data), index(mean.data)[1], units = "mins")
time.stamps <- as.numeric(time.stamps) %% 1440 # Cycles of 1 day are constructed (1440 min/day)
time.stamps <- (time.stamps*360/1440)*pi/180 # Cycles of minutes are converted to radians
diurnal.x <- do.call("cbind", lapply(1:d, function(n) {
  eval(parse(text=paste0("sin(",n,"*time.stamps)","+cos(",n,"*time.stamps)", collapse="+")))
}))
mean.data <- cbind(mean.data, diurnal.x)
#write.csv(mean.data, file=paste0(historian.import.path,"data_save_02.csv"))

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

if(anyNA(train.x)) {
   train.mean <- train.mean[!apply(train.x,2,anyNA)]
   train.sd <- train.sd[!apply(train.x,2,anyNA)]
   test.x <- test.x[,!apply(train.x,2,anyNA)]
   train.x <- train.x[,!apply(train.x,2,anyNA)]
}


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

#write.csv(train.x, file=paste0(historian.import.path,"data_save_03.csv"))

# print(Sys.getenv())

if(!is_keras_available()) tryCatch({install_keras()})



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
model.fit <- data.frame("System.Time" = as.character(Sys.time()),
						"Forecast.Time" = as.character(index(testing.data)), 
						"Training.Days" = training.window,
						"Diurnal.Pairs" = d,
						"Frequncy.Mins" = data.interval/60,
						"Horizon.Mins"=forecast.horizon,
						"Rsqu" = r2, 
						"Forecast.Ammonia" = forecast, 
						"Persistence.Ammonia" = as.numeric(testing.data[,predictor.col]), 
 
						stringsAsFactors = FALSE)
if(!("ModelResults.csv" %in% list.files(path=historian.import.path))) {
  write.csv(model.fit, file=paste0(historian.import.path,"ModelResults.csv"), row.names = FALSE)
} else {
  all.model.fit <- read.csv(file=paste0(historian.import.path,"ModelResults.csv"))
  all.model.fit <- taRifx::remove.factors(all.model.fit)
  all.model.fit <- rbind(data.frame(all.model.fit, stringsAsFactors = FALSE), model.fit)
  write.csv(all.model.fit, file=paste0(historian.import.path,"ModelResults.csv"), row.names = FALSE)
}

#save(all.data, file = paste0(historian.import.path,"data_save_01.RData"))

#shell(shQuote(normalizePath("03 Historian Data Import CSV.bat")), wait=TRUE)
