library(rminer) 
library(foreach)
source("./tools.R")
source("./heat_map.R")
library(doMC)
registerDoMC(2)
library(mlbench)
library(caret)
library(zoo)

#server <- "arm"
#server <- "atom02"
server <- "bscgrid21"

# Sum cores load
coresSum <- TRUE

# # Remove metrics with high correlation before building model
removeCorrelatedVars <- FALSE

# Multiply metrics
muliplyMetrics <- FALSE

# Exponential metrics
exponentialMetrics <- TRUE

LOG_METRICS <- TRUE
REMOVE_IDLE <- FALSE
OUTPUT_FOR_PAPER <- TRUE

#load config file
source(paste0("./config/",server,".R"))

# Join files for training set
files <- list.files(path = PATH, full.names = TRUE)
training.file.list <- c()
for (f in files){
   training.file.list <- c(training.file.list, f)
}

training.data <- do.call(rbind,lapply(files,read.csv))
training.data <- removeVars(training.data)
training.data <- training.data[complete.cases(training.data), ]

# Sum load cores
if (coresSum){
  training.data <- sum_cores(training.data)
  METRICS <- names(training.data)
}else{
  METRICS <- names(training.data)
}


  #min = min(training.data$powerWatts)+0.5
  #ds.idle <- training.data[training.data$powerWatts <= min,]
  ds.idle <- sum_cores(read.csv(idle.file))
  ds.idle.median <- data.frame(t(as.matrix(apply(ds.idle, 2, max))))
  
if (REMOVE_IDLE){
    data.without.idle <- training.data
    foreach (m = names(training.data)) %do%{
      val = as.numeric(ds.idle.median[m])
      col.data <- data.without.idle[names(data.without.idle) %in% m]
      col.data <- lapply(col.data, function(x) x-val)
      data.without.idle[m] <- col.data
    }
    
    data <- data.without.idle
    
    # Replace negative values with 0
    data <- as.matrix(data)
    data[data<0] <- 0
    training.data <- as.data.frame(data)
    
    remove(data.without.idle, data)
}

training.orig <- training.data

#training.data <- training.orig
METRICS <- names(training.data)
METRICS <- METRICS[!METRICS %in% filter2]
#METRICS <- c("instructions","load","branches","branch.misses","L1.icache.loads","L1.dcache.loads","L1.dcache.load.misses","L1.dcache.stores","LLC.loads","LLC.load.misses","LLC.stores","DISPATCHED_FPU_OPS","L2_CACHE_MISS","REQUESTS_TO_L2","RETIRED_MMX_FP_INSTRUCTIONS_SSE","RETIRED_UOPS")
# Iter over files
resultsmatrix <- foreach(fname = files, .combine = 'rbind') %do%{
  if (coresSum){
    dataset <- sum_cores(read.csv(fname))
  }else{
    dataset <- read.csv(fname)
  }
  dataset <- dataset[complete.cases(dataset), ]
  
  # Iter over metrics
  matrixin <- foreach(metric.to.use = METRICS, .combine = 'rbind') %do%{

    if (REMOVE_IDLE){
       col.data <- dataset$powerWatts
       power <- sapply(col.data, function(x) x - ds.idle.median$powerWatts)
       
       col.data <- (dataset[,metric.to.use])
       val = as.numeric(ds.idle.median[metric.to.use])
       metric <- sapply(col.data, function(x) x - val)
       ds <- data.frame(power = power , metric = metric)
       
       # Replace negative with 0
       data <- as.matrix(ds)
       data[data<0] <- 0
       ds <- as.data.frame(data)
       remove(data)
       GAMMA <- 1/10
      #R_VALUE <- 0.9
    }else{
      ds <- data.frame(power = dataset$powerWatts, metric = dataset[metric.to.use])
      GAMMA <- 1/10
    }
    names(ds)[2] <- "metric"
    #cat(metric.to.use)
    #cat(": ")
    
    # Consider only dataset with more than 1 row
    if (nrow(ds) > 1){
      # If variance = 0 the column has always the same value. Ignore it.
      if ((var(ds$metric) > 0) & (!is.na(var(ds$metric)))) {
        
        exponent_ret <- predictMeNLSnew(dataset = ds, metric.to.use)
        
        if (!is.na(exponent_ret) ){
          c(metric.to.use, exponent_ret)

        }
      }
    }
  }
  
  if (!is.null(matrixin)){
    coeff.metric <- data.frame(metric = as.character(matrixin[,1]),
                               exponent = as.numeric(matrixin[,2]), stringsAsFactors=FALSE) 
  
    rbind(coeff.metric)
  }
  
}

results <- data.frame(metric = as.character(resultsmatrix[,1]),
                      exponent = as.numeric(resultsmatrix[,2]),stringsAsFactors=FALSE) 
results <- results[!is.na(results$exponent),]

error.sum <- source("./global_model_builder.R")
cat(error.sum[[1]])
