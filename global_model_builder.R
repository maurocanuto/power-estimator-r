#Keep only relevant metrics
training.data <- training.orig

 if (coresSum){
   training.data <- sum_cores(training.data)
 }

#training.orig <- training.data
training.data <- cbind(powerWatts = training.data$powerWatts, training.data[,(names(training.data) %in% METRICS)])

#train.for.heat <- training.data[,!(names(training.data) %in% "powerWatts")]
if (muliplyMetrics){
  heatmap.values <- get_heat_map_values(training.data)
}

training.exp <- data.frame(matrix(ncol = 0,nrow = nrow(training.data)))

if (exponentialMetrics){
  # Get unique exponent for each metric
  for (m in METRICS){
    eso <- results[results$metric == m,]$exponent
    res <- eso[!duplicated(eso)]
    # create dataset with new values
    for (e in res){
      if (e != LOG_IDENTIFIER){
        ds <- get_polynomial_values(vector_data = training.data[,m], m, e)
        training.exp <- data.frame(training.exp, ds)}
      else{
        ds_log <- get_log_values(vector_data = training.data[,m], m)
        training.exp <- data.frame(training.exp, ds_log)}
      }
    }
  }

if (removeCorrelatedVars){
     ds <- cbind(powerWatts = training.data$powerWatts, training.exp)
     heatmap.values.to.filter <- get_heat_map_valuesVal(ds)
     training.exp <- training.exp[,(names(training.exp) %in% heatmap.values.to.filter)]
}

if(muliplyMetrics == TRUE){
  #datamultiplyed <- as.data.frame(apply(X = heatmap.values, MARGIN = 1, FUN = functionmuliply, dataset = training.data))
  ds <- training.data[,(names(training.data) %in% heatmap.values)]
  datamultiplyed <- muliply_metrics(ds)
  if (!is.null(keepme)){
    training.data <- cbind(powerWatts = training.data$powerWatts, datamultiplyed, training.exp,  training.orig[,(names(training.orig) %in% keepme)])
  }else{
    training.data <- cbind(powerWatts = training.data$powerWatts, datamultiplyed, training.exp)
  }
}else{
  if (!is.null(keepme)){
    training.data <- data.frame(powerWatts = training.data$powerWatts, training.exp, training.orig[,(names(training.orig) %in% keepme)])
  }else{
    training.data <- cbind(powerWatts = training.data$powerWatts, training.exp)
  }
    
}


# tt <- split_training_validation_testing(training.data, percentage = c(70.0, 30.0, 0.0), seed=NULL)
# training.data <- tt$trainset
# testing.data <- tt$validationset


n <- names(training.data)
f <- as.formula(paste("powerWatts ~", paste(n[!n %in% "powerWatts"], collapse = " + "))) 
lm.model.test <- lm(f, training.data, na.action = na.omit)
keepRank <- keepNonRankDeficientVariables(lm.model.test)
# if (length(keepRank) > 1){
  training.data <- data.frame(powerWatts = training.data$powerWatts, training.data[,(names(training.data) %in% keepRank)])
# }else
# {
  training.data <- data.frame(powerWatts = training.data$powerWatts, training.data[keepRank])
# }
n <- names(training.data) 
f <- as.formula(paste("powerWatts ~", paste(n[!n %in% "powerWatts"], collapse = " + "))) 
lm.model <- lm(f, training.data, na.action = na.omit)
lll.model <- lm.model

# Validation "

validation.files <- list.files(path = VALIDATION_PATH)

f <- validation.files[2]
result_sum <- 0

if (OUTPUT_FOR_PAPER){
  cat (paste0("Platform  & Benchmark & MAPE (%) & MdAPE", " \\", "\\ \n"))
}



for (f in validation.files){
  #if (grepl("CloudSuiteDataCaching*",f)){
  
#     cat(f)
#     cat("\n")
    validation.data <- removeVars(read.csv(paste0(VALIDATION_PATH, f)))
    validation.data <- validation.data[complete.cases(validation.data), ]
    
    if (coresSum){
      validation.data <- sum_cores(validation.data)
    }
    
    if (REMOVE_IDLE){
      data.without.idle <- validation.data
      foreach (m = names(validation.data)) %do%{
        val = as.numeric(ds.idle.median[m])
        col.data <- data.without.idle[names(data.without.idle) %in% m]
        col.data <- lapply(col.data, function(x) x-val)
        data.without.idle[m] <- col.data
      }
      data <- data.without.idle
      data <- as.matrix(data)
      data[data<0] <- 0
      validation.data <- as.data.frame(data)
      remove(data.without.idle, data)
    }
    
    validation.data.ready <- data.frame(matrix(ncol = 0,nrow = nrow(validation.data)))
    valid.orig <- validation.data
    
    validation.exp <- data.frame(matrix(ncol = 0,nrow = nrow(validation.data)))
    
    if (exponentialMetrics){
      validation.exp <- data.frame(matrix(ncol = 0,nrow = nrow(validation.data)))
      for (m in METRICS){

        eso <- results[results$metric == m,]$exponent
        res <- eso[!duplicated(eso)]
        
        # create dataset with new values
        for (e in res){
          if (e != LOG_IDENTIFIER){
            ds <- get_polynomial_values(vector_data = validation.data[,m], m, e)
            validation.exp <- data.frame(validation.exp, ds)
          }else{
            ds_log <- get_log_values(vector_data = validation.data[,m], m)
            validation.exp <- data.frame(validation.exp, ds_log)
          }
        }
        
      }
    }
    
    if (removeCorrelatedVars){
      #validation.exp <-  validation.exp[,!(names(validation.exp) %in% highCorr)]
      #validation.exp <- validation.exp[, -highCorr]
      
      validation.exp <- validation.exp[,(names(validation.exp) %in% heatmap.values.to.filter)]
    }
    
    if(muliplyMetrics == TRUE){
      #valid.multiplyed <- as.data.frame(apply(X = heatmap.values, MARGIN = 1, FUN = functionmuliply, dataset = validation.data))
      #validation.data <- cbind(powerWatts = validation.data$powerWatts, valid.multiplyed, validation.exp)
      
      ds <- validation.data[,(names(validation.data) %in% heatmap.values)]
      valid.multiplyed <- muliply_metrics(ds)
      validation.data <- cbind(powerWatts = validation.data$powerWatts, valid.multiplyed, validation.exp, valid.orig[,(names(valid.orig) %in% keepme)])
    }else{
    #  if (length(names(validation.exp) %in% n) > 1){
        validation.data <- cbind(powerWatts = validation.data$powerWatts, validation.exp[,(names(validation.exp) %in% n)] , valid.orig[,(names(valid.orig) %in% keepme)])
     # }else{
      #  validation.data <- cbind(powerWatts = validation.data$powerWatts, validation.exp[(names(validation.exp) %in% n)] , valid.orig[,(names(valid.orig) %in% keepme)]) 
      #}
  }
  # if (length(keepRank) > 1){
    validation.data <- data.frame(powerWatts = validation.data$powerWatts, validation.data[,(names(validation.data) %in% keepRank)])
#   }else{
#     validation.data <- data.frame(powerWatts = validation.data$powerWatts, validation.data[keepRank])
#   }
    
  validation.data.ready <- rbind(validation.data.ready, validation.data)

  result <- predictModel(validation.data, lm.model, f)
  #result <- predictModelNoGraph(validation.data, lm.model)
  cat("\n")
  
  result_sum <- result_sum + result$MAE
# }
}

cat("Saving model and parameters...\n")
lm.model <- lll.model
idle.values <- ds.idle.median
save (lm.model, file=paste0("/home/mcanuto/BSC/autonomicbsc/powermodelgenerator-r/models/model-",server,".RData"))
save (idle.values, file=paste0("/home/mcanuto/BSC/autonomicbsc/powermodelgenerator-r/models/idle-values-",server,".RData"))


 return (result_sum)

