
library(dplyr)


######### Validate ##########
validate_model <- function(model, m.to.remove){
  validation.test <- subset(validation.data.ready, select = names(validation.data.ready) %ni% m.to.remove)
  #validation.test <- testing.data
  
  #result <- predictModel(validation.test, model," ")
  result <- predictModelNoGraph(validation.test, model)
  return (result)
}
######### Initialize_MAPE ##########
initialize_MAPE <- function(t){
  n <- names(t) 
  f <- as.formula(paste("powerWatts ~", paste(n[!n %in% "powerWatts"], collapse = " + "))) 
  lm.model <- lm(f, t, na.action = na.omit)
  
  r <- validate_model(lm.model, m.to.use)
  return(r$MAE)
}

##################################

`%ni%` <- Negate(`%in%`)
all_m <- names(training.data)
all_m <- all_m[!all_m %in% "powerWatts"]

training.test.orig <- training.data
m.to.use <- c()
metrics_not_good <- c()
MAPE.init <- initialize_MAPE(training.test.orig)
cat(paste0("MAPE_init = ", MAPE.init))

for (mm in all_m){
  
  m.to.use <- c(m.to.use, mm)
  training.test <- subset(training.test.orig,select = names(training.test.orig) %ni% m.to.use)

  
  n <- names(training.test) 
  f <- as.formula(paste("powerWatts ~", paste(n[!n %in% "powerWatts"], collapse = " + "))) 
  lm.model <- lm(f, training.test, na.action = na.omit)
  
  r <- validate_model(lm.model, m.to.use)
  if (r$MAE < MAPE.init){
    metrics_not_good <- c(metrics_not_good, mm)
    MAPE.init <- r$MAE
  }else{
    m.to.use <- m.to.use[!m.to.use %in% mm]
  }
  
  cat (".")
  
}

# get_final_model()
# validate_distinct()

#cat (metrics_not_good)

################# Final model #####################

# get_final_model <- function() {
  training.final <- subset(training.test.orig,select = names(training.test.orig) %ni% metrics_not_good)
  n <- names(training.final) 
  f <- as.formula(paste("powerWatts ~", paste(n[!n %in% "powerWatts"], collapse = " + "))) 
  lm.model.final <- lm(f, training.final, na.action = na.omit)
# }


######### validate distinct ##################
  result_sum <- 0 
# validate_distinct <- function(){
  validation.files <- list.files(path = VALIDATION_PATH)
  
  #f <- validation.files[2]
  result_sum <- 0
  
  if (OUTPUT_FOR_PAPER){
    cat (paste0("Platform  & Benchmark & MAPE (%) & MdAPE", " \\", "\\ \n"))
  }
  
  for (f in validation.files){
    
    #cat(f)
    #cat("\n")
    validation.data <- removeVars(read.csv(paste0(VALIDATION_PATH, f)))
    validation.data <- validation.data[complete.cases(validation.data), ]
    
    valid.orig <- validation.data
    
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
      validation.exp <- validation.exp[,(names(validation.exp) %in% heatmap.values.to.filter)]
    }
    
    if(muliplyMetrics == TRUE){

      ds <- validation.data[,(names(validation.data) %in% heatmap.values)]
      valid.multiplyed <- muliply_metrics(ds)
      validation.data <- cbind(powerWatts = validation.data$powerWatts, valid.multiplyed, validation.exp, valid.orig[,(names(valid.orig) %in% keepme)])
    }else{
      validation.data <- cbind(powerWatts = validation.data$powerWatts, validation.exp[,(names(validation.exp) %in% n)] , valid.orig[,(names(valid.orig) %in% keepme)])

    }

    validation.data <- data.frame(powerWatts = validation.data$powerWatts, validation.data[,(names(validation.data) %in% keepRank)])
    validation.final <- subset(validation.data,select = names(validation.data) %ni% metrics_not_good)

    result <- predictModel(validation.data, lm.model.final, f)
    result_sum <- result_sum + result$MAPE
    #cat("\n")
  }

