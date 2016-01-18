library(ggplot2) 
library(reshape2) 
library(grid) 
library(gridExtra)
library(rminer) 
library(foreach)

LOG_IDENTIFIER <- -99999
sum_cores <- function(dataset){
  
  if ((server == "arm")||(server == "atom02")){
    d1 <- sum_cores_arm(dataset)
    return(d1)
  }else{
    cores <- dataset[grep(pattern = "Core_*", names(dataset))]
    
    if (length(cores) == 0){
      return (dataset)
    }
    

      core1cpu <- cores[grep(pattern = "Core_1CPU", names(cores))]
      core2cpu <- cores[grep(pattern = "Core_2CPU", names(cores))]
      
      cores <- cores[-grep(pattern = "Core_1CPU", names(cores))]
      cores <- cores[-grep(pattern = "Core_2CPU", names(cores))]
    
    
    load <- rowSums(cores)
    
    dataset <- dataset[-grep(pattern = "Core_*", names(dataset))]
    dataset <- cbind(dataset, Core_1CPU = core1cpu, Core_2CPU = core2cpu, load)
    return (dataset)
  }
}


sum_cores_arm <- function(dataset){
  
  cores <- dataset[grep(pattern = "Core_*", names(dataset))]
  
  if (length(cores) == 0){
    return (dataset)
  }
  
  load <- rowSums(cores)
  dataset <- dataset[-grep(pattern = "Core_*", names(dataset))]

  dataset <- cbind(dataset, load)
  return (dataset)
  
}


muliply_metrics <- function(dataset){
  original <- dataset
  power <- dataset[names(dataset) %in% c("powerWatts")]
  dataset <- dataset[!names(dataset) %in% c("powerWatts")]
  
  l <- length(names(dataset))
  multiplyds <- data.frame(matrix(ncol = 0, nrow = nrow(dataset)))  
  for (i in 1: l){
    for (j in 1: l){
      if (i < j){
        #         cat(names(dataset[i]))
        #         cat("*")
        #         cat(names(dataset[j]))
        #         cat ("\n")
        mul <- data.frame(dataset[i]*dataset[j])
        names(mul)[1]<-paste0(names(dataset[i]),"*",names(dataset[j]))
        multiplyds <- data.frame(multiplyds, mul)
        
      }
      
    }
  }
  
  return (multiplyds)
  
}

resample_by_power <- function(dataset, bins = 10) {
  
  hist_obj <- hist(dataset$powerWatts, breaks = bins)
  max_samples <- max(hist_obj$counts)
  bin_codes <- .bincode(dataset$powerWatts, breaks = hist_obj$breaks)
  dataset$bin_codes <- bin_codes
  dataset_split <- split(dataset, bin_codes)
  
  res <- lapply(dataset_split, function(subdataset) subdataset[sample(nrow(subdataset), max_samples, replace = TRUE),])
  res <- do.call("rbind", res)
  res$bin_codes <- NULL
  rownames(res) <- NULL
  
  cat("Resampled by power. Original: ",nrow(dataset),". Bins: ", bins, ". Empty bins: ", sum(hist_obj$counts == 0),". Num samples per bin: ", max_samples, ". New size: ", nrow(res),sep="")
  
  return(res)
}

printBoxplots <- function(dataset1, dataset2, metric = NULL, indexes = NULL, outliers = TRUE) {
  
  op <- NULL
  if(is.null(metric)) {
    if(!is.null(indexes)) {
      dataset1 = dataset1[,indexes]
      dataset2 = dataset2[,indexes]
    }
    par()
    colnames <- dimnames(dataset1)[[2]]
    side_size = ceiling(sqrt(length(colnames)))
    op <- par(mar = rep(2, 4), mfrow=c(side_size, side_size))
    for (i in 1:length(colnames)) {
      boxplot(dataset1[,i], dataset2[,i], horizontal = TRUE, names = c("T", "V"), col = c("blue", "red"), main = paste0(i, ": ", colnames[i]), outline = outliers)
    }
  } else {
    op <- par(mar = rep(2, 4), mfrow=c(1, 1))
    boxplot(dataset1[, which(names(dataset1) %in% metric)], dataset2[, which(names(dataset2) %in% metric)], horizontal = TRUE, names = c("T", "V"), col = c("blue", "red"), main = metric, outline = outliers)
  }
  par(op)
}
  


predictModel <- function(test, lm_model, name_in = ''){
  if (OUTPUT_FOR_PAPER){
    if (length(name_in) > 0)
    {
      if (grepl("NAS", name_in)){
        split <- strsplit(name_in, ".csv")
        split1 <- split[[1]][1]
        name <- paste(toupper(gsub("_", " ", split1)))
      }
      else{
        
        split <- strsplit(name_in, "_")
        split1 <- split[[1]][1]
        split2 <- split[[1]][2]
        
        
        if (!is.na(split2)){
          split_ext <- strsplit(split2, "[.]")
          name <- paste(split1, split_ext[[1]][1])
        }else{
          split_ext <- strsplit(split1, "[.]")
          name <- split_ext[[1]][1]
        }
        name <- paste0(toupper(substr(name, 1, 1)), substr(name, 2, nchar(name)))
        name <- simpleCap(name)
      }
    }  
    
    result <- predict(lm_model, test[!names(test) %in% "powerWatts"]) 
    x <- test$powerWatts 
    y <- result 
    
    # Correlation only to be used in temporal signals 
    results <- list(R2 = mmetric(x, y, "R2"), 
                    MAE = mmetric(x, y, "MAE"), 
                    RMSE = mmetric(x, y, "RMSE"), 
                    RAE = mmetric(x, y, "RAE"), 
                    MAPE = mmetric(x, y, "MAPE"), 
                    J = mmetric(x, y, "MSE")/2) 
      cat(name, "\n")
      cat("J =", results$J, "\n") 
      cat("R^2 =", results$R2, "\n") 
      cat("MAE =", results$MAE, "\n") 
      cat("RMSE =", results$RMSE, "\n") 
      cat("RAE =", results$RAE, "\n") 
      cat("MAPE =", results$MAPE, "\n") 
    #FOR PAPER
    #cat (paste0(name, " & ", round(results$MAPE, 2), " & ", round(mmetric(x, y, "MdAPE"),2), " \\", "\\ \n"))
    
    dataframe <- data.frame(Real = x, Predicted = y, Sample = seq(1, length(y))) 
    
    
    meltdf <- melt(dataframe,id="Sample") 
    names(meltdf)[match("value", names(meltdf))] <- "powerWatts" 
    names(meltdf)[match("variable", names(meltdf))] <- "Legend" 
    
    #p2 <- ggplot(meltdf,aes(x=Sample,y=powerWatts,colour=Legend,group=Legend)) + geom_line() 
    # Formatted for presentations
    p2 <- ggplot(meltdf,aes(x=Sample,y=powerWatts,colour=Legend,linetype=Legend,group=Legend)) + geom_line(size=1) + theme_bw() + theme(axis.text=element_text(size=30),
                                                                                                                                        axis.title=element_text(size=33),
                                                                                                                                        title=element_text(size=30),
                                                                                                                                        legend.text=element_text(size=30),
                                                                                                                                        axis.line = element_line(colour = "black"),
                                                                                                                                        legend.position = "bottom",
                                                                                                                                        panel.grid.major = element_line(colour = "grey"),
                                                                                                                                        panel.background = element_rect(fill = "white")) + scale_linetype_manual(values=c("solid", "dotted")) + scale_colour_manual(values = c("blue", "red"))+theme(legend.title=element_blank()) + xlab("Sample") + ylab("Power (W)")
    
    
    
    grid.arrange(p2, ncol = 1)                            
    
    #   #save graph
    #   g <- arrangeGrob(p1, p2, ncol = 1,   main=textGrob(paste0(name, ": real vs prediction"),gp=gpar(fontsize=40,font=1)))
    #   split <- strsplit(name_in, ".csv")
    #   split1 <- split[[1]][1]
    #   paste0("image\\",split1, ".png")
    #   ggsave(file=paste0("images\\",split1, ".png"), g)
    
    return (results)
  }else{
    if (length(name_in) > 0)
    {
      if (grepl("NAS", name_in)){
        split <- strsplit(name_in, ".csv")
        split1 <- split[[1]][1]
        name <- paste(toupper(gsub("_", " ", split1)))
      }
      else{
        
        split <- strsplit(name_in, "_")
        split1 <- split[[1]][1]
        split2 <- split[[1]][2]
        
        
        if (!is.na(split2)){
          split_ext <- strsplit(split2, "[.]")
          name <- paste(split1, split_ext[[1]][1])
        }else{
          split_ext <- strsplit(split1, "[.]")
          name <- split_ext[[1]][1]
        }
        name <- paste0(toupper(substr(name, 1, 1)), substr(name, 2, nchar(name)))
      }
    }  
    
    result <- predict(lm_model, test[!names(test) %in% "powerWatts"]) 
    x <- test$powerWatts 
    y <- result 
    
    # Correlation only to be used in temporal signals 
    results <- list(R2 = mmetric(x, y, "R2"), 
                    MAE = mmetric(x, y, "MAE"), 
                    RMSE = mmetric(x, y, "RMSE"), 
                    RAE = mmetric(x, y, "RAE"), 
                    MAPE = mmetric(x, y, "MAPE"), 
                    J = mmetric(x, y, "MSE")/2) 
      cat(name, "\n")
      cat("J =", results$J, "\n") 
      cat("R^2 =", results$R2, "\n") 
      cat("MAE =", results$MAE, "\n") 
      cat("RMSE =", results$RMSE, "\n") 
      cat("RAE =", results$RAE, "\n") 
      cat("MAPE =", results$MAPE, "\n", "\n") 
    # #   cat("---------\n")
    #   cat("MSE", mmetric(x, y, "MSE"), "\n")
    #   cat("MdAPE =", mmetric(x, y, "MdAPE"), "\n") 
    #   cat("RMSPE =", mmetric(x, y, "RMSPE"), "\n") 
    #   cat("SMAPE =", mmetric(x, y, "SMAPE"), "\n") 
    #   cat("RMdSPE", mmetric(x, y, "RMdSPE"), "\n")
    #   cat("MdAE", mmetric(x, y, "MdAE"), "\n")
    
    #FOR PAPER
    #cat (paste0(name, " & ", results$MAPE, " & ", mmetric(x, y, "MdAPE"), " \\", "\\ \n"))
    
    
    
    
    dataframe <- data.frame(Real = x, Predicted = y, Sample = seq(1, length(y))) 
    
    #     p1 <- ggplot(dataframe, aes(x=Real, y=Predicted)) + 
    #       geom_point(shape=1) + 
    #       geom_abline(intercept = 0, slope = 1, colour = "green", lty = "dashed")  
    #   
    # Formatted for presentations
    p1 <- ggplot(dataframe, aes(x=Real, y=Predicted)) + 
      geom_point(shape=1) + 
      geom_abline(intercept = 0, slope = 1, colour = "green", lty = "dashed")  + theme(axis.text=element_text(size=35),
                                                                                       axis.title=element_text(size=30),
                                                                                       title=element_text(size=20),
                                                                                       legend.text=element_text(size=20),
                                                                                       plot.margin= unit(c(3,1,1,1), "lines"))
    
    
    meltdf <- melt(dataframe,id="Sample") 
    names(meltdf)[match("value", names(meltdf))] <- "powerWatts" 
    names(meltdf)[match("variable", names(meltdf))] <- "Legend" 
    
    #p2 <- ggplot(meltdf,aes(x=Sample,y=powerWatts,colour=Legend,group=Legend)) + geom_line() 
    # Formatted for presentations
    p2 <- ggplot(meltdf,aes(x=Sample,y=powerWatts,colour=Legend,group=Legend)) + geom_line(size=2) + theme(axis.text=element_text(size=35),
                                                                                                           axis.title=element_text(size=30),
                                                                                                           title=element_text(size=20),
                                                                                                           legend.text=element_text(size=20))
    grid.arrange(p1, p2, ncol = 1,   main=textGrob(paste0(name, ": real vs prediction"),gp=gpar(fontsize=40,font=1)))
    
    #   #save graph
    #   g <- arrangeGrob(p1, p2, ncol = 1,   main=textGrob(paste0(name, ": real vs prediction"),gp=gpar(fontsize=40,font=1)))
    #   split <- strsplit(name_in, ".csv")
    #   split1 <- split[[1]][1]
    #   paste0("image\\",split1, ".png")
    #   ggsave(file=paste0("images\\",split1, ".png"), g)
    
    return (results)
    
  }
}


predictModelNoGraph <- function(test, lm_model){
  result <- predict(lm_model, test[!names(test) %in% "powerWatts"]) 
  x <- test$powerWatts 
  y <- result 
  
  # Correlation only to be used in temporal signals 
  results <- list(R2 = mmetric(x, y, "R2"), 
                  MAE = mmetric(x, y, "MAE"), 
                  RMSE = mmetric(x, y, "RMSE"), 
                  RAE = mmetric(x, y, "RAE"), 
                  MAPE = mmetric(x, y, "MAPE"), 
                  J = mmetric(x, y, "MSE")/2) 
#   cat("J =", results$J, "\n") 
#   cat("R^2 =", results$R2, "\n") 
#   cat("MAE =", results$MAE, "\n") 
#   cat("RMSE =", results$RMSE, "\n") 
#   cat("RAE =", results$RAE, "\n") 
#   cat("MAPE =", results$MAPE, "\n") 
  
  return (results)
}

removeVars <- function(dataset) {
  if (server == "renewit02"){
    vars_to_remove <- names(dataset) %in% c( "part_max_used",
                                         "disk_total",
                                         "disk_free",
                                         "drops_in",  
                                         "drops_out",  
                                         "errs_in", 
                                         "errs_out", 
                                         "Timestamp", 
                                         "current", 
                                         "timestamp",
                                         "mem_free",
                                         "mem_shared",
                                         "swap_in",
                                         "swap_out",
                                         "swap_free",
                                         "FP_COMP_OPS_EXE_SSE_PACKED_SINGLE")}
  else{
    vars_to_remove <- names(dataset) %in% c( "part_max_used",
                                             "disk_total",
                                             "disk_free",
                                             "drops_in",  
                                             "drops_out",  
                                             "errs_in", 
                                             "errs_out", 
                                             "Timestamp", 
                                             "current", 
                                             "timestamp",
                                             "mem_free",
                                             "mem_shared",
                                             "swap_in",
                                             "swap_out",
                                             "swap_free"
                                             )
  }
  return (dataset[!vars_to_remove])
  
}


# splitdf function will return a list of training and testing sets.
# Source: http://www.gettinggeneticsdone.com/2011/02/split-data-frame-into-testing-and.html
split_training_validation_testing <- function(dataframe, percentage = c(80.0, 20.0, 0.0), seed=NULL) {
  
  if(sum(percentage) != 100.0) {
    stop("Sum of percentages in the splitting must be 100")
  }
  
  # Step 3: Split into training and validation data frames
  if (!is.null(seed)) set.seed(seed)
  index <- 1:nrow(dataframe)
  trainindex <- sample(index, trunc((percentage[1] / 100.0) * length(index)))
  trainset <- dataframe[trainindex, ]
  
  validation_test_set <- dataframe[-trainindex, ]
  index <- 1:nrow(validation_test_set)
  validationindex <- sample(index, trunc((percentage[2] / (100.0-percentage[1])) * length(index)))
  validationset <- validation_test_set[validationindex, ]
  if( length(validationindex) == 0) {
    testset <- validation_test_set
  } else {
    testset <- validation_test_set[-validationindex, ]
  }
  
  cat("\tTotal length: ", nrow(dataframe), "\n", sep="")
  cat("\tTraining length (",percentage[1],"%): ", nrow(trainset), "\n", sep="")
  cat("\tValidation length (",percentage[2],"%): ", nrow(validationset), "\n", sep="")
  cat("\tTesting length (",percentage[3],"%): ", nrow(testset), "\n", sep="")
  
  return(list(trainset = trainset, validationset = validationset, testset = testset))
}


get_polynomial_values <- function(vector_data, name = NULL, grade = 0){
  
  if (grade == 0) {
    return (data)
  }
  ret <- data.frame(vector_data^grade)
  names(ret)[1] <- paste(name, grade, sep=".")
  return (ret)

}

get_log_values <- function(vector_data, name = NULL){
  v.temp <- log(vector_data)
  v.temp[is.infinite(v.temp)] <- 0
  ret <- data.frame(v.temp)
  names(ret)[1] <- paste(name, "log", sep=".")
  return (ret)
}

get_polynomial_values_old <- function(original_dataset, grade = 2) {
  
  #   if (grade == 0) {
  #     return (original_dataset)
  #   }
  
  # DO NOT MODIFY POWERWATTS!
  #power <- original_dataset[names(original_dataset) %in% c("powerWatts")]
  data <- original_dataset[!names(original_dataset) %in% c("powerWatts")]
  
  if (grade >= 2 && grade <= 6 ) {
    
    # grade 2
    ldata_exp2 <- lapply(data, function(x) {x^2})
    for (i in 1:length(ldata_exp2)){
      names(ldata_exp2)[i] <- paste(names(ldata_exp2)[i], ".2", sep="")
    }
    data_exp2 <- as.data.frame(ldata_exp2)
    
    if (grade == 2){
      return (cbind(data_exp2))
      #return (data_exp2)
    }
    
    #grade 3
    ldata_exp3 <- lapply(data, function(x) {x^3})
    for (i in 1:length(ldata_exp3)){
      names(ldata_exp3)[i] <- paste(names(ldata_exp3)[i], ".3", sep="")
    }
    data_exp3 <- as.data.frame(ldata_exp3)
    
    if (grade == 3){
      return (cbind(data_exp2, data_exp3))
    }
    
    #grade 4
    ldata_exp4 <- lapply(data, function(x) {x^4})
    for (i in 1:length(ldata_exp4)){
      names(ldata_exp4)[i] <- paste(names(ldata_exp4)[i], ".4", sep="")
    }
    data_exp4 <- as.data.frame(ldata_exp4)
    
    if (grade == 4){
      return (cbind(data_exp2, data_exp3, data_exp4))
    }
    
    #grade 5
    ldata_exp5 <- lapply(data, function(x) {x^5})
    for (i in 1:length(ldata_exp5)){
      names(ldata_exp5)[i] <- paste(names(ldata_exp5)[i], ".5", sep="")
    }
    data_exp5 <- as.data.frame(ldata_exp5)
    
    if (grade == 5){
      return (cbind(data_exp2, data_exp3, data_exp4, data_exp5))
    }
    
    #grade 6
    ldata_exp6 <- lapply(data, function(x) {x^6})
    for (i in 1:length(ldata_exp6)){
      names(ldata_exp6)[i] <- paste(names(ldata_exp6)[i], ".6", sep="")
    }
    data_exp6 <- as.data.frame(ldata_exp6)
    
    if (grade == 6){
      return (cbind(data_exp2, data_exp3, data_exp4, data_exp5, data_exp6))
    }
    
  }else {
    stop("Exponential grade too high: Max = 6, Min = 2")
  }
}

get_sqrt_values_old <- function(original_dataset, grade = 2) {
  
  # DO NOT MODIFY POWERWATTS!
  #power <- original_dataset[names(original_dataset) %in% c("powerWatts")]
  data <- original_dataset[!names(original_dataset) %in% c("powerWatts")]
  
  if (grade >= 2 && grade <= 6) {
    
    # grade 2
    ldata_sqrt <- lapply(data, sqrt)
    for (i in 1:length(ldata_sqrt)){
      names(ldata_sqrt)[i] <- paste(names(ldata_sqrt)[i], ".sqrt.2", sep="")
    }
    data_exp2 <- as.data.frame(ldata_sqrt)
    
    if (grade == 2){
      return (cbind(data_exp2))
      #return (data_exp2)
    }
    
    #grade 3
    ldata_exp3 <- lapply(data, function(x) {x^(1/3)})
    for (i in 1:length(ldata_exp3)){
      names(ldata_exp3)[i] <- paste(names(ldata_exp3)[i], ".sqrt.3", sep="")
    }
    data_exp3 <- as.data.frame(ldata_exp3)
    
    if (grade == 3){
      return (cbind(data_exp2, data_exp3))
    }
    
    #grade 4
    ldata_exp4 <- lapply(data, function(x) {x^(1/4)})
    for (i in 1:length(ldata_exp4)){
      names(ldata_exp4)[i] <- paste(names(ldata_exp4)[i], ".sqrt.4", sep="")
    }
    data_exp4 <- as.data.frame(ldata_exp4)
    
    if (grade == 4){
      return (cbind(data_exp2, data_exp3, data_exp4))
    }
    
    #grade 5
    ldata_exp5 <- lapply(data, function(x) {x^(1/5)})
    for (i in 1:length(ldata_exp5)){
      names(ldata_exp5)[i] <- paste(names(ldata_exp5)[i], ".sqrt.5", sep="")
    }
    data_exp5 <- as.data.frame(ldata_exp5)
    
    if (grade == 5){
      return (cbind(data_exp2, data_exp3, data_exp4, data_exp5))
    }
    
    #grade 6
    ldata_exp6 <- lapply(data, function(x) {x^(1/6)})
    for (i in 1:length(ldata_exp6)){
      names(ldata_exp6)[i] <- paste(names(ldata_exp6)[i], ".sqrt.6", sep="")
    }
    data_exp6 <- as.data.frame(ldata_exp6)
    
    if (grade == 6){
      return (cbind(data_exp2, data_exp3, data_exp4, data_exp5, data_exp6))
    }
    
  }else {
    stop("Exponential grade too high: Max = 6, Min = 2")
  }
}

# 

predictMe<- function(dataset, exponent){
  dataset$metric <- (dataset$metric)^exponent
  lm.t <- lm(dataset$power ~ dataset$metric)
  result <- predict(lm.t, dataset[!names(dataset) %in% "power"]) 
  x <- dataset$power 
  y <- result 
  
#   # Correlation only to be used in temporal signals 
#   results <- list(R2 = mmetric(x, y, "R2"), 
#                   MAE = mmetric(x, y, "MAE"), 
#                   RMSE = mmetric(x, y, "RMSE"), 
#                   RAE = mmetric(x, y, "RAE"), 
#                   MAPE = mmetric(x, y, "MAPE"), 
#                   J = mmetric(x, y, "MSE")/2) 

  return (mmetric(x, y, "R2"))
}


predictMeNLSnew2 <- function(dataset, mname){
  set.seed(100)
  exp_to_ret <- tryCatch({
    #log
    if (LOG_METRICS){
      correlation_log <- find_log_correlation(dataset)
    }else{
      correlation_log <- -1
    }
    #exp 1
    correlation1 <- cor(dataset$metric, dataset$power) 
    
    # exp N
    fit2 <- nls(power~metric^gamma,data=dataset,start=list(gamma=1/10))
    
    a <- summary(fit2, correlation = TRUE)
    exponent <- a$coefficients[1]
    
    dataset$metric <- (dataset$metric)^exponent
    correlation2 <- cor(dataset$metric, dataset$power) 
    
    
    if (max(correlation1, correlation2, correlation_log) >= R_VALUE){
      if (correlation1 >= correlation2 && correlation1 >= correlation_log){

        return (1)
      }else if(correlation2 >= correlation1 && correlation2 >= correlation_log){

        return (round(exponent,1))
      }else{

        return (LOG_IDENTIFIER)
      }
      #return(exponent)
    }
    else{
      return ("DISCARD")
    }
    
  }, error = function(err) {
    
    print(err)
    return ("DISCARD")
  })
  #cat(exp_to_ret)
  if ((exp_to_ret == "DISCARD")){
    return (NA)
  }else{
    return (exp_to_ret)
  }
  
}

predictMeNLSnew <- function(dataset, mname){
  set.seed(100)
  exp_to_ret <- tryCatch({
    #log
    if (LOG_METRICS){
      correlation_log <- find_log_correlation(dataset)
    }else{
      correlation_log <- -1
    }
    #exp 1
    correlation1 <- cor(dataset$metric, dataset$power) 
    
    # exp N
    fit2 <- nls(power~metric^gamma,data=dataset,start=list(gamma=GAMMA), control = list(warnOnly = TRUE))

    a <- summary(fit2, correlation = TRUE)
    exponent <- a$coefficients[1]

    dataset$metric <- (dataset$metric)^exponent
    correlation2 <- cor(dataset$metric, dataset$power) 

    
    if (max(correlation1, correlation2, correlation_log) >= R_VALUE){
      if (correlation1 >= correlation2 && correlation1 >= correlation_log){
        
        return (1)
      }else if(correlation2 >= correlation1 && correlation2 >= correlation_log){

        return (round(exponent,1))
      }else{

        return (LOG_IDENTIFIER)
      }
      #return(exponent)
    }
    else{
      return ("DISCARD")
    }
    
  }, error = function(err) {
      print (mname)  
      print(err)
      return ("DISCARD")
    })
  #cat(exp_to_ret)
  if ((exp_to_ret == "DISCARD")){
    return (NA)
  }else{
    return (exp_to_ret)
  }
  
}



build.dataset <- function(dataset.for.model, dataset.original){
  
  EXPONENT = 2
  if (exists("metrics_exp2")){
    for (metric in metrics_exp2){
      var2 <- get_polynomial_values(dataset.original[, metric], metric, EXPONENT)
      dataset.for.model <- cbind(dataset.for.model, var2)
    }
  }
  else {
    cat (paste("Not using exponential terms with grade", EXPONENT, "\n"))
  }
  
  EXPONENT = 3
  if (exists("metrics_exp3")){
    for (metric in metrics_exp3){
      var2 <- get_polynomial_values(dataset.original[, metric], metric, EXPONENT)
      dataset.for.model <- cbind(dataset.for.model, var2)
    }
  }
  else {
    cat (paste("Not using exponential terms with grade", EXPONENT, "\n"))
  }
  
  EXPONENT = 4
  if (exists("metrics_exp4")){
    for (metric in metrics_exp4){
      var2 <- get_polynomial_values(dataset.original[, metric], metric, EXPONENT)
      dataset.for.model <- cbind(dataset.for.model, var2)
    }
  }
  else {
    cat (paste("Not using exponential terms with grade", EXPONENT, "\n"))
  }
  
  EXPONENT = 5
  if (exists("metrics_exp5")){
    for (metric in metrics_exp5){
      var2 <- get_polynomial_values(dataset.original[, metric], metric, EXPONENT)
      dataset.for.model <- cbind(dataset.for.model, var2)
    }
  }
  else {
    cat (paste("Not using exponential terms with grade", EXPONENT, "\n"))
  }
  
  EXPONENT = 6
  if (exists("metrics_exp6")){
    for (metric in metrics_exp6){
      var2 <- get_polynomial_values(dataset.original[, metric], metric, EXPONENT)
      dataset.for.model <- cbind(dataset.for.model, var2)
    }
  }
  else {
    cat (paste("Not using exponential terms with grade", EXPONENT, "\n"))
  }
  
  EXPONENT = 1/2
  if (exists("metrics_sqrt2")){
    for (metric in metrics_sqrt2){
      var2 <- get_polynomial_values(dataset.original[, metric], metric, EXPONENT)
      dataset.for.model <- cbind(dataset.for.model, var2)
    }
  }
  else {
    cat (paste("Not using exponential terms with grade", EXPONENT, "\n"))
  }
  
  EXPONENT = 1/3
  if (exists("metrics_sqrt3")){
    for (metric in metrics_sqrt3){
      var2 <- get_polynomial_values(dataset.original[, metric], metric, EXPONENT)
      dataset.for.model <- cbind(dataset.for.model, var2)
    }
  }
  else {
    cat (paste("Not using exponential terms with grade", EXPONENT, "\n"))
  }
  
  EXPONENT = 1/4
  if (exists("metrics_sqrt4")){
    for (metric in metrics_sqrt4){
      var2 <- get_polynomial_values(dataset.original[, metric], metric, EXPONENT)
      dataset.for.model <- cbind(dataset.for.model, var2)
    }
  }
  else {
    cat (paste("Not using exponential terms with grade", EXPONENT, "\n"))
  }
  
  EXPONENT = 1/5
  if (exists("metrics_sqrt5")){
    for (metric in metrics_sqrt5){
      var2 <- get_polynomial_values(dataset.original[, metric], metric, EXPONENT)
      dataset.for.model <- cbind(dataset.for.model, var2)
    }
  }
  else {
    cat (paste("Not using exponential terms with grade", EXPONENT, "\n"))
  }
  
  EXPONENT = 1/6
  if (exists("metrics_sqrt6")){
    for (metric in metrics_sqrt6){
      var2 <- get_polynomial_values(dataset.original[, metric], metric, EXPONENT)
      dataset.for.model <- cbind(dataset.for.model, var2)
    }
  }
  else {
    cat (paste("Not using exponential terms with grade", EXPONENT, "\n"))
  }
  return (dataset.for.model)

}

findKForVarianceThreshold <- function(pca_model, threshold) {
  n <- length(pca_model$center)
  k <- n
  for(i in seq(1,n)) {
    var_explained <- sum((pca_model$sdev[1:i])^2)/sum((pca_model$sdev)^2)
    if(var_explained >= threshold) {
      k <- i
      break
    }
  }
  return(k)
}

procesDatasetPCA <- function(pca_model, dataset, k) {
  dataset_pca_input <- predict(pca_model, dataset[,-1])[,1:k]
  dataset_pca <- cbind(dataset$powerWatts, dataset_pca_input)
  colnames(dataset_pca)[1] <- "powerWatts"
  dataset_pca <- as.data.frame(dataset_pca)
  
  return(dataset_pca)
}
# EDIT
processDatasetGroupPCA <- function(dataset, variance_explained) {
  pca_model <- princomp(datasets[,-1])
  k <- findKForVarianceThreshold(pca_model, variance_explained)
  
  training_PCA <- procesDatasetPCA(pca_model, datasets$trainset, k)
  validation_PCA <- procesDatasetPCA(pca_model, datasets$validationset, k)
  testing_PCA <- procesDatasetPCA(pca_model, datasets$testset, k)
  
  data = list(trainset = training_PCA, validationset = validation_PCA, testset = testing_PCA, pca_model = pca_model)
  
  return(data)
}
# 
# nonLinearPredictMe <- function(dataset){
#     dataset <- testds
#     lm.t <- lm(dataset$power ~ dataset$metric)
#     result <- predict(lm.t, dataset[!names(dataset) %in% "power"]) 
#     y <- dataset$power 
#     x <- dataset$metric
#     plot(x,y)
#     abline(lm.t, col="blue", lwd=2)
#     mmetric(x, y, "R2")
#     
#     
#     library(rgp)
# #     library(rgpui)
# #     symbolicRegressionUi()
#     set.seed(1)
#     m1 <- symbolicRegression(power ~ metric, data = testds, stopCondition = makeTimeStopCondition(20), errorMeasure = rsquared)
#     modelBest <- m1$population[[which.min(m1$fitnessValues)]]
#     
#     plot(testds, col=1); 
#     points(predict(m1, newdata = testds), col=2, type="l")
#     
#     
#     u <- seq(from = 100, to = 200,length.out = 383)
#     plot(y = modelBest(u), x = u, type="l")
#       # Correlation only to be used in temporal signals 
#       results <- list(R2 = mmetric(x, y, "R2"), 
#                       MAE = mmetric(x, y, "MAE"), 
#                       RMSE = mmetric(x, y, "RMSE"), 
#                       RAE = mmetric(x, y, "RAE"), 
#                       MAPE = mmetric(x, y, "MAPE"), 
#                       J = mmetric(x, y, "MSE")/2) 
#     
#     return (mmetric(x, y, "R2"))
#       
#       
#       
#   fitnessFunction1 <- function(f) rsquared(f(x), y)
#   set.seed(1)
#   inputVariableSet1 <- inputVariableSet("x")    
#   functionSet1 <- functionSet("+", "*", "-")
#   interval1 <- x
#   l <- length(x)
#   constantFactorySet1 <- constantFactorySet(function() rnorm(1))
#   fitnessFunction1 <- function(f) (-rsquared(y, x))
#   gpResult1 <- geneticProgramming(functionSet = functionSet1,
#                                    inputVariables = inputVariableSet1,
#                                    fitnessFunction = fitnessFunction1,
#                                    stopCondition = makeTimeStopCondition(20))
#       
#   bestSolution1 <- gpResult1$population[[which.min(gpResult1$fitnessValues)]]
#       
#   }
#   


# splitdf function will return a list of training and testing sets.
# Source: http://www.gettinggeneticsdone.com/2011/02/split-data-frame-into-testing-and.html
split_training_validation_testing <- function(dataframe, percentage = c(70.0, 30.0, 0.0), seed=NULL) {
  
  if(sum(percentage) != 100.0) {
    stop("Sum of percentages in the splitting must be 100")
  }
  
  # Step 3: Split into training and validation data frames
  if (!is.null(seed)) set.seed(seed)
  index <- 1:nrow(dataframe)
  trainindex <- sample(index, trunc((percentage[1] / 100.0) * length(index)))
  trainset <- dataframe[trainindex, ]
  
  validation_test_set <- dataframe[-trainindex, ]
  index <- 1:nrow(validation_test_set)
  validationindex <- sample(index, trunc((percentage[2] / (100.0-percentage[1])) * length(index)))
  validationset <- validation_test_set[validationindex, ]
  if( length(validationindex) == 0) {
    testset <- validation_test_set
  } else {
    testset <- validation_test_set[-validationindex, ]
  }
  
  cat("\tTotal length: ", nrow(dataframe), "\n", sep="")
  cat("\tTraining length (",percentage[1],"%): ", nrow(trainset), "\n", sep="")
  cat("\tValidation length (",percentage[2],"%): ", nrow(validationset), "\n", sep="")
  cat("\tTesting length (",percentage[3],"%): ", nrow(testset), "\n", sep="")
  
  return(list(trainset = trainset, validationset = validationset, testset = testset))
}


find_log_correlation <- function(dataset){
  ds_temp <- data.frame(power = dataset$power, metric = log(dataset$metric))
  #ds_temp <- ds_temp[is.finite(ds_temp$metric),]
  ds_temp[is.infinite(ds_temp$metric),] <- 0
  #ignore dataset with few instances
  if (nrow(ds_temp) < 100){
    return (0)
  }else{
    return(cor(ds_temp$metric, ds_temp$power))
  }
}

simpleCap <- function(x) {
  s <- strsplit(x, " ")[[1]]
  paste(toupper(substring(s, 1,1)), tolower(substring(s, 2)),
        sep="", collapse=" ")
}

