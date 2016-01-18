library(ggplot2)
library(ggplot2)
library(reshape2)
library(Hmisc)
library(stats)

abbreviateSTR <- function(value, prefix){  # format string more concisely
  lst = c()
  for (item in value) {
    if (is.nan(item) || is.na(item)) { # if item is NaN return empty string
      lst <- c(lst, '')
      next
    }
    item <- round(item, 2) # round to two digits
    if (item == 0) { # if rounding results in 0 clarify
      item = '<.01'
    }
    item <- as.character(item)
    item <- sub("(^[0])+", "", item)    # remove leading 0: 0.05 -> .05
    item <- sub("(^-[0])+", "-", item)  # remove leading -0: -0.05 -> -.05
    lst <- c(lst, paste(prefix, item, sep = ""))
  }
  return(lst)
}



get_heat_map_values.old <- function(d){

  cormatrix = rcorr(as.matrix(d), type='spearman')
  cordata = melt(cormatrix$r)
  cordata$labelr = abbreviateSTR(melt(cormatrix$r)$value, 'r')
  cordata$labelP = abbreviateSTR(melt(cormatrix$P)$value, 'P')
  cordata$label = paste(cordata$labelr, "\n", 
                        cordata$labelP, sep = "")
  cordata$strike = ""
  cordata$strike[cormatrix$P > 10^-10] = "X"
#  cordata$strike[cormatrix$r < 0.7] = "X"
  
  cordata.power <- cordata[cordata$Var1 =="powerWatts",]
  #cordata.power <- cordata
  cordata <- cordata.power
  
  txtsize <- par('din')[2] / 2
  ggplot(cordata.power, aes(x=Var1, y=Var2, fill=value)) + geom_tile() + 
    theme(axis.text.x = element_text(angle=90, hjust=TRUE)) +
    xlab("") + ylab("") + 
    geom_text(label=cordata.power$label, size=txtsize ) + 
    geom_text(label=cordata.power$strike, size=txtsize * 4, color="red", alpha=0.4)
  
  vars.to.keep.me <- cordata.power[cordata.power$strike != "X",]$Var2
  
  cordata.good <- cordata[cordata$strike != "X",]
  cordata.distinct <- data.frame(vars = cordata.good[cordata.good$Var1 != cordata.good$Var2,"Var2"], stringsAsFactors = FALSE)
  ret <- as.vector(cordata.distinct$vars)
  return (ret)
}


get_heat_map_values <- function(d){
  
  cormatrix = rcorr(as.matrix(d), type='spearman')
  cordata = melt(cormatrix$P)
  cordata$labelr = abbreviateSTR(melt(cormatrix$r)$value, 'r')
  cordata$labelP = abbreviateSTR(melt(cormatrix$P)$value, 'P')
  cordata$label = paste(cordata$labelr, "\n", 
                        cordata$labelP, sep = "")
  cordata$strike = ""
  cordata$strike[cormatrix$P > 0.05] = "X"
  #cordata$strike[cormatrix$r < 0.7] = "X"
  
  cordata.power <- cordata[cordata$Var1 =="powerWatts",]
  #cordata.power <- cordata
  cordata <- cordata.power
  
  txtsize <- par('din')[2] / 2
  ggplot(cordata.power, aes(x=Var1, y=Var2, fill=value)) + geom_tile() + 
    theme(axis.text.x = element_text(angle=90, hjust=TRUE)) +
    xlab("") + ylab("") + 
    geom_text(label=cordata.power$label, size=txtsize ) + 
    geom_text(label=cordata.power$strike, size=txtsize * 4, color="red", alpha=0.4)
  
  vars.to.keep.me <- cordata.power[cordata.power$strike != "X",]$Var2
  
  cordata.good <- cordata[cordata$strike != "X",]
  cordata.distinct <- data.frame(vars = cordata.good[cordata.good$Var1 != cordata.good$Var2,"Var2"], stringsAsFactors = FALSE)
  ret <- as.vector(cordata.distinct$vars)
  return (ret)
}


get_heat_map_valuesVal <- function(d){
  
  cormatrix = rcorr(as.matrix(d), type='spearman')
  cordata = melt(cormatrix$r)
  cordata$labelr = abbreviateSTR(melt(cormatrix$r)$value, 'r')
  cordata$labelP = abbreviateSTR(melt(cormatrix$P)$value, 'P')
  cordata$label = paste(cordata$labelr, "\n", 
                        cordata$labelP, sep = "")
  cordata$strike = ""
   #cordata$strike[cormatrix$P > 0.05] = "X"
  cordata$strike[cormatrix$r < 0.7] = "X"
  
  cordata.power <- cordata[cordata$Var1 =="powerWatts",]
  #cordata.power <- cordata
  cordata <- cordata.power
  
  txtsize <- par('din')[2] / 2
  ggplot(cordata.power, aes(x=Var1, y=Var2, fill=value)) + geom_tile() + 
    theme(axis.text.x = element_text(angle=90, hjust=TRUE)) +
    xlab("") + ylab("") + 
    geom_text(label=cordata.power$label, size=txtsize ) + 
    geom_text(label=cordata.power$strike, size=txtsize * 4, color="red", alpha=0.4)
  
  vars.to.keep.me <- cordata.power[cordata.power$strike != "X",]$Var2
  
  cordata.good <- cordata[cordata$strike != "X",]
  cordata.distinct <- data.frame(vars = cordata.good[cordata.good$Var1 != cordata.good$Var2,"Var2"], stringsAsFactors = FALSE)
  ret <- as.vector(cordata.distinct$vars)
  return (ret)
}


keepNonRankDeficientVariables <- function(model){
  
  a <- summary(model)[4]
  var.model <- names(a$coefficients[,1])
  var.model <- var.model[!var.model %in% "(Intercept)"]
  return (var.model)
}