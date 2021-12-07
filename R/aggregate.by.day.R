#' Aggregate the raw data into an equally-spaced data frame
#'
#' @param dat Data in a data.frame format
#' @param variable Name of variable in the data set you would like to re-structure
#' @param ID Name of ID variable in the data set
#' @param timestamp Name of time variable in the data set
#' @param agg_fun Function used for aggregation. Default is sum.
#' @export

aggregate_by_day = function(dat, variable, ID, timestamp, agg_fun){
  if(agg_fun == "count"){
    dat[,timestamp] = as.Date(dat[,timestamp])
    dat = dat %>% group_by(!!sym(ID), !!sym(timestamp)) %>% summarize(agg = n())
  }else{
    dat[,timestamp] = as.Date(dat[,timestamp])
    dat = dat %>% group_by(!!sym(ID), !!sym(timestamp)) %>% summarize(agg = get(agg_fun)(!!sym(variable), na.rm = TRUE))
  }
  names(dat)[ncol(dat)] = paste0(agg_fun,'_',variable)
  return(dat)
}


