#' The 2SpamH algorithm
#'
#' This function performs the 2SpamH algorithm on a ceratin variable in the input data
#'
#' @param data A data frame contains the variables to be labelled and the variables on which principle component analysis is performed (PC vars).
#' @param variable A variable name or the position indexes of the variable to be labelled.
#' @param PC.vars A list object, such that each element of this list contains the vectors of either the name of the PC vars or the position indexes of the PC vars in the data frame.
#' @param step2.var A vector of variable names to be added into the step 2 KNN feature space.
#' @param imp.method A function which serves as the imputation method for missing data in PC vars. This function should take a vector with missing and retun a vector without.
#' @param thresholds A list of which the first element contains the 'low' quantile thresholds for each group of PC var, and the second for 'high'. Each element in it should be a vector eitehr of the same length as the number of PC var groups or 1.
#' @param num.neighbor Number of the neibors considered by each unlabelled data points in stage 2
#' @param check.cor Whether the highly correlated variables should be removed when performing stage 2 of the TSknn. If no, input should be NULL. If yes, input shou be the correlation threshold for variables to be removed.
#' @param plot.data If TRUE, the outputted results are for plotting. If FALSE, the function outputs the original data frame where the filtered variable is labelled with extra NAs.
#' @param TS.plot If set to true, the function outputs a time series plot that visualizes the positions of missing values and invaid observations.
#' @param seed The seed to be set, default is NULL.
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr select
#' @importFrom imputeTS na_mean
#' @importFrom dplyr mutate_if
#' @importFrom dplyr all_of
#' @importFrom purrr map2
#' @importFrom dplyr case_when
#' @importFrom dplyr any_of
#' @importFrom dplyr mutate
#' @importFrom caret findCorrelation
#' @importFrom dplyr filter
#' @importFrom class knn
#' @importFrom data.table :=
#' @importFrom dplyr pull
#' @importFrom imputeTS ggplot_na_distribution
#'
#' @return Either a dataframe object with filtered \code{variable} or a list for visualization of the labelling algorithm.
#'
#' @export

TwoSpamH_inner = function(data,
                          variable,
                          PC.vars,
                          step2.var = c(""),
                          imp.method = na_mean,
                          thresholds = list(low = c(0.3), high = c(0.7)),
                          num.neighbor = 5,
                          check.cor = 0.8, plot.data = F, TS.plot = F, seed = NULL){
  
  set.seed(seed)
  
  var.vec = unlist(PC.vars) %>% unname
  
  thresh.low = thresholds[[1]]
  thresh.high = thresholds[[2]]
  
  if(is.null(check.cor)){
    delete.cor = F
  }else{
    delete.cor = T
    cor.thresh = check.cor
  }
  
  if(is.numeric(var.vec)){
    var.vec = names(data)[var.vec]
    PC.vars = lapply(PC.vars, function(x){x = names(data)[x]})
  }
  
  if(is.numeric(variable)){
    variable = names(data)[variable]
  }
  ex.usage.imp = data %>%
    dplyr::select(all_of(c(var.vec, variable, step2.var))) %>%
    mutate_if(is.numeric, imp.method)
  
  scores = lapply(PC.vars, function(x){x.pc = princomp(ex.usage.imp[, x] %>% imp.method() %>% scale, cor = F)
  x.pc$scores[,1]})
  
  names(scores) = paste("pc", 1:length(scores))
  
  ex.step = data  %>% .[,variable] %>% unlist()
  
  low = map2(scores, thresh.low, function(x,y) x <= quantile(x, y)) %>% as.data.frame() %>% apply(1, all)
  
  low.threshes = scores %>% map2(thresh.low, function(x,y){quantile(x, y)})
  
  high = map2(scores, thresh.high, function(x,y) x >= quantile(x, y)) %>% as.data.frame() %>% apply(1, all)
  high.threshes = scores %>% map2(thresh.high, function(x,y){quantile(x, y)})
  
  training_mask = low | high
  
  data.all = ex.usage.imp %>%
    mutate(filter.old = case_when(low ~ "filtered",
                                  high ~ "not filtered",
                                  TRUE ~ "NA"),
           filter.new = filter.old) %>%
    mutate_if(function(x){is.numeric(x) & length(unique(x)) != 1}, scale)
  
  cat_cols = c("filter.old", "filter.new")
  
  if(delete.cor){ #used caret
    data.all.no.cate = data.all[!(names(data.all) %in% cat_cols | names(data.all) %in% step2.var)]
    cor_mat = cor(data.all.no.cate)
    index = findCorrelation(cor_mat, cor.thresh)
    to_be_removed = colnames(cor_mat)[index]
    data.training.x = data.all[!names(data.all) %in% to_be_removed] %>% filter(training_mask)
  }
  else{
    data.training.x = data.all %>% filter(training_mask) %>%  .[c(var.vec, step2.var, "filter.old")]
  }
  
  train.knn = select(data.training.x, any_of(var.vec), all_of(step2.var))
  
  data.all$filter.new = ifelse(training_mask, data.all$filter.old,
                               as.character(
                                 knn(train = as.matrix(train.knn),
                                     test = as.matrix(select(data.all, all_of(names(train.knn)))),
                                     cl = data.training.x$filter.old,
                                     k = num.neighbor)
                               ))
  if(TS.plot){
    p1 <- ggplot_na_distribution(ex.step,
                                 color_points = ifelse(data.all$filter.new == "filtered", "red", "steelblue"),
                                 title = "Timeseries Plot of Validity Distribution",
                                 subtitle = "Invalid observations are colored red \nMissing observations are replaced by red strips",
                                 ylab = variable,
                                 xlab = "Index")
    print(p1)
  }
  
  
  if(plot.data){
    return(list(df = data.all %>%
                  dplyr::select(filter.new, filter.old) %>%
                  mutate(filtered.var = ex.step) %>%
                  cbind(scores %>% as.data.frame),
                thresholds = list(low.threshes, high.threshes)))
  }
  
  return(data %>%
           mutate(!!variable := ifelse(data.all$filter.new == "filtered", NA, pull(data, !!as.name(variable)))))
}