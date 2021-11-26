var_missingness_index = function(dat, var_name){
  return(
    (1:nrow(dat))[is.na(dat[,var_name])]
  )
  
}


#' Make plots for imputation result
#' 
#' @param raw_df raw passive dataframe
#' @param filter_df filtered passive dataframe
#' @param impute_df imputed passive dataframe
#' @param var_name passive measurement variable name
#' @param subject_name name of interested subject
#' @return a ggplot2 object
#' @export

plot_imputation = function(raw_df, filter_df, impute_df, var_name, subject_name){
  clean_subjest_list = unique(filter_df$subject_id)
  raw_df = raw_df %>% filter(subject_id %in% clean_subjest_list)
  r_miss <- list()
  f_miss <- list()
  for(i in 3:ncol(raw_df)){
    r_miss[[i]] <- var_missingness_index(raw_df, names(raw_df)[i])
  }
  r_miss <- setNames(r_miss,names(raw_df))
  
  for(i in 3:ncol(filter_df)){
    f_miss[[i]] <- setdiff(
      var_missingness_index(filter_df, names(filter_df)[i]),
      r_miss[[i]])
  }
  f_miss <- setNames(f_miss,names(filter_df))
  impute_df$missing_type = 'notNA_or_filtered'
  impute_df$missing_type[r_miss[var_name][[1]]] = 'raw_NA'
  impute_df$missing_type[f_miss[var_name][[1]]] = 'filter_to_NA'
  raw_df$missing_type = 'raw_data'
  p = ggplot(impute_df %>% filter(subject_id == subject_name), aes(x=date, y=get(var_name))) +
    geom_line()+
    geom_point(data = raw_df %>% filter(subject_id == subject_name), mapping = aes(x = date, y=get(var_name),color=missing_type))+
    geom_point(aes(color=missing_type))+
    scale_color_manual(values=c("red", "green", "black", 'orange')) +
    theme(legend.position="top") +
    ggtitle(paste0("Quality check for filtering algorithm and imputation results: ", var_name)) +
    xlab('date') + ylab(paste0(var_name))
  return(p)
}

  
  
  