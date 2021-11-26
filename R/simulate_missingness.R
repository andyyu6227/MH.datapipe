missing_percentage_cross_subjects = function(dat, var_name){
  sub = unique(dat$subject_id)[1]
  var_df = dat[c('subject_id',var_name)]
  var_df$is_miss = ifelse(is.na(dat[var_name]),1,0)
  return(var_df %>% group_by(subject_id) %>% summarize(miss_perc = mean(is_miss)))  
}

label_na = function(dat, var_name, miss_df, n_round, seed, priority_miss_perc){
  rownames(dat) = 1:nrow(dat)
  dat = dat[!is.na(dat[var_name]),]
  label_lists = vector("list", n_round)
  set.seed(seed)
  seeds_list = sample(1:1000, n_round, replace=FALSE)
  if(is.na(priority_miss_perc)){
    for(n in 1:n_round){
      for(i in 1:length(unique(dat$subject_id))){
        num_obs = nrow(dat %>% filter(subject_id == unique(dat$subject_id)[i]))
        num_na = round(num_obs * miss_df[miss_df$subject_id == unique(dat$subject_id)[i],]$miss_perc,0)
        set.seed(seeds_list[n])
        label_lists[[n]] = append(label_lists[[n]], sample(rownames(dat[dat$subject_id == unique(dat$subject_id)[i],]), num_na, replace=FALSE))
      }   
    }
  }else{
    for(n in 1:n_round){
      for(i in 1:length(unique(dat$subject_id))){
        num_obs = nrow(dat %>% filter(subject_id == unique(dat$subject_id)[i]))
        num_na = round(num_obs * priority_miss_perc,0)
        set.seed(seeds_list[n])
        label_lists[[n]] = append(label_lists[[n]], sample(rownames(dat[dat$subject_id == unique(dat$subject_id)[i],]), num_na, replace=FALSE))
      }   
    }
  }
  return(label_lists)
}

my_missForest = function(dat, var_name, seed){
  dat = dat %>% select(-subject_id, -date)
  dat$homework = as.factor(dat$homework)
  dat$gender = as.factor(dat$gender)
  dat$marital_status = as.factor(dat$marital_status)
  dat$ethnicity = as.factor(dat$ethnicity)
  dat$race = as.factor(dat$race)
  dat$religion = as.factor(dat$religion)
  dat$finres_1 = as.factor(dat$finres_1)
  dat$finres_2 = as.factor(dat$finres_2)
  dat$finres_3 = as.factor(dat$finres_3)
  dat$finres_4 = as.factor(dat$finres_4)
  dat$finres_5 = as.factor(dat$finres_5)
  dat$pain = as.factor(dat$pain)
  dat$phq8_q1_alacrity = as.factor(dat$phq8_q1_alacrity)
  dat$phq8_q2_alacrity = as.factor(dat$phq8_q2_alacrity)
  dat$stress = as.factor(dat$stress)
  dat$arousal = as.factor(dat$arousal)
  dat$valence = as.factor(dat$valence)
  
  set.seed(seed)
  df.imp <- missForest::missForest(dat, variablewise = TRUE) #51.6s * 7 = 6 min
  dat = as.data.frame(df.imp$ximp)
  return(dat)  
}

#2. impute by moving average
seven_moving_everage = function(dat, var_name, seed){
  set.seed(seed)
  copy_row_name = rownames(dat)
  sql_query = paste0('SELECT subject_id,', var_name,', AVG(',var_name,') OVER (ORDER BY subject_id, date ROWS BETWEEN 7 PRECEDING AND CURRENT ROW) AS moving_average FROM dat ORDER BY subject_id, date')
  dat = sqldf::sqldf(sql_query)
  dat[which(is.na(dat$step_count)),][var_name] = dat[which(is.na(dat$step_count)),]['moving_average']
  rownames(dat) = copy_row_name
  return(dat)
}

imput_miss = function(dat, var_name, seed, label_list, imp_func){
  func <- match.fun(imp_func)
  n_round = length(label_list)
  rownames(dat) = 1:nrow(dat)
  dat = dat[!is.na(dat[var_name]),]
  impute_lists = vector("list", n_round)
  for(n in 1:length(label_list)){
    dat2 = dat
    dat2[rownames(dat2)[rownames(dat2) %in% label_list[[n]]],][var_name] = NA
    dat2 = func(dat2, var_name, seed) #check if row index still exist
    impute_lists[[n]] = dat2[rownames(dat2)[rownames(dat2) %in% label_list[[n]]],][var_name][1:length(label_list[[n]]),]
    
  }
  return(impute_lists)
}

compute_nmse = function(dat, var_name, impute_list, label_list){
  n_round = length(label_list)
  nmse_df_list = vector("list", n_round)
  nmse_df <- data.frame(matrix(ncol = 3, nrow = 0))
  colnames(nmse_df) <- c("subject_id", "nmse", 'round_index')
  dat = dat %>% select(subject_id, var_name)
  rownames(dat) = 1:nrow(dat)
  dat = dat[!is.na(dat[var_name]),]
  dat$round_index = NA
  for(n in 1:length(label_list)){
    dat2 = dat
    dat2 = dat2[rownames(dat2)[rownames(dat2) %in% label_list[[n]]],]
    dat2$impute_value = impute_list[[n]]
    colnames(dat2)[2] = 'true_value'
    nmse_df = rbind(nmse_df, dat2 %>% group_by(subject_id) %>% summarize(nmse = sqrt(mean((true_value - impute_value)^2)/var(true_value))) %>% mutate(round_index = n))  
  }

  return(nmse_df)
}

#' Make plots for imputation result
#' 
#' @param filter_df filtered passive data
#' @param active_df active data
#' @param baseline_df baseline data
#' @param var_name passive measurement variable name
#' @param priority_miss_perc Percentage of observations per subject to be labeled as missing;1)value between 0 and 1: same percentage for each subject;2)NA: automatically according to missing percentage of each subject
#' @param n_round number of times to randomly sample observations to be labeled
#' @param seed fix seed so that simulation results are reproduceable 
#' @param imp_func the function of imputation method, available options now: #1) missForest :my_missForest; #2) moving average of 7 days: seven_moving_everage
#' @return a data frame with imputation quality measurement metrics
#' @export

impute_quality_check = function(filter_df, active_df, baseline_df, var_name, priority_miss_perc, n_round, seed, imp_func){
  my_dat = left_join(filter_df, baseline_df, by = c("subject_id" = "subject_id") ) %>%
    arrange(subject_id, date)
  my_dat = left_join(my_dat, active_df, by = c("subject_id" = "subject_id", "date" = "date") ) %>%
    arrange(subject_id, date)
  miss_df = missing_percentage_cross_subjects(dat = my_dat,var_name = var_name)
  label_lists = label_na(dat = my_dat, var_name = var_name, miss_df = miss_df, n_round = n_round, seed = seed, priority_miss_perc = 0.2)
  imput_result = imput_miss(dat=my_dat, var_name=var_name, seed=seed, label_list=label_lists, imp_func = imp_func)
  eg_compute_nmse = compute_nmse(dat = my_dat, var_name = var_name, impute_list = imput_result, label_list = label_lists)
  return(eg_compute_nmse)
}