#' Get access to BOX API
#' 
#' @param id the id for box api
#' @param secret the secret for box api
#' @export
access_api = function(id, secret){
  boxr::box_auth(client_id = id, client_secret = secret)
}

#' Load raw passive data
#' 
#' @return A data frame of raw passive data
#' @export
load_raw_passive = function(){
  #Load data
  passive_raw_df = boxr::box_read_csv(882499322193) #read raw passive data
  #Data format
  passive_raw_df$date = as.Date(passive_raw_df$date)
  #clean and convert subject names into lower case:
  passive_raw_df$subject_id = tolower(passive_raw_df$subject_id)
  passive_raw_df$subject_id = gsub("gayerskyreds", "gayevskyreds", passive_raw_df$subject_id)
  passive_raw_df$subject_id = gsub("rodriguezreds", "rodriquezreds", passive_raw_df$subject_id)
  passive_raw_df$subject_id = gsub("rodriguezreds", "rodriquezreds", passive_raw_df$subject_id)
  passive_raw_df$subject_id = gsub(" ","", passive_raw_df$subject_id)
  passive_raw_df = passive_raw_df %>% arrange(subject_id, date)
  #Discard some passive variables
  passive_raw_df = passive_raw_df %>%
    select(subject_id, date, screen_unlocks, display_events, step_count, time_at_home, conversation_percent, tic_voiced_time, sleep_duration, travel_diameter, device_use_percent) %>%
    arrange(subject_id, date)
  return(passive_raw_df)
}

#' Load baseline data
#' 
#' @return A data frame of baseline data
#' @export
load_baseline = function(){
  import_data = function(sheet){data.frame(boxr::box_read_excel(882501310568, sheet = sheet))}
  reds = import_data("REDS") %>% 
    filter(is.na(hr_id) == FALSE) %>% 
    filter(redcap_event_name == "baseline_arm_1") %>% 
    select(hr_id, gender, age_yrs, marital_status, ethnicity, race, religion, education, bisbas_DRIVE_TOTAL,
           bisbas_funtot, bisbas_reward_tot, bistot, madrstot,
           finres_1, finres_2, finres_3, finres_4, finres_5, madrstot)
  reds[,2:ncol(reds)] = sapply(reds[,2:ncol(reds)], function(x) as.numeric(x))
  
  relief = import_data("RELIEF") %>% 
    filter(is.na(hr_id) == FALSE) %>% 
    filter(redcap_event_name == "baseline_arm_1") %>% 
    select(hr_id, gender, age_yrs, marital_status, ethnicity, race, religion, education, bisbas_DRIVE_TOTAL,
           bisbas_funtot, bisbas_reward_tot, bistot, madrstot,
           finres_1, finres_2, finres_3, finres_4, finres_5, madrstot)
  relief[,2:ncol(relief)] = sapply(relief[,2:ncol(relief)], function(x) as.numeric(x))
  
  protect = import_data("PROTECT") %>% 
    filter(is.na(hr_id) == FALSE) %>% 
    filter(redcap_event_name == "baseline_arm_1") %>% 
    select(hr_id, gender, age_yrs, marital_status, ethnicity, race, religion, education, bisbas_DRIVE_TOTAL,
           bisbas_funtot, bisbas_reward_tot, bistot, madrstot,
           finres_1, finres_2, finres_3, finres_4, finres_5, madrstot)
  protect[,2:ncol(protect)] = sapply(protect[,2:ncol(protect)], function(x) as.numeric(x))
  
  base_df = bind_rows(reds, relief, protect)
  #clean and convert subject names into lower case:
  base_df$hr_id = tolower(base_df$hr_id)
  base_df$hr_id = gsub("gayerskyreds", "gayevskyreds", base_df$hr_id)
  base_df$hr_id = gsub("rodriguezreds", "rodriquezreds", base_df$hr_id)
  base_df$hr_id = gsub("rodriguezreds", "rodriquezreds", base_df$hr_id)
  base_df$hr_id = gsub(" ","",base_df$hr_id)
  
  names(base_df)[1] = "subject_id"
  
  return(base_df)
}

#' Load active data
#' 
#' @return A data frame of active data
#' @export
load_active = function(){
  #Load Active Data:
  active_long = boxr::box_read_csv(882501545763) %>% select(subject_id, timestamp, q_id, val) 
  #date format
  active_long$timestamp <- as.Date(active_long$timestamp) 
  #clean and convert subject names into lower case:
  active_long = active_long %>% 
    arrange(subject_id, timestamp)
  active_long$subject_id = tolower(active_long$subject_id)
  active_long$subject_id = gsub("gayerskyreds", "gayevskyreds", active_long$subject_id)
  active_long$subject_id = gsub("rodriguezreds", "rodriquezreds", active_long$subject_id)
  active_long$subject_id = gsub("rodriguezreds", "rodriquezreds", active_long$subject_id)
  active_long$subject_id = gsub(" ","", active_long$subject_id)
  
  #delect duplicat records
  active_long = active_long %>% distinct(subject_id, timestamp,  q_id, .keep_all = TRUE)
  
  #convert data from long format to wide format
  active_df = spread(active_long, q_id, val, fill = -999) %>% select(-invalid_format_no_response)
  active_df = active_df %>% 
    arrange(subject_id, timestamp) 
  
  #convert PAM related data from strings to numerics
  active_df = active_df %>% 
    mutate(PAM_row = as.numeric(substr(PAM, 2, 2)) - 1) %>%  
    mutate(PAM_col = as.numeric(substr(PAM, 4, 4))) %>% 
    mutate(PAMstep_row = as.numeric(substr(pam_step, 2, 2))) %>% 
    mutate(PAMstep_col = as.numeric(substr(pam_step, 4, 4)))
  active_df$PAM_row[which(active_df$PAM_row == 8)] = 0
  active_df$PAM_col[which(active_df$PAM_col == 9)] = 0
  active_df$PAMstep_row[which(active_df$PAMstep_row == 9)] = 0
  active_df$PAMstep_col[which(active_df$PAMstep_col == 9)] = 0
  
  #Convert picture selection data into correlated scores(the formula is based on explainations of app's website)
  active_df = active_df %>% 
    mutate(row = PAM_row+PAMstep_row) %>% 
    mutate(col = PAM_col+PAMstep_col) %>% 
    mutate(arousal = 5 - row) %>% 
    mutate(valence = col) %>% 
    select(subject_id, timestamp, homework, pain, phq8_q1_alacrity, phq8_q2_alacrity, stress, arousal, valence)
  #NA values
  active_df$arousal[which(active_df$arousal == 0)] = NA
  active_df$valence[which(active_df$valence == 0)] = NA
  active_df$pain[which(active_df$pain == '-999')] = NA
  active_df$phq8_q1_alacrity[which(active_df$phq8_q1_alacrity == '-999')] = NA
  active_df$phq8_q2_alacrity[which(active_df$phq8_q2_alacrity == '-999')] = NA
  active_df$stress[which(active_df$stress == '-999')] = NA
  active_df$homework[which(active_df$homework == '-999')] = NA
  names(active_df)[2] = "date"
  active_df[,3:ncol(active_df)] = sapply(active_df[,3:ncol(active_df)], function(x) as.numeric(x))
  
  return(active_df)
}

#' Load filtered passive data
#' 
#' @return A data frame of filtered passive data
#' @export
load_filtered_passive = function(){
  filtered_passive = boxr::box_read_csv(884263835242)
  filtered_passive$date = as.Date(filtered_passive$date)
  filtered_passive$subject_id = tolower(filtered_passive$subject_id)
  filtered_passive$subject_id = gsub("gayerskyreds", "gayevskyreds", filtered_passive$subject_id)
  filtered_passive$subject_id = gsub("rodriguezreds", "rodriquezreds", filtered_passive$subject_id)
  filtered_passive$subject_id = gsub("rodriguezreds", "rodriquezreds", filtered_passive$subject_id)
  filtered_passive$subject_id = gsub(" ", "", filtered_passive$subject_id)
  filtered_passive = filtered_passive %>% arrange(subject_id, date)
  filtered_passive = filtered_passive %>% select(subject_id, date, screen_unlocks, display_events, step_count, time_at_home, conversation_percent, tic_voiced_time, sleep_duration, travel_diameter, device_use_percent)
  return(filtered_passive)
  }

#' Load imputed data
#' 
#' @return A data frame of imputed passive data
#' @export
load_imputed_passive = function(){
  impute_p = boxr::box_read_csv(885059869455)[-1] %>% arrange(subject_id, date)
    return(impute_p)
}
