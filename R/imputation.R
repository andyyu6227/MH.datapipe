#' Impute missing value with missForest
#' 
#' @param dat merged dataset with filter passive/active/baseline data
#' @return A data frame after missing value imputation
#' @export
impute_missForest = function(dat){
  library(missForest)
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
  
  df.imp <- missForest::missForest(dat, variablewise = TRUE) #51.6s * 7 = 6 min runtime
  dat = as.data.frame(df.imp$ximp)
  return(dat)  
}