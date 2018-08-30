### data cleaning ###

## load baseline dataset prior to manipulation and renaming ##
sepsis_pre <- read_excel("data/raw_data/Sepsis Project dataset.xlsx")

## clean names and remove the first 10 subjects as well as the separately coded BMT subjects ##
sepsis_pre2 <- clean_names(sepsis_pre) %>% 
  filter(subj_id > 10) %>% 
  rename(no_heart_disease = heart_disease_0_change_to_actual_disease)




