### data cleaning ###

## load baseline dataset prior to manipulation and renaming ##
sepsis_pre <- read_csv("data/raw_data/RegistryOfCriticalIl_DATA_2018-09-11_0926.csv")

sepsis_pre2 <- clean_names(sepsis_pre) %>% ## clean names
  filter(subj_id > 10) ## remove the first 10 subjects as well as the separately coded BMT subjects ##
  
 
sepsis_pre3 <- sepsis_pre2 %>% 
  rename(no_heart_disease = heart_disease_0, ## renaming cardiac variables 
         coronary_artery_disease = heart_disease_1, 
         congestive_heart_failure = heart_disease_2,
         arrhythmia = heart_disease_3,
         valvular_disease = heart_disease_4,
         other_heart_disease = heart_disease_5) %>% 
  rename(init_dopamine = init_vasopressors_type_1, # renaming initial vasopressor type
         init_dobutamine = init_vasopressors_type_2,
         init_norepinephrine = init_vasopressors_type_3,
         init_epinephrine = init_vasopressors_type_4,
         init_phenylephrine = init_vasopressors_type_5,
         init_milrinone = init_vasopressors_type_6,
         init_vasopressin = init_vasopressors_type_7) %>% 
  rename(init_dopamine_dose = init_vasopressors_dose_1, # renaming initial vasopressor dose
         init_dopamine_dose_kg = init_vasopressors_kg_1,
         init_dobutamine_dose = init_vasopressors_dose_2,
         init_dobutamine_dose_kg = init_vasopressors_kg_2,
         init_norepinephrine_dose = init_vasopressors_dose_3,
         init_norepinephrine_dose_kg = init_vasopressors_kg_3,
         init_epinephrine_dose = init_vasopressors_dose_4,
         init_epinephrine_dose_kg = init_vasopressors_kg_4,
         init_epinephrine_dose = init_vasopressors_dose_4,
         init_epinephrine_dose_kg = init_vasopressors_kg_4,
         init_phenylephrine_dose = init_vasopressors_dose_5,
         init_phenylephrine_dose_kg = init_vasopressors_kg_5,
         init_milrinone_dose = init_vasopressors_dose_6,
         init_milrinone_dose_kg = init_vasopressors_kg_6,
         init_vasopressin_dose = init_vasopressors_dose_7,
         init_vasopressin_dose_kg = init_vasopressors_kg_7) %>% 
  rename(init_24_trans_rbc = init_24_trans_1, # renaming transfusion type variables
         init_24_trans_ffp = init_24_trans_2,
         init_24_trans_plt = init_24_trans_3) %>% 
  rename(apache_temp_pts = apache1, # renaming apache point variables
         apache_map_pts = apache2,
         apache_hr_pts = apache3,
         apache_rr_pts = apache4,
         apache_pao2_pts = apache5,
         apache_ph_pts = apache6,
         apache_na_pts = apache7,
         apache_k_pts = apache8,
         apache_cr_pts = apache9,
         apache_hct_pts = apache10,
         apache_wbc_pts = apache11,
         apache_gcs_pts = apache12,
         apache_ch_pts = apache14) %>% 
  rename(dopamine = vasopressors_type_1, # renaming vasopressors for each 24h period (yes/no)
         dobutamine = vasopressors_type_2,
         norepinephrine = vasopressors_type_3,
         epinephrine = vasopressors_type_4,
         phenylephrine = vasopressors_type_5,
         milrinone = vasopressors_type_6,
         vasopressin = vasopressors_type_7) %>% 
  rename(dopamine_dose = vasopressors_dose_1, # renaming vasopressor dose and units for each 24h period
         dopamine_dose_units = vasopressors_units_1,
         dobutamine_dose = vasopressors_dose_2,
         dobutamine_dose_units = vasopressors_units_2,
         norepinephrine_dose = vasopressors_dose_3,
         norepinephrine_dose_units = vasopressors_units_3,
         epinephrine_dose = vasopressors_dose_4,
         epinephrine_dose_units = vasopressors_units_4,
         phenylephrine_dose = vasopressors_dose_5,
         phenylephrine_dose_units = vasopressors_units_5,
         milrinone_dose = vasopressors_dose_6,
         milrinone_units = vasopressors_units_6,
         vasopressin_dose = vasopressors_dose_7,
         vasopressin_units = vasopressors_units_7) %>% 
  rename(blood_trans_rbc = blood_trans_type_1, # rename type of blood transfusion for each 24h period
         blood_trans_ffp = blood_trans_type_2,
         blood_trans_plt = blood_trans_type_3) %>% 
  rename(sepsis_source_bacteremia = sepsis_source_0, # rename sepsis source variables
         sepsis_source_lung = sepsis_source_1,
         sepsi_source_gi = sepsis_source_2,
         sepsis_source_gu = sepsis_source_3,
         sepsis_source_cns = sepsis_source_4,
         sepsis_source_skin = sepsis_source_5,
         sepsis_source_other = sepsis_source_6,
         sepsis_source_unknown = sepsis_source_9) %>% 
  mutate(redcap_event_name = str_remove(redcap_event_name,"_arm_1")) 

# recode final diagnosis for sepsis yes/no
sepsis_pre4 <- sepsis_pre3 %>% 
  mutate(sepsis_yn_old = ifelse(final_diag==1|final_diag==2,1,0)) %>% 
  mutate(sepsis_yn_new = ifelse(final_diag_new==1|final_diag_new==2|final_diag_new==5|final_diag_new==6,1,0)) %>% 
  select(-hosp_adm_date,-hosp_adm_time,-icu_adm_date,-icu_adm_time)

# load dates dataset
roci_dates_pre1 <- read_excel("data/raw_data/ROCI DATA SA. Dates.xls")
roci_dates_pre2 <- clean_names(roci_dates_pre1) # clean names
roci_dates_pre3 <- roci_dates_pre2 %>% 
  rename(subj_id=subject_id) %>% # change variables to be in line with primary dataset
  mutate(redcap_event_name=case_when(event_name == "Baseline" ~ "baseline",
                                     event_name == "Day 0" ~ "day_0",
                                     event_name == "Day 3" ~ "day_3",
                                     event_name == "Day 7" ~ "day_7")) %>% 
  mutate(date_actual=case_when(event_name == "Baseline" ~ initial_24_hours_evaluation_starting_date,
                                  redcap_event_name != "Baseline" ~ date)) %>% 
  mutate(time_actual=case_when(event_name == "Baseline" ~ initial_24_hours_evaluation_start_time_hour_0_23,
                               redcap_event_name != "Baseline" ~ time_in_hours_0_23)) %>% 
  select(subj_id,redcap_event_name,date_actual,time_actual)

# merge dates with baseline dataset
sepsis_pre5 <- inner_join(roci_dates_pre3,sepsis_pre4) %>% 
  mutate(date_actual=as.Date(date_actual))

# import MRN
roci_mrn <- read_excel("data/raw_data/roci_mrn_2018_12_17.xlsx")

# merge dataset with mrn dataset
sepsis_pre6 <- inner_join(roci_mrn,sepsis_pre5)

# export cleaned dataset with actual dates
write_csv(sepsis_pre6,"data/clean_data/roci_clean_2018_12_17.csv")
         





