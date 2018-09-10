### data cleaning ###

## load baseline dataset prior to manipulation and renaming ##
sepsis_pre <- read_excel("data/raw_data/Sepsis Project dataset.xlsx")

sepsis_pre2 <- clean_names(sepsis_pre) %>% ## clean names
  filter(subj_id > 10) ## remove the first 10 subjects as well as the separately coded BMT subjects ##
  
 
sepsis_pre3 <- sepsis_pre2 %>% 
  rename(no_heart_disease = heart_disease_0_change_to_actual_disease, ## renaming cardiac variables 
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
         init_dopamine_dose_kg = init_vasopressors_kg_1_not_sure_how_is_this_helpful,
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
  rename(apache_temp_pts = apache1, #renaming apache point variables
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
         apache_gcs_pts = apache12)

         





