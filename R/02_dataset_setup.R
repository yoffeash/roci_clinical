### data cleaning ###

## load baseline dataset prior to manipulation and renaming ##
sepsis_pre <- read_csv("data/raw_data/RegistryOfCriticalIl_DATA_2019-02-11_1401.csv")

sepsis_pre2 <- clean_names(sepsis_pre) %>% ## clean names
  filter(subj_id > 10) ## remove the first 10 subjects as well as the separately coded BMT subjects ##
  
sepsis_pre3 <- sepsis_pre2 %>% 
  mutate(redcap_event_name = str_remove(redcap_event_name,"_arm_1")) %>% # remove _arm_1 from event name
  mutate(initial_start_date=case_when(redcap_event_name!="baseline" ~ blood_coll_date, # add other day dates to initial date column
                                      redcap_event_name=="baseline" ~ initial_start_date)) %>% 
  mutate(initial_start_time=case_when(redcap_event_name!="baseline" ~ blood_coll_time, # add other day dates to initial time column
                                      redcap_event_name=="baseline" ~ initial_start_time)) %>% 
  mutate(urine_24hr_output=case_when(redcap_event_name!="baseline" ~ as.numeric(bc_urine_24hr_output), # add urine output from other days to initial column
                                     redcap_event_name=="baseline" ~ as.numeric(urine_24hr_output))) %>% 
  mutate(init_temperature_f_high=case_when(redcap_event_name!="baseline" ~ temperature_f_high, # add temperature from other days to initial column
                                           redcap_event_name=="baseline" ~ init_temperature_f_high)) %>% 
  mutate(init_temperature_f_low=case_when(redcap_event_name!="baseline" ~ temperature_f_low, # add temperature from other days to initial column
                                          redcap_event_name=="baseline" ~ init_temperature_f_low)) %>% 
  mutate(init_systolic_high=case_when(redcap_event_name!="baseline" ~ systolic_high, # add sbp from other days to initial column
                                      redcap_event_name=="baseline" ~ init_systolic_high)) %>% 
  mutate(init_systolic_low=case_when(redcap_event_name!="baseline" ~ systolic_low, # add sbp from other days to initial column
                                     redcap_event_name=="baseline" ~ init_systolic_low)) %>% 
  mutate(init_diastolic_high=case_when(redcap_event_name!="baseline" ~ diastolic_high, # add dbp from other days to initial column
                                       redcap_event_name=="baseline" ~ init_diastolic_high)) %>% 
  mutate(init_diastolic_low=case_when(redcap_event_name!="baseline" ~ diastolic_low, # add dbp from other days to initial column
                                      redcap_event_name=="baseline" ~ init_diastolic_low)) %>% 
  mutate(mean_bp_high=case_when(redcap_event_name!="baseline" ~ as.numeric(bc_mean_bp_high), # add map from other days to initial column
                                redcap_event_name=="baseline" ~ mean_bp_high)) %>% 
  mutate(mean_bp_low=case_when(redcap_event_name!="baseline" ~ as.numeric(bc_mean_bp_low), # add map from other days to initial column
                               redcap_event_name=="baseline" ~ mean_bp_low)) %>% 
  mutate(init_vasopressors=case_when(redcap_event_name!="baseline" ~ vasopressors, # add vasopressors yes/no to initial column
                                     redcap_event_name=="baseline" ~ init_vasopressors)) %>% 
  mutate(init_vasopressors_num=case_when(redcap_event_name!="baseline" ~ vasopressors_num, # add vasopressor number to initial column
                                         redcap_event_name=="baseline" ~ init_vasopressors_num)) %>% 
  mutate(init_vasopressors_type_1=case_when(redcap_event_name!="baseline" ~ vasopressors_type_1, # vasopressor type
                                            redcap_event_name=="baseline" ~ init_vasopressors_type_1)) %>% 
  mutate(init_vasopressors_type_2=case_when(redcap_event_name!="baseline" ~ vasopressors_type_2,
                                            redcap_event_name=="baseline" ~ init_vasopressors_type_2)) %>% 
  mutate(init_vasopressors_type_3=case_when(redcap_event_name!="baseline" ~ vasopressors_type_3,
                                            redcap_event_name=="baseline" ~ init_vasopressors_type_3)) %>% 
  mutate(init_vasopressors_type_4=case_when(redcap_event_name!="baseline" ~ vasopressors_type_4,
                                            redcap_event_name=="baseline" ~ init_vasopressors_type_4)) %>% 
  mutate(init_vasopressors_type_5=case_when(redcap_event_name!="baseline" ~ vasopressors_type_5,
                                            redcap_event_name=="baseline" ~ init_vasopressors_type_5)) %>% 
  mutate(init_vasopressors_type_6=case_when(redcap_event_name!="baseline" ~ vasopressors_type_6,
                                            redcap_event_name=="baseline" ~ init_vasopressors_type_6)) %>% 
  mutate(init_vasopressors_type_7=case_when(redcap_event_name!="baseline" ~ vasopressors_type_7,
                                            redcap_event_name=="baseline" ~ init_vasopressors_type_7)) %>% 
  mutate(init_vasopressors_dose_1=case_when(redcap_event_name!="baseline" ~ vasopressors_dose_1, # vasopressor dose
                                            redcap_event_name=="baseline" ~ init_vasopressors_dose_1)) %>% 
  mutate(init_vasopressors_dose_2=case_when(redcap_event_name!="baseline" ~ vasopressors_dose_2,
                                            redcap_event_name=="baseline" ~ init_vasopressors_dose_2)) %>% 
  mutate(init_vasopressors_dose_3=case_when(redcap_event_name!="baseline" ~ vasopressors_dose_3,
                                            redcap_event_name=="baseline" ~ init_vasopressors_dose_3)) %>% 
  mutate(init_vasopressors_dose_4=case_when(redcap_event_name!="baseline" ~ vasopressors_dose_4,
                                            redcap_event_name=="baseline" ~ init_vasopressors_dose_4)) %>% 
  mutate(init_vasopressors_dose_5=case_when(redcap_event_name!="baseline" ~ vasopressors_dose_5,
                                            redcap_event_name=="baseline" ~ init_vasopressors_dose_5)) %>% 
  mutate(init_vasopressors_dose_6=case_when(redcap_event_name!="baseline" ~ vasopressors_dose_6,
                                            redcap_event_name=="baseline" ~ init_vasopressors_dose_6)) %>% 
  mutate(init_vasopressors_dose_7=case_when(redcap_event_name!="baseline" ~ vasopressors_dose_7,
                                            redcap_event_name=="baseline" ~ init_vasopressors_dose_7)) %>% 
  mutate(init_hr_high=case_when(redcap_event_name!="baseline" ~ as.numeric(hr_high), # add HR to initial column
                                redcap_event_name=="baseline" ~ as.numeric(init_hr_high))) %>% 
  mutate(init_hr_low=case_when(redcap_event_name!="baseline" ~ as.numeric(hr_low),
                               redcap_event_name=="baseline" ~ as.numeric(init_hr_low))) %>% 
  mutate(init_resp_rate_high=case_when(redcap_event_name!="baseline" ~ as.numeric(resp_rate_high), # add RR to initial column
                                       redcap_event_name=="baseline" ~ as.numeric(init_resp_rate_high))) %>% 
  mutate(init_resp_rate_low=case_when(redcap_event_name!="baseline" ~ as.numeric(resp_rate_low),
                                      redcap_event_name=="baseline" ~ as.numeric(init_resp_rate_low))) %>% 
  mutate(init_gcs=case_when(redcap_event_name!="baseline" ~ as.numeric(gcs), # add worst gcs to initial column
                            redcap_event_name=="baseline" ~ as.numeric(init_gcs))) %>% 
  add_column(gcs_high=NA,.after="init_gcs") %>% # add high gcs
  mutate(init_wbc_high=case_when(redcap_event_name!="baseline" ~ as.numeric(wbc_high), # add wbc to initial column
                                 redcap_event_name=="baseline" ~ as.numeric(init_wbc_high))) %>% 
  mutate(init_wbc_low=case_when(redcap_event_name!="baseline" ~ wbc_low,
                                redcap_event_name=="baseline" ~ init_wbc_low)) %>% 
  mutate(init_band=case_when(redcap_event_name!="baseline" ~ band, # add band to initial column
                              redcap_event_name=="baseline" ~ init_band)) %>% 
  add_column(band_low=NA,.after="init_band") %>% # add low bands
  add_column(platelets_high=NA,.before="platelets_low") %>% # add high platelets
  mutate(platelets_low=case_when(redcap_event_name!="baseline" ~ as.numeric(bc_platelets_low),
                                 redcap_event_name=="baseline" ~ as.numeric(platelets_low))) %>% 
  add_column(bun_low=NA,.after="bun") %>% 
  mutate(creatinine_high=case_when(redcap_event_name!="baseline" ~ as.numeric(bc_creatinine_high),
                                    redcap_event_name=="baseline" ~ as.numeric(creatinine_high))) %>% 
  mutate(creatitine_low=case_when(redcap_event_name!="baseline" ~ as.numeric(bc_creatinine_low),
                                  redcap_event_name=="baseline" ~ as.numeric(creatinine_low))) %>%
  mutate(lactate=case_when(redcap_event_name!="baseline" ~ as.numeric(lactic_acid),
                           redcap_event_name=="baseline" ~ as.numeric(lactate))) %>% 
  add_column(lactate_low=NA,.after="lactate") %>% 
  add_column(troponin_low=NA,.after="troponin") %>% 
  mutate(bilirubin_highest=case_when(redcap_event_name!="baseline" ~ as.numeric(bc_bilirubin_highest),
                                     redcap_event_name=="baseline" ~ as.numeric(bilirubin_highest))) %>% 
  add_column(bilirubin_low=NA,.after="bilirubin_highest") %>% 
  mutate(blood_gas_avail=case_when(redcap_event_name!="baseline" ~ art_gas_avail,
                                   redcap_event_name=="baseline" ~ blood_gas_avail)) %>% 
  mutate(po2_low=case_when(redcap_event_name!="baseline" ~ as.numeric(po2),
                           redcap_event_name=="baseline" ~ as.numeric(po2_low))) %>% 
  mutate(pco2_ap_high=case_when(redcap_event_name!="baseline" ~ as.numeric(pco2),
                                redcap_event_name=="baseline" ~ as.numeric(pco2_ap_high))) %>% 
  mutate(art_ph_ap_low=case_when(redcap_event_name!="baseline" ~ as.numeric(art_ph),
                                 redcap_event_name=="baseline" ~ as.numeric(art_ph_ap_low))) %>% 
  mutate(init_fio2_worst=case_when(redcap_event_name!="baseline" ~ as.numeric(fio2_worst),
                                   redcap_event_name=="baseline" ~ as.numeric(init_fio2_worst))) %>% 
  mutate(init_inv_ventilation=case_when(redcap_event_name!="baseline" ~ inv_vent,
                                        redcap_event_name=="baseline" ~ init_inv_ventilation)) %>% 
  mutate(init_non_inv_ventilation=case_when(redcap_event_name!="baseline" ~ non_inv_vent,
                                            redcap_event_name=="baseline" ~ init_non_inv_ventilation)) %>% 
  mutate(init_ni_type=case_when(redcap_event_name!="baseline" ~ as.numeric(ni_type),
                                redcap_event_name=="baseline" ~ as.numeric(init_ni_type))) %>% 
  mutate(init_ipap=case_when(redcap_event_name!="baseline" ~ as.numeric(ipap),
                             redcap_event_name=="baseline" ~ as.numeric(init_ipap))) %>% 
  mutate(init_epap=case_when(redcap_event_name!="baseline" ~ as.numeric(epap),
                             redcap_event_name=="baseline" ~ as.numeric(init_epap))) %>% 
  mutate(peep_ni_init_24=case_when(redcap_event_name!="baseline" ~ as.numeric(peep_ni),
                                   redcap_event_name=="baseline" ~ as.numeric(peep_ni_init_24))) %>% 
  mutate(init_fio2=case_when(redcap_event_name!="baseline" ~ as.numeric(fio2_noninv),
                             redcap_event_name=="baseline" ~ as.numeric(init_fio2))) %>% 
  mutate(peep_init_24=case_when(redcap_event_name!="baseline" ~ as.numeric(peep),
                                redcap_event_name=="baseline" ~ as.numeric(peep_init_24))) %>% 
  mutate(vent_mode2_cf0=case_when(redcap_event_name!="baseline" ~ as.numeric(vent_mode),
                                  redcap_event_name=="baseline" ~ as.numeric(vent_mode2_cf0))) %>% 
  mutate(init_inv_vent_tidal=case_when(redcap_event_name!="baseline" ~ as.numeric(vent_volume),
                                       redcap_event_name=="baseline" ~ as.numeric(init_inv_vent_tidal))) %>% 
  mutate(tidal_vol_pbw=case_when(redcap_event_name!="baseline" ~ as.numeric(vent_tidal_vol_pbw),
                                 redcap_event_name=="baseline" ~ as.numeric(tidal_vol_pbw))) %>% 
  mutate(init_supp_o2=case_when(redcap_event_name!="baseline" ~ as.numeric(supp_o2),
                                redcap_event_name=="baseline" ~ as.numeric(init_supp_o2))) %>% 
  mutate(intubated_24hr=case_when(redcap_event_name!="baseline" ~ as.numeric(blood_coll_intubated),
                                  redcap_event_name=="baseline" ~ as.numeric(intubated_24hr))) %>% 
  mutate(intub_dt_init=case_when(redcap_event_name!="baseline" ~ blood_coll_intubated_dt,
                                 redcap_event_name=="baseline" ~ intub_dt_init)) %>% 
  mutate(pf_ratio_init=case_when(redcap_event_name!="baseline" ~ pf_ratio,
                                 redcap_event_name=="baseline" ~ pf_ratio_init)) %>% 
  mutate(cvp_low=case_when(redcap_event_name!="baseline" ~ cvp,
                           redcap_event_name=="baseline" ~ cvp_low)) %>%
  mutate(trasfusion_2days=case_when(redcap_event_name!="baseline" ~ blood_trans,
                                     redcap_event_name=="baseline" ~ trasfusion_2days)) %>% 
  mutate(init_24_trans_1=case_when(redcap_event_name!="baseline" ~ blood_trans_type_1,
                                   redcap_event_name=="baseline" ~ init_24_trans_1)) %>% 
  mutate(init_24_trans_2=case_when(redcap_event_name!="baseline" ~ blood_trans_type_2,
                                   redcap_event_name=="baseline" ~ init_24_trans_2)) %>% 
  mutate(init_24_trans_3=case_when(redcap_event_name!="baseline" ~ blood_trans_type_3,
                                   redcap_event_name=="baseline" ~ init_24_trans_3)) %>% 
  select(subj_id,redcap_event_name,hosp_adm_date,hosp_adm_time,icu_adm_date,icu_adm_time,icu_source,race,age,gender,
         drug_alch_overdose,acute_resp_failure,pneumonia,copd_exac,asthma_exac,ards_transfer,heart_fail,chest_pain,
         hypotension,gi_bleed,portal_hypertension,acute_liver_fail,acute_liver_fail,acute_kidney_fail,
         altered_mental,sepsis_suspected,additional_diagnosis,
         heart_disease_0,heart_disease_1,heart_disease_2,heart_disease_3,heart_disease_4,heart_disease_5,
         diabetes,copd,asthma,osha,liver_disease,chronic_kidney_disease,substance_abuse,morbid_obese,
         malignancy,cancer_type,immunosupression,immuno_meds,bone_marrow,smoking_avail,smoking_hist,pack_years,
         elective_operation,emergency_operation,know_chronic_hypoxia,chronic_kidney_disease,chronic_renal_replace,additional_history,
         initial_start_date,initial_start_time,weight_kg,pbw,height_cm,bmi,
         urine_24hr_output,init_temperature_f_high,init_temperature_f_low,init_systolic_high,init_systolic_low,mean_bp_high,mean_bp_low,
         init_vasopressors,init_vasopressors_num,
         init_vasopressors_type_1,init_vasopressors_type_2,init_vasopressors_type_3,init_vasopressors_type_4,init_vasopressors_type_5,init_vasopressors_type_6,init_vasopressors_type_7,
         init_vasopressors_dose_1,init_vasopressors_dose_2,init_vasopressors_dose_3,init_vasopressors_dose_4,init_vasopressors_dose_5,init_vasopressors_dose_6,init_vasopressors_dose_7,
         init_vasopressors_kg_1,init_vasopressors_kg_2,init_vasopressors_kg_3,init_vasopressors_kg_4,init_vasopressors_kg_5,init_vasopressors_kg_6,init_vasopressors_kg_7,
         init_hr_high,init_hr_low,init_resp_rate_high,init_resp_rate_low,init_gcs,gcs_high,intubated_24hr,intub_dt_init,intubated_lowest_rr,intubated_highest_rr,
         hct_high,hct_low,init_wbc_high,init_wbc_low,init_band,band_low,platelets_high,platelets_low,sodium_high,sodium_low,potassium_high,potassium_low,
         bun,bun_low,creatinine_high,creatinine_low,glucose_high,glucose_low,bicarbonate_high,bicarbonate_low,albumin_high,albumin_low,bilirubin_highest,bilirubin_low,
         lactate,lactate_low,troponin,troponin_low,inr,ptt,pt_ini,blood_gas_avail,po2_high,po2_low,art_co2_lowo2,pco2_ap_high,pco2_ap_low,art_ph_ap_high,art_ph_ap_low,
         cvp_measured,cvp_high,cvp_low,trasfusion_2days,init_24_trans_1,init_24_trans_2,init_24_trans_3,
         init_inv_ventilation,vent_mode2_cf0,init_fio2_worst,init_inv_vent_tidal,peep_init_24,tidal_vol_pbw,
         init_non_inv_ventilation,init_ni_type,init_ipap,init_epap,peep_ni_init_24,init_fio2,init_supp_o2,pf_ratio_init,obs_init_24hrs,
         blood_coll_loc,vasopressors_units_1,vasopressors_units_2,vasopressors_units_3,vasopressors_units_4,vasopressors_units_5,vasopressors_units_6,vasopressors_units_7,
         o2_sat,mech_vent,renal_repl,
         cxr_avail1,cxr_date1,cxr_time1,cxr_reviewed1,cxr_reviewed_date1,cxr_quadrant1_1,cxr_quadrant2_1,cxr_quadrant3_1,cxr_quadrant4_1,ct_avail1,ct_date1,
         cxr_avail2,cxr_date2,cxr_time2,cxr_reviewed2,cxr_reviewed_date2,cxr_quadrant1_2,cxr_quadrant1_2,cxr_quadrant3_2,cxr_quadrant4_2,ct_avail2,ct_date2,
         septic_shock,final_diag,final_diag_new,sepsis_sev,final_diag_ards,final_diag_ards_proned,final_diag_ards_block,final_diag_ards_vaso,final_diag_ards_vaso_type,
         sepsis_source_0,sepsis_source_1,sepsis_source_2,sepsis_source_3,sepsis_source_4,sepsis_source_5,sepsis_source_6,sepsis_source_9,sepsis_source_9,sepsis_source_oth,
         bacteremic_culture,bacteremic_culture_type_1,bacteremic_culture_type_2,bacteremic_culture_type_3,bacteremic_culture_type_spec,
         sputum_culture,sputum_culture_type_1,sputum_culture_type_2,sputum_culture_type_3,sputum_culture_type_spec,
         gi_culture,gi_culture_type_1,gi_culture_type_2,gi_culture_type_3,gi_culture_type_4,gi_culture_type_spec,
         urine_culture,urine_culture_type_1,urine_culture_type_2,urine_culture_type_3,urine_culture_type_4,urine_culture_type_spec,
         spinal_culture,spinal_culture_type_2,spinal_culture_type_3,spinal_culture_type_spec,
         other_culture,other_culture_spec,other_culture_type_1,other_culture_type_2,other_culture_type_3,other_culture_type_4,other_culture_type_spec,
         mech_vent_hours,disch_status,icu_disch_date,hosp_disch_date,disch_to,in_hosp_mort,death_date,death_cause,death_comfort,death_28,death_60) %>%
  rename(no_heart_disease = heart_disease_0, ## renaming cardiac variables 
         coronary_artery_disease = heart_disease_1, 
         congestive_heart_failure = heart_disease_2,
         arrhythmia = heart_disease_3,
         valvular_disease = heart_disease_4,
         other_heart_disease = heart_disease_5) %>% 
  rename(start_date=initial_start_date,start_time=initial_start_time,
         temperature_f_high=init_temperature_f_high,temperature_f_low=init_temperature_f_low,
         systolic_high=init_systolic_high,systolic_low=init_systolic_low,diastolic_high=init_diastolic_high,diastolic_low=init_diastolic_low,
         vasopressors=init_vasopressors,vasopressors_num=init_vasopressors_num) %>% 
  rename(dopamine = init_vasopressors_type_1, # renaming initial vasopressor type
         dobutamine = init_vasopressors_type_2,
         norepinephrine = init_vasopressors_type_3,
         epinephrine = init_vasopressors_type_4,
         phenylephrine = init_vasopressors_type_5,
         milrinone = init_vasopressors_type_6,
         vasopressin = init_vasopressors_type_7) %>% 
  rename(dopamine_dose = init_vasopressors_dose_1, # renaming initial vasopressor dose
         dopamine_dose_kg = init_vasopressors_kg_1,
         dobutamine_dose = init_vasopressors_dose_2,
         dobutamine_dose_kg = init_vasopressors_kg_2,
         norepinephrine_dose = init_vasopressors_dose_3,
         norepinephrine_dose_kg = init_vasopressors_kg_3,
         epinephrine_dose = init_vasopressors_dose_4,
         epinephrine_dose_kg = init_vasopressors_kg_4,
         epinephrine_dose = init_vasopressors_dose_4,
         epinephrine_dose_kg = init_vasopressors_kg_4,
         phenylephrine_dose = init_vasopressors_dose_5,
         phenylephrine_dose_kg = init_vasopressors_kg_5,
         milrinone_dose = init_vasopressors_dose_6,
         milrinone_dose_kg = init_vasopressors_kg_6,
         vasopressin_dose = init_vasopressors_dose_7,
         vasopressin_dose_kg = init_vasopressors_kg_7) %>% 
  rename(trans_rbc = init_24_trans_1, # renaming transfusion type variables
         trans_ffp = init_24_trans_2,
         trans_plt = init_24_trans_3) %>% 
  
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



# recode final diagnosis for sepsis yes/no
sepsis_pre4 <- sepsis_pre3 %>% 
  mutate(sepsis_yn_old = ifelse(final_diag==1|final_diag==2,1,0)) %>% 
  mutate(sepsis_yn_new = ifelse(final_diag_new==1|final_diag_new==2|final_diag_new==5|final_diag_new==6,1,0)) 

# load dates dataset
# roci_dates_pre1 <- read_excel("data/raw_data/ROCI DATA SA. Dates.xls")
# roci_dates_pre2 <- clean_names(roci_dates_pre1) # clean names
# roci_dates_pre3 <- roci_dates_pre2 %>% 
#  rename(subj_id=subject_id) %>% # change variables to be in line with primary dataset
#  mutate(redcap_event_name=case_when(event_name == "Baseline" ~ "baseline",
#                                     event_name == "Day 0" ~ "day_0",
#                                     event_name == "Day 3" ~ "day_3",
#                                     event_name == "Day 7" ~ "day_7")) %>% 
#  mutate(date_actual=case_when(event_name == "Baseline" ~ initial_24_hours_evaluation_starting_date,
#                                  redcap_event_name != "Baseline" ~ date)) %>% 
#  mutate(time_actual=case_when(event_name == "Baseline" ~ initial_24_hours_evaluation_start_time_hour_0_23,
#                               redcap_event_name != "Baseline" ~ time_in_hours_0_23)) %>% 
#  select(subj_id,redcap_event_name,date_actual,time_actual)

# merge dates with baseline dataset
# sepsis_pre5 <- inner_join(roci_dates_pre3,sepsis_pre4) %>% 
#  mutate(date_actual=as.Date(date_actual))

# import MRN
roci_mrn <- read_excel("data/raw_data/roci_mrn_2018_12_17.xlsx")

# merge dataset with mrn dataset
sepsis_pre6 <- inner_join(roci_mrn,sepsis_pre4)

# export cleaned dataset with actual dates
write_csv(sepsis_pre6,"data/clean_data/roci_clean_2019_02_12.csv")
         





