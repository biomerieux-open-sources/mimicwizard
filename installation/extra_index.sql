
-- SCHEMA HOSP

-- admissions 
CREATE INDEX IF NOT EXISTS admissions_idx_new_1 ON mimiciv_hosp.admissions USING btree (subject_id);

-- diagnoses_icd 
CREATE INDEX IF NOT EXISTS diagnoses_icd_idx_new_1 ON mimiciv_hosp.diagnoses_icd USING btree (subject_id);
CREATE INDEX IF NOT EXISTS diagnoses_icd_idx_new_2 ON mimiciv_hosp.diagnoses_icd USING btree (hadm_id);

-- drgcodes 
CREATE INDEX IF NOT EXISTS drgcodes_idx_new_1 ON mimiciv_hosp.drgcodes USING btree (subject_id);
CREATE INDEX IF NOT EXISTS drgcodes_idx_new_2 ON mimiciv_hosp.drgcodes USING btree (hadm_id);

-- emar 
CREATE INDEX IF NOT EXISTS emar_idx_new_1 ON mimiciv_hosp.emar USING btree (subject_id);
CREATE INDEX IF NOT EXISTS emar_idx_new_2 ON mimiciv_hosp.emar USING btree (hadm_id);

-- emar_detail 
CREATE INDEX IF NOT EXISTS emar_detail_idx_new_1 ON mimiciv_hosp.emar_detail USING btree (emar_id);
CREATE INDEX IF NOT EXISTS emar_detail_idx_new_2 ON mimiciv_hosp.emar_detail USING btree (subject_id);

-- hcpcsevents 
CREATE INDEX IF NOT EXISTS hcpcsevents_idx_new_1 ON mimiciv_hosp.hcpcsevents USING btree (subject_id);
CREATE INDEX IF NOT EXISTS hcpcsevents_idx_new_2 ON mimiciv_hosp.hcpcsevents USING btree (hadm_id);
CREATE INDEX IF NOT EXISTS hcpcsevents_idx_new_3 ON mimiciv_hosp.hcpcsevents USING btree (hcpcs_cd);

-- labevents 
-- CREATE INDEX IF NOT EXISTS labevents_idx_new_1 ON mimiciv_hosp.labevents USING btree (subject_id); -- DROPPED #249
CREATE INDEX IF NOT EXISTS labevents_idx_new_2 ON mimiciv_hosp.labevents USING btree (itemid);
-- CREATE INDEX IF NOT EXISTS labevents_idx_new_3 ON mimiciv_hosp.labevents USING btree (hadm_id); -- DROPPED #249
CREATE INDEX IF NOT EXISTS labevents_itemid_value_idx ON mimiciv_hosp.labevents (itemid,value); -- Trac #244
CREATE INDEX IF NOT EXISTS labevents_itemid_valuenum_idx ON mimiciv_hosp.labevents (itemid,valuenum); -- Trac #244
CREATE INDEX IF NOT EXISTS labevents_subject_id_idx ON mimiciv_hosp.labevents (subject_id,hadm_id); -- Trac #249


-- microbiologyevents 
CREATE INDEX IF NOT EXISTS microbiologyevents_idx_new_1 ON mimiciv_hosp.microbiologyevents USING btree (hadm_id);
CREATE INDEX IF NOT EXISTS microbiologyevents_idx_new_2 ON mimiciv_hosp.microbiologyevents USING btree (subject_id);

-- pharmacy 
CREATE INDEX IF NOT EXISTS pharmacy_idx_new_1 ON mimiciv_hosp.pharmacy USING btree (subject_id);
CREATE INDEX IF NOT EXISTS pharmacy_idx_new_2 ON mimiciv_hosp.pharmacy USING btree (hadm_id);

-- poe 
CREATE INDEX IF NOT EXISTS poe_idx_new_1 ON mimiciv_hosp.poe USING btree (subject_id);
CREATE INDEX IF NOT EXISTS poe_idx_new_2 ON mimiciv_hosp.poe USING btree (hadm_id);

-- poe_detail 
CREATE INDEX IF NOT EXISTS poe_detail_idx_new_1 ON mimiciv_hosp.poe_detail USING btree (poe_id);
CREATE INDEX IF NOT EXISTS poe_detail_idx_new_2 ON mimiciv_hosp.poe_detail USING btree (subject_id);

-- prescriptions 
CREATE INDEX IF NOT EXISTS prescriptions_idx_new_1 ON mimiciv_hosp.prescriptions USING btree (subject_id);
CREATE INDEX IF NOT EXISTS prescriptions_idx_new_2 ON mimiciv_hosp.prescriptions USING btree (hadm_id);

-- procedures_icd 
CREATE INDEX IF NOT EXISTS procedures_icd_idx_new_1 ON mimiciv_hosp.procedures_icd USING btree (hadm_id);
CREATE INDEX IF NOT EXISTS procedures_icd_idx_new_2 ON mimiciv_hosp.procedures_icd USING btree (subject_id);

-- services 
CREATE INDEX IF NOT EXISTS services_idx_new_1 ON mimiciv_hosp.services USING btree (hadm_id);
CREATE INDEX IF NOT EXISTS services_idx_new_2 ON mimiciv_hosp.services USING btree (subject_id);

-- transfers 
CREATE INDEX IF NOT EXISTS transfers_idx_new_1 ON mimiciv_hosp.transfers USING btree (subject_id);
CREATE INDEX IF NOT EXISTS transfers_idx_new_2 ON mimiciv_hosp.transfers USING btree (hadm_id); -- manually added 


-- SCHEMA ICU

-- chartevents 
CREATE INDEX IF NOT EXISTS chartevents_idx_new_1 ON mimiciv_icu.chartevents USING btree (itemid);
CREATE INDEX IF NOT EXISTS chartevents_subject_id_idx ON mimiciv_icu.chartevents (subject_id,hadm_id,stay_id);
CREATE INDEX IF NOT EXISTS chartevents_itemid_value_idx ON mimiciv_icu.chartevents (itemid,value); -- Trac #244
CREATE INDEX IF NOT EXISTS chartevents_itemid_valuenum_idx ON mimiciv_icu.chartevents (itemid,valuenum); -- Trac #244
CREATE INDEX IF NOT EXISTS chartevents_stay_id_idx ON mimiciv_icu.chartevents (stay_id,charttime); -- Trac #249
CREATE INDEX IF NOT EXISTS chartevents_stay_id_itemid_idx ON mimiciv_icu.chartevents (stay_id,itemid); -- Trac #364


-- datetimeevents 
CREATE INDEX IF NOT EXISTS datetimeevents_idx_new_1 ON mimiciv_icu.datetimeevents USING btree (hadm_id);
CREATE INDEX IF NOT EXISTS datetimeevents_idx_new_2 ON mimiciv_icu.datetimeevents USING btree (stay_id);
CREATE INDEX IF NOT EXISTS datetimeevents_idx_new_3 ON mimiciv_icu.datetimeevents USING btree (itemid);
CREATE INDEX IF NOT EXISTS datetimeevents_idx_new_4 ON mimiciv_icu.datetimeevents USING btree (subject_id);
CREATE INDEX IF NOT EXISTS datetimeevents_itemid_value_idx ON mimiciv_icu.datetimeevents (itemid,value); -- Trac #244

-- icustays 
CREATE INDEX IF NOT EXISTS icustays_idx_new_1 ON mimiciv_icu.icustays USING btree (hadm_id);
CREATE INDEX IF NOT EXISTS icustays_idx_new_2 ON mimiciv_icu.icustays USING btree (subject_id);
-- CREATE INDEX IF NOT EXISTS icustays_intime_idx ON mimiciv_icu.icustays (subject_id,hadm_id,intime - interval '24 hour',outtime + interval '24 hour'); -- Trac #249
CREATE INDEX IF NOT EXISTS icustays_intime_idx ON mimiciv_icu.icustays (subject_id, hadm_id, tsrange(intime - interval '24 hour', outtime + interval '24 hour')); -- Trac #249

-- ingredientevents 
CREATE INDEX IF NOT EXISTS ingredientevents_idx_new_1 ON mimiciv_icu.ingredientevents USING btree (hadm_id); -- manually added 
CREATE INDEX IF NOT EXISTS ingredientevents_idx_new_2 ON mimiciv_icu.ingredientevents USING btree (stay_id); -- manually added 
CREATE INDEX IF NOT EXISTS ingredientevents_idx_new_3 ON mimiciv_icu.ingredientevents USING btree (itemid); -- manually added 
CREATE INDEX IF NOT EXISTS ingredientevents_idx_new_4 ON mimiciv_icu.ingredientevents USING btree (subject_id); -- manually added 

CREATE INDEX IF NOT EXISTS ingredientevents_itemid_amount_idx ON mimiciv_icu.ingredientevents (itemid,amount); -- Trac #244
CREATE INDEX IF NOT EXISTS ingredientevents_itemid_rate_idx ON mimiciv_icu.ingredientevents (itemid,rate); -- Trac #244
CREATE INDEX IF NOT EXISTS ingredientevents_stay_id_idx ON mimiciv_icu.ingredientevents (stay_id,starttime); -- Trac #249


-- inputevents 
CREATE INDEX IF NOT EXISTS inputevents_idx_new_1 ON mimiciv_icu.inputevents USING btree (hadm_id);
CREATE INDEX IF NOT EXISTS inputevents_idx_new_2 ON mimiciv_icu.inputevents USING btree (stay_id);
CREATE INDEX IF NOT EXISTS inputevents_idx_new_3 ON mimiciv_icu.inputevents USING btree (itemid);
CREATE INDEX IF NOT EXISTS inputevents_idx_new_4 ON mimiciv_icu.inputevents USING btree (subject_id);
CREATE INDEX IF NOT EXISTS inputevents_itemid_amount_idx ON mimiciv_icu.inputevents (itemid,amount); -- Trac #244
CREATE INDEX IF NOT EXISTS inputevents_itemid_rate_idx ON mimiciv_icu.inputevents (itemid,rate); -- Trac #244
CREATE INDEX IF NOT EXISTS inputevents_stay_id_idx ON mimiciv_icu.inputevents (stay_id,starttime); -- Trac #249

-- outputevents 
CREATE INDEX IF NOT EXISTS outputevents_idx_new_1 ON mimiciv_icu.outputevents USING btree (hadm_id);
CREATE INDEX IF NOT EXISTS outputevents_idx_new_2 ON mimiciv_icu.outputevents USING btree (stay_id);
CREATE INDEX IF NOT EXISTS outputevents_idx_new_3 ON mimiciv_icu.outputevents USING btree (itemid);
CREATE INDEX IF NOT EXISTS outputevents_idx_new_4 ON mimiciv_icu.outputevents USING btree (subject_id);
CREATE INDEX IF NOT EXISTS outputevents_itemid_value_idx ON mimiciv_icu.outputevents (itemid,value); -- Trac #244
CREATE INDEX IF NOT EXISTS outputevents_stay_id_idx ON mimiciv_icu.outputevents (stay_id,charttime); -- Trac #249

-- procedureevents 
CREATE INDEX IF NOT EXISTS procedureevents_idx_new_1 ON mimiciv_icu.procedureevents USING btree (hadm_id);
CREATE INDEX IF NOT EXISTS procedureevents_idx_new_2 ON mimiciv_icu.procedureevents USING btree (stay_id);
CREATE INDEX IF NOT EXISTS procedureevents_idx_new_3 ON mimiciv_icu.procedureevents USING btree (itemid);
CREATE INDEX IF NOT EXISTS procedureevents_idx_new_4 ON mimiciv_icu.procedureevents USING btree (subject_id);
CREATE INDEX IF NOT EXISTS procedureevents_itemid_value_idx ON mimiciv_icu.procedureevents (itemid,value); -- Trac #244
CREATE INDEX IF NOT EXISTS procedureevents_stay_id_idx ON mimiciv_icu.procedureevents (stay_id,starttime); -- Trac #249

-- SCHEMA ED

-- diagnosis 
CREATE INDEX IF NOT EXISTS diagnosis_idx_new_1 ON mimiciv_ed.diagnosis USING btree (stay_id);

-- medrecon 
CREATE INDEX IF NOT EXISTS medrecon_idx_new_1 ON mimiciv_ed.medrecon USING btree (stay_id);

-- pyxis 
CREATE INDEX IF NOT EXISTS pyxis_idx_new_1 ON mimiciv_ed.pyxis USING btree (stay_id);

-- triage 
CREATE INDEX IF NOT EXISTS triage_idx_new_1 ON mimiciv_ed.triage USING btree (stay_id);

-- vitalsign 
CREATE INDEX IF NOT EXISTS vitalsign_idx_new_1 ON mimiciv_ed.vitalsign USING btree (stay_id);


-- SCHEMA DERIVED

-- vitalsign 
CREATE INDEX IF NOT EXISTS vitalsign_idx_new_1 ON mimiciv_derived.vitalsign USING btree (subject_id);
CREATE INDEX IF NOT EXISTS vitalsign_idx_new_2 ON mimiciv_derived.vitalsign USING btree (stay_id);

-- age 
CREATE INDEX IF NOT EXISTS age_idx_new_1 ON mimiciv_derived.age USING btree (subject_id);
CREATE INDEX IF NOT EXISTS age_idx_new_2 ON mimiciv_derived.age USING btree (hadm_id);

-- antibiotic 
CREATE INDEX IF NOT EXISTS antibiotic_idx_new_1 ON mimiciv_derived.antibiotic USING btree (subject_id);
CREATE INDEX IF NOT EXISTS antibiotic_idx_new_2 ON mimiciv_derived.antibiotic USING btree (hadm_id);
CREATE INDEX IF NOT EXISTS antibiotic_idx_new_3 ON mimiciv_derived.antibiotic USING btree (stay_id);

-- apsiii 
CREATE INDEX IF NOT EXISTS apsiii_idx_new_1 ON mimiciv_derived.apsiii USING btree (subject_id);
CREATE INDEX IF NOT EXISTS apsiii_idx_new_2 ON mimiciv_derived.apsiii USING btree (hadm_id);
CREATE INDEX IF NOT EXISTS apsiii_idx_new_3 ON mimiciv_derived.apsiii USING btree (stay_id);

-- bg 
CREATE INDEX IF NOT EXISTS bg_idx_new_1 ON mimiciv_derived.bg USING btree (subject_id);
CREATE INDEX IF NOT EXISTS bg_idx_new_2 ON mimiciv_derived.bg USING btree (hadm_id);

-- blood_differential 
CREATE INDEX IF NOT EXISTS blood_differential_idx_new_1 ON mimiciv_derived.blood_differential USING btree (subject_id);
CREATE INDEX IF NOT EXISTS blood_differential_idx_new_2 ON mimiciv_derived.blood_differential USING btree (hadm_id);

-- cardiac_marker 
CREATE INDEX IF NOT EXISTS cardiac_marker_idx_new_1 ON mimiciv_derived.cardiac_marker USING btree (subject_id);
CREATE INDEX IF NOT EXISTS cardiac_marker_idx_new_2 ON mimiciv_derived.cardiac_marker USING btree (hadm_id);

-- charlson 
CREATE INDEX IF NOT EXISTS charlson_idx_new_1 ON mimiciv_derived.charlson USING btree (subject_id);
CREATE INDEX IF NOT EXISTS charlson_idx_new_2 ON mimiciv_derived.charlson USING btree (hadm_id);

-- chemistry 
CREATE INDEX IF NOT EXISTS chemistry_idx_new_1 ON mimiciv_derived.chemistry USING btree (subject_id);
CREATE INDEX IF NOT EXISTS chemistry_idx_new_2 ON mimiciv_derived.chemistry USING btree (hadm_id);

-- coagulation 
CREATE INDEX IF NOT EXISTS coagulation_idx_new_1 ON mimiciv_derived.coagulation USING btree (subject_id);
CREATE INDEX IF NOT EXISTS coagulation_idx_new_2 ON mimiciv_derived.coagulation USING btree (hadm_id);

-- complete_blood_count 
CREATE INDEX IF NOT EXISTS complete_blood_count_idx_new_1 ON mimiciv_derived.complete_blood_count USING btree (subject_id);
CREATE INDEX IF NOT EXISTS complete_blood_count_idx_new_2 ON mimiciv_derived.complete_blood_count USING btree (hadm_id);

-- creatinine_baseline 
CREATE INDEX IF NOT EXISTS creatinine_baseline_idx_new_1 ON mimiciv_derived.creatinine_baseline USING btree (hadm_id);

-- crrt 
CREATE INDEX IF NOT EXISTS crrt_idx_new_1 ON mimiciv_derived.crrt USING btree (stay_id);

-- dobutamine 
CREATE INDEX IF NOT EXISTS dobutamine_idx_new_1 ON mimiciv_derived.dobutamine USING btree (stay_id);

-- dopamine 
CREATE INDEX IF NOT EXISTS dopamine_idx_new_1 ON mimiciv_derived.dopamine USING btree (stay_id);

-- enzyme 
CREATE INDEX IF NOT EXISTS enzyme_idx_new_1 ON mimiciv_derived.enzyme USING btree (subject_id);
CREATE INDEX IF NOT EXISTS enzyme_idx_new_2 ON mimiciv_derived.enzyme USING btree (hadm_id);

-- epinephrine 
CREATE INDEX IF NOT EXISTS epinephrine_idx_new_1 ON mimiciv_derived.epinephrine USING btree (stay_id);

-- first_day_bg 
CREATE INDEX IF NOT EXISTS first_day_bg_idx_new_1 ON mimiciv_derived.first_day_bg USING btree (subject_id);
CREATE INDEX IF NOT EXISTS first_day_bg_idx_new_2 ON mimiciv_derived.first_day_bg USING btree (stay_id);

-- first_day_bg_art 
CREATE INDEX IF NOT EXISTS first_day_bg_art_idx_new_1 ON mimiciv_derived.first_day_bg_art USING btree (subject_id);
CREATE INDEX IF NOT EXISTS first_day_bg_art_idx_new_2 ON mimiciv_derived.first_day_bg_art USING btree (stay_id);

-- first_day_gcs 
CREATE INDEX IF NOT EXISTS first_day_gcs_idx_new_1 ON mimiciv_derived.first_day_gcs USING btree (subject_id);
CREATE INDEX IF NOT EXISTS first_day_gcs_idx_new_2 ON mimiciv_derived.first_day_gcs USING btree (stay_id);

-- first_day_height 
CREATE INDEX IF NOT EXISTS first_day_height_idx_new_1 ON mimiciv_derived.first_day_height USING btree (subject_id);
CREATE INDEX IF NOT EXISTS first_day_height_idx_new_2 ON mimiciv_derived.first_day_height USING btree (stay_id);

-- first_day_lab 
CREATE INDEX IF NOT EXISTS first_day_lab_idx_new_1 ON mimiciv_derived.first_day_lab USING btree (subject_id);
CREATE INDEX IF NOT EXISTS first_day_lab_idx_new_2 ON mimiciv_derived.first_day_lab USING btree (stay_id);

-- first_day_rrt 
CREATE INDEX IF NOT EXISTS first_day_rrt_idx_new_1 ON mimiciv_derived.first_day_rrt USING btree (subject_id);
CREATE INDEX IF NOT EXISTS first_day_rrt_idx_new_2 ON mimiciv_derived.first_day_rrt USING btree (stay_id);

-- first_day_sofa 
CREATE INDEX IF NOT EXISTS first_day_sofa_idx_new_1 ON mimiciv_derived.first_day_sofa USING btree (subject_id);
CREATE INDEX IF NOT EXISTS first_day_sofa_idx_new_2 ON mimiciv_derived.first_day_sofa USING btree (hadm_id);
CREATE INDEX IF NOT EXISTS first_day_sofa_idx_new_3 ON mimiciv_derived.first_day_sofa USING btree (stay_id);

-- first_day_urine_output 
CREATE INDEX IF NOT EXISTS first_day_urine_output_idx_new_1 ON mimiciv_derived.first_day_urine_output USING btree (subject_id);
CREATE INDEX IF NOT EXISTS first_day_urine_output_idx_new_2 ON mimiciv_derived.first_day_urine_output USING btree (stay_id);

-- first_day_vitalsign 
CREATE INDEX IF NOT EXISTS first_day_vitalsign_idx_new_1 ON mimiciv_derived.first_day_vitalsign USING btree (subject_id);
CREATE INDEX IF NOT EXISTS first_day_vitalsign_idx_new_2 ON mimiciv_derived.first_day_vitalsign USING btree (stay_id);

-- first_day_weight 
CREATE INDEX IF NOT EXISTS first_day_weight_idx_new_1 ON mimiciv_derived.first_day_weight USING btree (subject_id);
CREATE INDEX IF NOT EXISTS first_day_weight_idx_new_2 ON mimiciv_derived.first_day_weight USING btree (stay_id);

-- gcs 
CREATE INDEX IF NOT EXISTS gcs_idx_new_1 ON mimiciv_derived.gcs USING btree (subject_id);
CREATE INDEX IF NOT EXISTS gcs_idx_new_2 ON mimiciv_derived.gcs USING btree (stay_id);

-- height 
CREATE INDEX IF NOT EXISTS height_idx_new_1 ON mimiciv_derived.height USING btree (subject_id);
CREATE INDEX IF NOT EXISTS height_idx_new_2 ON mimiciv_derived.height USING btree (stay_id);

-- icp 
CREATE INDEX IF NOT EXISTS icp_idx_new_1 ON mimiciv_derived.icp USING btree (subject_id);
CREATE INDEX IF NOT EXISTS icp_idx_new_2 ON mimiciv_derived.icp USING btree (stay_id);

-- icustay_detail 
CREATE INDEX IF NOT EXISTS icustay_detail_idx_new_1 ON mimiciv_derived.icustay_detail USING btree (subject_id);
CREATE INDEX IF NOT EXISTS icustay_detail_idx_new_2 ON mimiciv_derived.icustay_detail USING btree (hadm_id);
CREATE INDEX IF NOT EXISTS icustay_detail_idx_new_3 ON mimiciv_derived.icustay_detail USING btree (stay_id);

-- icustay_hourly 
CREATE INDEX IF NOT EXISTS icustay_hourly_idx_new_1 ON mimiciv_derived.icustay_hourly USING btree (stay_id);
CREATE UNIQUE INDEX IF NOT EXISTS icustay_hourly_stay_id_idx ON mimiciv_derived.icustay_hourly (stay_id,hr,endtime); -- Trac #249

-- icustay_times 
CREATE INDEX IF NOT EXISTS icustay_times_idx_new_1 ON mimiciv_derived.icustay_times USING btree (subject_id);
CREATE INDEX IF NOT EXISTS icustay_times_idx_new_2 ON mimiciv_derived.icustay_times USING btree (hadm_id);
CREATE INDEX IF NOT EXISTS icustay_times_idx_new_3 ON mimiciv_derived.icustay_times USING btree (stay_id);

-- inflammation 
CREATE INDEX IF NOT EXISTS inflammation_idx_new_1 ON mimiciv_derived.inflammation USING btree (subject_id);
CREATE INDEX IF NOT EXISTS inflammation_idx_new_2 ON mimiciv_derived.inflammation USING btree (hadm_id);

-- invasive_line 
CREATE INDEX IF NOT EXISTS invasive_line_idx_new_1 ON mimiciv_derived.invasive_line USING btree (stay_id);

-- kdigo_creatinine 
CREATE INDEX IF NOT EXISTS kdigo_creatinine_idx_new_1 ON mimiciv_derived.kdigo_creatinine USING btree (hadm_id);
CREATE INDEX IF NOT EXISTS kdigo_creatinine_idx_new_2 ON mimiciv_derived.kdigo_creatinine USING btree (stay_id);

-- kdigo_stages 
CREATE INDEX IF NOT EXISTS kdigo_stages_idx_new_1 ON mimiciv_derived.kdigo_stages USING btree (subject_id);
CREATE INDEX IF NOT EXISTS kdigo_stages_idx_new_2 ON mimiciv_derived.kdigo_stages USING btree (hadm_id);
CREATE INDEX IF NOT EXISTS kdigo_stages_idx_new_3 ON mimiciv_derived.kdigo_stages USING btree (stay_id);

-- kdigo_uo 
CREATE INDEX IF NOT EXISTS kdigo_uo_idx_new_1 ON mimiciv_derived.kdigo_uo USING btree (stay_id);

-- lods 
CREATE INDEX IF NOT EXISTS lods_idx_new_1 ON mimiciv_derived.lods USING btree (subject_id);
CREATE INDEX IF NOT EXISTS lods_idx_new_2 ON mimiciv_derived.lods USING btree (hadm_id);
CREATE INDEX IF NOT EXISTS lods_idx_new_3 ON mimiciv_derived.lods USING btree (stay_id);

-- meld 
CREATE INDEX IF NOT EXISTS meld_idx_new_1 ON mimiciv_derived.meld USING btree (subject_id);
CREATE INDEX IF NOT EXISTS meld_idx_new_2 ON mimiciv_derived.meld USING btree (hadm_id);
CREATE INDEX IF NOT EXISTS meld_idx_new_3 ON mimiciv_derived.meld USING btree (stay_id);

-- milrinone 
CREATE INDEX IF NOT EXISTS milrinone_idx_new_1 ON mimiciv_derived.milrinone USING btree (stay_id);

-- neuroblock 
CREATE INDEX IF NOT EXISTS neuroblock_idx_new_1 ON mimiciv_derived.neuroblock USING btree (stay_id);

-- norepinephrine 
CREATE INDEX IF NOT EXISTS norepinephrine_idx_new_1 ON mimiciv_derived.norepinephrine USING btree (stay_id);

-- norepinephrine_equivalent_dose 
CREATE INDEX IF NOT EXISTS norepinephrine_equivalent_dose_idx_new_1 ON mimiciv_derived.norepinephrine_equivalent_dose USING btree (stay_id);

-- oasis 
CREATE INDEX IF NOT EXISTS oasis_idx_new_1 ON mimiciv_derived.oasis USING btree (subject_id);
CREATE INDEX IF NOT EXISTS oasis_idx_new_2 ON mimiciv_derived.oasis USING btree (hadm_id);
CREATE INDEX IF NOT EXISTS oasis_idx_new_3 ON mimiciv_derived.oasis USING btree (stay_id);

-- oxygen_delivery 
CREATE INDEX IF NOT EXISTS oxygen_delivery_idx_new_1 ON mimiciv_derived.oxygen_delivery USING btree (subject_id);
CREATE INDEX IF NOT EXISTS oxygen_delivery_idx_new_2 ON mimiciv_derived.oxygen_delivery USING btree (stay_id);

-- phenylephrine 
CREATE INDEX IF NOT EXISTS phenylephrine_idx_new_1 ON mimiciv_derived.phenylephrine USING btree (stay_id);

-- rhythm 
CREATE INDEX IF NOT EXISTS rhythm_idx_new_1 ON mimiciv_derived.rhythm USING btree (subject_id);

-- rrt 
CREATE INDEX IF NOT EXISTS rrt_idx_new_1 ON mimiciv_derived.rrt USING btree (stay_id);

-- sapsii 
CREATE INDEX IF NOT EXISTS sapsii_idx_new_1 ON mimiciv_derived.sapsii USING btree (subject_id);
CREATE INDEX IF NOT EXISTS sapsii_idx_new_2 ON mimiciv_derived.sapsii USING btree (hadm_id);
CREATE INDEX IF NOT EXISTS sapsii_idx_new_3 ON mimiciv_derived.sapsii USING btree (stay_id);

-- sepsis3 
CREATE INDEX IF NOT EXISTS sepsis3_idx_new_1 ON mimiciv_derived.sepsis3 USING btree (subject_id);
CREATE INDEX IF NOT EXISTS sepsis3_idx_new_2 ON mimiciv_derived.sepsis3 USING btree (stay_id);

-- sirs 
CREATE INDEX IF NOT EXISTS sirs_idx_new_1 ON mimiciv_derived.sirs USING btree (subject_id);
CREATE INDEX IF NOT EXISTS sirs_idx_new_2 ON mimiciv_derived.sirs USING btree (hadm_id);
CREATE INDEX IF NOT EXISTS sirs_idx_new_3 ON mimiciv_derived.sirs USING btree (stay_id);

-- sofa 
CREATE INDEX IF NOT EXISTS sofa_idx_new_1 ON mimiciv_derived.sofa USING btree (stay_id);

-- suspicion_of_infection 
CREATE INDEX IF NOT EXISTS suspicion_of_infection_idx_new_1 ON mimiciv_derived.suspicion_of_infection USING btree (subject_id);
CREATE INDEX IF NOT EXISTS suspicion_of_infection_idx_new_2 ON mimiciv_derived.suspicion_of_infection USING btree (stay_id);
CREATE INDEX IF NOT EXISTS suspicion_of_infection_idx_new_3 ON mimiciv_derived.suspicion_of_infection USING btree (hadm_id);

-- urine_output 
CREATE INDEX IF NOT EXISTS urine_output_idx_new_1 ON mimiciv_derived.urine_output USING btree (stay_id);

-- urine_output_rate 
CREATE INDEX IF NOT EXISTS urine_output_rate_idx_new_1 ON mimiciv_derived.urine_output_rate USING btree (stay_id);

-- vasoactive_agent 
CREATE INDEX IF NOT EXISTS vasoactive_agent_idx_new_1 ON mimiciv_derived.vasoactive_agent USING btree (stay_id);

-- vasopressin 
CREATE INDEX IF NOT EXISTS vasopressin_idx_new_1 ON mimiciv_derived.vasopressin USING btree (stay_id);

-- ventilation 
CREATE INDEX IF NOT EXISTS ventilation_idx_new_1 ON mimiciv_derived.ventilation USING btree (stay_id);

-- ventilator_setting 
CREATE INDEX IF NOT EXISTS ventilator_setting_idx_new_1 ON mimiciv_derived.ventilator_setting USING btree (subject_id);
CREATE INDEX IF NOT EXISTS ventilator_setting_idx_new_2 ON mimiciv_derived.ventilator_setting USING btree (stay_id);

-- weight_durations 
CREATE INDEX IF NOT EXISTS weight_durations_idx_new_1 ON mimiciv_derived.weight_durations USING btree (stay_id);
