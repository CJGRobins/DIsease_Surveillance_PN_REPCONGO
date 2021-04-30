#This creates tables of missing data from the google sheets document we made

library(tidyverse)
library(readr)
library(readxl)
library(arsenal)
library(knitr)
library(epiDisplay)
library(gmodels)
library(tinytex)
library(googlesheets4)

setwd("~/R Model/AFP_Pointe_Noire/Missing_data")

Missing_Data_AFP <- read_sheet("https://docs.google.com/spreadsheets/d/1VSPwR330-Z58nhAFp69JzplaTLQLeAUUGQAAtZrmXBk/edit#gid=0",
                               col_types = "Cnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnn")
Missing_Data_Measles <- read_sheet("https://docs.google.com/spreadsheets/d/1VSPwR330-Z58nhAFp69JzplaTLQLeAUUGQAAtZrmXBk/edit#gid=1651164113", 
                                   sheet = "Rougeole",
                                   col_types = "Cnnnnnnnnnnnnnnnnnnnnnnnnnnnnnn")
Missing_Data_Yellow_Fever <- read_sheet("https://docs.google.com/spreadsheets/d/1VSPwR330-Z58nhAFp69JzplaTLQLeAUUGQAAtZrmXBk/edit#gid=1202999282", 
                                        sheet = "Fievre Jaune",
                                        col_types = "Cnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnn")

# Missing Data ------------------------------------------------------------

afp_table_df <- Missing_Data_AFP %>%
  dplyr::select(!c(form_id))

mns_afp <- colMeans(afp_table_df, na.rm=TRUE)
afp_table_df<- afp_table_df[,order(mns_afp)]

table_one_afp <- tableby(~., data = afp_table_df,
                     control = tableby.control(
                       test = F,
                       total = F,
                       numeric.test = "notest", cat.test = "notest",
                       cat.stats = "countpct",
                       numeric.stats = "countpct",
                       digits = 2,
                       digits.p = 3
                     ))

table_one_afp_hd <- tableby(sante_ds~., data = afp_table_df,
                         control = tableby.control(
                           test = F,
                           total = F,
                           numeric.test = "notest", cat.test = "notest",
                           cat.stats = "countpct",
                           numeric.stats = "countpct",
                           digits = 2,
                           digits.p = 3
                         ))

summary(table_one_afp)

table_one_afp_labels <- list(
  sante_ds = "Health District No",
  district = "Health District Noted",
  province = "Province",
  formation = "Formation",
  telephone_adresse = "Telephone or Address",
  `ville/village` = "Town or Village",
  lat_long = "Coordinates (often incorrect)",
  nom = "Name of Patient",
  pere_mere = "Name of Mother/Father",
  ddn = "Date of Birth",
  age = "Age",
  sexe = "Sex",
  notification_cas = "Name of person notified",
  date_not = "Date of notification",
  date_enquette = "Date of investigation",
  hospitalisation = "Hospitalisation recorded",
  numero_hosp = "Number of Hospital",
  date_hosp = "Date of Hospitalisation",
  nom_hosp = "Name of hospital recorded",
  date_paralyse = "Date of paralysation recorded",
  fievre = "Fever present",
  progressive = "Is paralysis progressive",
  flasque = "Flaccid Paralysis",
  asymetrique = "Asymetric Paralysis",
  site_paralyse = "Site of paralysis",
  sensitive_doulour = "Is the limb sensitive to pain",
  intrramusculaire = "Recent IM Injetion",
  site_injection = "Site of Injection",
  diagnostic_prob	= "Probable Diagnosis",
  reellement	= "True AFP Case?",
  nombre_vacc	= "Total number of previous vaccines",
  dates_vac_vpo = "Dates of previous VPO Vaccines",
  vpo_avs_doses = "Doses of VPO delivered via AVS",
  vpo_avs_date = "Dates of VPO delivered via AVS",
  vpo_routine_doses	= "Doses of VPO delivered via routine immunisaiton",
  vpi_avs_doses = "Doses of VPI delivered via AVS",
  vpi_routine_doses	= "Dosese of VPI delivered via routine immunisation",
  vpi_avs_date = "Dates of VPI delievered via AVS",
  source_info_av = "Source of the vaccine information",
  selles_d1	= "Date of stool sample 1",
  selles_d2 = "Date of stool sample 2",
  selles_d3 = "Date of stool sample 2"
)

table_one_afp_exp <- summary(table_one_afp,
                         labelTranslations = table_one_afp_labels,
                         title = "AFP Filled Correctly")
write2pdf(table_one_afp_exp, "AFP_correct_entry.pdf")

table_one_afp_exp_hd <- summary(table_one_afp_hd,
                             labelTranslations = table_one_afp_labels,
                             title = "AFP Filled Correctly, by Health District")
write2html(table_one_afp_exp_hd, "AFP_correct_entry_district.html")

#####################################################################################

measles_table_df <- Missing_Data_Measles %>%
  dplyr::select(!c(form_id))

mns_measles <- colMeans(measles_table_df, na.rm=TRUE)
measles_table_df<- measles_table_df[,order(mns_measles)]

table_one_measles <- tableby(~., data = measles_table_df,
                         control = tableby.control(
                           test = F,
                           total = F,
                           numeric.test = "notest", cat.test = "notest",
                           cat.stats = "countpct",
                           numeric.stats = "countpct",
                           digits = 2,
                           digits.p = 3
                         ))

table_one_measles_hd <- tableby(district~., data = measles_table_df,
                             control = tableby.control(
                               test = F,
                               total = F,
                               numeric.test = "notest", cat.test = "notest",
                               cat.stats = "countpct",
                               numeric.stats = "countpct",
                               digits = 2,
                               digits.p = 3
                             ))

summary(table_one_measles_hd)

table_one_measles_labels <- list(
  district = "Health District No",
  district_sante = "Health District",
  province = "Province",
  formation = "Formation",
  village_ville = "Town or Village",
  urban_rural = "Urban or Rural Setting",
  nom = "Name",
  mere_pere = "Name of Mother/Father",
  addresse = "Address",
  ddn = "Date of Birth",
  age = "Age",
  sexe = "Sex",
  formation_notific_date = "Formation notification date",
  district_notif_date = "District notification date",
  date_rash_debut = "Date of rash debut",
  evolution_malade = "Evolution of the illness",
  number_vaccine_dose = "Number of vaccine doses",
  date_last_vac = "Date of last measles vaccine",
  plasma_date = "Date of plasma collection",
  date_dexpedition_lab = "Date of transfer of sample to lab",
  final_classification = "Final classification",
  date_dexpedition_lab_results = "Date of final results from the lab",
  source_identifiee = "Source identified",
  igm_test = "IgM Test sent",
  igm_result = "IgM Test Result",
  notifie_nom = "Name of notifier",
  notifie_titre = "Title of Notifier",
  notifie_unite = "Unit of notifier",
  notife_adresse = "Adresse of notifier",
  notifie_telephone = "Telephone of Notifier"
)

table_one_measles_exp <- summary(table_one_measles,
                             labelTranslations = table_one_measles_labels,
                             title = "Measles Filled Correctly")
write2pdf(table_one_measles_exp, "measles_correct_entry.pdf")

table_one_measles_exp_hd <- summary(table_one_measles_hd,
                                 labelTranslations = table_one_measles_labels,
                                 title = "Measles Filled Correctly, By Health District")
write2html(table_one_measles_exp_hd, "measles_correct_entry_district.html")

#############################################################################################

Yellow_Fever_table_df <- Missing_Data_Yellow_Fever %>%
  dplyr::select(!c(form_id))

mns_Yellow_Fever <- colMeans(Yellow_Fever_table_df, na.rm=TRUE)
Yellow_Fever_table_df<- Yellow_Fever_table_df[,order(mns_Yellow_Fever)]

table_one_Yellow_Fever <- tableby(~., data = Yellow_Fever_table_df,
                             control = tableby.control(
                               test = F,
                               total = F,
                               numeric.test = "notest", cat.test = "notest",
                               cat.stats = "countpct",
                               numeric.stats = "countpct",
                               digits = 2,
                               digits.p = 3
                             ))

table_one_Yellow_Fever_hd <- tableby(district~., data = Yellow_Fever_table_df,
                                  control = tableby.control(
                                    test = F,
                                    total = F,
                                    numeric.test = "notest", cat.test = "notest",
                                    cat.stats = "countpct",
                                    numeric.stats = "countpct",
                                    digits = 2,
                                    digits.p = 3
                                  ))

summary(table_one_Yellow_Fever)

table_one_Yellow_Fever_labels <- list(
  district = "Health District Number",
  district_sante = "Health District Recorded",
  province = "Province",	
  formation = "Formation",
  adresse	 = "Address",
  village_ville = "Town or village",
  nom = "Name of patient",
  pere_mere = "Name of mother or father",
  ddn	= "Date of birth",
  age	= "Age",
  sexe = "Sex",
  nom_hopital = "Name of Hospital",
  notification_cas_par = "Who was notified about the case",
  date_notification = "Date of notification",
  date_enquete = "Date of inquiry",
  hist_fievre	= "History of fever",
  hist_fievre_date = "Date of fever",
  hist_ictere	= "History of Jaundice",
  hist_ictere_date = "Date of Jaundice",
  hist_haem	= "History of Heamorrhage",
  hist_haem_date = "Date of Haemorrhage",	
  autres_cas = "Any other cases identified",
  districts_visite = "Districts visited within last 2 weeks",
  vaccine_dose = "Number of vaccine doses",
  vaccine_date = "Last vaccination date",
  sang_date_prev_1 = "Date of first blood sample",
  sang_date_recept_lab_1 = "Date first blood sample recieved at the lab",
  classification_finale	= "Final Classification", 
  enq_nom	= "Investigator Name",
  enq_titre = "Investigator Title",
  enq_unite	= "Investigator Unit",
  enq_adresse	= "Investigator address",
  enq_tel = "Investigator Telephone Number"
)

table_one_Yellow_Fever_exp <- summary(table_one_Yellow_Fever,
                                 labelTranslations = table_one_Yellow_Fever_labels,
                                 title = "Yellow_Fever Filled Correctly")
write2pdf(table_one_Yellow_Fever_exp, "Yellow_Fever_correct_entry.pdf")

table_one_Yellow_Fever_exp_hd <- summary(table_one_Yellow_Fever_hd,
                                      labelTranslations = table_one_Yellow_Fever_labels,
                                      title = "Yellow_Fever Filled Correctly, By Health District")
write2html(table_one_Yellow_Fever_exp_hd, "Yellow_Fever_correct_entry_district.html")
