cat("start: ",as.character(Sys.time()))


# PURPOSE -----------------------------------------------------------------

#' To retrieve young age category biased ADRs computed from PhV_of_ADrugClassRs_using_vocab_taxonomy
#' 
#' EXAMPLE RUN ON LOCAL:
#' ---------------------
#' 
#' Rscript select_young_age_category_biased_ADRs_control_detection_synthetic_aeolus "Synthetic"
#' 
#' Output:
#' 
#' confusion matrix
#' 


# load libraries ----------------------------------------------------------

library(tidyverse)
library(data.table)

# set variables -----------------------------------------------------------

args = commandArgs(trailingOnly=TRUE)
type = args[1]
#type="Synthetic"
data_dir <- "../../data/"
file <- paste0(data_dir,"ADRs_PhV/",type,"/PhV_of_ADRs_using_SNOMED_taxonomy_stratified_by_dev_age_cat.csv")

# make sure I identify the positive control in simulated_aeolus -----------------------------------------------------------

phv <- read_csv(file) %>% 
  setDT()

setkey(phv,drug_concept_id,outcome_concept_id)

control_aeolus <- read_csv(paste0(data_dir,tolower(type),"_aeolus.csv")) %>% setDT()

setkey(control_aeolus,drug_concept_id,outcome_concept_id)

pos_adrs <- unique(control_aeolus[control=="P", .(drug_concept_id,outcome_concept_id) ])

neg_adrs <- unique(control_aeolus[control=="N", .(drug_concept_id,outcome_concept_id) ])

source("PhV_filters.R")


# filters -----------------------------------------------------------------

pos_filter <- young_bias_condition_prr_norm_mse
neg_filter <- adult_bias_condition_prr_mse
sanity <- sanity_check_condition

# TPs ---------------------------------------------------------------------

tp <- phv %>% 
  inner_join(pos_adrs) %>% 
  group_by(drug_concept_id,outcome_concept_id) %>% 
  filter_(pos_filter) %>% 
  filter_(sanity) %>% 
  ungroup() %>% 
  select(drug_concept_id,outcome_concept_id) %>% 
  distinct() %>% 
  nrow()


# FPs ---------------------------------------------------------------------

fp <- phv %>% 
  inner_join(neg_adrs) %>% 
  group_by(drug_concept_id,outcome_concept_id) %>% 
  filter_(pos_filter) %>% 
  filter_(sanity) %>% 
  ungroup() %>% 
  select(drug_concept_id,outcome_concept_id) %>% 
  distinct() %>% 
  nrow()

# FNs ---------------------------------------------------------------------


fn <- phv %>% 
  inner_join(neg_adrs) %>% 
  group_by(drug_concept_id,outcome_concept_id) %>% 
  filter_(pos_filter) %>% 
  filter_(sanity) %>% 
  ungroup() %>% 
  select(drug_concept_id,outcome_concept_id) %>% 
  distinct() %>% 
  nrow()


# TNs ---------------------------------------------------------------------

tn <- phv %>% 
  inner_join(neg_adrs) %>% 
  group_by(drug_concept_id,outcome_concept_id) %>% 
  filter_(neg_filter) %>% 
  filter_(sanity) %>% 
  ungroup() %>% 
  select(drug_concept_id,outcome_concept_id) %>% 
  distinct() %>% 
  nrow()


# CONFUSION MATRIX --------------------------------------------------------

matrix(c(tp,fn,fp,tn),byrow = T, nrow=2,ncol=2,dimnames=list(c("T","F"),c("P","N")))


cat("end: ",as.character(Sys.time()))
