cat("start: ",as.character(Sys.time()))


# PURPOSE -----------------------------------------------------------------

#' Compute PhV statistics for synhetic aeolus
#' 
#' analagous to PhV_of_ADRs.R script but using synthetic dataset
#' 
#' EXAMPLE RUN ON CLUSTER:
#' ---------------------
#' 
#' Rscript PhV_of_ADRs_using_vocab_taxonomy_synthetic_aeolus.R > PhV_of_ADRs_using_vocab_taxonomy_synthetic_aeolus.log
#' 
# set variables -----------------------------------------------------------

data_dir <- "../../data/"

aeolus_file <- "synthetic_aeolus"

group <- "dev_age_cat"

# load libraries ----------------------------------------------------------

library(tidyverse)
library(data.table)
library(doParallel)
registerDoParallel(cores=2)
library(fst)

# load PhV drug and outcome hierarchy functions ------------------------------------------------

source("PhV_drug_outcome_hierarchy_functions.R")

# load aeolus -------------------------------------------------------------

aeolus <- read_fst(paste0(data_dir,aeolus_file,".fst"),
                   columns=c("id","drug_concept_id","outcome_concept_id","dev_age_cat","control"),
                   as.data.table = T) %>% na.omit() %>% unique()

setkey(aeolus,drug_concept_id,outcome_concept_id)
adrs <- unique(aeolus[, .(drug_concept_id,outcome_concept_id)])
cat(paste0(format(nrow(adr_pairs),big.mark=",")," AEDARs"))

# set table for covariate -------------------------------------------------

cov_table <- aeolus[,.N,by=.(eval(as.name(group)))][,.(as.name)] %>% 
  as_tibble() %>% 
  mutate(
    !!group := as.character(as.name)
  ) %>% 
  select(-as.name)


# load PhV stats functions ------------------------------------------------

source("PhVstats.R")


# set ATC class -----------------------------------------------------------

atc_class <- "ATC 5th"

# Set starting vector for collecting statistics ---------------------------

start_vec <- c(
  "num_drugs_and_descendants" = 0,
  "num_outcomes_and_descendants" = 0,
  "a" = 0, 
  "b" = 0, 
  "c" = 0, 
  "d" = 0, 
  "RRR" = 0, 
  "ROR" = 0, 
  "PRR" = 0)


# calculate PhV statistics ------------------------------------------------

# SET KEYS in drug hierarchy
setkey(drug_hierarchy,descendant_concept_id)

# SET KEYS in outcome hierarchy
setkey(outcome_hierarchy,ancestor_concept_id)

# SET KEYS in AEOLUS
setkey(aeolus,drug_concept_id,outcome_concept_id)

df <- foreach(i=1:nrow(adrs),
        .combine='rbind') %dopar% {
  
  adr <- adrs[i]
  
  drug_id <- adr[,drug_concept_id]
  
  drug_name <- rxnormdrugid2name(drug_id,keyed=T)
  
  target_drugs <- drug_id #same vocabulary as ADR dataset drug
  
  outcome_id <- adr[,outcome_concept_id]
  
  outcome_name <- snomedoutcomeid2name(outcome_id,keyed=T)
  
  target_outcomes <- get_snomedoutcomeid_descendants(outcome_id,keyed=T) #same vocabulary as ADR dataset outcome
  
  #START FOR TRANSFER#
  #NEED:
  # aeolus, 
  # ancestor drug id,name,atc_class & outcome id and name
  # target_drugs,target_outcomes e.g. descendants of ancestor
  # cov_table e.g. age_cat_tbl, make sure age_cat=="(25,100]"
  # ends with tmp_norm
  ###################
  # make contigency table of drug-outcome pair grouped by the covariate
  count_dt <- make_count_dt(aeolus,target_drugs,target_outcomes,group=group,cov_table = cov_table)
  
  ####################
  # bind previous dataframe with example given and comput age independent statistics
  tmp <- make_PhV_from_counts_dt(count_dt,
                                 drug=drug_id,
                                 drug_name=drug_name,
                                 drug_class="ATC 5th",
                                 outcome = outcome_id,
                                 outcome_name = outcome_name,
                                 target_drugs = target_drugs,
                                 target_outcomes = target_outcomes,
                                 group=group,
                                 cov_table = cov_table)
  
  ####################
  # Further transformations - getting ratio relative to adult values
  
  young_vs_adult_dev_age_cat(tmp)
  
  
}

# output ------------------------------------------------------------------

# OUTPUTTING DATA TO IT'S OWN FILE WITHIN IT'S OWN FOLDER
out_dir <- ""
out_dir <- paste0(data_dir,"ADRs_PhV/")
if(!dir.exists(out_dir)){
  dir.create(out_dir, showWarnings = F)
}
out_dir <- paste0(out_dir,"Synthetic/")
if(!dir.exists(out_dir)){
  dir.create(out_dir, showWarnings = F)
}


filename <- paste0("PhV_of_ADRs_using_SNOMED_taxonomy_stratified_by_",group,".csv")

df %>% 
  write_csv(paste0(out_dir,filename))



cat("end: ",as.character(Sys.time()))
