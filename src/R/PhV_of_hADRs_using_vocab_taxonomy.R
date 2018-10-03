cat("start: ",as.character(Sys.time()))


# PURPOSE -----------------------------------------------------------------

#' Compute PhV statistics for adverse reactions to all drugs in ATC encoded drug classes taking into account taxonomic information
#' 
#' Calculates both ADRs and hierarchical ADRs, including all descendants
#' 
#' EXAMPLE RUN ON MUNNIN:
#' ---------------------
#' 
#' 1st parameter : ANY ATC CLASS NAME OR RXNORM INGREDIENT NAME
#' 2nd parameter : [ 1, 2, 3, 4, 5 ] WILL MAP TO CONCEPT_CLASS_ID. 
#' 3rd parameter : covariate in aeolus (column name to stratify by)
#' 
#' source ~/Scripts/bash/submit_Rjob_3arg.sh PhV_of_hADRs_using_vocab_taxonomy.R "ANTIEPILEPTICS" 3 "dev_age_cat"
#' 
#' 
#' EXAMPLE RUN ON LOCAL:
#' ---------------------
#' 
#' Rscript PhV_of_hADRs_using_vocab_taxonomy.R "ANTIEPILEPTICS" 3 "dev_age_cat"

# set variables -----------------------------------------------------------

args = commandArgs(trailingOnly=TRUE)

if (length(args)==0) {
  stop("At least one argument must be supplied", call.=FALSE)
} else if (length(args)==2) {
  cat("\nDrug/Drug Class: ",args[1]," and ATC Class level: ",args[2]," and stratification variable: ",args[3],"\n")
}


data_dir <- "../../data/"

atc_name <- args[1] 
atc_class_int <- as.integer(args[2]) 
group <- args[3]

#atc_name="ANTIEPILEPTICS"
#atc_class_int <- 3
#group <- "dev_age_cat"

# load libraries ----------------------------------------------------------

library(tidyverse)
library(data.table)
library(doParallel)
registerDoParallel(cores=25)
library(fst)

# load PhV drug and outcome hierarchy functions -- loads those hierarchies ------------------------------------------------

source("PhV_drug_outcome_hierarchy_functions.R")

# get atc class name from given int ---------------------------------------

atc_class <- atcint2name(atc_class_int)

# get all drugs in provided drug class ------------------------------------

drugs_to_limit_by <- atcname2rxnormdrugs(atc_name,atc_class)

# load aeolus -------------------------------------------------------------

aeolus <- read_fst(paste0(data_dir,"aeolus.fst"),
                   columns=c("id","drug_concept_id",
                             "outcome_concept_id","dev_age_cat"),
                   as.data.table = T) %>% na.omit() %>% unique()

# set table for covariate -------------------------------------------------

cov_table <- aeolus[,.N,by=.(eval(as.name(group)))][,.(as.name)] %>% 
  as_tibble() %>% 
  mutate(
    !!group := as.character(as.name)
  ) %>% 
  select(-as.name)


# get all ADRs using drugs in drug class ----------------------------------

#Need to get all ADR pairs in aeolus
adr_pairs <- unique(aeolus[drug_concept_id %in% drugs_to_limit_by,
                           .(.N),
                           by=.(drug_concept_id,outcome_concept_id)][
                             N>=nrow(cov_table), #limited to ADRs with counts more than 50
                             .(drug_concept_id,outcome_concept_id)
                             ]
)

cat(paste0(format(nrow(adr_pairs),big.mark=",")," AEDARs"))

# get all hADRs using the ADRs --------------------------------------------

# hADRs are ADRs that have their reaction generalized out using the outcome hierarchy (is_a relationships only). This way more general ADRs, which may increase the sample size, are considered.

hadr_pairs <- unique(
  merge(adr_pairs,outcome_hierarchy,
        by.x="outcome_concept_id",by.y="descendant_concept_id")[
          ,.(drug_concept_id,outcome_concept_id = ancestor_concept_id)]
  )

cat(paste0(format(nrow(hadr_pairs),big.mark=",")," AEDARs"))

# load PhV stats functions ------------------------------------------------

source("PhVstats.R")

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

df <- foreach(i=1:nrow(hadr_pairs),
        .combine='rbind') %dopar% {
  
  adr <- hadr_pairs[i]
  
  drug_id <- adr[,drug_concept_id]
  
  drug_name <- rxnormdrugid2name(drug_id,keyed=T)
  
  target_drugs <- drug_id #same vocabulary as ADR dataset drug
  
  outcome_id <- adr[,outcome_concept_id]
    
  outcome_name <- snomedoutcomeid2name(outcome_id,keyed=T)
  
  target_outcomes <- get_snomedoutcomeid_descendants(outcome_id,keyed=T) #same vocabulary as ADR dataset outcome
  
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
out_dir <- paste0(out_dir,"ATC/")
if(!dir.exists(out_dir)){
  dir.create(out_dir, showWarnings = F)
}
out_dir <- paste0(out_dir,gsub(" ","_",atc_class),"/")
if(!dir.exists(out_dir)){
  dir.create(out_dir, showWarnings = F)
}
out_dir <- paste0(out_dir,atc_name,"/")
if(!dir.exists(out_dir)){
  dir.create(out_dir, showWarnings = F)
}
out_dir <- paste0(out_dir,"ALL_DRUGS/")
if(!dir.exists(out_dir)){
  dir.create(out_dir, showWarnings = F)
}


filename <- paste0("PhV_of_hADRs_using_SNOMED_taxonomy_stratified_by_",group,".csv")

df %>% 
  write_csv(paste0(out_dir,filename))



cat("end: ",as.character(Sys.time()))
