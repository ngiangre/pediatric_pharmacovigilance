cat("start: ",as.character(Sys.time()),"\n")


# PURPOSE -----------------------------------------------------------------

#' To retrieve young age category biased ADRs computed from PhV_of_ADR scripts. Also evaluates ADRs for confounding/association with covariates.
#' 
#' 
#' EXAMPLE RUN ON CLUSTER
#' --------------------
#' 
#' Rscript select_young_age_category_biased_ADRs_dev.R ../../data/ADRs_PhV/ATC/ATC_3rd/ANTIEPILEPTICS/ALL_DRUGS/PhV_of_ADRs.csv 
#' 


# load libraries ----------------------------------------------------------

library(tidyverse)
library(data.table)
library(fst)

# set variables -----------------------------------------------------------

data_dir <- "../../data/"

args = commandArgs(trailingOnly=TRUE)
cat("File: ",args[1])
file=args[1]
#file="../../data/ADRs_PhV/ATC/ATC_3rd/ANTIEPILEPTICS/ALL_DRUGS/PhV_of_ADRs_stratified_by_dev_age_cat.csv"
filename=basename(file)
dir=dirname(file)
dirs <- strsplit(dir,"/")[[1]]

source("PhV_filters.R")

# get young biased adrs from a dataset-----------------------------------------------------------

pos_filter <- young_bias_condition_prr_norm_mse
sanity <- sanity_check_condition

suppressWarnings(phv_of_adrs <- read_csv(file) %>% 
  setDT())

setkey(phv_of_adrs,drug_concept_id,outcome_concept_id)


phv_of_adrs_young_biased <- phv_of_adrs[,.SD[eval(parse(text=pos_filter)) &
                      eval(parse(text=sanity))],
                    by= .(drug_concept_name,outcome_concept_name)]

cat(paste0("\nThere are ",
           nrow(unique(phv_of_adrs_young_biased[,.(drug_concept_id,outcome_concept_id)])),
           " young age category biased ADRs\n"))


# get candidate ADRs ------------------------------------------------------

young_candidates <- unique(phv_of_adrs_young_biased[,.(drug_concept_id,outcome_concept_id)])

# Load aeolus for subsequent covariate testing ----------------------------

aeolus <- read_fst(paste0(data_dir,"aeolus.fst"),
                   as.data.table = T) %>% na.omit() %>% unique()

setkey(aeolus,drug_concept_id,outcome_concept_id)

source("PhVstats.R")


# covariate testing with poisson test --------------------------------------------------------------------

cat("\nCovariate testing\n")

covs <- c("gender_code","report_year","age_code")
covariatetests <- list()
for(group in covs){
  if(group=="gender_code"){
    cov_df <- aeolus[,.N,by=.(eval(as.name(group)))][,.(as.name)][as.name %in% c("M","F")]
  }else{
    cov_df <- aeolus[,.N,by=.(eval(as.name(group)))][,.(as.name)]
  }
  covtests <- NULL
  for(i in 1:nrow(young_candidates)){
    drug <- young_candidates[i,drug_concept_id]
    outcome <- young_candidates[i,outcome_concept_id]
    
    
    count_dt <- merge(cov_df,aeolus[
        .(drug,outcome),.N,by=.(eval(as.name(group)))],
        by="as.name",all.x=T)
    
    count_dt[is.na(count_dt)] <- 0
    
    lst <- map2(count_dt$N,mean(count_dt$N),poisson.test)
    
    tmp <- cbind(drug_concept_id = drug,
                 outcome_concept_id = outcome,
                 cov_df, 
                 poistest_lwr = sapply(lst,function(x){x$conf.int[1]}),
                 poistest_upr = sapply(lst,function(x){x$conf.int[2]}))
    
    covtests <- rbind(covtests,tmp)
  }
  
  covariatetests[[group]] <- covtests
  
}

pois_filter <- "any((poistest_lwr<1 & poistest_upr>1) | (poistest_lwr>1 & poistest_upr<1))"

covariatetests_filtered <- lapply(
  covariatetests,function(x){
    x[,
      .SD[eval(parse(text=pois_filter))],
      by=.(drug_concept_id,outcome_concept_id)
      ]
    }
  )

young_candidates_againstcovariates <- unique(
  do.call(
    bind_rows,
    covariatetests_filtered)[
      ,.(drug_concept_id,outcome_concept_id)
      ]
  )

# all candidate ADRs with minimal confounding --------------------------------------------

young_candidates_w_minimal_confounding <- young_candidates[young_candidates_againstcovariates,
                                               on=c("drug_concept_id","outcome_concept_id")] 
cat(paste0("\nNumber of ADRs with minimal covariate confounding: ",nrow(young_candidates_w_minimal_confounding),"\n"))

young_candidates_w_confounding <- young_candidates[!young_candidates_w_minimal_confounding,
                                       on=c("drug_concept_id","outcome_concept_id")]

cat(paste0("\nNumber of ADRs with observed covariate confounding: ",nrow(young_candidates_w_confounding),"\n"))


# output ------------------------------------------------------------------

phv_of_adrs$id <- 1:nrow(phv_of_adrs)

ids <- phv_of_adrs[young_candidates_w_minimal_confounding,id]

log <- phv_of_adrs$id %in% ids

phv_of_adrs$candidate <- log

phv_of_adrs %>% 
  write_csv(paste0(dir,"/",strsplit(filename,"\\.")[[1]][1],"_candidates_annotated.csv"))




cat("end: ",as.character(Sys.time()))
