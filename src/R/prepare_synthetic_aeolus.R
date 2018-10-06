cat("start: ",as.character(Sys.time()),"\n")


# PURPOSE -----------------------------------------------------------------

#' Make synthetic AEOLUS dataset where synthetic ADRs are injected
#' 
#' These injected ADRs will serve as positive and negative control age-related ADRs
#' 
#' EXAMPLE RUN ON CLUSTER
#' ----------------------
#' 
#' Rscript prepare_synthetic_aeolus.R > prepare_synthetic_aeolus.log
#' 


# load libraries ----------------------------------------------------------

library(data.table)
library(fst)
library(tidyverse)

# set variables -----------------------------------------------------------

data_dir <- "../../data/"
out_file <- "synthetic_aeolus"

source("PhVstats.R")

# load aeolus -------------------------------------------------------------

aeolus <- read_fst(paste0(data_dir,"aeolus.fst"),as.data.table = T)

setkey(aeolus,drug_concept_id,outcome_concept_id)

age_cats <- unique(aeolus[,dev_age_cat]) %>% as.character()
ord <- order(sapply(strsplit(age_cats,"[\\[(]"),function(x){strsplit(x[2],",")[[1]][1]}) %>% as.integer())
seq_age_cats <- age_cats[ord]
seq_age_cats <- seq_age_cats[!is.na(seq_age_cats)]

age_cat_tbl <- seq_age_cats %>% 
  as_tibble() %>% 
  rename(
    age_cat = value
  ) %>% 
  mutate(
    age_cat = as.character(age_cat)
  )

# get ADRs with large counts ----------------------------------------------

adr_pairs <- aeolus[
  ,.(.N),by=.(drug_concept_id,outcome_concept_id)
  ][
  order(-N)
  ][
    N > 100
  ][
    ,.(drug_concept_id,outcome_concept_id)
  ]

rm(aeolus)

# choose from aeolus 2 positive and 2 negative drugs and 2 positive and 2 negative adverse reactions -------------------------------------------------

drugs <- adr_pairs[1:100,drug_concept_id]
positive_drug <- drugs[1:2]
negative_drug <- drugs[3:4]

outcomes <- adr_pairs[1:100,outcome_concept_id]
positive_outcome <- outcomes[1:2]
negative_outcome <- outcomes[3:4]

# make synthetic ADR count age tbls ---------------------------------------

dfs <- NULL
for(o in outcomes){
  for(d in drugs){
    n <- 50
    if((o %in% positive_outcome) & (d %in% positive_drug)){
      n <- 80
      tmp <- age_cat_tbl
      npos <- floor(n*.80)
      nother <- n - npos
      other_intervals <- cut_interval(1:nother,n = 7)
      nother_intervals <- table(other_intervals) %>% unname
      tmp$N <- c(npos,nother_intervals)
      distribution <- rep(tmp$age_cat,tmp$N)
      dfs <- rbind(dfs,
                   tibble(
                        drug_concept_id = d,
                        outcome_concept_id = o,
                        dev_age_cat = distribution,
                        control = "P"
                      )
      )
    }else if((o %in% negative_outcome) & (d %in% negative_drug)){
      n <- 100
      tmp <- age_cat_tbl
      npos <- floor(n*.90)
      nother <- n - npos
      other_intervals <- cut_interval(1:nother,n = 7)
      nother_intervals <- table(other_intervals) %>% unname
      tmp$N <- c(nother_intervals,npos)
      distribution <- rep(tmp$age_cat,tmp$N)
      dfs <- rbind(dfs,
                   tibble(
                       drug_concept_id = d,
                       outcome_concept_id = o,
                       dev_age_cat = distribution,
                       control = "N"
                     )
      )
      }else{
      tmp <- age_cat_tbl
      npos <- floor(n*(1/8))
      nother <- n - npos
      other_intervals <- cut_interval(1:nother,n = 7)
      nother_intervals <- table(other_intervals) %>% unname
      tmp$N <- c(npos,nother_intervals)
      distribution <- rep(tmp$age_cat,tmp$N)
      dfs <- rbind(dfs,
                   tibble(
                       drug_concept_id = d,
                       outcome_concept_id = o,
                       dev_age_cat = distribution,
                       control = "U"
                     )
      )
    }
  }
}
dfs <- data.table(dfs)
dfs$id <- 1:nrow(dfs)

dfs %>% 
  write_csv(paste0(data_dir,out_file,".csv"))

dfs %>% 
  fst::write_fst(paste0(data_dir,out_file,".fst"))

dfs %>% 
  feather::write_feather(paste0(data_dir,out_file,".feather"))


cat("end: ",as.character(Sys.time()))
