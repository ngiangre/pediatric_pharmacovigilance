
# PURPOSE -----------------------------------------------------------------

#' Helper functions for computations over taxonomy hierarchies
#' 



# load libraries ----------------------------------------------------------

library(fst)
library(data.table)
library(tidyverse)

# set variables -----------------------------------------------------------

data_dir <- "../../data/"

# Load SNOMED and RxNorm-ATC hierarchies -------------------------------------------------

cols <- c("ancestor_concept_id",
          "ancestor_concept_name",
          "ancestor_concept_class_id",
          "descendant_concept_id",
          "descendant_concept_name",
          "descendant_concept_class_id")


outcome_hierarchy <- read_fst(paste0(data_dir,"SNOMED_is_a_hierarchy.fst"),as.data.table = T,
                              columns = cols )
for(k in cols[c(1,4)])set(outcome_hierarchy, j = k, value = as.integer(outcome_hierarchy[[k]]))



drug_hierarchy <- read_fst(paste0(data_dir,
                                  "ATC_RxNorm_hierarchy.fst"),
                           as.data.table = T,
                           columns = c("ancestor_concept_id",
                                       "ancestor_concept_name",
                                       "ancestor_concept_class_id",
                                       "descendant_concept_id",
                                       "descendant_concept_name",
                                       "descendant_concept_class_id"))

for(k in cols[c(1,4)])set(drug_hierarchy, j = k, value = as.integer(drug_hierarchy[[k]]))


# ATC class int to name map -----------------------------------------------

atc_map <- list("ATC 5th" = 5, 
                "ATC 4th" = 4, 
                "ATC 3rd" = 3, 
                "ATC 2nd" = 2, 
                "ATC 1st" = 1)


# drug and outcome hierarchy functions----------------------------------------------

atcint2name <- function(atc_int){
  
  names(atc_map)[ atc_map %in% atc_int]
  
}

atcname2rxnormdrugs <- function(atc_name,atc_class){
  
  drug_hierarchy[ancestor_concept_name==atc_name &
                   ancestor_concept_class_id==atc_class &
                   descendant_concept_class_id=="Ingredient",
                 descendant_concept_id]
  
}

#' drug_id in descendant_concept_id column
rxnormdrugid2name <- function(drug_id,keyed=F){
  
  if(keyed){
    drug_hierarchy[.(drug_id),
                   unique(descendant_concept_name)]
    }else{
      drug_hierarchy[descendant_concept_id==drug_id,
                 unique(descendant_concept_name)]
    }
  
}

#' drug_id in descendant_concept_id column
rxnormdrugname2id <- function(drug_name,keyed=F){
  
  if(keyed){
    drug_hierarchy[.(drug_name),
                   unique(descendant_concept_id)]
  }else{
    drug_hierarchy[descendant_concept_name==drug_name,
                   unique(descendant_concept_id)]
  }
  
}

#' outcome_id in ancestor_concept_id column
snomedoutcomeid2name <- function(outcome_id,keyed=F){
  
  if(keyed){
    outcome_hierarchy[.(outcome_id),
                      unique(ancestor_concept_name)]
    }else{
      outcome_hierarchy[ancestor_concept_id==outcome_id,
                    unique(ancestor_concept_name)]
    }
  
}

#' outcome_id in ancestor_concept_id column
get_snomedoutcomeid_descendants <- function(outcome_id,keyed=F){
  
  if(keyed){
    outcome_hierarchy[.(outcome_id),
                      unique(descendant_concept_id)]
    }else{
      outcome_hierarchy[ancestor_concept_id==outcome_id,
                    unique(descendant_concept_id)]
    }

}

