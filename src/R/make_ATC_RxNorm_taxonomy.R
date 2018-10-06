cat("start: ",as.character(Sys.time()),"/n")

# PURPOSE -----------------------------------------------------------------

#' To make the drug taxonomy
#' 
#' The 5th ATC level will be RxNorm drugs (Ingrediant)
#' The higher levels will be ATC classes 4, 3, 2, and 1
#' 
#' This will be useful for doing the PhV statistics later on
#' 
#' EXAMPL RUN ON CLUSTER:
#' ----------------------
#' 
#' Rscript make_ATC_RxNorm_taxonomy.R 


# load libraries ----------------------------------------------------------

library(tidyverse)
library(data.table)

# set variables -----------------------------------------------------------

data_dir <- "../../data/"

dir_path <- paste0(data_dir,"vocabulary_download_v5_{aaf1154e-b4a4-479b-9ed8-817e30c0ba84}_1508422779545/")

# load OHDSI vocabularies -------------------------------------------------

C <- fread(paste0(dir_path,"CONCEPT.csv"),sep="\t",sep2 = " ",quote="",colClasses = 'character')

CA <- fread(paste0(dir_path,"CONCEPT_ANCESTOR.csv"),sep="\t",sep2 = " ",quote="",colClasses = 'character')

CR <- fread(paste0(dir_path,"CONCEPT_RELATIONSHIP.csv"),sep="\t",sep2 = " ",quote="",colClasses = 'character')


# Get RxNorm Ingredient to ATC 5th class mapping --------------------------

C_rxnorm <- C[vocabulary_id=="RxNorm" & concept_class_id=="Ingredient"]
C_atc <- C[vocabulary_id=="ATC"]

tmp <- C_rxnorm
colnames(tmp) <- paste0("descendant_",colnames(tmp))
CA_rxnorm_vocab <- merge(tmp,CA,by=c("descendant_concept_id"))

tmp <- C_atc
colnames(tmp) <- paste0("ancestor_",colnames(tmp))
CA_atc_vocab <- merge(CA_rxnorm_vocab,tmp,by=c("ancestor_concept_id")) %>% 
  arrange(desc(max_levels_of_separation)) %>% 
  data.table()


CA_rxnorm_vocab_atc_merge <- merge(CA_rxnorm_vocab,CR,by.x=c("descendant_concept_id"),by.y=c("concept_id_1"))[relationship_id=="RxNorm - ATC"]
tmp <- C
colnames(tmp) <- paste0("ancestor_",colnames(tmp))
CA_atc_rxnorm_vocab <- merge(CA_rxnorm_vocab_atc_merge,tmp,
      by.x=c("concept_id_2"),
      by.y=c("ancestor_concept_id"))


# output ------------------------------------------------------------------


bind_rows(CA_atc_vocab,CA_atc_rxnorm_vocab) %>% 
  fst::write_fst(paste0(data_dir,"ATC_RxNorm_hierarchy.fst"))



cat("end: ",as.character(Sys.time()))
