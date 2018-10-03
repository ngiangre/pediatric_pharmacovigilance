
# PURPOSE -----------------------------------------------------------------

#' To create taxonomy of chosen vocabulary to use in inheritance
#' 
#' uses OHDSI curated vocabularies
#' 
#' Need tables downloaded from Athena at http://athena.ohdsi.org/search-terms/terms
#' 
#' EXAMPLE RUN ON LOCAL:
#' source in RStudio
#' 

# load libraries ----------------------------------------------------------

library(tidyverse)
library(data.table)

# set variables -----------------------------------------------------------

dir_path <- "../../vocabulary_download_v5_{aaf1154e-b4a4-479b-9ed8-817e30c0ba84}_1508422779545/"

vocab <- "SNOMED"

data_dir <- "../../data/"

# load OHDSI vocabularies -------------------------------------------------

C <- fread(paste0(dir_path,"CONCEPT.csv"),sep="\t",sep2 = " ",quote="",colClasses = 'character')

CA <- fread(paste0(dir_path,"CONCEPT_ANCESTOR.csv"),sep="\t",sep2 = " ",quote="",colClasses = 'character')

CR <- fread(paste0(dir_path,"CONCEPT_RELATIONSHIP.csv"),sep="\t",sep2 = " ",quote="",colClasses = 'character')

CR_is_a <- CR[relationship_id=="Is a"]


# want to subset by a vocabulary of my choosing ---------------------------

if(nrow(C[vocabulary_id==vocab])==0){
  stop(paste0(vocab," is not in the concept table. Can't make taxonomy"))
}

C_vocab <- C[vocabulary_id==vocab & standard_concept=="S"]

#get concepts only in vocab
concepts_vocab <- subset(unique(C_vocab),select=concept_id) %>% 
  unlist %>% unname()

#subset CA by concepts only in vocab
CA_vocab <- CA[(ancestor_concept_id %in% concepts_vocab) & (descendant_concept_id %in% concepts_vocab)]

#is_a relationships only
CR_is_a_vocab <- CR_is_a[(concept_id_1 %in% concepts_vocab) & (concept_id_2 %in% concepts_vocab)]


# join other concept information on descendant/ancestor ids -------------------------------------------

tmp <- C_vocab
colnames(tmp) <- paste0("ancestor_",colnames(tmp))
setkey(tmp,ancestor_concept_id)
setkey(CA_vocab,ancestor_concept_id)
CA_vocab_join <- CA_vocab[tmp]

tmp <- C_vocab
colnames(tmp) <- paste0("descendant_",colnames(tmp))
setkey(tmp,descendant_concept_id)
setkey(CA_vocab_join,descendant_concept_id)
CA_vocab_join <- CA_vocab_join[tmp]


# outputting vocab hierarchy ----------------------------------------------

CR_vocab <- CR[(concept_id_1 %in% concepts_vocab) & (concept_id_2 %in% concepts_vocab)]

setkey(CA_vocab_join,descendant_concept_id)
setkey(CR_vocab,concept_id_1)
tmp <- merge(CA_vocab_join,CR_vocab,
             by.x=c("descendant_concept_id"),
             by.y=c("concept_id_1"),
             allow.cartesian=TRUE)

all <- tmp[(relationship_id=="Is a") & (min_levels_of_separation==max_levels_of_separation)]

all %>% 
  fwrite(file=paste0(data_dir,vocab,"_is_a_hierarchy.csv"),nThread = 16,sep=",")

all %>% 
  fst::write_fst(paste0(data_dir,vocab,"_is_a_hierarchy.fst"))
