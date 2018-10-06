cat("start: ",as.character(Sys.time()),"\n")


# PURPOSE ------------------------------------------------------------


#' Process and clean AEOLUS dataset 
#' 
#' hosted on our remote, sql database
#' 
#' EXAMPLE RUN ON CLUSTER:
#' ----------------------
#' source ../bash/submit_Rjob.sh prepare_aeolus_ATC.R 
#' 

# Libraries ---------------------------------------------------------------

if(require("RMySQL")){library(RMySQL)}else{install.packages("RMySQL", repos='http://cran.us.r-project.org');library(RMySQL)}
if(require("fst")){library(fst)}else{install.packages("fst", repos='http://cran.us.r-project.org');library(fst)}
if(require("DBI")){library(DBI)}else{install.packages("DBI", repos='http://cran.us.r-project.org');library(DBI)}
if(require("data.table")){library(data.table)}else{install.packages("data.table", repos='http://cran.us.r-project.org');library(data.table)}
if(require("tidyverse")){library(tidyverse)}else{install.packages("tidyverse", repos='http://cran.us.r-project.org');library(tidyverse)}
if(require("stringi")){library(stringi)}else{install.packages("stringi", repos='http://cran.us.r-project.org');library(stringi)}

# AEOLOUS Tables download ---------------------------------------------------

cat("AEOLOUS Tables download...\n\n")


db <- "effect_faers"

con <- DBI::dbConnect(RMySQL::MySQL(),
                      user = readr::read_tsv("../../.my.cnf")$u,
                      password = readr::read_tsv("../../.my.cnf")$pw,
                      host = "127.0.0.1",
                      port=3307,
                      dbname=db)


standard_drug_outcome_drilldown <- as_tibble(RMySQL::dbReadTable(conn = con, name='standard_drug_outcome_drilldown'))

standard_ages <- as_tibble(RMySQL::dbReadTable(conn = con, name='standard_ages'))

cat('FAERS reports: ',
    format(n_distinct(standard_ages$primaryid), 
           big.mark=",", scientific=FALSE),"\n\n")

standard_ages_laers <- as_tibble(RMySQL::dbReadTable(conn = con, name='standard_ages_laers'))

cat('LAERS reports: ',
    format(n_distinct(standard_ages_laers$isr), 
           big.mark=",", scientific=FALSE),"\n\n")

# AEOLOUS Tables tidy up. Remove rows without age_code -------------------------------------------------------

cat("AEOLOUS Tables tidy up. Remove rows without age_code...\n\n")

standard_ages_cleaned <- standard_ages %>% 
  filter(age_code != "") %>% # has to have an agecode (unit)
  filter(!is.na(age_code)) # has to have a non-na age code (unit)

cat('FAERS reports missing age code: ', 
    round(
      length(which(standard_ages$age_code==""))/nrow(standard_ages)*100
      ,2)
    ,'%\n\n')

standard_ages_laers_cleaned <- standard_ages_laers %>% 
  filter(age_code != "") %>% # has to have an agecode (unit)
  filter(!is.na(age_code)) # has to have a non-na age code (unit)

cat('LAERS reports missing age code: ', 
    round(
      length(which(standard_ages_laers$age_code==""))/nrow(standard_ages)*100
      ,2)
    ,'%\n\n')

rm(standard_ages)
rm(standard_ages_laers)

standard_drug_outcome_drilldown_cleaned <- standard_drug_outcome_drilldown %>% 
  filter(!is.na(snomed_outcome_concept_id))

perc <- 1 - (nrow(standard_drug_outcome_drilldown_cleaned)) / (nrow(standard_drug_outcome_drilldown) %>% unname())

rm(standard_drug_outcome_drilldown)

cat(round(perc,4)*100,'% of outcomes have no mapping to snomed ids, but all drugs have a concept.\n\n',sep="")

rm(perc)

# AEOLOUS Tables joining of ages to all reports -----------------------------

cat("AEOLOUS Tables joining of ages to all reports...\n\n")

faers_age_joined <- standard_drug_outcome_drilldown_cleaned %>% 
  left_join(standard_ages_laers_cleaned,
            by = c( "isr" ) ) %>% 
  left_join(standard_ages_cleaned,
            by = c( "primaryid" )) %>% 
  filter( !is.na(age.x) | !is.na(age.y) )

rm(standard_ages_cleaned)
rm(standard_ages_laers_cleaned)
rm(standard_drug_outcome_drilldown_cleaned)

#putting age from faers and laers reports in one variable
tmp <- faers_age_joined$isr
tmp[is.na(tmp)] <- faers_age_joined$primaryid[!is.na(faers_age_joined$primaryid)]
id <- tmp

tmp <- faers_age_joined$age.x
tmp[is.na(tmp)] <- faers_age_joined$age.y[!is.na(faers_age_joined$age.y)]
age <- tmp

tmp <- faers_age_joined$age_code.x
tmp[is.na(tmp)] <- faers_age_joined$age_code.y[!is.na(faers_age_joined$age_code.y)]
age_code <- tmp

tmp <- faers_age_joined$report_year.x
tmp[is.na(tmp)] <- faers_age_joined$report_year.y[!is.na(faers_age_joined$report_year.y)]
report_year <- tmp

#indicating laers or faers
aers <- rep("FAERS",nrow(faers_age_joined))
aers[is.na(faers_age_joined$age.x)] <- "LAERS"

faers_age_joined_cleaned <- faers_age_joined %>% 
  mutate(
    id = id,
    age = age,
    age_code = age_code,
    aers = aers,
    report_year = report_year
  ) %>% 
  select(id,age,age_code,report_year,aers,drug_concept_id,snomed_outcome_concept_id)

rm(id)
rm(age)
rm(age_code)
rm(tmp)
rm(aers)
rm(report_year)
rm(faers_age_joined)

cat('Total AERS reports with standard concepts and age: ',
    format(
      n_distinct(faers_age_joined_cleaned$id),
      big.mark=",", scientific=FALSE)
    ,"\n\n")

# AEOLOUS master age ------------------------------------------------------

cat("AEOLOUS master age...\n\n")

round_n <- 3

tmp <-   faers_age_joined_cleaned %>% 
  
  #taking into account age based on age_code given
  mutate(
    day_years = ifelse(age_code=="DY",round(age/365,round_n),age),
    week_years = ifelse(age_code=="WK",round(age/52,round_n),age),
    month_years = ifelse(age_code=="MON",round(age/12,round_n),age),
    deceased_years = ifelse(age_code=="DEC",age,age),
    year_years = ifelse(age_code=="YR",age,age),
    days = as.integer(age_code=="DY"),
    weeks = as.integer(age_code=="WK"),
    months = as.integer(age_code=="MON"),
    years = as.integer(age_code=="YR"),
    deceased = as.integer(age_code=="DEC"),
    master_age = day_years * days + week_years * weeks + month_years * months + year_years * years + deceased_years * deceased
  )

rm(faers_age_joined_cleaned)

total_reports <- length(unique(tmp %>% select(id) %>% pull() ))

tmp2 <- tmp %>% 
  group_by(id) %>% 
  summarize( 
    report_age = mean(age,na.rm = T)
  )

cat("Report age < 0: ",
    (tmp2 %>% filter(report_age<0) %>% nrow())/total_reports*100,"%\n\n"
)

cat("Report age == 0: ",
    (tmp2 %>% filter(report_age==0) %>% nrow())/total_reports*100,"%\n\n"
)

cat("Report age >= 120: ",
    (tmp2 %>% filter(report_age>=120) %>% nrow())/total_reports*100,"%\n\n"
)

rm(tmp2)

aged <- tmp %>% 
  
  #filtering for reports aged >0 and below 120 for sanity check
  filter(master_age>0 & master_age<120) %>% 
  
  #only use the outcome coded as a snomed-ct concept
  rename(
    outcome_concept_id = snomed_outcome_concept_id
  ) %>% 
  
  mutate(
    deceased = as.logical(deceased)
  ) %>% 
  
  distinct()

rm(tmp)


#adding variable for age category
aged_wagecat <- aged %>%  
  
  mutate(
    dev_age_cat = cut(master_age,breaks=c(0,2,5,8,11,14,18,25,100)), # more developmentally consistent age categories
    age_cat = cut(master_age,breaks=seq(0,120,5)) # equally space age category intervals
  )

cat('Total AERS reports with standard concepts and age: ',
    format(
      n_distinct(aged_wagecat$id),
      big.mark=",", scientific=FALSE)
    ,"\n\n")

# Download and joining ohdsi concept ids/names/ATC class, hosted on our server but also available at http://athena.ohdsi.org/search-terms/terms, to counts tables -----------

cat("Download and joining ohdsi concept ids/names to counts tables...\n\n")

db <- "clinical_cumc_v5"

con2 <- dbConnect(RMySQL::MySQL(),
                  user = readr::read_tsv("../../.my.cnf")$u,
                  password = readr::read_tsv("../../.my.cnf")$pw,
                  host = "127.0.0.1",
                  port=3307,
                  dbname=db)

concepts <- as_tibble(RMySQL::dbReadTable(conn = con2, name='concept'))
snomed <- concepts%>% 
  filter(vocabulary_id=="SNOMED")

concept_ancestor <- as_tibble(RMySQL::dbReadTable(conn = con2, name='concept_ancestor'))

joined <- aged_wagecat %>% 
  select(drug_concept_id) %>% 
  distinct() %>% 
  left_join(concept_ancestor,
            by=c("drug_concept_id"="descendant_concept_id")) %>% 
  left_join(concepts %>% 
              filter(vocabulary_id=="ATC"),
            by=c("ancestor_concept_id"="concept_id")) %>%  
  select(drug_concept_id,concept_name,concept_class_id) %>% 
  distinct() 

ids <- 1:nrow(joined)
tmp <- joined %>% 
  mutate(id=ids) %>% 
  spread(concept_class_id,concept_name) %>% 
  select(-!!as.name("<NA>"),-id)

joined2 <- as_tibble(data.table(tmp)[, lapply(.SD, na.omit), by = drug_concept_id])

rm(ids)

aged2 <- left_join(aged_wagecat,joined2)
colnames(aged2) <- colnames(aged2) %>% 
  stri_trans_tolower() %>% 
  stri_replace_all_fixed(" ","_")

rm(aged)
rm(aged_wagecat)

joined <- aged2 %>% 
  select(outcome_concept_id) %>% 
  distinct() %>% 
  left_join(concepts,
            by=c("outcome_concept_id"="concept_id")) %>%  
  select(outcome_concept_id,concept_name,domain_id) %>% 
  distinct()

aged3 <- aged2 %>% 
  left_join(joined) %>% 
  select(-concept_name) %>% 
  rename(
    outcome_domain_id = domain_id
  )

rm(aged2)

aged_wdrugconcepts <- aged3 %>% 
  
  left_join(concepts%>% 
              filter(vocabulary_id=="RxNorm"),
            by=c("drug_concept_id" = "concept_id")) %>% 
  select(id:concept_name) %>% 
  rename(
    drug_concept_name = concept_name
  )

rm(aged3)
rm(joined)
rm(joined2)

aged_wdrugoutcomeconcepts <- aged_wdrugconcepts %>% 
  
  left_join(snomed,
            by=c("outcome_concept_id" = "concept_id")) %>% 
  rename(
    outcome_concept_name = concept_name
  ) %>% 
  mutate(
    drug_outcome_name = stri_c(
      drug_concept_name,"_",outcome_concept_name)
  ) %>% 
  select(id:outcome_concept_name,drug_outcome_name)

rm(snomed)
rm(concepts)
rm(concept_ancestor)
rm(aged_wdrugconcepts)

cat("Final report number: ",length(unique(aged_wdrugoutcomeconcepts %>% select(id) %>% dplyr::pull())),"\n\n")

cat("Final drug_outcome number: ",length(unique(aged_wdrugoutcomeconcepts %>% select(drug_outcome_name) %>% dplyr::pull())),"\n\n")


# AEOLUS joining of gender ------------------------------------------------

demographics <- dplyr::tbl(con,'demographics')

demographics <- demographics %>% 
  mutate(isr_report_id = as.character(isr_report_id))

for_joining <- demographics %>% 
  select(isr_report_id,gender_code) %>% 
  collect()

rm(demographics)

aged_wdrugoutcomeconceptsgender <- left_join(aged_wdrugoutcomeconcepts,
                                       for_joining,
                                       by=c("id" = "isr_report_id"))

rm(aged_wdrugoutcomeconcepts)

# Writing tables ----------------------------------------------------------

cat("Writing tables...\n\n")

file <- "../../data/aeolus"
aged_wdrugoutcomeconceptsgender %>% fst::write_fst(paste0(file,".fst"))




cat("end: ",as.character(Sys.time()))
