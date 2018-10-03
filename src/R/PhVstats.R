

# PURPOSE -----------------------------------------------------------------

#' Functionds for disproportionality measures
#' 
#' multiple iterations are still listed but may not be used
#' 

# Pharmacovigilance statistic equations -----------------------------------

# contigency table:
# rows: outcome, not outcome
# columns: drug, not drug
# 1,1 a
# 1,2 b
# 2,1 c
# 2,2 d

#Observed over Expected Ratio
obs_vs_exp <- function(a,b,c,d,...){
  
  (a/(a + b + c + d)) / ((a + b)/(a + c)) 
  
}

#Chi Squared
chi2 <- function(a,b,c,d,...){
  
  (a*d - b*c)^2 * (a + b + c + d)/ ((a + c)*(b + d)*(a + b)*(c + d))
  
}

#Relative Reporting Ratio
rrr <- function(a,b,c,d,...){
  
  l <- exp( ( log(a) + log(a + b + c + d) ) - ( log(a + c) + log(a + b) ) )
  
  l
  
}

#Relative Reporting Ratio 95% upper bound
rrr_pse <- function(rrr,a,c,drug_marginal,outcome_marginal,N,...){
  
  s <- exp( (c/(a*drug_marginal)) + ( (N-outcome_marginal)/( (N-outcome_marginal)*N) ) )
  
  exp( log( rrr ) + 1.96*sqrt( log( s ) ) )
  
}

#Relative Reporting Ratio 95% lower bound
rrr_mse <- function(rrr,a,c,drug_marginal,outcome_marginal,N,...){
  
  s <- (c/(a*drug_marginal)) + ( (N-outcome_marginal)/( (N-outcome_marginal)*N) )
  
  exp( log( rrr ) - 1.96*sqrt(s) )
  
}

#Relative Odds Ratio
ror <- function(a,b,c,d,...){
  
  l <- exp( ( log(a) - log(c) ) - ( log(b) - log(d) ) )
  
  l
  
}

#Relative Odds Ratio 95% upper bound
ror_pse <- function(ror,a,b,c,d,...){
  
  s <- sqrt( (1/a) + (1/c) + (1/b) + (1/d) )
  
  exp( log( ror ) + 1.96*s)
  
}

#Relative Odds Ratio 95% lower bound
ror_mse <- function(ror,a,b,c,d,...){
  
  s <- sqrt( (1/a) + (1/c) + (1/b) + (1/d) )
  
  exp( log( ror ) - 1.96*s)
  
}

#Proportional Reporting Ratio
prr <- function(a,b,c,d,...){
  
  l <- exp( ( log(a) - log(a+c) ) - (log(b) - log(b + d) ) )
  
  l
  
}

#http://www.ema.europa.eu/docs/en_GB/document_library/Regulatory_and_procedural_guideline/2009/11/WC500011437.pdf
#Proportional Reporting Ratio 95% upper bound
prr_pse <- function(prr,a,b,c,d,...){
  
  s <- sqrt( (1 / a) + (1 / b) - (1 / (a + c)) - 1/(b + d) )
  
  prr*exp(1.96*s)
  
}

#Proportional Reporting Ratio 95% lower bound
prr_mse <- function(prr,a,b,c,d,...){
  
  s <- sqrt( (1 / a) + (1 / b) - (1 / (a + c)) - 1/(b + d) )
  
  prr/exp(1.96*s)
  
}

#Information Component
ic <- function(rrr,...){
  
  l <- log2(rrr)
  
  l
  
}

#Information Component Posterior Expectation
ic_post_exp <- function(a,b,c,d,N,...){
  
  delta <- ( (N + 2) / (a + b + 1) ) * ( (N + 2) / (a + c + 1) )
  
  (log(a + 1) + log(N + 2)^2 ) - ( log(N + delta) + log(a + b + 1) + log(a + c + 1))
  
}

# ADR counts dt function --------------------------------------------------

#' Calculate Contigency formatted datatable for pairs of drugs-outcomes grouped by a covariate such as age
#' 
#' Input:
#' 
#' df:
#' datatable of observation frequencies of ADRs
#' 
#' drugs:
#' set of concept_id of vocabulary_id=="RxNorm"
#' 
#' outcomes:
#' set of concept_id of vocabulary_id=="SNOMED"
#' 
#' outcome_col:
#' outcome column name in dataset
#' 
#' drug_cols:
#' drug column name in dataset
#' 
#' group:
#' variable in cov_table below
#' 
#' cov_table:
#' stratification table e.g. age categories
make_count_dt <- function(df,drugs,outcomes,drug_col="drug_concept_id",outcome_col="outcome_concept_id",group="dev_age_cat",cov_table=age_cat_tbl,...){
  
  
  drugs <- as.integer(drugs)
  outcomes <- as.integer(outcomes)
  
  if(class(group)=="character" & (group %in% colnames(df))){
    
    colnames(cov_table) <- as.character(as.name(group))
    
    
    dt <- df[,.N,.(eval(as.name(drug_col)) %in% drugs,
                   eval(as.name(outcome_col)) %in% outcomes,
                   by=eval(as.name(group)))
             ]
    
    setnames(dt,"as.name",as.character(as.name(drug_col)))
    setnames(dt,"as.name.1",as.character(as.name(outcome_col)))
    setnames(dt,"by",as.character(as.name(group)))
    
  }else{
    dt <- df[,.N,.(eval(as.name(drug_col)) %in% drugs,
                   eval(as.name(outcome_col)) %in% outcomes)
             ]
    setnames(dt,"as.name",as.character(as.name(drug_col)))
    setnames(dt,"as.name.1",as.character(as.name(outcome_col)))
    setnames(dt,"by",as.character(as.name(group)))
    
  }
  
  #join previous dataframe, subsetting by each drug/outcome combination, to the correct order of age categories from the age category table
  # contigency table: 
  # columns: drug, not drug
  # rows: outcome, not outcome
  # 1,1 a
  # 1,2 b
  # 2,1 c
  # 2,2 d
  tmp <- left_join(cov_table,dt[drug_concept_id==T &
                              outcome_concept_id==T],
                   by=as.character(as.name(group))) %>% 
    rename(a = N)
  tmp <- left_join(tmp,dt[drug_concept_id==F &
                              outcome_concept_id==T],
                   by=as.character(as.name(group))) %>% 
    rename(b = N)
  
  tmp <- left_join(tmp,dt[drug_concept_id==T &
                              outcome_concept_id==F],
                   by=as.character(as.name(group))) %>% 
    rename(c = N)
  
  tmp <- left_join(tmp,dt[drug_concept_id==F &
                              outcome_concept_id==F],
                   by=as.character(as.name(group))) %>% 
    rename(d = N)
  
  tmp <- tmp %>% 
    select(!!as.symbol(as.name(group)),a,b,c,d)
  
  tmp[is.na(tmp)] <- 0
  
  tmp
  
}

#' Calculate PhV statistics from count table and associated metadata
#' 
#' Input:
#' 
#' count_dt:
#' output of previous function-count table for drug,outcome pair
#' 
#' drug, drug_name:
#' drug concept id and name
#' 
#' outcome, outcome_name:
#' outcome concept id and name
#' 
#' target_drugs:
#' descendent drug ids
#' 
#' target_outcomes:
#' descendent outcome ids
#' 
#' cov_table:
#' stratification table
make_PhV_from_counts_dt <- function(count_dt,drug="",drug_name="",drug_class="",outcome,outcome_name="",target_drugs="",target_outcomes="",group="",cov_table=""){
  
  count_dt %>% 
    mutate(
      drug_concept_id = rep(drug,nrow(cov_table)),
      drug_concept_name = rep(drug_name,nrow(cov_table)),
      drug_concept_class_id = rep(drug_class,nrow(cov_table)),
      outcome_concept_id = rep(outcome,nrow(cov_table)),
      outcome_concept_name = rep(outcome_hierarchy[.(outcome),unique(ancestor_concept_name)],nrow(cov_table)),
      !!group := cov_table[,1] %>% unlist %>% unname,
      drug_marginal = a + c,
      outcome_marginal = a + b,
      N = a + b + c + d,
      o_vs_e = obs_vs_exp(a,b,c,d),
      RRR = rrr(a,b,c,d),
      ROR = ror(a,b,c,d),
      PRR = prr(a,b,c,d),
      PRR_pse = prr_pse(PRR,a,b,c,d),
      PRR_mse = prr_mse(PRR,a,b,c,d),
      number_of_drugs_and_descendants = rep(length(target_drugs),
                                            nrow(cov_table)),
      number_of_outcomes_and_descendants = rep(length(target_outcomes),
                                               nrow(cov_table)),
      a_N = rep(sum(a),nrow(cov_table)),
      b_N = rep(sum(b),nrow(cov_table)),
      c_N = rep(sum(c),nrow(cov_table)),
      d_N = rep(sum(d),nrow(cov_table)),
      drug_marginal_N = a_N + c_N,
      outcome_marginal_N = a_N + b_N,
      N_N = a_N + b_N + c_N + d_N,
      o_vs_e_N = obs_vs_exp(a_N,b_N,c_N,d_N),
      RRR_N = rrr(a_N,b_N,c_N,d_N),
      ROR_N = ror(a_N,b_N,c_N,d_N),
      PRR_N = prr(a_N,b_N,c_N,d_N),
      PRR_N_pse = prr_pse(PRR_N,a_N,b_N,c_N,d_N),
      PRR_N_mse = prr_mse(PRR_N,a_N,b_N,c_N,d_N),
      RRR_vs_N = RRR / RRR_N,
      ROR_vs_N = ROR / ROR_N,
      PRR_vs_N = PRR / PRR_N,
      PRR_vs_N_pse = PRR_pse / PRR_N_pse,
      PRR_vs_N_mse = PRR_mse / PRR_N_mse
    )
  
}

#' Calculate ratio between young and adult age categories with PhV statistics
#' 
#' also control for PhV statistics without stratification
#' 
young_vs_adult_dev_age_cat <- function(tmp){
  
  adult <- tmp %>%
    filter(get(group) == "(25,100]")
  
  tmp_norm <- tmp %>%
    mutate(
      RRR_norm = RRR / adult$RRR,
      ROR_norm = ROR / adult$ROR,
      PRR_norm = PRR / adult$PRR,
      RRR_stat = RRR_norm / RRR_N,
      ROR_stat = ROR_norm / ROR_N,
      PRR_stat = PRR_norm / PRR_N
    )
  
  tmp_norm
  
}
