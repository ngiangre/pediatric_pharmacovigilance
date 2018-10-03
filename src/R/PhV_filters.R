
# PURPOSE -----------------------------------------------------------------

#' Filter strings for disproportionality in young age category filtering
#' 
#' to be used within grouping of drugs and outcomes
#' 
#' multiple iterations of conditions are still listed but may not be used
#' 


# conditions --------------------------------------------------------------

young_bias_condition_prr_norm_mse <- "any((dev_age_cat!='(25,100]' & PRR_norm>2 & PRR_mse>2))"

adult_bias_condition_prr_norm_mse <- "(dev_age_cat=='(25,100]' & PRR_norm>2 & PRR_mse>2)"

young_bias_condition_prr_mse <- "any((dev_age_cat!='(25,100]' & PRR_mse>2))"

adult_bias_condition_prr_mse <- "(dev_age_cat=='(25,100]' & PRR_mse>2)"

young_bias_condition_norm <- "any((dev_age_cat!='(25,100]' & PRR_norm>2))"

adult_bias_condition_norm <- "(devage_cat=='(25,100]' & PRR_norm<2)"

young_bias_condition_stat <- "any((age_cat!='(25,100]' & PRR_stat>2))"

young_bias_condition <- "any((age_cat!='(25,100]' & PRR > 2 & PRR_stat>2))"

young_bias_condition_norm_stat <- "any((age_cat!='(25,100]' & PRR>2 & PRR_norm>2 & PRR_stat>2))"

adult_bias_condition_norm_stat <- "(age_cat=='(25,100]' & PRR < 2 & PRR_norm<2 & PRR_stat<2)"

adult_bias_condition <- "any((age_cat!='(25,100]' & PRR<=2 & PRR_stat<=2)) & (age_cat=='(25,100]' & PRR>2 & PRR_stat>2)"

extreme_young_bias_condition_norm_stat <- "any(age_cat!='(25,100]' & PRR>2 & PRR_norm>5 & PRR_stat>2)"

sanity_check_condition <- "all(a>1 & b>1 & c>1 & d>1 & is.finite(PRR))"

