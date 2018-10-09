#!/bin/bash

Rscript select_young_age_category_biased_ADRs.R ../../data/ADRs_PhV/ATC/ATC_3rd/ANTIEPILEPTICS/ALL_DRUGS/PhV_of_ADRs_stratified_by_dev_age_cat.csv > ../../data/ADRs_PhV/ATC/ATC_3rd/ANTIEPILEPTICS/ALL_DRUGS/PhV_of_ADRs__stratified_by_dev_age_cat_biased_ADRs.log

Rscript select_young_age_category_biased_ADRs.R ../../data/ADRs_PhV/ATC/ATC_3rd/ANTIEPILEPTICS/ALL_DRUGS/PhV_of_ADRs_using_SNOMED_taxonomy_stratified_by_dev_age_cat.csv > ../../data/ADRs_PhV/ATC/ATC_3rd/ANTIEPILEPTICS/ALL_DRUGS/PhV_of_ADRs_using_SNOMED_taxonomy_stratified_by_dev_age_cat_biased_ADRs.log

Rscript select_young_age_category_biased_ADRs.R ../../data/ADRs_PhV/ATC/ATC_3rd/ANTIEPILEPTICS/ALL_DRUGS/PhV_of_hADRs_using_SNOMED_taxonomy_stratified_by_dev_age_cat.csv > ../../data/ADRs_PhV/ATC/ATC_3rd/ANTIEPILEPTICS/ALL_DRUGS/PhV_of_hADRs_using_SNOMED_taxonomy_stratified_by_dev_age_cat_biased_ADRs.log
