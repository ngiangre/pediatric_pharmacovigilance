Exploratory Data Analysis of young age category biased adverse drug
reactions
================

## Introduction

This notebook serves as the initialv exploration of the calculated
pharmacovigilance statistics for determining ADRs disproportionally
within young age categories. The question of whether these ADRs would be
associated to child development (where age associations are used here as
a proxy for) is yet to be determined. This analysis serves as the
initial step in pursuit of developmentally-regulated
    ADRs.

## Set up

``` r
library(tidyverse)
```

    ## ── Attaching packages ──────────────────────────────────────────────── tidyverse 1.2.1 ──

    ## ✔ ggplot2 3.0.0     ✔ purrr   0.2.5
    ## ✔ tibble  1.4.2     ✔ dplyr   0.7.6
    ## ✔ tidyr   0.8.1     ✔ stringr 1.3.1
    ## ✔ readr   1.1.1     ✔ forcats 0.3.0

    ## ── Conflicts ─────────────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()

``` r
theme_set(theme_classic(base_size = 16,base_family = "Times New Roman"))
library(data.table)
```

    ## 
    ## Attaching package: 'data.table'

    ## The following objects are masked from 'package:dplyr':
    ## 
    ##     between, first, last

    ## The following object is masked from 'package:purrr':
    ## 
    ##     transpose

``` r
data_dir <- "../data/"

type <- c("ADR","ADR_class","hADR")
colors <- c("")
type_colors <- list("ADR")
dir_name <- paste0(data_dir,"ADRs_PhV/ATC/ATC_3rd/ANTIEPILEPTICS/ALL_DRUGS/")

files <- list.files(dir_name,"*annotated.csv")

phv <- lapply(
  files,
  function(x){
    fread(paste0(dir_name,x))
  })
names(phv) <- c("ADR","ADR_class","hADR")



phv_binded <- bind_rows(phv,.id="type")

dev_age_cats <- as.character(unique(phv_binded$dev_age_cat))
ord <- order(sapply(strsplit(dev_age_cats,"[\\[(]"),function(x){strsplit(x[2],",")[[1]][1]}) %>% as.integer())
phv_binded$dev_age_cat <- factor(phv_binded$dev_age_cat,levels=dev_age_cats[ord])
```

## Number of ADR candidates

``` r
candidates <- unique(
  phv_binded[candidate==T,
             .(drug_concept_name,outcome_concept_name,type)]
  )

data <- candidates[,.N,by=type]

data %>% 
  ggplot(aes(type,N,fill=type)) + 
  geom_bar(stat="identity") + 
  geom_text(aes(label=N,y=N+5)) +
  scale_fill_brewer(palette="Set1") +
  ylab("Number of ADRs") + 
  xlab("") +
  theme(
    text = element_text(face="bold",size=16)
  )
```

![](../docs/imgs/Number_ADR_candidates_per_type-1.png)<!-- -->

## ADRs by ADR type

``` r
candidates <- unique(
  phv_binded[candidate==T,
             .(drug_concept_name,outcome_concept_name,type)]
  )

data <- candidates[,.N,by=.(outcome_concept_name,type)][order(-N)]

data %>%
  group_by(type) %>% 
  top_n(5) %>% 
  ungroup() %>% 
  ggplot(aes(reorder(outcome_concept_name,N),N,fill=outcome_concept_name)) + 
  geom_bar(stat="identity") + 
  facet_grid(type~.,scales="free") +
  ylab("N") +
  xlab("ADRs") +
  coord_flip() +
  guides(
    fill = guide_legend(title = "ADR",
                        title.position = "top",
                        label.position = "left",
                        ncol=3)
    ) +
  theme(
    legend.text = element_text(size=10,face="bold"),
    axis.text.y = element_blank(),
    legend.position = "bottom"
  )
```

    ## Selecting by N

![](../docs/imgs/frequent_candidate_ADRs_per_type-1.png)<!-- -->

## Drugs by ADR type

``` r
candidates <- unique(
  phv_binded[candidate==T,
             .(drug_concept_name,outcome_concept_name,type)]
  )

data <- candidates[,.N,by=.(drug_concept_name,type)][order(-N)]

data %>%
  ggplot(aes(reorder(drug_concept_name,N),
             N,fill=drug_concept_name)) + 
  geom_bar(stat="identity") + 
  facet_grid(type~.,scales="free") +
  ylab("N") +
  xlab("Drugs") +
  coord_flip() +
  #scale_fill_brewer(palette="Set1") +
  scale_y_continuous(labels=scales::number) + 
  guides(
    fill = guide_legend(title = "",
                        label.position = "left",
                        ncol=3)
    ) +
  theme(
    legend.text = element_text(size=16,face="bold"),
    text = element_text(face="bold",size=10),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.title.y = element_text(size=16),
    legend.position = "bottom"
    )
```

![](../docs/imgs/Drug_frequency_in_candidate_ADRs_per_type-1.png)<!-- -->

## Number of ADRs per type by age category

``` r
candidates <- phv_binded[candidate==T,
             .(drug_concept_name,outcome_concept_name,dev_age_cat,
               type,PRR_mse,PRR_norm)][
                 PRR_mse>2&PRR_norm>2][
                 ,.(drug_concept_name,outcome_concept_name,
                    dev_age_cat,type)] %>% 
  unique()

data <- candidates[,
                   .N,
                   by=.(dev_age_cat,type)][order(-N)]

dev_age_cats <- as.character(unique(data$dev_age_cat))
ord <- order(sapply(strsplit(dev_age_cats,"[\\[(]"),function(x){strsplit(x[2],",")[[1]][1]}) %>% as.integer())
data$dev_age_cat <- factor(data$dev_age_cat,levels=dev_age_cats[ord])

data %>% 
  ggplot(aes(dev_age_cat,N,fill=type)) +
  geom_bar(stat="identity",position = "dodge") +
  scale_fill_brewer(palette="Set1") +
  theme(
    text = element_text(face="bold")
  )
```

![](../docs/imgs/Number_of_candidate_ADRs_per_type_by_dev_age_cat-1.png)<!-- -->

## PRR\_norm distribution of ADRs per type and dev\_age\_cat

``` r
candidates <- unique(
  phv_binded[candidate==T,
             .(drug_concept_name,outcome_concept_name,dev_age_cat,PRR_norm,type)]
  )

data <- candidates[,.N,by=.(outcome_concept_name,type)][order(-N)]

candidates %>% 
  ggplot(aes(PRR_norm,fill=type)) + 
  geom_histogram() + 
  scale_x_log10() +
  facet_grid(dev_age_cat~type) +
  scale_fill_brewer(palette="Set1") +
  theme(
    text = element_text(face="bold",size=16)
  )
```

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

![](../docs/imgs/PRR_norm_distribution_per_ADR_type_and_dev_age_cat-1.png)<!-- -->

## Top ADRs per type in age categories

``` r
candidates <- phv_binded[candidate==T,
             .(drug_concept_name,outcome_concept_name,dev_age_cat,
               type,PRR_mse,PRR_norm)][
                 PRR_mse>2&PRR_norm>2]

candidates %>% 
  group_by(drug_concept_name,outcome_concept_name,dev_age_cat,PRR_norm) %>% 
  top_n(5,PRR_norm) %>% 
  ungroup() %>% 
  ggplot(aes(outcome_concept_name,PRR_norm,fill=drug_concept_name)) +
  geom_bar(stat="identity",position = "dodge") +
  facet_grid(drug_concept_name~dev_age_cat,scales = "free_y") +
  coord_flip() +
  xlab("") +
  ylab("PRR_norm") +
  theme_bw() +
  theme(
    legend.position = " bottom",
    text = element_text(face="bold",size=24),
    axis.text.x = element_text(size=20),
    axis.text.y = element_text(size=14)
  )
```

![](../docs/imgs/top_candidate_ADRs_per_type_in_dev_age_cat-1.png)<!-- -->
\#\# Number of ADRs shared between dev\_age\_cats

``` r
candidates <- phv_binded[candidate==T,
             .(drug_concept_name,outcome_concept_name,dev_age_cat,
               type,PRR_mse,PRR_norm)][
                 PRR_mse>2&PRR_norm>2]

data <- candidates[
  ,.N,
  by=.(drug_concept_name,outcome_concept_name)][
    ,.(shared = .N),
    by=N
    ]

data$N <- factor(data$N)

data %>% 
  ggplot(aes(N,shared,fill=N)) +
  geom_bar(stat="identity") +
  ylab("Number of ADRs\nshared across age categories") +
  xlab("Number of shared ADRs") +
  theme(
    legend.position = "none",
    text = element_text(face="bold",size=24)
  )
```

![](../docs/imgs/Number_of_shared_ADRs_per_type_between_dev_age_cat-1.png)<!-- -->
