# Using precision pharmacoviglance to detect developmentally-regulated adverse drug reactions: a case study with antiepileptic drugs

Code and documentation accompanying the poster "Using precision pharmacoviglance to detect developmentally-regulated adverse drug reactions: a case study with antiepileptic drugs" presented at the 2018 OHDSI symposium in Bethesda, MD, USA

## Introduction

This poster outlines a first-pass pharmacovigilance methodology for detecting ADRs that disproportionally affect those within younger age categories relative to adults. The following briefly describes the code used to generate the findings, and refer to the Rmarkdown notebook for figure creation and other visualizations. If you find this useful, would like to discuss this work, or would like to collaborate, please contact me at nick.giangreco@gmail.com. If you would like to improve the code and documentation, feel free to do a pull request!

## Coding Workflow

This section outlines the workflow taken for producing the results of this poster. The most detail are in the code itself. But the boxes (indicating the script type) contain the scripts that are used in the methodology.

<object src="figs/Script-workflow.pdf"><embed src="figs/Script-workflow.pdf" type="application/pdf"></embed></object>

Legend:

* Thick Arrow: 'is used in'
* Thin arrow: 'output of previous is funneled into'
* Trapezoids: 'top script uses the bottom script'

### <u>Constructing Vocabulary Taxonomies & Hierarchies</u>

These scripts using the OMOP CDM files (downloaded from [Athena](http://athena.ohdsi.org/search-terms/terms), but the directory with the files is ignored in this repository) to construct the taxonomic hierarchies used in the analysis.

### <u>Helper Functions & Filters</u>

These scripts contain functions, filters, and data processing to support the main ADR detection and evaluation scripts. These are used in the analysis 

### <u>Preprocessing AEOLUS</u>

The AEOLUS<sup>1</sup> dataset was [downloaded](https://datadryad.org/resource/doi:10.5061/dryad.8q0s4)<sup>2</sup>, curated and loaded onto our lab's mysql servers. I further processed the dataset so that all reports of ADRs had 

* SNOMED encoded outcomes
* RxNorm encoded drugs
* Reported age 

Further processing details can be found in the *Real Data* script.

A synthetic dataset was constructed (see *Synthetic Data* script), where the formatting is exactly the same, but the only difference is that the distribution of ages for 4/1000 sampled ADRs were redistributed to simulate the disproportionality in age categories we want and don't want to detect. 

### <u>Adverse drug reaction detection methods</u>

This is the main analysis where pharmacovigilance is performed. These scripts are meant to be used on a cluster (for processing the real data; the synthetic data can be run locally). I ran each script on our cluster using 25 cores, and each individual script took about 3 hours to complete. 

The three types of ADRs, as outlined in the poster, are computed in the respective scripts in *Real Data ADR detection*.

The ADRs, with controls, in the synthetic dataset are computed in *Synthetic Data ADR detection*. The ADR type is the same as the 'ADRs' type in the poster i.e. coded in the PhV_of_ADRs.R script. 

### Real Data ADR Evaluation

The positive control ADRs were assessed whether they could be detected and whether the negative control ADRs could not be detected. Fortunately, all positive controls are detected, and no negative controls are detected. This is shown in a confusion matrix in a log file. 

### Real Data ADR Evaluation

This is where the candidate ADRs are detected, using the same filtering as the synthetic dataset. Numbers of candidate ADRs are outputted into a log file. 

## Data Availability

Because of the size of the processed AEOLU dataset (9.53Gb), it is ignored in this repository. But if you would like access to it, please let me know at nick.giangreco@gmail.com, and I'd be happy to share it!

## References

1. Banda JM, Evans L, Vanguri RS, Tatonetti NP, Ryan PB, Shah NH (2016) A curated and standardized adverse drug event resource to accelerate drug safety research. Scientific Data 3: 160026. https://doi.org/10.1038/sdata.2016.26
2. Banda JM, Evans L, Vanguri RS, Tatonetti NP, Ryan PB, Shah NH (2016) Data from: A curated and standardized adverse drug event resource to accelerate drug safety research. Dryad Digital Repository. https://doi.org/10.5061/dryad.8q0s4
