---
date: "2016-04-27T00:00:00Z"
external_link: ""
image:
  caption: Spring To Life project logo
  focal_point: "Smart"
  preview_only: true
# links:
# - icon: twitter
#   icon_pack: fab
#   name: Follow
#   url: ""
# slides: ""
summary: A model of people.
weight: 3
tags:
- CHOICE MODELS
- MICROSIMULATION
- MODEL MODULES
- OUTCOME MEASUREMENT
- UTILITY MAPPING
- MODELS
title: Spring To Life
url_code: ""
url_pdf: ""
url_slides: ""
url_video: ""
---

## Purpose
Spring To Life is a model of selected attributes, behaviours, relationships and outcomes of individuals and groups relevant to public mental health policy development and system design.

## Published output
Published output includes reports, datasets and software and to date is principally focused on utility mapping in clinical populations of young people. 

### Reports
A scientific summary of a study to map outcome measures collected in mental health services to a youth appropriate measure of health utility is available as [a preprint](https://www.medrxiv.org/content/10.1101/2021.07.07.21260129v2).

### Datasets
The [results / replication dataset](https://doi.org/10.7910/DVN/DKDIB0) for the previously mentioned study contains detailed catalogues of utility mapping models that can be applied to clinical youth mental health populations.

Two synthetic (fake) datasets of young people are available in an [online repository](https://doi.org/10.7910/DVN/HJXYKQ), along with their associated data-dictionaries. One of these synthetic populations is reasonably representative of the participants in the study reported in the preprint and thus may be useful for those doing **exploratory** simulation models of primary youth mental health services. The second synthetic population was derived from the first, but purely for the purposes of testing the generalisability of some of the software we developed. It is not suitable for other purposes.

### Software
A number of development version R packages have been released to assist with modelling populations of interest. These include:

- [youthvars](https://ready4-dev.github.io/youthvars/index.html), which provides tools for describing and validating datasets of individuals;
- [heterodox](https://ready4-dev.github.io/heterodox/index.html), which provides tools for exploring heterogeneity in quality of life data; 
- [scorz](https://ready4-dev.github.io/scorz/index.html), which provides tools for deriving total instrument scores (currently restricted to selected health utility measures) from questionnaire responses;
- [specific](https://ready4-dev.github.io/specific/index.html), which provides tools to specify and test selected types of statistical models for exploring patterns in population data (currently restricted to developing utility mapping models); 
- [TTU](https://ready4-dev.github.io/ttu/index.html), which provides tools for reporting utility mapping studies; and
- [youthu](https://ready4-dev.github.io/youthu/index.html), which provides tools for using utility mapping models to make predictions with new data.

Examples of how most of these R packages can be used in practice is provided in the following programs:

- A program to [conduct and report a utility mapping study](https://doi.org/10.5281/zenodo.6116077); and
- A program to [apply utility mapping models to new data](https://doi.org/10.5281/zenodo.6317179).

## Future releases
In addition to improving the documentation and ease of use of the R packages already developed (and preparing them for eventual submission to [CRAN](https://cran.r-project.org/)), the main current development priorities are to complete and publish work to 
- provide improved synthetic representations of Australian clinical youth mental health populations; 
- provide valid synthetic representations of the household structure of the Australian general population;
- provide tools to support the conduct and reporting of choice modelling studies in mental health populations.