source("Prep/functions.R")
## Application
library(ready4)
rbibutils::bibConvert("Prep/nihr.nbib",
                      "Prep/nihr.bib")
# scholar_pubs_tb <- scholar::get_publications("t1ZHrCoAAAAJ") %>%
#   tibble::as_tibble() # Update with ready4show ref once migration complete
# Note : need to write script that compacts keywords and place call here
# Implementing a combined individual placement and support and vocational peer work program in integrated youth mental health setting
# Social inclusion, intersectionality, and profiles of vulnerable groups of young people seeking mental health support
# Study protocol for the Multimodal Approach to Preventing Suicide in Schools (MAPSS) project: a regionally based randomised trial of an integrated response to suicide risk among # MOVE From PREPRINT
# Providing a pathway to community‐based psychosocial or mental health support services for young people following initial encounters with police: a scoping review
# Inequalities in Access to Mental Health Treatment by Australian Youths During the COVID‐19 Pandemic
# Mental health systems modelling for evidence‐informed service reform in Australia #PP
# Heterogeneity of quality of life in young people attending primary mental health services

pubs_df <- make_pubs_df(path_to_bib_1L_chr = "PREP/nihr.bib",#"PREP/My_PR_PUBS.bib"
                        given_nm_1L_chr = "Matthew",
                        middle_nms_chr = "Phillip",
                        family_nm_1L_chr = "Hamilton",
                        preprint_srvrs_chr = c("aRXiv", "bioRxiv","medRxiv","Research Square")) 
mssng_vals_ls <- make_mssng_vals_ls(pubs_df)
# pubs_df <- pubs_df %>% 
#   replace_mssng_vals_in_pubs_df(mssng_vals_ls = mssng_vals_ls,
#                                 replacements_ls = list(ABSTRACT = c("A brief opinion piece concerning mental health reform in Australia.",
#                                                                     "A suggested policy framework for addressing mental health reform in Australia. Addresses the failure to resource and integrate mental health into the mainstream of the health care system."),
#                                                        KEYWORDS = c("ready4 framework, taxonomies, abbreviations",
#                                                                     "spatial epidemiology, prevalence, mental disorders, adolescence, Melbourne",
#                                                                     "demography, prevalence, mental disorders, adolescence, Australia",
#                                                                     "ready4 framework, readyforwhatsnext, conceptual model, spatial epidemiology",
#                                                                     "synthetic data, utility mapping",
#                                                                     "synthetic data, utility mapping",
#                                                                     "anxiety, AQoL, depression, psychological distress, QALYs, utility mapping",
#                                                                     "suicide prevention, schools, psychoeducation, screening, iCBT",
#                                                                     "assessment, headspace, mental health, need, neuropsychology, survey, youth",
#                                                                     "internal self-management, external strategies, environmental modification, errorless learning, schizophrenia, severe mental illness, functional outcome",
#                                                                     "psoriasis, clinician training, clinician decision-making, guideline concordance, motivational interviewing",
#                                                                     "Neuropsychological assessment, headspace, need, nonmetropolitan, youth mental health",
#                                                                     "qualitative methods, risk factors, service models, social proximity, youth mental health",
#                                                                     #"Bioaerosol, Respiratory infection, Multi-route transmission, Short-range airborne route, Long-range airborne route, Building ventilation",
#                                                                     "Health services administration, Health occupations, Mental disorders",
#                                                                     "Health services administration Mental disorders General medicine",
#                                                                     "Psoriasis, treatment, economic-evaluation",
#                                                                     "headspace, primary mental health care, youth mental health, mental health reform",
#                                                                     "Health services administration, Mental disorders",
#                                                                     "adolescent, depression, Internet, recurrence, secondary prevention",
#                                                                     "cognitive behaviour therapy, graphic medicine, online psychosocial interventions, peer support, social anxiety"),
#                                                        publn_date_tags_chr = c(paste0("2022-02-",
#                                                                                       c(rep("0",9),
#                                                                                         rep("",6)),
#                                                                                       1:27,
#                                                                                       "T00:00:00Z"),
#                                                                                "2015-12-10T00:00:00Z"),
#                                                        URL = c("https://www.tandfonline.com/doi/abs/10.1080/21622965.2019.1624170?journalCode=hapc20",
#                                                                "https://link.springer.com/article/10.1007/s00127-020-02020-6",
#                                                                "https://journals.sagepub.com/doi/10.1177/0004867415624553",
#                                                                "https://www.mja.com.au/journal/2017/206/11/broken-promises-and-missing-steps-mental-health-reform",
#                                                                "https://onlinelibrary.wiley.com/doi/10.1111/eip.13100")
#                                                        ))
pubs_entries_ls <- make_pubs_entries_ls(pubs_df,
                                        tmpl_pub_md_chr = readLines("Prep/pub_template/index.md"))
write_widget_entries(pubs_entries_ls,
                     pub_entries_dir_1L_chr = "content/publication",
                     overwrite_1L_lgl = F)
unlink(paste0("content/publication/",
              c("conference-paper","journal-article","preprint")), 
       recursive = T)
talks_df <- read.csv("Prep/talks.csv") %>%
  dplyr::filter(DESCRIPTION != "")
names(talks_df)
talks_entries_ls <- make_talks_entries_ls(talks_df,
                      tmpl_talk_md_chr = readLines("Prep/talk_template/index.md"),
                      auth_nm_matches_chr = "MP Hamilton",
                      auth_nm_tag_1L_chr = "admin")
write_widget_entries(talks_entries_ls,
                  pub_entries_dir_1L_chr = "content/talk",
                  overwrite_1L_lgl = F)
unlink(paste0("content/talk/",
              c("Coding","Qualitative","Synthesizing")), 
       recursive = T)
# Manual edits to Eoin, Mario and pub type for preprint.
