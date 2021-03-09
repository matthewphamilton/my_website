# Functions
add_auth_tags_to_pubs_df <- function(pubs_df,
                                     auth_nm_matches_chr,
                                     auth_nm_tag_1L_chr = "admin"){
  pubs_df <- pubs_df %>%
    dplyr::mutate(auth_tags_chr = AUTHOR %>% 
                    purrr::map_chr(~{
                      paste0(" - ",
                             .x %>% purrr::map_chr(~{
                               ifelse(.x %in% auth_nm_matches_chr,
                                      auth_nm_tag_1L_chr,
                                      .x)
                             }), 
                             collapse = "\n")
                    })) 
  return(pubs_df)
}
add_url_tag_vars_to_pubs_df <- function(pubs_df){ # Purely placeholder for the moment. Needs updating
  updated_pubs_df <- pubs_df %>%
    dplyr::mutate(name_url_tags_chr = ifelse(T, # MODIFY
                                             paste0("# links:\n# - name: \"\"\n#   url: \"\""),
                                             paste0("links:\n - name: \"NAME_GOES_HERE\"\n   url: \"URL_GOES_HERE\"") # MODIFY
    ),
    pdf_url_tags_chr = ifelse(T, # MODIFY
                              paste0("# url_pdf: ''"),
                              paste0("url_pdf: URL_GOES_HERE") # MODIFY
    ),
    code_url_tags_chr = ifelse(T, # MODIFY
                               paste0("# url_dataset: ''"),
                               paste0("url_dataset: 'URL_GOES_HERE'") # MODIFY
    ),
    dataset_url_tags_chr = ifelse(T, # MODIFY
                                  paste0("# url_dataset: ''"),
                                  paste0("url_dataset: 'URL_GOES_HERE'") # MODIFY
    ),
    poster_url_tags_chr = ifelse(T, # MODIFY
                                 paste0("# url_poster: ''"),
                                 paste0("url_poster: 'URL_GOES_HERE'") # MODIFY
    ),
    project_url_tags_chr = ifelse(T, # MODIFY
                                  paste0("# url_project: ''"),
                                  paste0("url_project: 'URL_GOES_HERE'") # MODIFY
    ),
    slides_url_tags_chr = ifelse(T, # MODIFY
                                 paste0("# url_slides: ''"),
                                 paste0("url_slides: 'URL_GOES_HERE'") # MODIFY
    ),
    source_url_tags_chr = ifelse(T, # MODIFY
                                 paste0("# url_source: ''"),
                                 paste0("url_source: 'URL_GOES_HERE'") # MODIFY
    ),
    video_url_tags_chr = ifelse(T, # MODIFY
                                paste0("# url_video: '' "),
                                paste0("url_video: 'URL_GOES_HERE'") # MODIFY
    )
    )
  return(updated_pubs_df)
}
expand_term_fn <- function(term_1L_chr){
  terms_chr <- c(c(term_1L_chr,c(term_1L_chr) %>% 
        toupper(),c(term_1L_chr,
                    c(term_1L_chr) %>% toupper()) %>% paste0(":")))
  return(terms_chr)
}
get_auth_surnm_mtchs <- function(authorship_ls,
                                 surname_1L_chr){
  surname_matches_chr <- authorship_ls %>% 
    purrr::map_chr(~.x[stringr::str_detect(.x,surname_1L_chr)]) %>% 
    unique()
  return(surname_matches_chr)
}
make_cref_res_ls <- function(doi_chr){ # Add argument for cref polite option
  cref_res_ls <- rcrossref::cr_cn(doi_chr %>% na.omit() %>% as.vector(), 
                                  format = "citeproc-json")  %>% 
    stats::setNames(doi_chr %>% na.omit() %>% as.vector())
  return(cref_res_ls)
}
transform_abstract_ls <- function(abstract_ls){
  abstracts_chr <- abstract_ls %>% purrr::map(~corpus::text_split(.x) %>%
                                                   dplyr::mutate(text = as.character(text)) %>%
                                                   dplyr::pull("text")) %>%
    purrr::map(~{
      abstract_chr <- .x 
      abstract_chr[1] <- ifelse(stringr::word(abstract_chr[1]) %in% (purrr::map(c("Abstract", "Summary"), 
                                                                                ~ expand_term_fn(.x)) %>% 
                                                                       purrr::flatten_chr()),
                                stringr::str_remove(abstract_chr[1],
                                                    stringr::word(abstract_chr[1])) %>%
                                  trimws(which = "left"),
                                abstract_chr[1])
      purrr::map2_chr(abstract_chr,
                      abstract_chr %>% 
                        purrr::map_lgl(~
                                         (stringr::str_detect(.x, 
                                                              "^\\b[A-Z]\\w+:?\\s+\\b[A-Z]")[[1]] |
                                            stringr::word(.x) == .x)
                                       
                        ),
                      ~ ifelse(.y,
                               stringr::str_replace(.x,stringr::word(.x),paste0("**",stringr::word(.x),"**")),
                               .x)
      )
    }) %>%
    purrr::map_chr(~{
      collapsed_1L_chr <- paste0(.x, collapse = "") 
      ifelse(collapsed_1L_chr == "NA",
             NA_character_,
             collapsed_1L_chr)
    })
  return(abstracts_chr)
}
## Application
scholar_pubs_tb <- scholar::get_publications("t1ZHrCoAAAAJ") %>%
  tibble::as_tibble() # Update with ready4show ref once migration complete

preprint_srvrs_chr <- c("aRXiv",
                        "bioRxiv",
                        "medRxiv")
# Note : need to write script that compacts keywords and place call here
bib_pubs_df <- bib2df::bib2df("PREP/My_PR_PUBS.bib") %>%
  dplyr::mutate(abstract_tags_chr = ABSTRACT %>% transform_abstract_ls()) %>%
  add_url_tag_vars_to_pubs_df()
surname_matches_chr <- get_auth_surnm_mtchs(bib_pubs_df$AUTHOR,
                                            "Hamilton")
bib_pubs_df <- bib_pubs_df %>%
  add_auth_tags_to_pubs_df(auth_nm_matches_chr = surname_matches_chr[1:length(surname_matches_chr)])
##
cref_res_ls <- make_cref_res_ls(bib_pubs_df$DOI)
bib_pubs_df <- bib_pubs_df %>%
  dplyr::mutate(publn_date_tags_chr = DOI %>% 
                  purrr::map_chr(~ ifelse(is.na(.x),
                                        NA_character_, # Need alternative source for no-doi docs
                                        cref_res_ls %>%
                                          purrr::pluck(.x) %>%
                                          purrr::pluck("created") %>%
                                          purrr::pluck(2)))) %>%
  dplyr::mutate(doi_tags_chr = DOI %>% purrr::map_chr(~ ifelse(is.na(.x),
                                                             "#doi: ",
                                                             paste0("doi: ",.x)))) %>%
  dplyr::mutate(jrnl_long_nm_tags_chr = purrr::map2_chr(.$DOI, 
                                                   .$JOURNAL,
                                                   ~ ifelse(is.na(.x),
                                                            .y, 
                                                            {jrnl_long_1L_chr <- cref_res_ls %>%
                                                              purrr::pluck(.x) %>%
                                                              purrr::pluck("container-title")
                                                            if(is.null(jrnl_long_1L_chr))
                                                              jrnl_long_1L_chr <- .y
                                                            jrnl_long_1L_chr
                                                            }))) %>%
  dplyr::mutate(jrnl_short_nm_tags_chr = purrr::map2_chr(.$DOI, 
                                                         .$jrnl_long_nm_tags_chr, 
                                                         ~ ifelse(is.na(.x),
                                          .y, 
                                          {jrnl_short_1L_chr <- cref_res_ls %>%
                                            purrr::pluck(.x) %>%
                                            purrr::pluck("container-title-short")
                                          if(is.null(jrnl_short_1L_chr))
                                            jrnl_short_1L_chr <- .y
                                          jrnl_short_1L_chr
                                            }))) %>%
  dplyr::mutate(pub_type_tags_chr = purrr::map2_chr(.$TYPE,
                                                    .$jrnl_long_nm_tags_chr, 
                                                    ~ ifelse(.x == "Journal Article",
                                                                   ifelse(.y %in% preprint_srvrs_chr,
                                                                          "3",
                                                                          "2"),
                                                                   "4")))
  

##



###
pubn_md_chr <- readLines("R/template/index.md")
.x<-1
idx_int <- .x



purrr::map_chr(pubn_md_chr, ~ {
  stringr::str_replace(.x,"ABSTRACT_PLACEHODER",bib_pubs_df[idx_int,]$ABSTRACT) %>% # TRANSFORMED_ABSTRACT
    stringr::str_replace("AUTHOR_PLACEHODER",
                         paste0(" - ",bib_pubs_df[idx_int,]$AUTHOR[[1]], collapse = "\n")) %>%
    stringr::str_replace_all("DATE_PLACEHOLDER",
                         ifelse(is.na(bib_pubs_df$DOI[idx_int]),
                                "", # Need alternative source for no-doi docs
                                cref_res_ls %>%
                                  purrr::pluck(bib_pubs_df$DOI[idx_int]) %>%
                           purrr::pluck("created") %>%
                           purrr::pluck(2))) %>%
    stringr::str_replace("DOI_PLACEHOLDER",
                         ifelse(is.na(bib_pubs_df$DOI[idx_int]),
                                "#doi: ",
                                paste0("doi: ",bib_pubs_df$DOI[idx_int]))) %>%
    stringr::str_replace("NAME_URL_PLACEHOLDER",
                             ifelse(T, # MODIFY
                                    paste0("# links:\n# - name: \"\"\n#   url: \"\""),
                                    paste0("links:\n - name: \"NAME_GOES_HERE\"\n   url: \"URL_GOES_HERE\"") # MODIFY
                                    )) %>%
    stringr::str_replace("URL_PDF_PLACEHOLDER",
                         ifelse(T, # MODIFY
                                paste0("# url_pdf: ''"),
                                paste0("url_pdf: URL_GOES_HERE") # MODIFY
                         )) %>%
    stringr::str_replace("URL_CODE_PLACEHOLDER",
                         ifelse(T, # MODIFY
                                paste0("# url_dataset: ''"),
                                paste0("url_dataset: 'URL_GOES_HERE'") # MODIFY
                         )) %>%
    stringr::str_replace("URL_DATASET_PLACEHOLDER",
                         ifelse(T, # MODIFY
                                paste0("# url_dataset: ''"),
                                paste0("url_dataset: 'URL_GOES_HERE'") # MODIFY
                         )) %>%
    stringr::str_replace("URL_POSTER_PLACEHOLDER",
                         ifelse(T, # MODIFY
                                paste0("# url_poster: ''"),
                                paste0("url_poster: 'URL_GOES_HERE'") # MODIFY
                         )) %>%
    stringr::str_replace("URL_PROJECT_PLACEHOLDER",
                         ifelse(T, # MODIFY
                                paste0("# url_project: ''"),
                                paste0("url_project: 'URL_GOES_HERE'") # MODIFY
                         )) %>%
    stringr::str_replace("URL_SLIDES_PLACEHOLDER",
                         ifelse(T, # MODIFY
                                paste0("# url_slides: ''"),
                                paste0("url_slides: 'URL_GOES_HERE'") # MODIFY
                         )) %>%
    stringr::str_replace("URL_SOURCE_PLACEHOLDER",
                         ifelse(T, # MODIFY
                                paste0("# url_source: ''"),
                                paste0("url_source: 'URL_GOES_HERE'") # MODIFY
                         )) %>%
    stringr::str_replace("URL_VIDEO_PLACEHOLDER",
                         ifelse(T, # MODIFY
                                paste0("# url_video: '' "),
                                paste0("url_video: 'URL_GOES_HERE'") # MODIFY
                         )) %>%
    stringr::str_replace("JOURNAL_LONG_PLACEHOLDER",
                         bib_pubs_df$JOURNAL[idx_int]) %>%
    stringr::str_replace("JOURNAL_SHORT_PLACEHOLDER",
                         bib_pubs_df$JOURNAL[idx_int])  %>% #MODIFY
    stringr::str_replace("PUB_TYPE_PLACEHOLDER",
                         ifelse(bib_pubs_df$CATEGORY[idx_int] == "ARTICLE",
                                ifelse(bib_pubs_df$JOURNAL[idx_int] %in% c("aRXiv",
                                                                    "bioRxiv",
                                                                    "medRxiv"),
                                       "3",
                                       "2"),
                         "4")) %>%
    stringr::str_replace("SUMMARY_PLACEHOLDER",
                         corpus::text_split(bib_pubs_df$ABSTRACT[idx_int]) %>%
                           dplyr::mutate(text = as.character(text)) %>%
                           dplyr::slice(1:2) %>%
                           dplyr::pull("text") %>%
                           as.vector() %>%
                           paste0(collapse=" ") %>%
                           trimws()  %>%
                           paste0("..")
                         )
}) %>% writeLines()
