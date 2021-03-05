scholar_pubs_tb <- scholar::get_publications("t1ZHrCoAAAAJ") %>%
  tibble::as_tibble() # Update with ready4show ref once migration complete
# Note : need to write script that compacts keywords and place call here
bib_pubs_df <- bib2df::bib2df("R/My_Pubs_With_Abstract.txt") 
surname_matches_chr <- bib_pubs_df$AUTHOR %>% purrr::map_chr(~.x[stringr::str_detect(.x,"Hamilton")]) %>% unique()
admin_matches_chr <- surname_matches_chr[1:length(surname_matches_chr)]
# bib_pubs_df$DOI[1] %>% fulltext::ft_get() -> test_ls