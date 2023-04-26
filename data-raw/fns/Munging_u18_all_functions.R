#Function to link databases via common bins (i.e. agents in common bins)
add_linked_db_id_col <- function(unique_bin_DB_1,
                                 ids_from_same_bin_DB_2){
  unique_bin_DB_1 %>%
    dplyr::mutate(matched_id_db_2 = sample(ids_from_same_bin_DB_2, size = nrow(unique_bin_DB_1), replace = TRUE))
}

match_dbs_by_bin <- function(db_1_bins,
                             matched_bins_in_db2,
                             db_1,
                             db_2,
                             db_1_bin_col,
                             db_2_id_col,
                             data_lookup_tb
                             #target_variable)
){
  purrr::map2_dfr(db_1_bins,
                  matched_bins_in_db2,
                  ~ add_linked_db_id_col(unique_bin_DB_1 = db_1 %>%
                                           dplyr::filter(!!rlang::sym(db_1_bin_col) == .x), # sos_sex_age_par_sib_ses_CALD_ATSI
                                         ids_from_same_bin_DB_2 = db_2 %>%
                                           dplyr::filter(!!rlang::sym(data_get(data_lookup_tb = data_lookup_tb,
                                                                                           lookup_reference = .y %>% stringr::str_length(),
                                                                                           lookup_variable = "str_length",
                                                                                           target_variable = "col_name",
                                                                                           evaluate = FALSE)) == .y) %>%
                                           dplyr::pull(!!rlang::sym(db_2_id_col))
                  )
  )
}

