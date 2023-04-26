rename_to_rfwn_vars <- function(db,
                                lookup_tb,
                                old_var_name_col,
                                old_var_stub_col,
                                wave_prefix){
  lookup_tb <- lookup_tb %>%
    dplyr::mutate(!!rlang::sym(old_var_name_col) := ifelse(is.na(!!rlang::sym(old_var_name_col)),
                                                           ifelse(!is.na(!!rlang::sym(old_var_stub_col)),
                                                                  paste0(wave_prefix,!!rlang::sym(old_var_stub_col)),
                                                                  NA_character_),
                                                           !!rlang::sym(old_var_name_col)))

  old_var_names_vec <- names(db)[names(db) %in% (lookup_tb %>%
                                                   dplyr::pull(!!rlang::sym(old_var_name_col)) %>%
                                                   purrr::discard(is.na))]
  db %>%
    dplyr::rename_at(dplyr::vars(old_var_names_vec),
                     ~ get_rfwn_new_var_vec(old_var_names_vec = old_var_names_vec,
                                                      lookup_tb = lookup_tb,
                                                      old_var_name_col = old_var_name_col))
}

get_rfwn_new_var_vec <- function(old_var_names_vec,
                                 lookup_tb,
                                 old_var_name_col){
  purrr::map_chr(old_var_names_vec,
                 ~ data_get(data_lookup_tb = lookup_tb,
                                        lookup_variable = old_var_name_col,
                                        lookup_reference = .,
                                        target_variable = "rfwn_var_name",
                                        evaluate = FALSE))
}
