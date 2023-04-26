# Run functions related to generation of household members for 18-25 group (set of functions used during munge)

# Function to evaluate different length multiwave variables in list
get_nth_from_each_element <- function(col_ls,
                                      n,
                                      init_var_type){
  if(init_var_type == "numeric")
    purrr::map_dbl(col_ls,
                   ~ .x[n])
}
gen_multi_wave_var <- function(db,
                               var_stub,
                               init_var_type,
                               condition){
  sub_db <- db
  col_names <- sub_db %>%
    dplyr::select(dplyr::contains(var_stub)) %>%
    names()
  if(identical(col_names,character(0))){
    rep(NA,times=nrow(db))
  }else{
    col_ls <- purrr::map(col_names,
                         ~ sub_db %>% dplyr::pull(.x) )
    col_length <- length(col_ls %>% purrr::pluck(1))
    purrr::map_lgl(1:col_length,
                   ~ eval(parse(text=paste0("any(get_nth_from_each_element(col_ls = col_ls,n = .x,init_var_type = \"",
                                            init_var_type,
                                            "\")",
                                            condition,
                                            ",na.rm=TRUE)"))))
  }
}

## Function(s) for deriving vars from intermediate vars NOT IN ALL waves/tibbles
get_CALD_one_case_one_wave <- function(anbcob,
                                       anlote){
  ifelse(!is.null(anlote),
         ifelse((anlote==1 |
                   anbcob==3),
                TRUE,
                FALSE),
         ifelse(anbcob==3,
                TRUE,
                FALSE))
}
get_CALD_all_cases_one_wave <- function(db,
                                        wave_prefix){

  anbcob_vec <- db %>% dplyr::pull(!!rlang::sym(paste0(wave_prefix,"anbcob")))
  if(paste0(wave_prefix,"anlote") %in% names(db)){
    anlote_vec <- db %>% dplyr::pull(!!rlang::sym(paste0(wave_prefix,"anlote")))
    purrr::map2_lgl(anbcob_vec,
                    anlote_vec,
                    ~ get_CALD_one_case_one_wave(anbcob = .x,
                                                 anlote = .y))
  }else{
    purrr::map_lgl(anbcob_vec,
                   ~ get_CALD_one_case_one_wave(anbcob = .x,
                                                anlote = NULL))
  }
}


