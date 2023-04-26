##  Function to rename default data files to merge-friendly file names:
rename_default_LSAC <- function(old_path,
                                new_path,
                                age_from,
                                age_to,
                                age_interval,
                                letters,
                                files_countdown){
  files=list.files(old_path)
  waves_by_age=seq(from=age_from,to=age_to,by=age_interval)
  table_replacement=cbind(waves_by_age,letters)
  rownames(table_replacement)=waves_by_age
  waves_by_age=sort(seq(from=age_from,to=age_to,by=age_interval),decreasing=TRUE)
  files_countdown=files

  for (n in waves_by_age)
  {
    n=as.character(n)
    #print(n)
    file_index=grep(n,files_countdown,fixed=TRUE)
    if (length(file_index)!=0)
    {
      files_subfiles=files_countdown[file_index]
      for (f in files_subfiles)
      {
        new_name=gsub(n,paste0("_",table_replacement[n,"letters"]),x=f)
        files_countdown=files_countdown[-grep(f,files_countdown,fixed=TRUE)]
        #print(new_name)
        file.copy(from=paste0(old_path,f),to=paste0(new_path,new_name))
      }
    }
  }
}

# ## Function to create the generic version of a wave-specific variable
# generic_var <- function(wave_specific_vec){
#   purrr::map_chr(wave_specific_vec,
#                  ~ stringr::str_sub(.x, start =2))
# }

## Function to import multiple databases, remove unwanted variables from each and merge the trimmed datasets.
read_merge_waves_LSAC <- function(waves,
                             path_to_source,
                             included_vars_generic,
                             included_vars_xwave,
                             included_condition_txt = NULL,
                             data_type,
                             release_version = "rr",
                             cohort,
                             file_extension){
  trimmed_databases_list <- purrr::map(waves,
                                       ~ read_specific_wave_LSAC(wave = .x,
                                                            path_to_source = path_to_source,
                                                            included_vars_generic = included_vars_generic,
                                                            included_vars_xwave = included_vars_xwave,
                                                            included_condition_txt = included_condition_txt,
                                                            data_type = data_type,
                                                            release_version = release_version,
                                                            cohort = cohort,
                                                            file_extension = file_extension))
  merged_dataset <- purrr::reduce(trimmed_databases_list,
                                  ~ dplyr::full_join(.x,
                                                     .y,
                                  ))
  return(merged_dataset)
}

## Function to import a specific dataset and remove unwanted variables
read_specific_wave_LSAC <- function(wave,
                               path_to_source,
                               included_vars_generic,
                               included_vars_xwave,
                               included_condition_txt,
                               data_type,
                               release_version,
                               cohort,
                               file_extension){
  wave_number <- letters[wave]
  data_file_path <- paste0(path_to_source,
                           data_type,
                           release_version,
                           cohort,
                           wave_number,
                           file_extension)
  included_vars_this_wave <- c(included_vars_xwave,paste0(wave_number,
                                                          included_vars_generic))
  if(file_extension == ".dta")
    full_database <- haven::read_dta(data_file_path)
  ##For generic .csv format (NB LSAC not initially provided this way)
  if(file_extension == ".csv")
    full_database <- readr::read_csv(data_file_path,
                                     escape_double = FALSE,
                                     trim_ws = TRUE)
  included_var_list <- setdiff(included_vars_this_wave,
                               names(full_database)) %>%
    setdiff(included_vars_this_wave,.)
  if(!is.null(included_condition_txt)){
    conditional_vars <- eval(parse(text=paste0("names(full_database)[names(full_database) %>% ",
                                               included_condition_txt,
                                               "]")))
    included_var_list <- c(included_var_list, conditional_vars)
  }
  trimmed_database <- full_database %>%
    dplyr::select(included_var_list)
  return(trimmed_database)
}

## Function to calculate socioeconomic position using appropriate wave-specific variable
calculate_sep <- function(wave, sep, sep2){
  purrr::pmap_chr(list(wave,
                       sep,
                       sep2),
                  ~
                    #..1 * ..2 * ..3
                    ifelse(..1 == 1,
                           ifelse(..2 <= -0.842,
                                  "Q1",
                                  ifelse(..2 <= -0.253,
                                         "Q2",
                                         ifelse(..2 <= 0.253,
                                                "Q3",
                                                ifelse(..2 <= 0.842,
                                                       "Q4",
                                                       "Q5")))),
                           ifelse(..3 <= -0.842,
                                  "Q1",
                                  ifelse(..3 <= -0.253,
                                         "Q2",
                                         ifelse(..3 <= 0.253,
                                                "Q3",
                                                ifelse(..3 <= 0.842,
                                                       "Q4",
                                                       "Q5"))))
                    ))
}

##Function for pulling correct wave-specific parent variable from LSAC:
get_col_ref_by_wave_cohort <- function(data_lookup_tb,
                                       wave,
                                       cohort,
                                       target_in_lookup){
  purrr::map2_chr(wave,
                  cohort,
                  ~ data_get(data_lookup_tb = data_lookup_tb %>%
                                           dplyr::filter(COHORT == .y),
                                         lookup_variable = "WAVE",
                                         lookup_reference = as.character(.x),
                                         target_variable = target_in_lookup,
                                         evaluate = FALSE) %>%
                    stringr::str_sub(start=2)

  )
}

add_wave_chort_dep_col <- function(data_tb,
                                   agent_id_col = "hicid",
                                   data_lookup_tb = LSAC_lookup_parent_variables,
                                   wave_col = "wave",
                                   cohort_col = "cohort",
                                   target_in_lookup,
                                   new_col_name){
  data_tb <- data_tb %>%
    dplyr::mutate(!!rlang::sym(new_col_name) := get_col_ref_by_wave_cohort(data_lookup_tb = data_lookup_tb,
                                                                           wave = !!rlang::sym(wave_col),
                                                                           cohort = !!rlang::sym(cohort_col),
                                                                           target_in_lookup = target_in_lookup)
    )
  selected_vars <- data_tb %>%
    dplyr::pull(!!rlang::sym(new_col_name)) %>%
    unique()
  selected_vars <- c(agent_id_col,selected_vars)
  data_tb %>%
    dplyr::mutate(!!rlang::sym(new_col_name) := purrr::map2_dbl(data_tb %>% dplyr::pull(!!rlang::sym(new_col_name)),
                                                                data_tb %>% dplyr::pull(!!rlang::sym(agent_id_col)),
                                                                ~ data_get(data_lookup_tb = data_tb %>%
                                                                                         dplyr::select(selected_vars),
                                                                                       lookup_reference = .y,
                                                                                       lookup_variable = agent_id_col,
                                                                                       target_variable = .x,
                                                                                       evaluate = FALSE)))
}

## Function to rename original LSAC variable names to rfwn variable names:
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
                     dplyr::funs(get_rfwn_new_var_vec(old_var_names_vec = old_var_names_vec,
                                                      lookup_tb = lookup_tb,
                                                      old_var_name_col = old_var_name_col)))
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
