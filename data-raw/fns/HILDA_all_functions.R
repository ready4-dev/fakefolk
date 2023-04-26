## ALL HILDA FUNCTIONS

## Functions to create included variables list and automate database import and merging

## Function to create the generic version of a wave specific variable
# generic_var <- function(wave_specific_vec){
#   purrr::map_chr(wave_specific_vec,
#                  ~ stringr::str_sub(.x, start =2))
# }

## 1.2 Function to import multiple databases, remove unwanted variables from each and merge the trimmed datasets.
read_merge_waves_HILDA <- function(waves,
                             path_to_source,
                             included_vars_generic,
                             included_vars_xwave,
                             data_type,
                             release_version = "170u",
                             file_extension

){
  trimmed_databases_list <- purrr::map(waves,
                                       ~ read_specific_wave_HILDA(wave = .x,
                                                            path_to_source = path_to_source,
                                                            included_vars_generic = included_vars_generic,
                                                            included_vars_xwave = included_vars_xwave,
                                                            data_type = data_type,
                                                            release_version = release_version,
                                                            file_extension = file_extension))
  merged_dataset <- purrr::reduce(trimmed_databases_list,
                                  ~ dplyr::full_join(.x,
                                                     .y))
  return(merged_dataset)
}

## Function to import a specific dataset and remove unwanted variables
read_specific_wave_HILDA <- function(wave,
                               path_to_source,
                               included_vars_generic,
                               included_vars_xwave,
                               data_type,
                               release_version,
                               file_extension){
  wave_letter <- letters[wave]
  data_file_path <- paste0(path_to_source,
                           data_type,
                           wave_letter,
                           release_version,
                           file_extension)
  included_vars_this_wave <- c(included_vars_xwave,paste0(wave_letter,
                                                          included_vars_generic))
  if(file_extension == ".dta")
    full_database <- haven::read_dta(data_file_path)
  if(file_extension == ".tab")
    full_database <- readr::read_delim(data_file_path,
                                       escape_double = FALSE,
                                       trim_ws = TRUE, delim="\t")
  included_var_list <- setdiff(included_vars_this_wave,
                               names(full_database)) %>%
    setdiff(included_vars_this_wave,.)
  trimmed_database <- full_database %>%
    dplyr::select(included_var_list)
  return(trimmed_database)
}

## 1.4 Function to get ids to use when creating balanced dataset
balanced_dataset_ids <- function(path_to_source,
                                 release_version = "170u",
                                 file_extension){

  data_file_path <- paste0(path_to_source,
                           "Master_",
                            "q",
                           release_version,
                           file_extension)
  if(file_extension == ".dta")
    master_lookup <- haven::read_dta(data_file_path)
  if(file_extension == ".tab")
    master_lookup <- readr::read_delim(data_file_path,
                                       escape_double = FALSE,
                                       trim_ws = TRUE, delim = "\t" )
  included_xwaveids_balanced <- master_lookup %>%
    dplyr::filter(ivwptn=="XXXXXXXXXXXXXXXX") %>%
    dplyr::pull(xwaveid) %>%
    as.vector()
  return(included_xwaveids_balanced)
}

##Functions for parental incarceration status
#Function to generate population
gen_incarcerated_hh_mbr_pop <- function(db,
                                        target_variable = "socio_econ_ever_incarcerated_hh_mbr",
                                        agt_age_vect,
                                        moth_id_vect,
                                        fath_id_vect,
                                        agt_id_col = "xwaveid"
){
  purrr::pmap(list(agt_age_vect,
                   moth_id_vect,
                   fath_id_vect),
              ~ gen_incarcerated_hh_mbr_ind(db = db,
                                            agt_age =..1,
                                            moth_id = switch((..2=="")+1,..2,NULL),# implements: https://www.r-bloggers.com/use-switch-instead-of-ifelse-to-return-a-null/
                                            fath_id = switch((..3=="")+1,..3,NULL),#ifelse(..3=="",NULL,..3),
                                            target_variable = target_variable,
                                            agt_id_col = agt_id_col)) %>%
    unlist()
}

gen_incarcerated_hh_mbr_ind <- function(db,
                                        agt_age,
                                        moth_id,
                                        fath_id,
                                        target_variable,
                                        agt_id_col
){
  char_vec <- purrr::map_lgl(c(moth_id,fath_id),
                             ~ ifelse(is.null(.x), NA_real_,
                                      data_get(data_lookup_tb = db,
                                               lookup_variable = agt_id_col,
                                               lookup_reference = .x,
                                               target_variable = target_variable,
                                               evaluate = FALSE)) )

  mother_inc_hh_mbr <- char_vec[1]
  father_inc_hh_mbr <- char_vec[2]
  ifelse(agt_age>=18,
         NA,
         ifelse(all(c(is.na(mother_inc_hh_mbr),is.na(father_inc_hh_mbr))),
                NA,
                any(c(mother_inc_hh_mbr, father_inc_hh_mbr), na.rm = TRUE))
  )
}

##Functions for parental death status
gen_parental_death_pop <- function(db,
                                   target_variable = "family_ever_widowed",
                                   agt_age_vect,
                                   moth_id_vect,
                                   fath_id_vect,
                                   agt_id_col = "xwaveid"
){
  purrr::pmap(list(agt_age_vect,
                   moth_id_vect,
                   fath_id_vect),
              ~ gen_parental_death_ind(db = db,
                                       agt_age =..1,
                                       moth_id = switch((..2=="")+1,..2,NULL),# implements: https://www.r-bloggers.com/use-switch-instead-of-ifelse-to-return-a-null/
                                       fath_id = switch((..3=="")+1,..3,NULL),#ifelse(..3=="",NULL,..3),
                                       target_variable = target_variable,
                                       agt_id_col = agt_id_col)) %>%
    unlist()
}

gen_parental_death_ind <- function(db,
                                   agt_age,
                                   moth_id,
                                   fath_id,
                                   target_variable,
                                   agt_id_col
){
  char_vec <- purrr::map_lgl(c(moth_id,fath_id),
                             ~ ifelse(is.null(.x), NA_real_,
                                      data_get(data_lookup_tb = db,
                                               lookup_variable = agt_id_col,
                                               lookup_reference = .x,
                                               target_variable = target_variable,
                                               evaluate = FALSE)) )

  mother_widow <- char_vec[1]
  father_widow <- char_vec[2]
  ifelse(agt_age>=18,
         NA,
         any(mother_widow, father_widow, na.rm=TRUE))
}


#Functions to generate parental mental illness status
gen_parental_MI_pop <- function(db,
                                target_variable = "mental_health_curr_prev_adult_md_bl",
                                agt_age_vect,
                                moth_id_vect,
                                fath_id_vect,
                                agt_id_col = "xwaveid"
){
  purrr::pmap(list(agt_age_vect,
                   moth_id_vect,
                   fath_id_vect),
              ~ gen_parental_MI_ind(db = db,
                                    agt_age =..1,
                                    moth_id = switch((..2=="")+1,..2,NULL),# implements: https://www.r-bloggers.com/use-switch-instead-of-ifelse-to-return-a-null/
                                    fath_id = switch((..3=="")+1,..3,NULL),#ifelse(..3=="",NULL,..3),
                                    target_variable = target_variable,
                                    agt_id_col = agt_id_col)) %>%
    unlist()
}

gen_parental_MI_ind <- function(db,
                                agt_age,
                                moth_id,
                                fath_id,
                                target_variable,
                                agt_id_col
){
  char_vec <- purrr::map_lgl(c(moth_id,fath_id),
                             ~ ifelse(is.null(.x), NA_real_,
                                      data_get(data_lookup_tb = db,
                                               lookup_variable = agt_id_col,
                                               lookup_reference = .x,
                                               target_variable = target_variable,
                                               evaluate = FALSE)) )

  mother_MI <- char_vec[1]
  father_MI <- char_vec[2]
  ifelse(agt_age>=18,
         NA,
         ifelse(all(c(is.na(mother_MI),is.na(father_MI))),
                NA,
                any(c(mother_MI, father_MI), na.rm = TRUE)))
}

# Functions to generate parental substance abuse population
gen_parental_SU_pop <- function(db,
                                target_variable = "mental_health_ever_substance_abuse",
                                agt_age_vect,
                                moth_id_vect,
                                fath_id_vect,
                                agt_id_col = "xwaveid"
){
  purrr::pmap(list(agt_age_vect,
                   moth_id_vect,
                   fath_id_vect),
              ~ gen_parental_SU_ind(db = db,
                                    agt_age =..1,
                                    moth_id = switch((..2=="")+1,..2,NULL),# implements: https://www.r-bloggers.com/use-switch-instead-of-ifelse-to-return-a-null/
                                    fath_id = switch((..3=="")+1,..3,NULL),#ifelse(..3=="",NULL,..3),
                                    target_variable = target_variable,
                                    agt_id_col = agt_id_col)) %>%
    unlist()
}

gen_parental_SU_ind <- function(db,
                                agt_age,
                                moth_id,
                                fath_id,
                                target_variable,
                                agt_id_col
){
  char_vec <- purrr::map_lgl(c(moth_id,fath_id),
                             ~ ifelse(is.null(.x), NA_real_,
                                      data_get(data_lookup_tb = db,
                                               lookup_variable = agt_id_col,
                                               lookup_reference = .x,
                                               target_variable = target_variable,
                                               evaluate = FALSE)) )

  mother_SU <- char_vec[1]
  father_SU <- char_vec[2]
  ifelse(agt_age>=18,
         NA,
         ifelse(all(c(is.na(mother_SU),is.na(father_SU))),
                NA,
                any(c(mother_SU, father_SU), na.rm = T)))
}

#Function to generate Country of Birth status to children <15years
#Assumes that if both mother and father are Australian, child is Australian [requires dual parent for this; will impute for single parent and overseas born]
gen_cob_pop <- function(db,
                        target_variable = "ianbcob",
                        agt_age_vect,
                        moth_id_vect,
                        fath_id_vect,
                        agt_id_col = "xwaveid"
){
  purrr::pmap(list(agt_age_vect,
                   moth_id_vect,
                   fath_id_vect),
              ~ gen_cob_ind(db = db,
                            agt_age = ..1,
                            moth_id = switch((..2=="")+1,..2,NULL),
                            fath_id = switch((..3=="")+1,..3,NULL),
                            target_variable = target_variable,
                            agt_id_col = agt_id_col)) %>%
    unlist()
}

gen_cob_ind <- function(db,
                        agt_age,
                        moth_id,
                        fath_id,
                        target_variable,
                        agt_id_col
){
  char_vec <- purrr::map_dbl(c(moth_id, fath_id),
                             ~ ifelse(is.null(.x), NA_real_,
                                      data_get(data_lookup_tb = db,
                                               lookup_variable = agt_id_col,
                                               lookup_reference = .x,
                                               target_variable = target_variable,
                                               evaluate = FALSE)) )

  mother_cob <- char_vec[1]
  father_cob <- char_vec[2]
  ifelse(mother_cob ==1 & father_cob ==1,
         1,
         NA)
}

# Functions to generate generational status
gen_ausgen_pop <- function(db,
                           target_variable = "demographics_country_of_birth",
                           agt_id_vect,
                           moth_id_vect,
                           fath_id_vect,
                           agt_id_col = "xwaveid"
){
  purrr::pmap(list(agt_id_vect,
                   moth_id_vect,
                   fath_id_vect),
              ~ gen_ausgen_ind(db = db,
                               agt_id = ..1,
                               moth_id = switch((..2=="")+1,..2,NULL),# implements: https://www.r-bloggers.com/use-switch-instead-of-ifelse-to-return-a-null/
                               fath_id = switch((..3=="")+1,..3,NULL),#ifelse(..3=="",NULL,..3),
                               target_variable = target_variable,
                               agt_id_col = agt_id_col)) %>%
    unlist()
}

gen_ausgen_ind <- function(db,
                           agt_id,
                           moth_id,
                           fath_id,
                           target_variable,
                           agt_id_col
){
  char_vec <- purrr::map_dbl(c(agt_id, moth_id, fath_id),
                             ~ ifelse(is.null(.x), NA_real_,
                                      data_get(data_lookup_tb = db,
                                               lookup_variable = agt_id_col,
                                               lookup_reference = .x,
                                               target_variable = target_variable,
                                               evaluate = FALSE)) )
  agent_cob <- char_vec[1]
  mother_cob <- char_vec[2]
  father_cob <- char_vec[3]
  ifelse(mother_cob==1 & father_cob == 1,
         "THIRD_PLUS",
         ifelse((agent_cob==1 & (mother_cob %in% c(2,3) | father_cob %in% c(2,3))),
                "SECOND",
                NA))
}

#Function to apply parental ATSI status to children <15years
gen_parental_ATSI_pop <- function(db,
                                  target_variable = "demographics_ATSI_status",
                                  agt_age_vect,
                                  moth_id_vect,
                                  fath_id_vect,
                                  agt_id_col = "xwaveid"
){
  purrr::pmap(list(agt_age_vect,
                   moth_id_vect,
                   fath_id_vect),
              ~ gen_parental_ATSI_ind(db = db,
                                      agt_age =..1,
                                      moth_id = switch((..2=="")+1,..2,NULL),# implements: https://www.r-bloggers.com/use-switch-instead-of-ifelse-to-return-a-null/
                                      fath_id = switch((..3=="")+1,..3,NULL),#ifelse(..3=="",NULL,..3),
                                      target_variable = target_variable,
                                      agt_id_col = agt_id_col)) %>%
    unlist()
}

gen_parental_ATSI_ind <- function(db,
                                  agt_age,
                                  moth_id,
                                  fath_id,
                                  target_variable,
                                  agt_id_col
){
  char_vec <- purrr::map_lgl(c(moth_id,fath_id),
                             ~ ifelse(is.null(.x), NA_real_,
                                      data_get(data_lookup_tb = db,
                                               lookup_variable = agt_id_col,
                                               lookup_reference = .x,
                                               target_variable = target_variable,
                                               evaluate = FALSE)) )

  mother_ATSI <- char_vec[1]
  father_ATSI <- char_vec[2]
  any(c(mother_ATSI, father_ATSI), na.rm = TRUE)

}

#Function to apply parental CALD status to children <15years
gen_parental_CALD_pop <- function(db,
                                  target_variable = "demographics_CALD_status",
                                  agt_age_vect,
                                  moth_id_vect,
                                  fath_id_vect,
                                  agt_id_col = "xwaveid"
){
  purrr::pmap(list(agt_age_vect,
                   moth_id_vect,
                   fath_id_vect),
              ~ gen_parental_CALD_ind(db = db,
                                      agt_age =..1,
                                      moth_id = switch((..2=="")+1,..2,NULL),# implements: https://www.r-bloggers.com/use-switch-instead-of-ifelse-to-return-a-null/
                                      fath_id = switch((..3=="")+1,..3,NULL),#ifelse(..3=="",NULL,..3),
                                      target_variable = target_variable,
                                      agt_id_col = agt_id_col)) %>%
    unlist()
}

gen_parental_CALD_ind <- function(db,
                                  agt_age,
                                  moth_id,
                                  fath_id,
                                  target_variable,
                                  agt_id_col
){
  char_vec <- purrr::map_lgl(c(moth_id,fath_id),
                             ~ ifelse(is.null(.x), NA_real_,
                                      data_get(data_lookup_tb = db,
                                               lookup_variable = agt_id_col,
                                               lookup_reference = .x,
                                               target_variable = target_variable,
                                               evaluate = FALSE)) )

  mother_CALD <- char_vec[1]
  father_CALD <- char_vec[2]
  any(c(mother_CALD, father_CALD), na.rm = TRUE)

}

#Functions to generate CHILD population, individuals and lists
get_child_id_pop <- function(db,
                             target_variable = "xwaveid",
                             agt_id_vec,
                             hh_id_col,
                             agt_rel_type_col,
                             agt_id_col = "xwaveid"){
  purrr::map(agt_id_vec,
             ~ get_child_id_ind(db = db,
                                agt_id = .x,
                                hh_id_col,
                                agt_rel_type_col,
                                target_variable = target_variable,
                                agt_id_col = agt_id_col))
}

get_child_id_ind <- function(db,
                             agt_id,
                             hh_id_col,
                             agt_rel_type_col,
                             target_variable,
                             agt_id_col){
  agt_rel_type <- data_get(data_lookup_tb = db,
                           lookup_variable = agt_id_col,
                           lookup_reference = agt_id,
                           target_variable = agt_rel_type_col,
                           evaluate = FALSE)
  if(!agt_rel_type %in% c(1:3, 5:7)){
    NA_character_
  }else{
    hh_id <- data_get(data_lookup_tb = db,
                      lookup_variable = agt_id_col,
                      lookup_reference = agt_id,
                      target_variable = hh_id_col,
                      evaluate = FALSE)

    hh_db <- db %>%
      dplyr::filter(!!rlang::sym(hh_id_col)==hh_id) %>%
      dplyr::select(agt_rel_type_col,
                    agt_id_col)

    hh_db <- hh_db %>%
      dplyr::filter(!!rlang::sym(agt_id_col) != agt_id) %>%
      dplyr::filter(!!rlang::sym(agt_rel_type_col) %in% c(8:10))

    childs_ids <- dplyr::pull(hh_db,agt_id_col)
    if(identical(childs_ids,
                 character(0))){
      NA_character_
    }else{
      childs_ids
    }
  }
}

#Functions to generate SIBLING population and lists
get_sibling_id_pop <- function(db,
                               target_variable = "xwaveid",
                               agt_id_vec,
                               hh_id_col,
                               agt_rel_type_col,
                               agt_id_col = "xwaveid"){
  purrr::map(agt_id_vec,
             ~ get_sibling_id_ind(db = db,
                                  agt_id = .x,
                                  hh_id_col,
                                  agt_rel_type_col,
                                  target_variable = target_variable,
                                  agt_id_col = agt_id_col))
}

get_sibling_id_ind <- function(db,
                               agt_id,
                               hh_id_col,
                               agt_rel_type_col,
                               target_variable,
                               agt_id_col){
  agt_rel_type <- data_get(data_lookup_tb = db,
                           lookup_variable = agt_id_col,
                           lookup_reference = agt_id,
                           target_variable = agt_rel_type_col,
                           evaluate = FALSE)
  if(!agt_rel_type %in% 8:10){
    NA_character_
  }else{
    hh_id <- data_get(data_lookup_tb = db,
                      lookup_variable = agt_id_col,
                      lookup_reference = agt_id,
                      target_variable = hh_id_col,
                      evaluate = FALSE)

    hh_db <- db %>%
      dplyr::filter(!!rlang::sym(hh_id_col)==hh_id) %>%
      dplyr::select(agt_rel_type_col,
                    agt_id_col)

    hh_db <- hh_db %>%
      dplyr::filter(!!rlang::sym(agt_id_col) != agt_id) %>%
      dplyr::filter(!!rlang::sym(agt_rel_type_col) %in% c(8:10))

    siblings_ids <- dplyr::pull(hh_db,agt_id_col)
    if(identical(siblings_ids,
                 character(0))){
      NA_character_
    }else{
      siblings_ids
    }
  }
}

#Function to generate parents of ACE children
parent_ace_ind <- function(data_tb,
                           child_ids){
  ifelse(any(is.na(child_ids)),
         NA,
         purrr::map_lgl(child_ids,
                        ~ data_get(data_lookup_tb = data_tb,
                                   lookup_variable = "xwaveid",
                                   lookup_reference = .x,
                                   target_variable = "ace_present_bl",
                                   evaluate = FALSE)) %>%
           check_any_true())

}
check_any_true <- function(input_vec){
  input_vec <- input_vec[!is.na(input_vec)]
  ifelse(identical(input_vec,logical()),
         NA,
         ifelse(sum(input_vec > 0),
                TRUE,
                FALSE))
}

#Function to generate siblings of ACE children
sibling_ace_ind <- function(data_tb,
                            sibling_ids){
  ifelse(is.na(sibling_ids),
         NA,
         purrr::map_lgl(sibling_ids,
                        ~ data_get(data_lookup_tb = data_tb,
                                   lookup_variable = "xwaveid",
                                   lookup_reference = .x,
                                   target_variable = "ace_present_bl",
                                   evaluate = FALSE) %>%
                          any(na.rm = TRUE)))
}

## Function to rename HILDA-specific (and LSAC) variable names to RFWN var names based on look-up table
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
