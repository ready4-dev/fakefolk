data_get <- function(data_lookup_tb,
                     lookup_reference,
                     lookup_variable,
                     target_variable,
                     evaluate = TRUE){
  return_object_ref <- data_lookup_tb %>%
    dplyr::filter(!!rlang::sym(lookup_variable)==lookup_reference) %>%
    dplyr::select(!!target_variable) %>%
    dplyr::pull()
  if(evaluate){
    if(stringr::str_detect(return_object_ref,"::")){
      colon_positions <- stringr::str_locate(return_object_ref,
                                             "::")
      namespace_ref <- stringr::str_sub(return_object_ref,
                                        start=1,
                                        end=colon_positions[1,"start"]-1)
      object_ref <- stringr::str_sub(return_object_ref,
                                     start=colon_positions[1,"end"]+1)

      if(sum(stringr::str_detect(search(),paste0("package:",
                                                 namespace_ref))) == 0){
        namespace_ref_sym <- rlang::sym(namespace_ref)
        attachNamespace(namespace_ref)
        return_object <- get(x = object_ref,
                             envir = as.environment(paste0("package:",
                                                           namespace_ref)))
        detach(paste0("package:",
                      namespace_ref),
               character.only = TRUE)
      }else{
        return_object <- get(x = object_ref,
                             envir = as.environment(paste0("package:",
                                                           namespace_ref)))
      }
    }else{
      return_object<- get(x = return_object_ref)
    }
  }else{
    return_object <- return_object_ref
  }
  return(return_object)
}
