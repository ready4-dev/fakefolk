### Function to evaluate best order of additional vars to model/add to synthetic data
# NB. Function sequentially evaluates "best" dependent var to model based on exlpanatory power and using existing predictors
estimate_mdl_type <- function(y,
                              seed_predictors_vec,
                              rfwn_seed,
                              model_type){
  predictors_ls <- purrr::map(seed_predictors_vec,
                              ~ rfwn_seed %>% dplyr::select(.x) %>% dplyr::pull()) %>%
    stats::setNames(seed_predictors_vec)
  mdl_1_str <- paste0("y ~ ",paste0("predictors_ls$",seed_predictors_vec, collapse = " + "))
  if(model_type == "lm")
    mdl <- lm(as.formula(mdl_1_str),data = rfwn_seed)
  if(model_type == "multinom")
    mdl <- nnet::multinom(as.formula(mdl_1_str),  data = rfwn_seed)
  mdl
}
get_best_predictor <- function(rfwn_seed,
                               seed_predictors_vec,
                               var_type_vec,
                               criterion_stat_nm,
                               var_type
){
  if (var_type == "continuous") {
    model_type = "lm"
  } else {
    model_type = "multinom"
  }

  if(var_type == "continuous"){
    rfwn_seed %>%
      dplyr::select(var_type_vec, -seed_predictors_vec) %>%
      purrr::map(~estimate_mdl_type(y = .x, seed_predictors_vec = seed_predictors_vec, rfwn_seed = rfwn_seed, model_type = model_type)) %>%
      purrr::map(summary) %>%
      purrr::map_dbl(criterion_stat_nm) %>%
      sort(decreasing = T) %>%
      names() %>%
      .[1] # Extract name of var with highest metric
  } else {
    rfwn_seed %>%
      dplyr::select(var_type_vec, -seed_predictors_vec) %>%
      purrr::map(~estimate_mdl_type(y = .x, seed_predictors_vec = seed_predictors_vec, rfwn_seed = rfwn_seed, model_type = model_type)) %>%
      purrr::map_dbl(DescTools::PseudoR2, which = criterion_stat_nm) %>%
      sort(decreasing = T) %>%
      names() %>%
      .[1] # Extract name of var with highest metric
  }
}

order_dep_vars <- function(rfwn_seed,
                           criterion_stat_nm,
                           seed_predictors_vec,
                           var_type_vec,
                           var_type){
  purrr::reduce(1:length( var_type_vec),
                .init = seed_predictors_vec,
                ~ c(.x,get_best_predictor(rfwn_seed = rfwn_seed,
                                          seed_predictors_vec = .x,
                                          var_type_vec = setdiff( var_type_vec,.x),
                                          criterion_stat_nm = criterion_stat_nm,
                                          var_type = var_type))) %>%
    setdiff(seed_predictors_vec)
}

