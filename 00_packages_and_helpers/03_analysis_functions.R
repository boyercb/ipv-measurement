# Main estimator function -------------------------------------------------

main_estimator <- function(outcome, covariates = NULL, level = "couple", 
                           subgroup = NULL, treatment_variable = "Z", no_blocks=F,
                           weights = NULL, data){
  
  if(is.null(covariates)){
    formula <- reformulate(termlabels = treatment_variable, 
                           response = outcome)
  } else {
    formula <- reformulate(termlabels = c(treatment_variable, covariates, 
                                          paste0(treatment_variable,":", covariates)), 
                           response = outcome)
  }
  
  if(!is.null(subgroup)){
    if(!is.character(subgroup)) stop("You must provide subgroup as a character string.")
    data <- subset(data, eval(parse(text = subgroup)))
  }
  
  if(!is.null(weights)) {
    if(!is.character(weights)) stop("You must provide weights as a character string.")
    weights <- parse(text = weights)
  }
  
  if(level == "couple"){
    if (!no_blocks) {
      lm_robust(formula = formula, 
                fixed_effects = ~ blocks,
                se_type = "HC2",
                weights = eval(weights), 
                data = data)
    } else {
      lm_robust(formula = formula, 
                se_type = "HC2",
                weights = eval(weights), 
                data = data)
    }
  } else if(level == "individual"){
    lm_robust(formula = formula, 
              fixed_effects = ~ blocks,
              se_type = SE_type, 
              clusters = cup_id, 
              weights = eval(weights), 
              data = data)
  } else if(level == "RI") {
    lm_robust(formula = formula, 
              fixed_effects = ~ blocks,
              se_type = "none", 
              weights = eval(weights), 
              data = data)
  } else {
    stop("You have supplied a level that doesn't exist (please use 'couple' or 'individual').")
  }
  
}


# Function to grab covariates ---------------------------------------------

get_covariates <- function(outcome_name, covariate_frame){
  if(!outcome_name %in% covariate_frame$outcome) {
    print(paste0("No covariates found for ",outcome_name,".\nDid you use the right lasso dataset?"))
    return(NULL)}
  covariate_frame %>% filter(outcome == outcome_name) %>% select(term) %>% 
    unlist()
}


# Function to get hypotheses ----------------------------------------------

get_hypothesis <- function(outcome_name, outcome_data) {
  if (!all(outcome_name %in% outcome_data$outcome)) {
    bad_outcomes <- outcome_name[!outcome_name %in% outcome_data$outcome]
    stop(paste0("No information found for ", bad_outcomes, "."))
  }
  
  outcome_data %>%
    filter(outcome %in% outcome_name) %>%
    pull(hypothesis)
}

# Multinomial estimator ---------------------------------------------------

multinom_estimator <- function(outcome, covariates = NULL,
                               block_FE = TRUE,
                               subgroup = NULL, 
                               level = NULL,
                               weights = NULL,
                               treatment_variable = "Z", data){
  
  if(is.null(covariates)){
    formula <- reformulate(termlabels = treatment_variable, 
                           response = outcome)
  } else {
    formula <- reformulate(termlabels = c(treatment_variable, covariates, 
                                          paste0(treatment_variable,":", covariates)), 
                           response = outcome)
  }
  
  if(block_FE){
    formula <- update.formula(old = formula, new = . ~ . + as.factor(blocks))
  }
  
  if(!is.null(subgroup)){
    if(!is.character(subgroup)) stop("You must provide subgroup as a character string.")
    data <- subset(data, eval(parse(text = subgroup)))
  }
  
  if(!is.null(weights)) {
    if(!is.character(weights)) stop("You must provide weights as a character string.")
    weights <- parse(text = weights)
  }
  
  multinom(formula = formula, data = data, weights = eval(weights), MaxNWts = 10000000)
  
}


# Logistic estimator ------------------------------------------------------

logit_estimator <- function(outcome, covariates = NULL,
                            block_FE = TRUE,
                            subgroup = NULL,
                            level = NULL,
                            weights = NULL,
                            treatment_variable = "Z", data) {
  
  if(is.null(covariates)) {
    formula <- reformulate(termlabels = treatment_variable,
                           response = outcome)
  } else {
    formula <-
      reformulate(
        termlabels = c(
          treatment_variable,
          covariates,
          paste0(treatment_variable, ":", covariates)
        ),
        response = outcome
      )
  }
  
  if (block_FE) {
    formula <-
      reformulate(
        termlabels = c(
          treatment_variable,
          covariates,
          paste0(treatment_variable, ":", covariates),
          "1 | blocks"
        ),
        response = outcome
      )
  }
  
  if(!is.null(subgroup)){
    if(!is.character(subgroup)) stop("You must provide subgroup as a character string.")
    data <- subset(data, eval(parse(text = subgroup)))
  }
  
  if(!is.null(weights)) {
    if(!is.character(weights)) stop("You must provide weights as a character string.")
    weights <- parse(text = weights)
  }
  
  bife(formula = formula, data = data, model = "logit") 
  
}


# Probit estimator ------------------------------------------------------

probit_estimator <- function(outcome, covariates = NULL,
                             block_FE = TRUE,
                             subgroup = NULL,
                             level = NULL,
                             weights = NULL,
                             treatment_variable = "Z", data) {
  
  if (is.null(covariates)) {
    formula <- reformulate(termlabels = treatment_variable,
                           response = outcome)
  } else {
    formula <-
      reformulate(
        termlabels = c(
          treatment_variable,
          covariates,
          paste0(treatment_variable, ":", covariates)
        ),
        response = outcome
      )
  }
  
  if (block_FE) {
    formula <-
      reformulate(
        termlabels = c(
          treatment_variable,
          covariates,
          paste0(treatment_variable, ":", covariates),
          "1 | blocks"
        ),
        response = outcome
      )
  }
  
  if(!is.null(subgroup)){
    if(!is.character(subgroup)) stop("You must provide subgroup as a character string.")
    data <- subset(data, eval(parse(text = subgroup)))
  }
  
  if(!is.null(weights)) {
    if(!is.character(weights)) stop("You must provide weights as a character string.")
    weights <- parse(text = weights)
  }
  
  bife(formula = formula, data = data, model = "probit")
  
}
