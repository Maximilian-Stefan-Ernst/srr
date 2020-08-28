step_tree2dummy <- function(
  recipe, 
  ..., 
  role = "predictor_treedummy", 
  trained = FALSE, 
  ref_dist = NULL,
  options = list(tree = NA),
  skip = FALSE,
  id = rand_id("tree2dummy")
) {

  terms <- ellipse_check(...) 
  
  add_step(
    recipe, 
    step_tree2dummy_new(
      terms = terms, 
      trained = trained,
      role = role, 
      ref_dist = ref_dist,
      options = options,
      skip = skip,
      id = id
    )
  )
}

step_tree2dummy_new <- 
  function(terms, role, trained, ref_dist, options, skip, id) {
    step(
      subclass = "tree2dummy", 
      terms = terms,
      role = role,
      trained = trained,
      ref_dist = ref_dist,
      options = options,
      skip = skip,
      id = id
    )
  }

prep.step_tree2dummy <- function(x, training, info = NULL, ...) {
  col_names <- terms_select(terms = x$terms, info = info) 
  dummies <- tree2dummy(x$options$tree, training)
}

prep.step_tree2dummy <- function(x, training, info = NULL, ...) {
  col_names <- terms_select(terms = x$terms, info = info) 
  ## You can add error trapping for non-numeric data here and so on. 
  
  dummies <- tree2dummy(x$options$tree, training)
  ## Use the constructor function to return the updated object. 
  ## Note that `trained` is now set to TRUE
  
  step_tree2dummy_new(
    terms = x$terms, 
    trained = TRUE,
    role = x$role, 
    ref_dist = dummies,
    options = x$options,
    skip = x$skip,
    id = x$id
  )
}

bake.step_tree2dummy <- function(object, new_data, ...) {
  ## For illustration (and not speed), we will loop through the affected variables
  ## and do the computations
  dummies <- object$ref_dist
  
  new_data <- bind_cols(new_data, dummies)
  
  ## Always convert to tibbles on the way out
  tibble::as_tibble(new_data)
}

# function to generate dummies --------------------------------------------

tree2dummy <- function(tree, training){
  rules <- rpart.utils::rpart.subrules.table(tree)
  rules <- dplyr::filter(rules, 
                         stringr::str_detect(Subrule, "L"))
  rules$Subrule <- NULL
  names <- pmap_chr(rules, ~get_name_(..1, ..2, ..3, ..4))
  dummies <- pmap(rules, ~get_dummy_(..1, ..2, ..3, ..4, data = training))
  dummies <- map(dummies, as.numeric)
  
  newdata <- suppressMessages(bind_cols(dummies))
  names(newdata) <- names
  
  return(newdata)
}

get_dummy_ <- function(variable, value, less, greater, data){
  variable <- pull(data, variable)
  dummy <- rep(FALSE, length(variable))
  
  if (is.numeric(variable)) {
    if (!is.na(value)) {
      cond <- variable == as.numeric(value)
      dummy <- dummy | cond
    }
    if (!is.na(less)) {
      cond <- variable < as.numeric(less)
      dummy <- dummy | cond
    }
    if (!is.na(greater)) {
      cond <- variable > as.numeric(greater)
      dummy <- dummy | cond
    }
  }
  
  if(!is.numeric(variable)){
    cond <- variable == value
    dummy <- dummy | cond
  }
  return(dummy)
}

get_name_ <- function(variable, value, less, greater) {
  name <- c()
  if (!is.na(value))
    name <- append(name, paste0("eq", value))
  if (!is.na(less))
    name <- append(name, paste0("gr", less))
  if (!is.na(greater))
    name <- append(name, paste0("lo", greater))
  name <- paste(name, collapse = "_or_")
  name <- paste(c(variable, name), collapse = "_is_")
  return(name)
}

