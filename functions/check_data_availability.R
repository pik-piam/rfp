# This file contains two functions that check the availability of variables in
# scenario data to make sure that all RFPs can be calculated.
# The function check_data_availability calls the model specific function 
# check_data_availability_model

check_data_availability_model <- function(data, i_model, equations, assumptions) {
  
  # Get assumption elements (names)
  assumption_elements <- unique(unlist(lapply(assumptions, function(x) {lapply(x, names)})))
  
  # List variables for all equations
  list_vars_per_eq <- lapply(equations$rhs, function(x) {
    myformula <- as.formula(paste("~", x))
    out <- all.vars(myformula)
    out <- out[which(!out %in% assumption_elements)]
    return(out)
  })
  names(list_vars_per_eq) <- gsub("`", "", equations$lhs)
  
  # Remove equations based on RFP variables
  list_vars_per_eq <- Filter(function(x) !any(grepl("RFP", x)), list_vars_per_eq)
  
  tmp_data <- data %>% 
    filter(grepl(i_model, model))
  
  model_name <- unique(tmp_data$model)
  
  all_scenarios <- unique(tmp_data$scenario)
  all_regions   <- unique(tmp_data$region)
  all_periods   <- unique(tmp_data$period[which(!is.na(tmp_data$value) & tmp_data$variable == "GDP|PPP")])
  
  all_combinations <- expand.grid("scenario"=all_scenarios, "region"=all_regions, "period"=all_periods) %>% 
    unite("all", scenario, region, period, sep = "#", remove = FALSE) %>% 
    mutate(label = paste0("(", scenario, ", ", region, ", ", period, ")"))
  
  eq_info <- lapply(names(list_vars_per_eq), function(x) {
    
    vars <- list_vars_per_eq[[x]]
    
    info <- lapply(vars, function(y) {
      tmp <- tmp_data %>% 
        filter(variable == y) %>% 
        unite("all", scenario, region, period, sep = "#", remove = FALSE)
      
      if (nrow(tmp) > 0) {
        if (nrow(tmp) == nrow(all_combinations)) {
          out <- list(
            "status" = "Fully available",
            "number of missing elements" = 0,
            "missing elemts" = NA,
            "missing data" = NULL
          )
        } else {
          missing_elements <- all_combinations$label[which(!all_combinations$all %in% tmp$all)]
          
          out <- list(
            "status" = "Partially available",
            "number of missing elements" = length(missing_elements),
            "missing" = paste(missing_elements[1:10], collapse= ","),
            "missing data" = all_combinations[which(!all_combinations$all %in% tmp$all),] %>% 
              mutate(model = model_name, variable = y, unit = "", value = 0) %>% 
              select(model, scenario, region, variable, unit, period, value)
          )          
        }
      } else {
        out <- list(
          "status" = "Unavailable",
          "number of missing elements" = nrow(all_combinations),
          "missing" = "All",
          "missing data" = all_combinations %>% 
            mutate(model = model_name, variable = y, unit = "", value = 0) %>% 
            select(model, scenario, region, variable, unit, period, value)
        )
      }
    })
    
    names(info) <- vars
    
    all_status <- sapply(info, function(x) x$status)
    
    if (all(all_status == "Fully available")) {
      computable <- "Fully"
    } else {
      if (any(all_status == "Unavailable")) {
        computable <- "No"
      } else {
        computable <- "Partially"
      }
    }
    
    out2 <- list(
      "summary" = data.frame(
        rfp = x,
        computable = computable
      ),
      "variable info" = info
    )
    
    return(out2)
  })
  
  equation_information <- lapply(eq_info, function(x) x[["variable info"]])
  names(equation_information) <- names(list_vars_per_eq)
  
  out <- list(
    "summary" = lapply(eq_info, function(x) x$summary) %>% do.call("rbind", .),
    "equation information" = equation_information
  )
  
  return(out)
  
}


check_data_availability <- function(data, equations, assumptions) {
  model_names <- unique(data$model)
  
  out <- lapply(model_names, function(x) check_data_availability_model(data, x, equations, assumptions))
  
  names(out) <- model_names
  
  return(out)
  
}
