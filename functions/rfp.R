# This file contains the functions that take in scenario data, RFP equations and
# assumptions to compute RFPs

compute_variables <- function(data, equations, assumptions = NULL, units = NA,
                             na.rm = TRUE, completeMissing = TRUE, only.new = TRUE,
                             variable = "variable", unit = NA,
                             value = "value",
                             VERBOSE=FALSE) {
  # ---- guardians ----
  if (!is.data.frame(data))
    stop("Only works with data frames")
  
  if (!is.list(equations))
    stop("'equations' must be a list of formula strings")
  
  if (!is.null(assumptions)) {
    if (!is.list(assumptions))
      stop("'assumptions' must be a list of data frames")
    
    if (is.list(assumptions) & !all(sapply(assumptions, is.data.frame))){
      stop("'assumptions' must be a list of data frames")
    }
    
  }
  
  .colnames <- colnames(data)
  
  if (!variable %in% .colnames)
    stop("No column '", variable, "' found'")
  
  if (!value %in% .colnames)
    stop("No column '", value, "' found'")
  
  if (is.na(unit)) {
    if ("unit" %in% .colnames)
      unit <- "unit"
  } else {
    if (!unit %in% .colnames)
      stop("No column '", unit, "' found.")
  }
  
  # ignore magrittr dot
  . <- NULL
  
  # Check/add units
  if (!all(is.na(units))) { # If all elements of the vector units are not NAs
    if (length(units) == length(equations)) { # Check if units has the same length as the number of provided equations
      for (i in 1:length(equations))
        equations[i][[1]] <- c(equations[i][[1]], units[i])
    } else if (1 == length(units)) { # If the length is 1 (using the same unit for all equations)
      for (i in 1:length(equations))
        equations[i][[1]] <- c(equations[i][[1]], units)
    } else # If not raise error and stop
      stop("units must be of the same length as equations or of length one.")
  }
  
  # ---- parse equations ----
  .units <- lapply(equations, function(l) { l[2] }) %>%
    unlist()
  
  equations <- lapply(equations,
                      function(l) {
                        paste0("~", l[[1]]) %>%
                          stats::formula() %>%
                          lazyeval::interp()
                      })
  names(equations) <- gsub("`", "", names(equations))
  
  equations.rhs.varnames <- lapply(equations, all.vars) %>%
    unlist() %>%
    unique() %>%
    setdiff(names(equations))
  
  if (length(equations.rhs.varnames) == 0) { # If there is only one equation and the rhs is NA the length of equations.rhs.varnames is 0
    .colnames_novarval <- setdiff(.colnames, c("variable", "unit", "value"))
    dimensions <- lapply(.colnames_novarval, function(x) {unique(data[[x]])} )
    names(dimensions) <- .colnames_novarval
    
    data_ <- expand.grid(dimensions) %>% 
      mutate(variable = names(equations)) %>% 
      mutate(unit = units) %>% 
      mutate(value = NA) 
    
  } else {
    # ---- filter for variables used on rhs ----
    data_ <- data %>%
      filter(!!sym(variable) %in% equations.rhs.varnames)
    
    # ---- drop unit column, if necessary ----
    if (!is.na(unit)) {
      variables.units <- data_ %>%
        distinct(!!sym(variable), !!sym(unit)) %>%
        filter(!!sym(variable) %in% equations.rhs.varnames)
      
      data_ <- data_ %>%
        select(-!!sym(unit))
    }
    
    # ---- fill missing data ----
    if(is.logical(completeMissing)){
      if(completeMissing){
        completeMissing_test <- TRUE
        .expand_cols <- setdiff(colnames(removeColNa(data_)), value)
      } else {
        completeMissing_test <- FALSE
      }
    } else {
      completeMissing_test <- TRUE
      .expand_cols <- completeMissing
    }
    
    if (completeMissing_test){
      .fill_list <- list(0)
      names(.fill_list) <- value
      
      data_ <- data_ %>%
        droplevels() %>% # drop unused levels from factors in the data frame, if necessary
        complete(!!!syms(.expand_cols), fill = .fill_list) # add missing dimensions and set value to 0
    }
    
    # ---- calculation ----
    data_ <- data_ %>%
      pivot_wider(names_from = variable, values_from = value) #, values_fill = NA, id_cols = c("model", "scenario", "region", "period")
    
    # ---- add assumptions ----
    assumptions.names <- c()
    for (ka in names(assumptions)) { # Loop over assumption categories
      if (!any(names(assumptions[[ka]]) %in% names(data_))) { # If no assumptions are in the data
        data_ <- cbind(data_, assumptions[[ka]])
        assumptions.names <- c(assumptions.names, names(assumptions[[ka]]))
      } else {
        common_columns <- names(assumptions[[ka]])[which(names(assumptions[[ka]]) %in% names(data_))]
        data_ <- left_join(data_,
                           assumptions[[ka]],
                           by=common_columns)
        assumptions.names <- c(assumptions.names, names(assumptions[[ka]][which(!names(assumptions[[ka]]) %in% common_columns)]))
      }
    }
    
    data_ <- data_[,which(names(data_) %in% (.colnames %>% setdiff(value) %>% union(equations.rhs.varnames)))]
    
    # remove assumptions from equations.rhs.varnames
    if (!is.null(assumptions)) {
      equations.rhs.varnames.noassumption <- equations.rhs.varnames[which(!equations.rhs.varnames %in% assumptions.names)]
    }
    
    
    for (i in 1:length(equations)) {
      if (VERBOSE) {
        cat(paste0("  > ", names(equations[i]), "\n"))
        cat(paste0("    ", equations[[i]], "\n"))
        #tmp_verbose <- data_ %>% 
        #  select(c("model", "scenario", "region", "period", all.vars(equations[[i]])))
        #print(tmp_verbose %>% head())
        #print(str(tmp_verbose))
      }
      
      data_ <- data_ %>%
        mutate(!!sym(names(equations[i])) := f_eval(f = equations[[i]], data = .))
    }
    
    # remove assumptions (columns) from data
    if (!is.null(assumptions)) {
      # data_ <- data_ %>%
      #   select(-assumptions.names)
      data_ <- data_ %>%
        select(-setdiff(equations.rhs.varnames, equations.rhs.varnames.noassumption))
    }
    
    data_ <- data_ %>%
      pivot_longer(unique(c(equations.rhs.varnames.noassumption, names(equations))),
                   names_to = variable, values_to = value)
    
    # ---- filter new variables ----
    if (only.new) {
      data_ <- data_ %>%
        filter(!!sym(variable) %in% names(equations))
    }
    
    # ---- filter NAs ----
    if (na.rm) {
      data_ <- data_ %>%
        filter(!is.na(!!sym(value)))
    }
    
    # ---- restore unit column, if necessary ----
    if (!is.na(unit)) {
      .units <- data.frame(variable = gsub("`", "", names(.units)),
                           unit = as.character(.units))
      colnames(.units) <- c(variable, unit)
      
      data_ <- inner_join(
        data_,
        rbind(variables.units, .units),
        by = variable
      )
    }
    
    # ---- add unaffected variables ----
    if (!only.new) {
      data_ <- rbind(
        data %>%
          filter(!(!!sym(variable) %in% equations.rhs.varnames.noassumption)),
        data_
      )
    }
  }
  
  out <- data_ %>% select(!!!syms(.colnames))
  
  return(out)
}

compute_rfp <- function(data, equations, assumptions, mapping, VERBOSE=FALSE) {
  
  list_rfp <- list()
  
  for (krfp in c("Direct emissions cost", "Indirect cost", "Low-carbon capital expenditure", "Revenue")) {
    if (VERBOSE) cat(paste0("RFP category: ", krfp, "\n"))
    if (length(grep(krfp, equations$lhs)) != 0) { # if there are questions for the current RFP category
      list_equations <- lapply(equations$rhs[grep(krfp, equations$lhs)], function(x) x)
      names(list_equations) <- gsub("`", "", equations$lhs[grep(krfp, equations$lhs)])
      
      #if (VERBOSE) cat(paste0(paste0("  > ", names(list_equations), collapse="\n"), "\n"))
      
      # Updating regional information of assumptions using regional mapping
      for (ka in names(assumptions[[krfp]])) {
        if ("region" %in% names(assumptions[[krfp]][[ka]])) {
          assumptions[[krfp]][[ka]] <- assumptions[[krfp]][[ka]] %>% 
            left_join(mapping_region_ngfs %>% 
                        rename(region=region_rfp),
                      by=c("region")) %>% 
            select(-region) %>% 
            rename(region=region_new)
        }
      }
      
      list_rfp[[krfp]] <- compute_variables(data, list_equations, assumptions=assumptions[[krfp]], units = "Billion $US", VERBOSE=VERBOSE)
    }
    
  }
  
  out <- do.call("rbind", list_rfp) %>% 
    mutate(unit = "Billion $US") %>% 
    select(model, scenario, region, variable, unit, period, value)
  
  return(out)
  
}

rfp_separate <- function(i_data, REMOVE_PREFIX=FALSE) {
  out <- i_data %>% 
    mutate(rfp = "") %>% 
    mutate(rfp = ifelse(grepl("Direct emissions cost", variable),          "Diagnostics|RFP|Direct emissions cost", rfp)) %>% 
    mutate(rfp = ifelse(grepl("Indirect cost", variable),                  "Diagnostics|RFP|Indirect cost", rfp)) %>% 
    mutate(rfp = ifelse(grepl("Low-carbon capital expenditure", variable), "Diagnostics|RFP|Low-carbon capital expenditure", rfp)) %>% 
    mutate(rfp = ifelse(grepl("Revenue", variable),                        "Diagnostics|RFP|Revenue", rfp)) %>% 
    mutate(rfp = ifelse(grepl("Total Costs", variable),                    "Diagnostics|RFP|Total Costs", rfp)) %>%
    mutate(rfp = ifelse(grepl("Total", variable),                          "Diagnostics|RFP|Total", rfp)) %>%
    mutate(rfp = ifelse(grepl("Overall", variable),                        "Diagnostics|RFP|Overall", rfp)) %>%
    mutate(sector = substr(variable, nchar(rfp)+2, nchar(variable))) %>% 
    select(-variable)
  
  if (REMOVE_PREFIX) out <- out %>% mutate(rfp = gsub("Diagnostics\\|RFP\\|", "", rfp))
  
  return(out)
}

rfp_union <- function(i_data) {
  out <- i_data %>% 
    mutate(variable = paste0(rfp, "|", sector)) %>% 
    select(-rfp, -sector)
  
  return(out)
}

compute_npv <- function(i_data, R=0.05, TIMEHORIZON=2050) {
  out <- i_data %>%
    filter(period >= 2020, period <= TIMEHORIZON) %>% 
    select(model, scenario, region, rfp, sector, period, value, baseline) %>%
    group_by(model, scenario, region, rfp, sector, baseline) %>% 
    arrange(period) %>% 
    mutate(dt = lead(period, default = TIMEHORIZON) - period) %>%
    mutate(disc = 1/(1+R)^(period-2020)) %>% 
    mutate(value = value*disc) %>% 
    summarise(value = sum((lead(value, default = 0) + value)/2*dt)) %>% 
    ungroup()
  
  return(out)
}