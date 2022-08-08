# This file contains a set of tool functions that are used in the project.

# inline.data.frame allows to define a dta.frame inline 
inline.data.frame <- function(..., sep = ";", quote = "") {
  .dots <- list(...)
  
  # .dots is one long character vector?
  if (1 == length(.dots) & "character" == typeof(.dots[[1]])) {
    table <- .dots[[1]] %>%
      paste(sep = "\n")
    
    # .dots is a list of character vectors or NULL?
  } else if (all(unlist(lapply(.dots, typeof)) %in% c("character", "NULL"))) {
    table <- .dots %>%
      unlist() %>%
      paste0(collapse = "\n")
    
    # .dots is something else ...
  } else {
    stop("Can't handle parameter. Need string or list of strings.")
  }
  
  table %>%
    textConnection() %>%
    read.table(header = TRUE, sep = sep, quote = quote, comment.char = "",
               strip.white = TRUE, stringsAsFactors = FALSE,
               check.names = FALSE) %>%
    as_tibble()
}

# removeColNa deletes columns that contains NAs
removeColNa <- function(df){
  
  df <- df[colSums(!is.na(df)) > 0]
  
  return(df)
}  

# NAto0 replaces NA values by 0
NAto0 <- function(i_value) {
  out <- ifelse(is.na(i_value), 0, i_value)
  return(out)
}


# Calculate new variable
calc_addVariable_old <- function(data, ..., units = NA, na.rm = TRUE,
                             completeMissing = FALSE, only.new = FALSE,
                             variable = variable, unit = NA, value = value) {
  
  .dots    <- list(...)
  
  if (!all(is.na(units))) {
    if (length(units) == length(.dots)) {
      for (i in 1:length(.dots))
        .dots[i][[1]] <- c(.dots[i][[1]], units[i])
    } else if (1 == length(units)) {
      for (i in 1:length(.dots))
        .dots[i][[1]] <- c(.dots[i][[1]], units)
    } else
      stop("units must be of the same length as ... or of length one.")
  }
  
  variable <- deparse(substitute(variable))
  unit     <- ifelse('NA' == deparse(substitute(unit)), NA,
                     deparse(substitute(unit)))
  value    <- deparse(substitute(value))
  
  calc_addVariable_old_(data, .dots, na.rm,completeMissing, only.new, variable,
                    unit, value)
}

calc_addVariable_old_ <- function(data, .dots, na.rm = TRUE,
                              completeMissing = FALSE, only.new = FALSE,
                              variable = "variable", unit = NA,
                              value = "value") {
  # ---- guardians ----
  if (!is.data.frame(data))
    stop("Only works with data frames")
  
  if (!is.list(.dots))
    stop("'.dots' must be a list of formula strings")
  
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
  
  # ---- parse .dots ----
  .units <- lapply(.dots, function(l) { l[2] }) %>%
    unlist()
  
  .dots <- lapply(.dots,
                  function(l) {
                    paste0("~", l[[1]]) %>%
                      gsub('\\n *', ' ', .) %>%
                      stats::formula() %>%
                      lazyeval::interp()
                  })
  names(.dots) <- gsub("`", "", names(.dots))
  
  .dots.names <- lapply(.dots, all.vars) %>%
    unlist() %>%
    unique() %>%
    setdiff(names(.dots))
  
  # --- filter for variables used on rhs ----
  data_ <- data %>%
    filter(!!sym(variable) %in% .dots.names)
  
  
  # ---- drop unit column, if necessary ----
  if (!is.na(unit)) {
    variables.units <- data_ %>%
      distinct(!!sym(variable), !!sym(unit)) %>%
      filter(!!sym(variable) %in% .dots.names)
    
    data_ <- data_ %>%
      select(-!!sym(unit))
  }
  
  # ---- fill missing data ----
  if (is.logical(completeMissing)) {
    if (completeMissing) {
      completeMissing_test <- TRUE
      .expand_cols <- setdiff(colnames(removeColNa(data_)), value)
    } else {
      completeMissing_test <- FALSE
    }
  } else {
    completeMissing_test <- TRUE
    .expand_cols <- completeMissing
  }
  
  if (completeMissing_test) {
    .fill_list <- list(0)
    names(.fill_list) <- value
    
    data_ <- data_ %>%
      droplevels() %>%
      complete(!!!syms(.expand_cols), fill = .fill_list)
  }
  
  # ---- check for duplicated rows ----
  duplicates <- data_ %>%
    group_by(!!!syms(setdiff(colnames(data_), value))) %>%
    filter(1 < n()) %>%
    ungroup()
  if (nrow(duplicates)) {
    stop(paste(c('Duplicate rows in data.frame', format(duplicates)),
               collapse = '\n'))
  }
  
  # ---- calculation ----
  data_ <- data_ %>%
    pivot_wider(names_from = sym(variable), values_from = sym(value))
  
  for (i in 1:length(.dots))
  {
    data_ <- data_ %>%
      mutate(!!sym(names(.dots[i])) := f_eval(f = .dots[[i]], data = .))
  }
  
  data_ <- data_ %>%
    pivot_longer(unique(c(.dots.names, names(.dots))),
                 names_to = variable, values_to = value)
  
  # ---- filter new variables ----
  if (only.new) {
    data_ <- data_ %>%
      filter(!!sym(variable) %in% names(.dots))
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
        filter(!(!!sym(variable) %in% .dots.names)),
      data_
    )
  }
  
  return(data_ %>% select(!!!syms(.colnames)))
}

calc_addVariable <- function(data, equations, assumptions = NULL, units = NA,
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
      # if (!any(names(assumptions[[ka]]) %in% names(data_))) {
      #   data_ <- cbind(data_, assumptions[[ka]])
      #   assumptions.names <- c(assumptions.names, names(assumptions[[ka]]))
      # } else {
      #   common_columns <- names(assumptions[[ka]])[which(names(assumptions[[ka]]) %in% names(data_))]
      #   data_ <- left_join(data_,
      #                      assumptions[[ka]],
      #                      by=common_columns)
      #   assumptions.names <- c(assumptions.names, names(assumptions[[ka]][which(!names(assumptions[[ka]]) %in% common_columns)]))
      # }
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

