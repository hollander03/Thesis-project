# data_model_setup.R

# -------------------------------------------------------------------------
# Functions for Model Specification and Estimation
# -------------------------------------------------------------------------

# Create a Time Series Dummy Variable
create_dummy_ts <- function(data, start_date, end_date) {
  # Extract the dates from data
  dates <- as.Date(as.yearmon(time(data)))

  # Create the dummy variable
  dummy_vector <- as.numeric(dates >= as.Date(start_date) & dates <= as.Date(end_date))

  # Ensure the dummy vector aligns with data
  dummy_ts <- ts(
    data = dummy_vector,
    start = start(data),
    frequency = frequency(data)
  )

  return(dummy_ts)
}

# Prepare Data for SVAR Model Estimation
prepare_model_data <- function(data, lag_exogenous = NULL, exogenous_var = NULL, endogenous_vars) {

  # Prepare endogenous variables (select specified columns)
  data_endog <- data[, endogenous_vars]

  # If no lag_exogenous is provided, return just the endogenous data
  if (is.null(lag_exogenous) || is.null(exogenous_var)) {
    return(list(endog = as.matrix(data_endog), exog = NULL))
  }

  # Check if exogenous_var is provided as a matrix
  if (is.matrix(exogenous_var)) {
    # Number of observations
    TT <- nrow(data)

    # Initialize matrix for lagged exogenous variables
    lagged_exog <- matrix(NA, nrow = TT - lag_exogenous, ncol = 0)

    # Loop through each lag and append the lagged exogenous variables
    for (i in 0:lag_exogenous) {
      lagged_exog <- cbind(lagged_exog, exogenous_var[(lag_exogenous - i + 1):(TT - i), ])
    }
  } else {
    stop("Exogenous variable must be provided as a matrix.")
  }

  # Adjust the endogenous variables to match the exogenous variables after lagging
  adjusted_endog <- tail(as.matrix(data_endog), nrow(lagged_exog))

  # Return both endogenous and exogenous data
  return(list(
    endog = adjusted_endog,
    exog = lagged_exog
  ))
}

# Function to build the B matrix
build_B_matrix <- function(variables, restrictions = NULL) {
  N <- length(variables)  # Number of variables
  B <- matrix(TRUE, N, N)  # Start with all TRUE values (no restrictions)

  # Apply default lower triangular structure: Each variable affects itself and those below it
  B[upper.tri(B)] <- FALSE  # Upper triangle set to FALSE by default

  # Apply custom restrictions if provided
  if (!is.null(restrictions)) {
    for (restriction in restrictions) {
      var1 <- restriction$var1
      var2 <- restriction$var2
      affects <- restriction$affects

      # Get the positions of the variables in the B matrix
      i <- which(variables == var1)
      j <- which(variables == var2)

      if (length(i) == 1 && length(j) == 1) {  # Ensure valid indices
        B[i, j] <- affects  # Apply the restriction
      } else {
        warning(paste("Variable(s) not found in model_select:", var1, "or", var2))
      }
    }
  }

  return(B)
}

generate_restrictions <- function(variables, capital = "capital_index") {
  # Remove 'car_tot' from the list of variables
  other_variables <- setdiff(variables, capital)

  # Initialize an empty list for restrictions
  restrictions <- list()

  # 1. CAR_total does not affect any other variable
  for (var in other_variables) {
    restrictions <- append(restrictions, list(list(var1 = capital, var2 = var, affects = FALSE)))
  }

  # 2. No other variables affect CAR_total
  for (var in other_variables) {
    restrictions <- append(restrictions, list(list(var1 = var, var2 = capital, affects = FALSE)))
  }

  return(restrictions)
}

# Create a function to apply a constant value over a date range to a specific column
apply_constant_in_date_range <- function(data, column, start_date, end_date, constant_value) {
  # Ensure the date column is in Date format
  data$Date <- as.Date(data$Date)

  # Apply the constant value for the specified date range
  data[[column]][data$Date >= as.Date(start_date) & data$Date <= as.Date(end_date)] <- constant_value

  return(data)
}


process_series <- function(data, date_column, series_to_select, series_to_percentage = NULL,
                           start_date = NULL, end_date = NULL, frequency = 12,
                           outlier_method = NULL, outlier_vars = NULL, winsor_limits = c(0.05, 0.95)) {

  # Ensure the date column is in Date format
  data[[date_column]] <- as.Date(data[[date_column]])

  # Filter the data based on the specified start and end dates
  if (!is.null(start_date) && !is.null(end_date)) {
    data <- data %>%
      filter(!!sym(date_column) >= start_date & !!sym(date_column) <= end_date)
  } else if (!is.null(start_date)) {
    data <- data %>%
      filter(!!sym(date_column) >= start_date)
  } else if (!is.null(end_date)) {
    data <- data %>%
      filter(!!sym(date_column) <= end_date)
  }

  # Select the specified series, including the date column
  selected_df <- data %>%
    select(all_of(c(date_column, series_to_select)))

  # Convert specific series to percentage terms by dividing by 100, if specified
  if (!is.null(series_to_percentage)) {
    selected_df <- selected_df %>%
      mutate(across(all_of(series_to_percentage), ~ . / 100))
  }

  # Handle outliers based on specified method (optional)
  if (!is.null(outlier_method) && !is.null(outlier_vars)) {
    # Helper function for Winsorization
    winsorize <- function(x, lower_limit, upper_limit) {
      pmax(pmin(x, upper_limit), lower_limit)
    }

    # Apply Winsorization or Imputation
    selected_df <- selected_df %>%
      mutate(across(
        all_of(outlier_vars),
        ~ {
          if (outlier_method == "winsorize") {
            lower_limit <- quantile(., winsor_limits[1], na.rm = TRUE)
            upper_limit <- quantile(., winsor_limits[2], na.rm = TRUE)
            winsorize(., lower_limit, upper_limit)
          } else if (outlier_method == "impute") {
            median_value <- median(., na.rm = TRUE)
            outlier_limit_low <- quantile(., 0.25, na.rm = TRUE) - 1.5 * IQR(., na.rm = TRUE)
            outlier_limit_high <- quantile(., 0.75, na.rm = TRUE) + 1.5 * IQR(., na.rm = TRUE)
            ifelse(. < outlier_limit_low | . > outlier_limit_high, median_value, .)
          } else {
            .
          }
        }
      ))
  }

  # Drop rows with any missing values
  cleaned_df <- selected_df %>%
    drop_na()

  # Extract the start date for the time series
  ts_start_date <- min(cleaned_df[[date_column]], na.rm = TRUE)

  # Convert the data to a time series object
  data <- ts(as.matrix(cleaned_df %>% select(-all_of(date_column))),
             start = c(as.numeric(format(ts_start_date, "%Y")), as.numeric(format(ts_start_date, "%m"))),
             frequency = frequency)

  return(data)
}


# Function to get labels from a JSON file
get_labels_from_data <- function(file_path) {
  # Check if the file exists
  if (!file.exists(file_path)) {
    stop("Label file not found!")
  }

  # Load labels from the JSON file
  variable_labels <- fromJSON(file_path)

  # Convert list to a named character vector
  labels_vector <- sapply(variable_labels, function(x) x[1])

  return(labels_vector)
}

# Define model combinations (list of variables for each model)
define_model_combinations <- function(gov_spread = "sa_us_10yr", repo = "bank_rate", capital = "exc_cap_2_yoy", car_var = "capital_index") {
  model_list <- list(
    model_1 = c(car_var, "gdp_yoy", "inflation", repo, "cred_cs_yoy", "cred_hs_yoy"),
    model_2 = c(car_var, "gdp_yoy", "inflation", repo, "cred_cs_yoy", "cred_hs_yoy", gov_spread),
    model_3 = c(car_var, "gdp_yoy", "inflation", repo, "cred_cs_yoy", "corp_spread", gov_spread),
    model_4 = c(car_var, "gdp_yoy", "inflation", repo, "cred_hs_yoy", "hh_spread", gov_spread),
    model_5 = c(car_var, "gdp_yoy", "inflation", repo, "cred_cs_yoy", "cred_hs_yoy", "corp_spread", "hh_spread", gov_spread),
    model_6 = c(car_var, "gdp_yoy", "inflation", repo, capital, "rwa_yoy", "cred_cs_yoy", "corp_spread", gov_spread),
    model_7 = c(car_var, "gdp_yoy", "inflation", repo, capital, "rwa_yoy", "cred_hs_yoy", "hh_spread", gov_spread),
    model_8 = c(car_var, "gdp_yoy", "inflation", repo, capital, "rwa_yoy", "cred_cs_yoy", "corp_spread", "cred_hs_yoy", "hh_spread", gov_spread),
    model_9 = c(car_var, "gdp_yoy", "inflation", repo, capital, "rwa_yoy", "cred_cs_yoy", "corp_spread", "bank_stock_index", gov_spread),
    model_10 = c(car_var, "gdp_yoy", "inflation", repo, capital, "rwa_yoy", "cred_hs_yoy", "hh_spread", "bank_stock_index", gov_spread),
    model_11 = c(car_var, "cred_hs_yoy", "hh_spread", "exc_cap_2_yoy", "tot_capital_yoy", "rwa_yoy", gov_spread),
    model_12 = c(car_var, "gdp_yoy", "inflation", repo, "cred_cs_yoy", "corp_spread"),
    model_13 = c(car_var, "car_tot", "gdp_yoy", "inflation", repo, capital, "rwa_yoy", "cred_cs_yoy", "cred_hs_yoy", "corp_spread", "hh_spread", gov_spread),
    model_14 = c(car_var, "gdp_yoy", "inflation", repo, capital, "rwa_yoy", "cred_cs_yoy", "cred_hs_yoy", "corp_spread", "hh_spread", gov_spread)
  )

  return(model_list)
}


# Modified model_selection function
model_selection <- function(data = y_df, model_num = NULL, variables = NULL, start_date, end_date,
                            voi = NULL, shock = NULL, default_voi = NULL, default_shock = NULL,
                            label_file = "variable_labels.json") {

  # Initialize model_list
  model_list <- define_model_combinations()

  # Ensure either model_num or variables is provided
  if (is.null(model_num) & is.null(variables)) {
    stop("Either 'model_num' or 'variables' must be specified.")
  }

  # If model_num is provided, select the model based on model_num
  if (!is.null(model_num)) {
    selected_model <- model_list[[paste0("model_", model_num)]]
  } else {
    # If variables are provided, use the list of variables directly
    selected_model <- variables
  }

  # Process the time series data
  data <- process_series(
    data = data,
    date_column = "Date",
    series_to_select = selected_model,
    start_date = as.Date(start_date),
    end_date = as.Date(end_date)
  )

  # Determine variables of interest (voi) - use specified voi or defaults provided by the user
  if (is.null(voi)) {
    if (is.null(default_voi)) {
      stop("You must either specify 'voi' or provide a 'default_voi'.")
    }
    voi <- default_voi[default_voi %in% colnames(data)]
    if (length(voi) == 0) {
      stop("None of the default 'voi' variables are available in the selected model.")
    }
  } else {
    # Check if all specified voi exist in data
    missing_voi <- voi[!(voi %in% colnames(data))]
    if (length(missing_voi) > 0) {
      stop(paste("The following voi are not available in the selected model:", paste(missing_voi, collapse = ", ")))
    }
  }

  # Determine shock variables - use specified shock or defaults provided by the user
  if (is.null(shock)) {
    if (is.null(default_shock)) {
      stop("You must either specify 'shock' or provide a 'default_shock'.")
    }
    shock <- default_shock[default_shock %in% colnames(data)]
    if (length(shock) == 0) {
      stop("None of the default 'shock' variables are available in the selected model.")
    }
  } else {
    # Check if all specified shocks exist in data
    missing_shock <- shock[!(shock %in% colnames(data))]
    if (length(missing_shock) > 0) {
      stop(paste("The following shocks are not available in the selected model:", paste(missing_shock, collapse = ", ")))
    }
  }

  # Use the new get_labels_from_data function to read labels from the JSON file
  custom_labels <- get_labels_from_data(label_file)

  # Ensure the labels are in the same order as the columns in data
  ordered_labels <- custom_labels[match(colnames(data), names(custom_labels))]

  # Return the time series object, selected variables, shocks, and ordered labels
  return(list(data = data, voi = voi, shock = shock, custom_labels = ordered_labels))
}


# Specify and Estimate an SVAR Model with Custom Restrictions
specify_and_estimate <- function(data, p, exogenous = NULL, num_regimes = NULL, A_prior = NULL,
                                 S_burn = 1e4, S = 1e5, model_type = "basic", B = NULL) {

  # Model specification based on type
  if (model_type == "basic") {
    spec <- specify_bsvar$new(data = data, p = p, exogenous = exogenous, B = B)

  } else if (model_type == "svr_sv") {
    spec <- specify_bsvar_sv$new(data = data, p = p, B = B, exogenous = exogenous)

  } else if (model_type == "svr_t") {
    spec <- specify_bsvar_t$new(data = data, p = p, B = B, exogenous = exogenous)

  } else if (model_type == "svr_msh") {
    M <- ifelse(is.null(num_regimes), 2, num_regimes)  # Default to 2 regimes if not specified
    spec <- specify_bsvar_msh$new(data = data, p = p, B = B, M = M, exogenous = exogenous)

  } else if (model_type == "svr_mix") {
    M <- ifelse(is.null(num_regimes), 2, num_regimes)  # Default to 2 mixture components if not specified
    spec <- specify_bsvar_mix$new(data = data, p = p, B = B, M = M, exogenous = exogenous)

  } else {
    stop("Invalid model type specified.")
  }

  # Add prior for A if provided
  if (!is.null(A_prior)) {
    spec$prior$A <- A_prior
  }

  # Run estimation: Burn-in and posterior draws
  burn <- estimate(spec, S_burn, show_progress = TRUE)
  post <- estimate(burn, S)

  # Return the posterior draws
  return(post)
}

# -------------------------------------------------------------------------
# Functions for Model Analysis and Plotting
# -------------------------------------------------------------------------

# Function to process models for IRFs
process_irfs <- function(model, model_name, data, custom_labels, shocks = c(1, 4, 5), save_results = FALSE) {

  # Compute Impulse Responses
  irf_results <- compute_impulse_responses(model, horizon = 60)

  # Save results if requested
  if (save_results) {
    save(irf_results, file = paste0("code/R/models/irf_results_", model_name, ".rda"))
  }

  # Allow shock_indices to be specified by name or index
  if (is.character(shocks)) {
    shock_indices_num <- match(shocks, names(custom_labels))
    if (any(is.na(shock_indices_num))) {
      stop("One or more shock names not found in custom_labels.")
    }
  } else {
    shock_indices_num <- shocks
  }

  # Plot IRFs
  plot_responses_to_shock(
    x = irf_results,
    y = data,
    variable_names = custom_labels,
    shock_names = custom_labels,
    mar.multi = c(2.5, 1.5, 2.5, 1.5),
    oma.multi = c(2, 1, 4, 1),
    shocks = shock_indices_num,
    line_style = 1,
    line_width = 3,
    ribbon_alpha = 0.2,
    line_alpha = 0.9,
    probability = 0.6
  )
}

# Function to process models for Historical Decompositions
process_hds <- function(model, model_name, data, custom_labels, decomp_vars, date_vector, save_results = FALSE) {

  # Compute Historical Decompositions
  hds_results <- compute_historical_decompositions(model)

  # Save results if requested
  if (save_results) {
    save(hds_results, file = paste0("code/R/models/hds_", model_name, ".rda"))
  }

  # Allow decomp_var to be a list of variables (names or indices)
  for (decomp_var in decomp_vars) {

    # Determine the index of the decomposition variable
    if (is.character(decomp_var)) {
      decomp_var_index <- which(names(custom_labels) == decomp_var)
      if (length(decomp_var_index) == 0) {
        stop(paste("Variable", decomp_var, "not found in custom_labels."))
      }
    } else {
      decomp_var_index <- decomp_var
    }

    # Plot Historical Decomposition for the selected variable
    plot_historical_decomposition_bar(
      x = hds_results,
      selected_decompositions = decomp_var_index,
      date_vector = date_vector,
      main = paste("Historical Decomposition of", custom_labels[decomp_var_index]),
      xlab = "",
      ylab = "",
      legend_labels = custom_labels,
      line_width = 2.5,
      grid_opacity = 0.5,
      font_family = "Aptos",
      axis_font_size = 8,
      title_font_size = 8,
      tick_freq = 8,
      bar_width = 0.1
    )
  }
}


process_fevds <- function(model, model_name, custom_labels, selected_variables, save_results = FALSE) {

  # Compute FEVDs
  fevd_results <- compute_variance_decompositions(model, horizon = 60)

  # Save results if requested
  if (save_results) {
    save(fevd_results, file = paste0("code/R/models/fevd_", model_name, ".rda"))
  }

  # Allow selected_variables to be specified by names or indices
  if (is.character(selected_variables)) {
    selected_variable_indices <- match(selected_variables, names(custom_labels))
    if (any(is.na(selected_variable_indices))) {
      stop("One or more variable names not found in custom_labels.")
    }
  } else {
    selected_variable_indices <- selected_variables
  }

  # Determine the number of shocks
  num_shocks <- dim(fevd_results)[3]

  # Generate colors (limit to maximum palette size)
  max_colors <- min(num_shocks, 8)  # RColorBrewer Set3 palette has up to 8 colors
  cols <- RColorBrewer::brewer.pal(max_colors, "Set3")

  # Plot FEVD for each selected variable
  for (selected_variable_index in selected_variable_indices) {
    plot_fevd(
      fevd_results,
      selected_variable = selected_variable_index,
      cols = cols,
      main = paste("FEVD for", custom_labels[selected_variable_index]),
      xlab = "Horizon",
      ylab = "Contribution (%)",
      axis_font_size = 9,
      title_font_size = 11,
      legend_labels = custom_labels
    )
  }
}


process_forecasts <- function(model, model_name = NULL, data, custom_labels, forecast_vars,
                              horizon = 12, conditional_change = NULL, start_date = NULL,
                              history_length = 2, frequency = 12, save_results = FALSE) {

  # Unconditional Forecast
  predictive_uncond <- forecast(model, horizon = horizon)

  # Save unconditional forecast if requested
  if (save_results) {
    save(predictive_uncond, file = paste0("code/R/models/predictive_uncond_", model_name, ".rda"))
  }

  # Conditional Forecast (if conditional_change is provided)
  predictive_cond <- NULL
  if (!is.null(conditional_change)) {
    # Number of variables in the model
    num_variables <- ncol(data)

    # Initialize the conditional forecast matrix
    cf <- matrix(NA, horizon, num_variables)

    # Loop through the conditional_change list to set values for each specified variable
    for (cond_change in conditional_change) {
      # Allow conditional variable to be specified by name or index
      if (is.character(cond_change$variable)) {
        cond_var_index <- which(names(custom_labels) == cond_change$variable)
        if (length(cond_var_index) == 0) {
          stop(paste("Conditional variable name", cond_change$variable, "not found in custom_labels."))
        }
      } else {
        cond_var_index <- cond_change$variable
      }

      # Set the conditional path for the specified variable
      last_value <- tail(data[, cond_var_index], 1)
      cf[, cond_var_index] <- last_value + cond_change$change
    }

    # Run the conditional forecast
    predictive_cond <- forecast(
      posterior = model,
      horizon = horizon,
      conditional_forecast = cf
    )

    # Save conditional forecast if requested
    if (save_results) {
      save(predictive_cond, file = paste0("code/R/models/predictive_cond_", model_name, ".rda"))
    }
  }

  # Allow forecast_vars to be specified by name or index
  if (is.character(forecast_vars)) {
    forecast_var_indices <- match(forecast_vars, names(custom_labels))
    if (any(is.na(forecast_var_indices))) {
      stop("One or more forecast variable names not found in custom_labels.")
    }
  } else {
    forecast_var_indices <- forecast_vars
  }

  # Set default start_date if not provided
  if (is.null(start_date)) {
    start_date <- as.character(index(data)[1])
  }

  # Plot the forecasts for each forecast variable
  for (forecast_var_index in forecast_var_indices) {
    plot_forecast(
      x = predictive_uncond,
      x_overlay = predictive_cond,
      variables = forecast_var_index,
      variable_names = custom_labels,
      start_date = start_date,
      frequency = frequency,
      history_length = history_length, # in years
      col = "red",
      col_overlay = "#1f77b4",  # Color for conditional forecast
      axis_font_size = 11,
      title_font_size = 11,
      font_family = "Aptos",
      line_width = 3,
      line_type = 1,
      ribbon_alpha = 0.2,
      grid_opacity = 0.5
    )

    # Get the plotting area limits
    x_range <- par("usr")[1:2] # Get the x-axis range (min and max)
    y_range <- par("usr")[3:4] # Get the y-axis range (min and max)

    # Set coordinates for the legend manually within the visible range
    legend_x <- x_range[1] + 0.01 * (x_range[2] - x_range[1])  # 1% from the left
    legend_y <- y_range[1] + 0.40 * (y_range[2] - y_range[1])  # 40% from the bottom, more up

    # Add legend manually after the plot is created
    legend(x = legend_x, y = legend_y,
           legend = c("Unconditional Forecast", "Conditional Forecast"),
           col = c("red", "#1f77b4"),
           lty = 1, # Line type for both unconditional and conditional forecast
           lwd = 3, # Line width for both lines
           bty = "n", # Box type ('n' means no box around legend)
           cex = 0.9 # Font size for the legend
    )
  }
}

# Some experimental stuff to improve the workflow

load_config <- function(file_path) {
  config <- fromJSON(file_path)
  list(
    model_params = config$model_parameters,
    plotting_params = config$plotting
  )
}


define_shocks <- function(model_num, car_var, capital, repo) {
  if (model_num %in% c(1, 2, 3, 4, 5)) {
    return(c(car_var, repo))
  } else if (model_num %in% c(6, 7, 8, 9, 10, 13, 14)) {
    return(c(capital, car_var, repo))
  } else {
    return(NULL)
  }
}


select_model <- function(model_list, model_num) {
  model_list[[paste0("model_", model_num)]]
}

plot_model_series <- function(data, custom_labels, plotting_params) {
  custom_colors <- paletteer::paletteer_d(plotting_params$custom_colors)
  plot_all_series(
    data = data,
    custom_labels = custom_labels,
    nrows = plotting_params$nrows,
    line_width = plotting_params$line_width,
    line_type = plotting_params$line_type,
    font_family = plotting_params$font_family,
    font_size = plotting_params$font_size,
    title_size = plotting_params$title_size,
    opacity = plotting_params$opacity,
    grid_opacity = plotting_params$grid_opacity,
    colors = custom_colors
  )
}


