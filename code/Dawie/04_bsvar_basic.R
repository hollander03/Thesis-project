
# Libraries ---------------------------------------------------------------

if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  readxl, bsvars, bsvarSIGNs, tseries, readr, dplyr, tidyr,
  ggplot2, lubridate, zoo, extrafont, janitor, stringr, paletteer, jsonlite
)

# Load fonts and source functions
loadfonts()
source_files <- list.files("code/R/functions", full.names = TRUE)
invisible(sapply(source_files, source))

# Load and prepare data ---------------------------------------------------

# # Set the flag to indicate whether to force update the data
# force_update <- FALSE  # Set to TRUE if you have new data and need to update y_df
#
# # Check if y_df.rda exists, if so, load it unless force_update is TRUE
# if (file.exists("code/R/models/y_df.rda") && !force_update) {
#   load("code/R/models/y_df.rda")
# } else {
#   y_df <- load_and_prepare_data() # Load and get data in the right format
#   save(y_df, file = "code/R/models/y_df.rda")  # Save y_df for future use
# }

model_data <- load("~/Desktop/School/Masters/MThesis/Narrative sign restrictions/Master Thesis/Thesis project/code/R/models/model_data_1.rda")

# Model selection ---------------------------------------------------------

# Load Configuration
config <- load_config("code/R/config/config.json")
model_params <- config$model_params
plotting_params <- config$plotting

# Access model parameters
model_num <- 1 # Not in the config file
start_date <- model_params$start_date
end_date <- model_params$end_date
voi <- model_params$voi
label_file <- model_params$label_file
capital <- model_params$capital_variable
gov_spread <- model_params$gov_spread
repo <- model_params$repo_rate
car_var <- model_params$car_var

# Define shocks
shocks <- define_shocks(model_num, car_var, capital, repo)

# Define model combinations
model_list <- define_model_combinations(
  gov_spread = gov_spread,
  repo = repo,
  capital = capital,
  car_var = car_var
)

# Select model variables
selected_model <- select_model(model_list, model_num)

# Define the filename based on the model number
model_data_file <- paste0("code/R/models/model_data_", model_num, ".rda")

# Generate model data
model_data <- model_selection(
  data = model_data$data,
  model_num = model_num,
  start_date = start_date,
  end_date = end_date,
  voi = voi,
  shock = shocks,
  label_file = label_file,
  variables = selected_model
)

# Save model data
save(model_data, file = model_data_file)

# Extract time series and label information from model_data
data <- model_data$data

#I just want to wor

# Plot all series associated with the model
plot_model_series(data, custom_labels, plotting_params)


# Model setup -------------------------------------------------------------
set.seed(123)

# Restrictions on 'capital_index' or fallback to 'car_tot'
restrictions <- generate_restrictions(colnames(data))

# Build the B matrix with restrictions
B <- build_B_matrix(colnames(data), restrictions)

model_params <- list(S_burn = 1e2, S = 1e3, p = 4)

# Estimate the SVAR model
post_custom <- specify_and_estimate(
  data = data,
  p = model_params$p,
  model_type = "basic",
  B = B,
  S_burn = model_params$S_burn,
  S = model_params$S
)



# Process models for IRFs -------------------------------------------------

process_irfs(
  model = post_custom,
  data = data,
  custom_labels = custom_labels,
  shocks = shocks,
  save_results = FALSE
)

# Historical Decomposition ------------------------------------------------

date_vector <- seq.Date(from = as.Date(start_date), by = "month", length.out = nrow(data))

process_hds(
  model = post_custom,
  data = data,
  custom_labels = custom_labels,
  decomp_vars = voi,  # List of variables
  date_vector = date_vector,
  save_results = FALSE  # Optionally save the results
)

# FEVDs -------------------------------------------------------------------

process_fevds(
  model = post_custom,
  custom_labels = custom_labels,
  selected_variable = voi,
  save_results = FALSE
)

# Forecasting -------------------------------------------------------------

# Define the conditional change
conditional_change <- list(
  list(variable = "capital_index", change = 1)  # Hypothetical 1% increase in Capital Index
)

# Run the forecast processing
process_forecasts(
  model = post_custom,
  data = data,
  custom_labels = custom_labels,
  forecast_vars = voi,  # Forecasting multiple variables
  horizon = 12,
  conditional_change = conditional_change,
  start_date = "2010-01-01",
  history_length = 2,
  save_results = FALSE
)


# Model with structural and sign restrictions (bsvarSIGNs) -------------------------------

# Define the sign_structural matrix with 1 (unrestricted) and 0 (zero restrictions)
num_vars <- ncol(data)
sign_structural <- matrix(NA, nrow = ncol(data), ncol = ncol(data))  # Initialize with zeros (restrictions)

# Apply the unrestricted elements (set to 1) based on your matrix structure
sign_structural[1, 1] <- 1  # "CAR Total" only affects itself
sign_structural[2, 2] <- 1  # "GDP Growth" only affects itself
sign_structural[3, 2:3] <- 1  # "Repo Rate" affects GDP Growth and itself
# sign_structural[4, 2:4] <- 1  # "Inflation" affects Repo Rate, GDP Growth, and itself
# sign_structural[5, 2:5] <- 1  # "Corporate Credit Growth" affects Inflation, Repo, GDP, and itself
# sign_structural[6, 2:6] <- 1  # "Household Credit Growth" affects all variables

# Sign restrictions on impulse responses (sign_irf)
# Important note, the location of the row does not matter. We are imposing a structure to the relationship between variables in the IRFs themselves.
sign_irf <- matrix(NA, nrow = num_vars, ncol = num_vars)
sign_irf[, 1] <- c(1, NA, NA, NA, -1, -1)  # CAR Total (shock 1)
sign_irf[, 4] <- c(NA, NA, -1, 1, NA, NA)  # Monetary policy shock (shock 2, but in 4th row)
sign_irf <- array(sign_irf, dim = c(6, 6, 6)) # Restriction on impulse for first 5 time periods

# Specify and estimate the model
sign_spec <- specify_bsvarSIGN$new(
  data = data,
  p = 3,
  sign_irf = sign_irf,
  sign_structural = sign_structural
)

model_sign_restricted <- estimate(sign_spec, S = 10000)

model <- model_sign_restricted

shocks <- c("capital_index", "repo")

# Process IRFs
process_irfs(
  model = model,
  model_name = model_name,
  data = data,
  custom_labels = custom_labels,
  shocks = shocks,
  save_results = FALSE
)

# Model with narrative sign restrictions (bsvarSIGNs) ---------------------------------------

# Define the sequence of monthly dates starting from the start date
date_check <- seq.Date(from = as.Date("2009-01-01"), by = "month", length.out = nrow(data))

# Define the target date
target_date <- as.Date("2020-02-01")  # February 2020

# Find the index of the target date in the sequence
index <- which(date_check == target_date)

# Print the result
print(index)  # Should print 134

# Narrative sign restriction: the shock is positive in Feb 2020
sign_narrative <- list(
  specify_narrative(start = index, periods = 1, type = "S", sign = -1, shock = 1),
  specify_narrative(start = index, periods = 1, type = "B", sign = -1, shock = 1, var = 6),
  specify_narrative(start = index, periods = 1, type = "B", sign = -1, shock = 1, var = 5)
)

# Specify and estimate the model
narrative_spec <- specify_bsvarSIGN$new(
  data = data,
  p = 3,
  sign_narrative = sign_narrative,
  sign_irf = sign_irf,
  sign_structural = sign_structural
)

model_narrative <- estimate(narrative_spec, S = 10000)

model <- model_narrative

# Define shock indices (can be indices or variable names)
shocks <- c("capital_index")

# Process IRFs
process_irfs(
  model = model,
  model_name = model_name,
  data = data,
  custom_labels = custom_labels,
  shocks = shocks,
  save_results = FALSE
)


# END ---------------------------------------------------------------------