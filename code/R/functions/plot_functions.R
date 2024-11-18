
# plot_functions.R

# Helper function for color adjustment
get_color_with_alpha <- function(col, alpha) {
  adjustcolor(col, alpha.f = alpha)
}


# Helper function for drawing axis, zero lines, and title
draw_plot_annotations <- function(
    n, axis_font_size, variable_names, add_title = TRUE
) {
  axis(1, labels = TRUE, cex.axis = axis_font_size / 10)
  axis(2, cex.axis = axis_font_size / 10)
  abline(h = 0)
  if (add_title) {
    title(variable_names[n], line = 0.5, cex.main = axis_font_size / 10, font.main = 1)
  }
}

# Helper function for setting up the layout
set_plot_layout <- function(nrow, ncol, mar.multi, oma.multi, font_family) {
  par(
    mfrow = c(nrow, ncol),
    mar = mar.multi,
    oma = oma.multi,
    family = font_family
  )
}

# Define the customized plot_ribbon function
custom_plot_ribbon <- function(
    draws, 
    probability = 0.9,
    col = "#ff69b4",
    ylim = NULL,
    ylab = NULL,
    xlab = NULL,
    start_at = 0,
    add = FALSE,
    ribbon_alpha = 0.4,
    line_alpha = 1,
    lty = 1,
    lwd = 2,
    axis_font_size = 12,
    title_font_size = 14,
    font_family = "Aptos",
    ...
) {
  stopifnot(
    "Argument draws must be a matrix or an array." = any(class(draws) %in% c("matrix", "array")),
    "Argument probability must be a number from interval (0,1)." = is.numeric(probability) & length(probability) == 1 & probability > 0 & probability < 1,
    "Argument start_at must be an integer." = is.numeric(start_at) && start_at %% 1 == 0,
    "Argument add must be a logical value." = is.logical(add) & length(add) == 1
  )
  
  mat_or_array_tmp <- length(dim(draws))
  K <- dim(draws)[1]
  S <- dim(draws)[2]
  
  # Ensure draws is always an array with 3 dimensions
  if (mat_or_array_tmp == 2) {
    N <- 1
    draws_tmp <- array(NA, c(K, S, 1)) 
    draws_tmp[,,1] <- draws
    draws <- draws_tmp
  } else {
    N <- dim(draws)[3]
  }
  
  # Compute median and quantiles for confidence intervals
  draws_median <- apply(draws, c(1, 3), stats::median)
  draws_lb <- apply(draws, c(1, 3), stats::quantile, probs = 0.5 * (1 - probability))
  draws_ub <- apply(draws, c(1, 3), stats::quantile, probs = 1 - 0.5 * (1 - probability))
  
  if (is.null(ylim)) ylim <- range(draws_lb, draws_ub)
  if (is.null(ylab)) ylab <- ""
  if (is.null(xlab)) xlab <- ""
  
  # Set colors with specified transparency
  col_ribbon <- get_color_with_alpha(col, ribbon_alpha)
  col_line <- get_color_with_alpha(col, line_alpha)
  
  if (!add) {
    plot(
      x = start_at:(K - 1 + start_at),
      y = draws_median[, 1],
      type = "n",
      ylim = ylim,
      ylab = ylab,
      xlab = xlab,
      cex.lab = axis_font_size / 10,
      cex.axis = axis_font_size / 10,
      family = font_family,
      bty = "l",  # Keep only the left and bottom borders
      ...
    )
  }
  
  # Plot the ribbon and median line for each draw
  for (n in 1:N) {
    polygon(
      x = c(start_at:(K - 1 + start_at), (K - 1 + start_at):start_at),
      y = c(draws_lb[1:K, n], draws_ub[K:1, n]),
      col = col_ribbon,
      border = col_ribbon
    )
    lines(
      x = start_at:(K - 1 + start_at),
      y = draws_median[, n],
      lty = lty,
      lwd = lwd,
      col = col_line
    )
  }
}

# Define plot_responses_to_shock function
plot_responses_to_shock <- function(
    x,
    y,
    shocks = NULL,
    probability = 0.9,
    cols = NULL,
    main = NULL,
    xlab = "Horizon",
    ylab = "Response",
    shock_names = NULL,
    variable_names = NULL,
    layout = "grid",
    nrow = NULL,
    ncol = NULL,
    mar.multi = c(1, 4.1, 2, 1.1),  
    oma.multi = c(5, 0, 5, 0),
    line_style = 1,
    line_width = 2,
    ribbon_alpha = 0.4,
    line_alpha = 1,
    title_font_size = 12,
    axis_font_size = 12,
    font_family = "Aptos",
    ...
) {
  # Default to all shocks if no specific indices are provided
  shocks <- shocks %||% seq_len(dim(x)[2])
  
  shock_names <- shock_names %||% colnames(y)
  variable_names <- variable_names %||% colnames(y)
  
  cols <- cols %||% grDevices::rainbow(length(shocks))
  cols <- rep(cols, length.out = length(shocks))
  
  main <- main %||% paste("Responses to Shocks:", paste(shock_names[shocks], collapse = ", "))
  
  # Set up the layout based on the given parameters
  if (layout == "grid") {
    nrow <- nrow %||% length(shocks)
    ncol <- ncol %||% dim(x)[1]
  } else if (layout == "stacked") {
    nrow <- length(shocks)
    ncol <- 1
  } else if (layout == "side-by-side") {
    nrow <- 1
    ncol <- length(shocks)
  } else {
    stop("Invalid layout option. Choose from 'grid', 'stacked', or 'side-by-side'.")
  }
  
  # Set up layout
  oldpar <- set_plot_layout(nrow, ncol, mar.multi, oma.multi, font_family)
  on.exit(par(oldpar))
  
  # Loop over shock indices
  for (i in seq_along(shocks)) {
    shock_index <- shocks[i]
    
    for (n in 1:dim(x)[1]) {
      custom_plot_ribbon(
        draws = x[n, shock_index, ,],
        probability = probability,
        col = adjustcolor(cols[i], alpha.f = 1),
        ribbon_alpha = ribbon_alpha,
        line_alpha = line_alpha,
        lty = line_style,
        lwd = line_width,
        ylab = "",  
        xlab = if (n == dim(x)[1]) xlab else "",
        start_at = 0,
        bty = "l",
        axes = TRUE,
        ...
      )
      
      draw_plot_annotations(n, axis_font_size, variable_names, add_title = TRUE)
    }
  }
  
  # Main title for the entire plot
  mtext(main, side = 3, line = 2, outer = TRUE, cex = title_font_size / 10)
  
  invisible(x)
}


# Helper function for setting up individual plots
plot_series <- function(df_subset, y_range, xlab, ylab, main_title, color, line_width, line_type, grid_opacity) {
  plot(
    x = df_subset$Time,
    y = df_subset$Value,
    type = "l",
    xlab = "",
    ylab = ylab,
    main = main_title,
    ylim = y_range,
    col = color,
    lwd = line_width,
    lty = line_type,
    frame.plot = TRUE,
    xaxt = "s",
    yaxt = "s",
    bty = "l",
    font.main = 1
  )
  
  # Add custom grid with semi-transparent lines
  add_custom_grid(df_subset$Value, df_subset$Time, grid_opacity)
}

# Helper function to add custom gridlines
add_custom_grid <- function(value_range, time_range, grid_opacity) {
  abline(
    h = pretty(range(value_range)),
    v = pretty(range(time_range)),
    col = get_color_with_alpha("gray", grid_opacity),
    lty = "dotted"
  )
}

# Main function: plot_all_series
plot_all_series <- function(
    data, 
    custom_labels = NULL, 
    nrows = 3, 
    line_width = 2,
    line_type = 1, 
    font_family = "sans", 
    font_size = 1,
    title_size = 1, 
    xlab = "Time", 
    ylab = "Value",
    colors = NULL,  # Option to pass custom color palette
    opacity = 1, 
    grid_opacity = 0.5,
    exclude_colors = c("yellow", "lightgreen")  # Colors to exclude from the default palette
) {
  
  # Ensure custom labels are provided; if not, use default column names
  if (is.null(custom_labels)) {
    custom_labels <- colnames(data)
  }
  
  # Extract time index from the time series object
  time_index <- time(data)
  
  # Create a data frame for plotting
  df <- as.data.frame(data)
  df$Time <- time_index
  
  # Reshape data for plotting
  df_long <- tidyr::pivot_longer(df, cols = -Time, names_to = "Series", values_to = "Value")
  
  # Map custom labels to the Series variable
  df_long$Series <- factor(df_long$Series, levels = colnames(data), labels = custom_labels)
  
  # Determine number of series and set up plot layout dynamically
  n_series <- length(unique(df_long$Series))
  ncols <- ceiling(n_series / nrows)
  par(mfrow = c(nrows, ncols), mar = c(3, 3, 2, 1), family = font_family,
      cex.main = title_size, cex.lab = font_size, cex.axis = font_size)
  
  # Set colors if not provided
  if (is.null(colors)) {
    # Generate default colors excluding specified ones
    available_colors <- grDevices::colors()
    filtered_colors <- available_colors[!available_colors %in% exclude_colors]
    colors <- grDevices::rainbow(n_series, start = 0, end = 0.85)  # Customize the color range
  }
  
  # Ensure the number of colors matches the number of series
  colors <- rep(colors, length.out = n_series)
  
  # Set up individual plots per series
  for (i in unique(df_long$Series)) {
    # Filter data for the current series
    df_subset <- df_long[df_long$Series == i, ]
    
    # Calculate y-axis limits for the current series
    y_range <- range(df_subset$Value, na.rm = TRUE)
    
    # Plot each series using the helper function
    plot_series(
      df_subset = df_subset,
      y_range = y_range,
      xlab = xlab,
      ylab = ylab,
      main_title = i,
      color = get_color_with_alpha(colors[which(unique(df_long$Series) == i)], opacity),
      line_width = line_width,
      line_type = line_type,
      grid_opacity = grid_opacity
    )
  }
  
  # Reset plotting parameters
  par(mfrow = c(1, 1), family = "", cex.main = 1, cex.lab = 1, cex.axis = 1)
}



#' Plots fitted values of specified dependent variables with the option to overlay multiple models
#'
#' @description Plots fitted values of specified dependent variables including their 
#' median and percentiles, with each variable plotted in its own panel and dates on the x-axis.
#' The function incorporates customizable aesthetics such as font sizes, colors, and gridlines, and allows
#' overlaying predictions from multiple models.
#' 
#' @param x an object of class Forecasts obtained using the \code{forecast()} function containing posterior draws of fitted values of dependent variables.
#' @param x_overlay an optional object of class Forecasts for overlaying predictions from another model.
#' @param variables a numeric vector of indices or a character vector of names specifying which variables to plot. Defaults to all variables.
#' @param variable_names an optional vector of variable names corresponding to the variables in the dataset.
#' If provided, these names will be used for the y-axis labels.
#' @param probability a parameter determining the interval to be plotted. The interval stretches from the \code{0.5 * (1 - probability)} to \code{1 - 0.5 * (1 - probability)} percentile of the posterior distribution.
#' @param history_length the number of years (or periods) of historical data to include in the plot. If specified, it overrides \code{data_in_plot}.
#' @param data_in_plot a fraction value in the range (0, 1] determining how many of the last observations in the data should be plotted with the forecasts.
#' Ignored if \code{history_length} is provided.
#' @param start_date the start date of the historical data, in a format recognized by \code{as.Date}.
#' @param frequency the frequency of the data. Use 12 for monthly data, 4 for quarterly data, etc.
#' @param date_vector an optional vector of dates corresponding to the historical data. If provided, it overrides \code{start_date} and \code{frequency}.
#' @param col a color for the plot lines and ribbons of the main forecast.
#' @param col_overlay a color for the plot lines and ribbons of the overlay forecast.
#' @param main an alternative main title for the plot.
#' @param xlab an alternative x-axis label for the plot.
#' @param ylab an alternative y-axis label for the plot.
#' @param mar.multi the default \code{mar} argument setting in \code{graphics::par}. Modify with care!
#' @param oma.multi the default \code{oma} argument setting in \code{graphics::par}. Modify with care!
#' @param font_family the font family to be used in the plots.
#' @param axis_font_size the font size for axis labels.
#' @param title_font_size the font size for titles.
#' @param line_width the width of the plot lines.
#' @param line_type the type of the plot lines.
#' @param ribbon_alpha the transparency level of the ribbon (shaded area).
#' @param grid_opacity the transparency level of the grid lines.
#' @param ... additional arguments affecting the plot produced.
#' 
#' @export
plot_forecast <- function(
    x,
    x_overlay = NULL, # optional second set of forecasts for overlay
    variables = NULL, # specify variables to plot
    variable_names = NULL, # optional variable names
    probability = 0.68,
    history_length = NULL, # number of years or periods of historical data to plot
    data_in_plot = 1,
    start_date = NULL,
    frequency = 12,
    date_vector = NULL,
    col = "#ff69b4",
    col_overlay = "#1f77b4",  # color for the overlay plot
    main = "Forecasting",
    xlab = "Time",
    ylab = "Value",
    mar.multi = c(1, 2, 2, 1),  # Adjusted margins
    oma.multi = c(3, 1, 2, 1),
    font_family = "sans",
    axis_font_size = 12,
    title_font_size = 14,
    line_width = 2,
    line_type = 1,
    ribbon_alpha = 0.4,
    grid_opacity = 0.5,
    ...
) {
  # Helper function for color adjustment
  get_color_with_alpha <- function(col, alpha) {
    adjustcolor(col, alpha.f = alpha)
  }
  
  # Helper function to add custom gridlines
  add_custom_grid <- function(value_range, date_range, grid_opacity) {
    abline(
      h = pretty(range(value_range)),
      v = pretty(date_range),
      col = get_color_with_alpha("gray", grid_opacity),
      lty = "dotted"
    )
  }
  
  # Validate input parameters
  if (!is.null(data_in_plot)) {
    stopifnot("Argument data_in_plot must be a value within range (0,1]" = 
                is.numeric(data_in_plot) & data_in_plot > 0 & data_in_plot <= 1)
  }
  
  fore = x$forecasts
  Y    = x$Y
  
  N    = dim(fore)[1]
  H    = dim(fore)[2]
  T    = dim(Y)[2]
  
  # If x_overlay is provided, extract the forecasts for the overlay
  if (!is.null(x_overlay)) {
    fore_overlay = x_overlay$forecasts
    stopifnot("x_overlay must have the same number of variables and history length as x." = 
                all(dim(fore_overlay)[1] == N & dim(fore_overlay)[2] == H & dim(fore_overlay)[3] == dim(fore)[3]))
  }
  
  # Handle variable_names argument
  if (!is.null(variable_names)) {
    if (length(variable_names) != N) {
      stop("Length of variable_names must match the number of variables in the data.")
    }
  } else if (!is.null(x$variable_names)) {
    variable_names <- x$variable_names
  } else {
    variable_names <- paste("Variable", 1:N)
  }
  
  # Handle variables argument
  if (is.null(variables)) {
    variables_to_plot <- 1:N
  } else {
    if (is.numeric(variables)) {
      variables_to_plot <- variables
    } else if (is.character(variables)) {
      variables_to_plot <- match(variables, variable_names)
      if (any(is.na(variables_to_plot))) {
        stop("Some variable names not found in the forecast object.")
      }
    } else {
      stop("variables argument must be numeric indices or variable names.")
    }
  }
  
  num_vars <- length(variables_to_plot)
  
  # Generate colors with transparency
  col_ribbon <- get_color_with_alpha(col, ribbon_alpha)
  col_line <- get_color_with_alpha(col, 1)
  col_ribbon_overlay <- get_color_with_alpha(col_overlay, ribbon_alpha)
  col_line_overlay <- get_color_with_alpha(col_overlay, 1)
  
  # Determine the number of periods to include in the plot
  if (!is.null(history_length)) {
    # Convert history_length (in years) to number of periods
    periods_in_year <- frequency
    T_in_plot <- history_length * periods_in_year
    if (T_in_plot > T) T_in_plot <- T
  } else {
    # Use data_in_plot fraction
    T_in_plot <- floor(data_in_plot * T)
    if (T_in_plot < 1) T_in_plot <- 1
  }
  
  obs_in_plot = (T - T_in_plot + 1):T
  seq_in_plot = 1:T_in_plot
  for_in_plot = (T_in_plot + 1):(T_in_plot + H)
  
  total_periods <- T_in_plot + H
  
  # Create date sequence
  if (!is.null(date_vector)) {
    if (length(date_vector) != T) {
      stop("Length of date_vector must be equal to the number of observations in the historical data.")
    }
    dates_hist <- date_vector[obs_in_plot]
  } else if (!is.null(start_date) && !is.null(frequency)) {
    start_date <- as.Date(start_date)
    all_dates <- seq.Date(from = start_date, by = paste0(12 / frequency, " months"), length.out = T + H)
    dates_hist <- all_dates[obs_in_plot]
  } else {
    stop("Either date_vector or both start_date and frequency must be provided.")
  }
  # Generate dates for forecast periods
  dates_fore <- seq.Date(from = dates_hist[length(dates_hist)] + 1, by = paste0(12 / frequency, " months"), length.out = H)
  # Combine dates
  dates_all <- c(dates_hist, dates_fore)
  
  # Set up plotting area with multiple panels
  oldpar <- graphics::par(
    mfrow = c(num_vars, 1),
    mar = mar.multi,
    oma = oma.multi,
    family = font_family,
    cex.lab = axis_font_size / 10,
    cex.axis = axis_font_size / 10
  )
  on.exit(graphics::par(oldpar))
  
  for (i in seq_along(variables_to_plot)) {
    n <- variables_to_plot[i]
    # Compute forecasts characteristics
    fore_median <- apply(fore[n,,], 1, stats::median)
    fore_lb     <- apply(fore[n,,], 1, stats::quantile, probs = 0.5 * (1 - probability))
    fore_ub     <- apply(fore[n,,], 1, stats::quantile, probs = 1 - 0.5 * (1 - probability))
    
    # For overlay, compute forecast characteristics (if available)
    if (!is.null(x_overlay)) {
      fore_median_overlay <- apply(fore_overlay[n,,], 1, stats::median)
      fore_lb_overlay     <- apply(fore_overlay[n,,], 1, stats::quantile, probs = 0.5 * (1 - probability))
      fore_ub_overlay     <- apply(fore_overlay[n,,], 1, stats::quantile, probs = 1 - 0.5 * (1 - probability))
    }
    
    # Determine y-range
    y_range <- range(fore_lb, fore_ub, Y[n, obs_in_plot], na.rm = TRUE)
    if (!is.null(x_overlay)) {
      y_range <- range(y_range, fore_lb_overlay, fore_ub_overlay, na.rm = TRUE)
    }
    
    # Plot the variable forecast with axes = TRUE but disable x-axis ticks (xaxt = "n")
    plot(
      x = dates_all,
      y = c(Y[n, obs_in_plot], fore_median),
      type = "n",
      ylim = y_range,
      main = "",
      ylab = ylab,
      xlab = "",
      bty = "l",  # Keep only the left and bottom borders
      axes = TRUE,  # Enable axes
      xaxt = "n",   # Disable default x-axis ticks
      ...
    )
    
    # Add custom gridlines
    add_custom_grid(c(Y[n, obs_in_plot], fore_median, fore_lb, fore_ub), dates_all, grid_opacity)
    
    # Draw ribbon for main forecast
    polygon(
      x = c(dates_all[T_in_plot:(T_in_plot + H)], rev(dates_all[T_in_plot:(T_in_plot + H)])),
      y = c(c(Y[n, obs_in_plot[T_in_plot]], fore_lb), rev(c(Y[n, obs_in_plot[T_in_plot]], fore_ub))),
      col = col_ribbon,
      border = NA
    )
    
    # Plot forecast median line for main forecast
    lines(
      x = dates_all,
      y = c(Y[n, obs_in_plot], fore_median),
      lty = line_type,
      lwd = line_width,
      col = col_line
    )
    
    # Plot historical data
    lines(
      x = dates_all[1:T_in_plot],
      y = Y[n, obs_in_plot],
      lty = line_type,
      lwd = line_width,
      col = col_line
    )
    
    # If overlay data is available, plot it
    if (!is.null(x_overlay)) {
      # Draw ribbon for overlay forecast
      polygon(
        x = c(dates_all[T_in_plot:(T_in_plot + H)], rev(dates_all[T_in_plot:(T_in_plot + H)])),
        y = c(c(Y[n, obs_in_plot[T_in_plot]], fore_lb_overlay), rev(c(Y[n, obs_in_plot[T_in_plot]], fore_ub_overlay))),
        col = col_ribbon_overlay,
        border = NA
      )
      
      # Plot forecast median line for overlay forecast
      lines(
        x = dates_all,
        y = c(Y[n, obs_in_plot], fore_median_overlay),
        lty = line_type,
        lwd = line_width,
        col = col_line_overlay
      )
    }
    
    # Manually add the x-axis with yearly ticks
    if (i == num_vars) {  # Only add the x-axis for the last plot
      axis.Date(1, at = seq(dates_all[1], dates_all[length(dates_all)], by = "year"), format = "%Y", cex.axis = axis_font_size / 10)
    }
    
    # Add variable name as title
    title(variable_names[n], line = 0.5, cex.main = title_font_size / 10, font.main = 1)
    
    # Add a horizontal line at y = 0
    abline(h = 0)
  }
  
  # Add the main title for the entire plot
  mtext(
    main,
    side = 3,
    line = 2,
    outer = TRUE,
    cex = title_font_size / 10
  )
  
  # Add the x-axis label (only displayed once across all plots)
  mtext(
    xlab,
    side = 1,
    line = 3,
    outer = TRUE,
    cex = axis_font_size / 10
  )
  
  invisible(x)
}

#' @title Plots historical decompositions
#'
#' @description This function plots the posterior means of the historical decompositions.
#' It allows custom colors, titles, axis labels, and multiple customization options for fine-tuning the plot.
#' 
#' @param x an object of class PosteriorHD obtained using the \code{compute_historical_decompositions()} function containing posterior draws of historical decompositions.
#' @param cols a vector of colors to use for different shocks. If not provided, default colors will be used.
#' @param main an alternative main title for the plot. Defaults to "Historical Decompositions".
#' @param xlab an alternative x-axis label for the plot. Defaults to "time".
#' @param mar.multi the default \code{mar} argument setting in \code{graphics::par}. Modify with care.
#' @param oma.multi the default \code{oma} argument setting in \code{graphics::par}. Modify with care.
#' @param ... additional arguments affecting the plot produced.
#' 
#' @method plot PosteriorHD
#' 
#' @seealso \code{\link{compute_historical_decompositions}}
#'
#' @export
plot_historical_decomposition <- function(
    x,
    selected_decompositions = NULL, # Select specific decompositions to plot (based on variables)
    cols = NULL, # Colors for different shocks
    main = "Historical Decompositions",
    xlab = "Time",
    ylab = "Response",
    mar.multi = c(1, 4.1, 2, 1.1),  
    oma.multi = c(5, 0, 5, 0),
    line_width = 2,  # Customizable line width
    line_type = 1,   # Customizable line type
    ribbon_alpha = 0.4,  # Transparency for ribbons
    line_alpha = 0.9,    # Transparency for lines
    grid_opacity = 0.5,  # Transparency for gridlines
    font_family = "Aptos",  # Custom font
    axis_font_size = 12,  # Axis font size
    title_font_size = 14, # Title font size
    legend_labels = NULL, # Labels for the legend corresponding to shocks
    ...
) {
  
  # Compute mean historical decompositions from the posterior draws
  hd_mean <- apply(x, 1:3, mean)
  N <- dim(hd_mean)[1]  # Number of variables
  T <- dim(hd_mean)[3]  # Time periods
  
  # Set default for selected decompositions (plot all if not specified)
  if (is.null(selected_decompositions)) {
    selected_decompositions <- 1:N
  }
  
  # Ensure valid selection of decompositions
  if (!all(selected_decompositions %in% 1:N)) {
    stop("Selected decompositions must be valid indices within the decomposition list.")
  }
  
  # Set default colors if none are provided
  if (is.null(cols)) {
    color_palette <- grDevices::colorRampPalette(c("#ff69b4", "#ffd700"))
    cols <- color_palette(N)
  }
  
  # Set default legend labels if none are provided
  if (is.null(legend_labels)) {
    legend_labels <- paste("Shock", 1:N)  # Default labels as "Shock 1", "Shock 2", etc.
  }
  
  # Compute cumulative contributions for historical decompositions
  y_hat <- apply(hd_mean, c(1, 3), sum)
  ul <- list()
  
  for (n in 1:N) {
    ul[[n]] <- list()
    for (i in 1:N) {
      cum_mean <- apply(matrix(hd_mean[n, i:N, ], ncol = T), 2, sum)
      ul[[n]][[i]] <- matrix(0, 2, T)
      which_negative <- cum_mean < 0
      ul[[n]][[i]][1, which_negative] <- cum_mean[which_negative]
      ul[[n]][[i]][2, !which_negative] <- cum_mean[!which_negative]
    }
  }
  
  # Set up plotting parameters
  oldpar <- graphics::par(
    mfrow = c(length(selected_decompositions), 1),  # Adjust number of plots dynamically
    mar = mar.multi,
    oma = oma.multi,
    family = font_family  # Apply custom font family
  )
  on.exit(graphics::par(oldpar))
  
  # Loop over each selected decomposition to plot it
  # Loop over each selected decomposition to plot it
  for (n in selected_decompositions) {
    
    # Initialize cumulative sums for calculating the total range of the stacked plots
    cumulative_upper <- rep(0, T)
    cumulative_lower <- rep(0, T)
    
    # Calculate the cumulative sum for upper and lower bounds to determine the y-axis limits
    for (i in 1:N) {
      cumulative_upper <- cumulative_upper + ul[[n]][[i]][2, 1:T]
      cumulative_lower <- cumulative_lower + ul[[n]][[i]][1, 1:T]
    }
    
    # Determine the range of y-values (taking the minimum and maximum of cumulative sums)
    range_n <- range(c(cumulative_lower, cumulative_upper))
    
    # Plot the historical decomposition for the selected variable with the adjusted y-limits
    graphics::plot(
      x = 1:T,
      y = cumulative_upper,  # Use the cumulative upper bounds for plotting (this is just a placeholder)
      type = "n",
      ylim = range_n,  # Set y-axis limits based on the cumulative range
      ylab = ylab,  # Custom y-axis label
      main = "",    # We will use an outer title
      xlab = "",    # Leave the x-label for later
      bty = "n",
      axes = FALSE
    )
    
    # Add gridlines
    abline(
      h = pretty(range(range_n)),
      col = adjustcolor("gray", alpha.f = grid_opacity),
      lty = "dotted"
    )
    
    # Initialize cumulative sum for stacking again for actual plotting
    cumulative_upper <- rep(0, T)
    cumulative_lower <- rep(0, T)
    
    # Plot stacked ribbons for the decompositions
    for (i in 1:N) {
      # Define the upper and lower bounds for each shock's contribution
      upper <- cumulative_upper + ul[[n]][[i]][2, 1:T]
      lower <- cumulative_lower + ul[[n]][[i]][1, 1:T]
      
      # Draw the polygon for the current shock, stacked on top of the previous ones
      graphics::polygon(
        x = c(1:T, rev(1:T)),
        y = c(lower, rev(upper)),
        col = adjustcolor(cols[i], alpha.f = ribbon_alpha),  # Ribbon color for shocks with transparency
        border = adjustcolor(cols[i], alpha.f = line_alpha)  # Line border with transparency
      )
      
      # Update the cumulative sums for stacking
      cumulative_upper <- upper
      cumulative_lower <- lower
    }
    
    # Add horizontal line at zero
    graphics::abline(h = 0, lty = line_type, lwd = 1, col = "black")
    
    # Add axes
    graphics::axis(1, cex.axis = axis_font_size / 10)  # Custom axis font size
    graphics::axis(2, cex.axis = axis_font_size / 10)
  }
  
  
  # Add main title
  graphics::mtext(main, side = 3, line = 2, outer = TRUE, cex = title_font_size / 10)  # Custom title size
  
  # Add X-axis label only once
  graphics::mtext(xlab, side = 1, line = 3, outer = TRUE, cex = axis_font_size / 10)
  
  # Add the legend at the bottom, outside the plot
  graphics::par(mar = c(0, 0, 0, 0), oma = c(4, 0, 0, 0))  # Set margins for the legend
  graphics::legend(
    "bottom", 
    legend = legend_labels,  # Use the provided or default labels
    fill = cols,             # Colors matching the shocks
    horiz = TRUE,            # Horizontal layout
    bty = "n",               # No box around the legend
    inset = c(0, -0.15),     # Adjust placement just below the plot
    cex = axis_font_size / 10  # Adjust the size of legend text to match other font sizes
  )
  
  invisible(x)
}

plot_historical_decomposition_bar <- function(
    x,
    selected_decompositions = NULL,  # Select specific decompositions to plot (based on variables)
    cols = NULL,                     # Colors for different shocks
    main = "",
    xlab = "Time",
    ylab = "Response",
    mar.multi = c(1, 5, 0, 0.2),  
    oma.multi = c(6, 0, 4, 0.2),
    bar_width = 2,  # Width of the bars
    grid_opacity = 0.5,  # Transparency for gridlines
    font_family = "Aptos",  # Custom font
    axis_font_size = 12,  # Axis font size
    title_font_size = 14, # Title font size
    legend_labels = NULL, # Labels for the legend corresponding to shocks
    date_vector = NULL,  # Vector of dates for x-axis
    start_date = NULL,   # Start date for generating dates if no date_vector is provided
    frequency = 12,      # Frequency of the time series (12 for monthly, 4 for quarterly)
    tick_freq = 6,
    legend_position = "right",  # Position of the legend: "right" or "top"
    ...
) {
  
  # Compute mean historical decompositions from the posterior draws
  hd_mean <- apply(x, 1:3, mean)
  N <- dim(hd_mean)[1]  # Number of variables
  T <- dim(hd_mean)[3]  # Time periods
  
  # Set default for selected decompositions (plot all if not specified)
  if (is.null(selected_decompositions)) {
    selected_decompositions <- 1:N
  }
  
  # Ensure valid selection of decompositions
  if (!all(selected_decompositions %in% 1:N)) {
    stop("Selected decompositions must be valid indices within the decomposition list.")
  }
  
  # Set default colors if none are provided
  if (is.null(cols)) {
    cols <- RColorBrewer::brewer.pal(N, "Set3")  # Use Set3 color palette
  }
  
  # Set default legend labels if none are provided
  if (is.null(legend_labels)) {
    legend_labels <- paste("Shock", 1:N)  # Default labels as "Shock 1", "Shock 2", etc.
  }
  
  # Ensure a date vector or start_date is provided
  if (is.null(date_vector)) {
    if (is.null(start_date) || is.null(frequency)) {
      stop("Please provide either a date_vector or both start_date and frequency.")
    }
    # Generate the date vector using start_date and frequency
    date_vector <- seq.Date(from = as.Date(start_date), by = paste0(12 / frequency, " months"), length.out = T)
  }
  
  # Layout depending on the position of the legend
  if (legend_position == "right") {
    layout(matrix(c(1, 2), nrow = 1), widths = c(4, 1))  # FEVD plot and legend side by side
  } else {
    layout(matrix(c(1, 2), nrow = 2), heights = c(4, 1))  # FEVD plot and legend on top
  }
  
  # Set up plotting parameters for the bar plot
  oldpar <- graphics::par(
    mar = mar.multi,
    oma = oma.multi,
    family = font_family  # Apply custom font family
  )
  on.exit(graphics::par(oldpar))
  
  # Loop over each selected decomposition to plot it using stacked bar plot
  for (n in selected_decompositions) {
    
    # Initialize cumulative sums for stacking bars
    cumulative_upper <- rep(0, T)
    cumulative_lower <- rep(0, T)
    
    # Prepare bar plot data for each shock
    bar_data_pos <- matrix(0, nrow = N, ncol = T)
    bar_data_neg <- matrix(0, nrow = N, ncol = T)
    
    # Calculate positive and negative contributions separately
    for (i in 1:N) {
      bar_data_pos[i, ] <- hd_mean[n, i, ] * (hd_mean[n, i, ] >= 0)
      bar_data_neg[i, ] <- hd_mean[n, i, ] * (hd_mean[n, i, ] < 0)
    }
    
    # Calculate the range of cumulative sums
    cumulative_sum <- apply(bar_data_pos, 2, sum) + apply(bar_data_neg, 2, sum)
    ylim <- range(cumulative_sum)
    
    # Add a buffer to both sides of the y-axis range
    ylim_buffer <- 0.6 * diff(ylim)  # 60% of the range
    ylim <- c(ylim[1] - ylim_buffer, ylim[2] + ylim_buffer)
    
    # Plot the historical decomposition using stacked bar plot with black borders
    bar_x <- graphics::barplot(
      bar_data_pos,   # Data for the positive bars (stacked)
      beside = FALSE,  # Stack bars
      col = cols,  # Colors for each decomposition
      ylim = ylim,  # Set y-axis limits with buffer
      border = "black",  # Add fine black borders to each bar
      xlab = "",  # Leave x-label for later
      ylab = ylab,  # Custom y-axis label
      main = "",  # We will add the main title later
      space = 0,  # No space between bars
      axes = FALSE,  # Disable default axes
      width = bar_width  # Width of the bars
    )
    
    # Add the negative part of the bars
    graphics::barplot(
      bar_data_neg,   # Data for the negative bars (stacked)
      beside = FALSE,  # Stack bars
      col = cols,  # Colors for each decomposition
      ylim = ylim,  # Set y-axis limits with buffer
      border = "black",  # Add fine black borders to each bar
      xlab = "",  # Leave x-label for later
      space = 0,  # No space between bars
      axes = FALSE,  # Disable default axes
      add = TRUE,    # Add to the existing plot
      width = bar_width  # Width of the bars
    )
    
    # Add the fitted values as a line over the bars
    fitted_values_for_variable <- apply(hd_mean[n, , ], 2, sum)
    lines(bar_x, fitted_values_for_variable, type = "l", col = "black", lwd = 2)
    
    # Add gridlines
    abline(
      h = pretty(range(ylim)),
      col = adjustcolor("gray", alpha.f = grid_opacity),
      lty = "dotted"
    )
    
    # Add horizontal line at zero
    graphics::abline(h = 0, col = "black", lty = 1)
    
    # Add y-axis
    graphics::axis(2, cex.axis = axis_font_size / 10)
    
    # Custom date labels every 6 months on the x-axis
    date_labels <- format(date_vector, "%Y-%m")
    every_6_months <- seq(1, length(date_labels), by = tick_freq)  # Indices for every 6 months
    axis(1, at = bar_x[every_6_months], labels = date_labels[every_6_months], cex.axis = axis_font_size / 10, las = 2)
  }
  
  # Add main title
  graphics::mtext(main, side = 3, line = 2, outer = TRUE, cex = title_font_size / 10)  # Custom title size
  
  # Add X-axis label only once
  graphics::mtext(xlab, side = 1, line = 3, outer = TRUE, cex = axis_font_size / 10)
  
  # Switch to the legend panel in the layout
  graphics::par(mar = c(0, 0, 0, 0))  # Adjust margins for the legend space
  graphics::plot.new()  # Create a blank plot for the legend
  graphics::legend(
    "center",  # Center the legend in the separate plot
    legend = legend_labels,  # Legend labels
    fill = cols,             # Colors for each decomposition
    bty = "n",               # No box around the legend
    cex = axis_font_size / 10  # Adjust size of the legend text
  )
  
  invisible(x)
}



plot_fevd <- function(
    x,
    selected_variable = NULL,  # Select specific decomposition to plot
    cols = NULL,               # Colors for different shocks
    main = "Forecast Error Variance Decomposition",  # Main title
    xlab = "Horizon",          # X-axis label
    ylab = "Contribution (%)",  # Y-axis label
    mar.multi = c(1, 5, 0, 0.2),  
    oma.multi = c(6, 0, 4, 0.2),    # Add outer margin space
    legend_mar = c(0, 0, 0, 0),     # Margins for the legend plot
    font_family = "Aptos",          # Custom font
    axis_font_size = 12,            # Axis font size
    title_font_size = 14,           # Title font size
    legend_labels = NULL,           # Labels for the shocks in the legend
    grid_opacity = 0.5,             # Gridline transparency
    tick_freq = 6,                  # Frequency for ticks on the x-axis
    ...
) {
  
  # Compute mean FEVD from the posterior draws
  fevd <- apply(x, 1:3, mean)
  N <- dim(fevd)[1]  # Number of variables
  H <- dim(fevd)[3] - 1  # Horizon length
  
  # Set default for selected variable (plot all if not specified)
  if (is.null(selected_variable)) {
    selected_variable <- 1:N
  }
  
  # Ensure valid selection of the variable
  if (!all(selected_variable %in% 1:N)) {
    stop("Selected variable must be valid within the variable list.")
  }
  
  # Set default colors if none are provided
  if (is.null(cols)) {
    cols <- RColorBrewer::brewer.pal(N, "Set3")  # Use Set3 color palette
  }
  
  # Set default legend labels if none are provided
  if (is.null(legend_labels)) {
    legend_labels <- paste("Shock", 1:N)  # Default labels as "Shock 1", "Shock 2", etc.
  }
  
  # Initialize the FEVD plotting data
  FEVD <- list()
  for (n in 1:N) {
    FEVD[[n]] <- rbind(rep(0, H + 1), apply(fevd[n, , ], 2, cumsum))
  }
  
  # Set up a layout with two side-by-side plots (FEVD plot on the left, legend on the right)
  layout(matrix(c(1, 2), nrow = 1), widths = c(4, 1))  # Two parts: 4/5 for plots, 1/5 for legend
  
  # Set up plotting parameters for the FEVD plots
  oldpar <- graphics::par(
    mar = mar.multi,
    oma = oma.multi,
    family = font_family  # Apply custom font family
  )
  on.exit(graphics::par(oldpar))
  
  # Loop over each selected variable to plot it
  for (n in selected_variable) {
    
    # Set up plot area with proper limits and labels
    graphics::plot(
      x = 0:H,
      y = FEVD[[n]][1, ],
      type = "n",
      ylim = c(0, 100),  # y-axis is in percentage terms
      ylab = ylab,       # Custom y-axis label
      xlab = "",         # x-label will be added later
      bty = "n",
      axes = FALSE
    )
    
    # Draw the forecast error variance decomposition using polygons
    for (i in 1:N) {
      graphics::polygon(
        c(0:H, H:0), 
        c(FEVD[[n]][i, ], rev(FEVD[[n]][i + 1, ])), 
        col = adjustcolor(cols[i], alpha.f = 0.8),  # Color for each shock with transparency
        border = cols[i]  # Border color for each polygon
      )
    }
    
    # Add gridlines
    graphics::abline(
      h = pretty(c(0, 100)),
      col = adjustcolor("gray", alpha.f = grid_opacity),
      lty = "dotted"
    )
    
    # Add horizontal and vertical axes
    graphics::axis(1, cex.axis = axis_font_size / 10)
    graphics::axis(2, c(0, 50, 100), c(0, 50, 100), cex.axis = axis_font_size / 10)
  }
  
  # Switch to the legend panel in the layout
  graphics::par(mar = legend_mar)  # Set margins for the legend space
  graphics::plot.new()  # Create a blank plot for the legend
  graphics::legend(
    "center",  # Center the legend in the separate plot
    legend = legend_labels,  # Legend labels
    fill = cols,             # Colors for each shock
    bty = "n",               # No box around the legend
    cex = axis_font_size / 10  # Adjust size of the legend text
  )
  
  # Add the common main title and x-axis label
  graphics::mtext(main, side = 3, line = 1, outer = TRUE, cex = title_font_size / 10)
  graphics::mtext(xlab, side = 1, line = 3, outer = TRUE, cex = axis_font_size / 10)
  
  invisible(x)
}

plot_shocks <- function(
    x,
    probability = 0.9,
    shocks = NULL,  # Select specific shocks to plot
    col = "#ff69b4",
    main = "Structural Shocks",
    xlab = "Time",
    shock_names = NULL,  # Custom names for the shocks
    date_vector = NULL,  # New parameter for time/date vector
    start_date = NULL,  # Start date if date_vector not provided
    frequency = 12,  # Frequency of the data (monthly, quarterly, etc.)
    mar.multi = c(1, 4.6, 0, 2.1),
    oma.multi = c(6, 0, 5, 0),
    font_family = "Aptos",
    axis_font_size = 12,
    title_font_size = 14,
    line_width = 2,
    ribbon_alpha = 0.4,
    line_alpha = 1,
    grid_opacity = 0.5,
    add_grid = TRUE,
    ...
) {
  
  N <- dim(x)[1]
  
  # Default to all shocks if no specific indices are provided
  if (is.null(shocks)) {
    shocks <- 1:N
  }
  
  # Use default shock names if none provided
  if (is.null(shock_names)) {
    shock_names <- paste("Shock", 1:N)
  }
  
  # Handle time vector: use date_vector if provided, otherwise generate dates from start_date and frequency
  if (is.null(date_vector)) {
    if (!is.null(start_date) && !is.null(frequency)) {
      date_vector <- seq.Date(from = as.Date(start_date), by = paste0(12 / frequency, " months"), length.out = dim(x)[2])
    } else {
      stop("Either date_vector or both start_date and frequency must be provided.")
    }
  } else if (length(date_vector) != dim(x)[2]) {
    stop("Length of date_vector must match the number of periods in the data.")
  }
  
  # Set plot layout for the number of shocks selected
  set_plot_layout(nrow = length(shocks), ncol = 1, mar.multi = mar.multi, oma.multi = oma.multi, font_family = font_family)
  
  # Loop through each selected shock
  for (i in seq_along(shocks)) {
    n <- shocks[i]
    
    # Use custom_plot_ribbon to plot the ribbons for each selected shock
    custom_plot_ribbon(
      draws = x[n,,],
      probability = probability,
      col = col,
      ribbon_alpha = ribbon_alpha,
      line_alpha = line_alpha,
      lty = 1,
      lwd = line_width,
      axis_font_size = axis_font_size,
      ylim = NULL,
      ylab = shock_names[n],  # Use custom shock names
      xlab = if (i == length(shocks)) xlab else "",  # Only label x-axis for the last plot
      start_at = 1,
      font_family = font_family,
      bty = "n",   # Keep only left and bottom border
      axes = TRUE,
      add = FALSE,  # Create a new plot
      ...
    )
    
    # Optionally add gridlines
    if (add_grid) {
      add_custom_grid(value_range = range(x[n,,]), time_range = date_vector, grid_opacity = grid_opacity)
    }
    
    # Add y-axis labels and zero line
    draw_plot_annotations(n, axis_font_size, variable_names = shock_names[n], add_title = TRUE)
    
    # Add the x-axis with date labels (only for the last plot)
    if (i == length(shocks)) {
      axis.Date(1, at = seq(from = min(date_vector), to = max(date_vector), by = "year"), format = "%Y", cex.axis = axis_font_size / 10)
    }
  }
  
  # Add the main title and x-axis label
  graphics::mtext(main, side = 3, line = 2, outer = TRUE, cex = title_font_size / 10)
  graphics::mtext(xlab, side = 1, line = 3, outer = TRUE, cex = axis_font_size / 10)
  
  invisible(x)
}