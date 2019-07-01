

br_colors <- c("orange" = rgb(255/255, 71/255, 19/255),
               "yellow" = rgb(255/255, 206/255, 0/255),
               "green" = rgb(0/255, 139/255, 92/255),
               "red" = rgb(192/255, 11/255, 40/255),
               "purple" = rgb(144/255, 98/255, 188/255),
               "pink" = rgb(252/255, 155/255, 179/255),
               "black" = rgb(0/255, 0/255, 0/255),
               "grey" = rgb(124/255, 123/255, 127/255),
               "dark_orange" = rgb(205/255, 65/255, 25/255),
               "dark_yellow" = rgb(252/255, 169/255, 0/255),
               "dark_green" = rgb(0/255, 87/255, 60/255),
               "dark_red" = rgb(153/255, 0/255, 19/255),
               "dark_purple" = rgb(110/255, 63/255, 163/255),
               "dark_pink" = rgb(200/255, 0/255, 88/255),
               "white" = rgb(255/255, 255/255, 255/255))

br_colors_add_alpha <- function(rgb_color, alpha = 0.4)
{
  color_df <- col2rgb(rgb_color)
  color <- as.vector(color_df)
  names(color) <- row.names(color_df)
  color["alpha"] <- round(alpha * 255, 0)
  color <- color / 255
  rgb(red = color["red"], green = color["green"], blue = color["blue"], alpha = color["alpha"])
}


brplot_left_1 <- function(x, y, legend_text = "", theme = "grid")
{
  #theme can be "grid" or "box"
  
  if(theme == "grid")
  {
    par(mar = c(3,3,1,1), las = 1, col.main = "black", cex.axis = 0.9, cex.lab = 1, col = "black", cex.main = 1, family = "Calibri")
    plot(x, y, axes = FALSE, xlab = NA, ylab = NA, type = "l", col = br_colors[1], lwd = 2)
    grid(lty = 1, col = "lightgrey", ny = NULL, nx = 0)
    if(class(x) == "Date")
    {
      axis.Date(x = x, side = 1, tick = TRUE, col = NA, col.ticks = "grey")
    }else
    {
      axis(side = 1, tick = TRUE, col = NA, col.ticks = "grey")  
    }
    axis(side = 2, tick = FALSE, col = NA, hadj = 0.5)
    legend(x = min(x), y = max(y), legend = legend_text, xjust = 0.15, yjust = 1,
           col = c(br_colors[1]), lty = 1, lwd = 2, bty = "n", cex = 0.9) 
  }else if(theme == "box")
  {
    par(mar = c(3,3,1,1), las = 1, col.main = "black", cex.axis = 0.9, cex.lab = 1, col = "black", cex.main = 1, family = "Calibri")
    plot(x, y, axes = TRUE, xlab = NA, ylab = NA, type = "l", col = br_colors[1], lwd = 2, xaxt = NULL, yaxt = NULL)
    legend(x = min(x), y = max(y), legend = legend_text, xjust = 0.15, yjust = 1,
           col = c(br_colors[1]), lty = 1, lwd = 2, bty = "n", cex = 0.9)
    
  }else
  {
  }
}

brplot_left_2 <- function(x, y1, y2, legend_text = "", theme = "grid")
{
  # theme can be "grid" or "box"
  y_limits <- c(min(y1, y2), max(y1, y2))
  
  if(theme == "grid")
  {
    par(mar = c(3,3,1,1), las = 1, col.main = "black", cex.axis = 0.9, cex.lab = 1, col = "black", cex.main = 1, family = "Calibri")
    plot(x, y1, axes = FALSE, xlab = NA, ylab = NA, type = "l", col = br_colors[1], lwd = 2, ylim = y_limits)
    axis(side = 2, tick = FALSE, col = NA, hadj = 0.5)
    
    # add second line
    lines(x, y2, xlab = NA, ylab = NA, type = "l", col = br_colors[2], lwd = 2)
    grid(lty = 1, col = "lightgrey", ny = NULL, nx = 0)
    if(class(x) == "Date")
    {
      axis.Date(x = x, side = 1, tick = TRUE, col = NA, col.ticks = "grey")
    }else
    {
      axis(side = 1, tick = TRUE, col = NA, col.ticks = "grey")  
    }
    legend(x = min(x), y = max(y1, y2), legend = legend_text, xjust = 0.15, yjust = 1,
           col = c(br_colors[1], br_colors[2]), lty = 1, lwd = 2, bty = "n", cex = 0.9) 
  }else if(theme == "box")
  {
    par(mar = c(3,3,1,1), las = 1, col.main = "black", cex.axis = 0.9, cex.lab = 1, col = "black", cex.main = 1, family = "Calibri")
    plot(x, y1, axes = TRUE, xlab = NA, ylab = NA, type = "l", col = br_colors[1], lwd = 2, ylim = y_limits, xaxt = NULL, yaxt = NULL)
    lines(x, y2, xlab = NA, ylab = NA, type = "l", col = br_colors[2], lwd = 2)
    legend(x = min(x), y = max(y1, y2), legend = legend_text, xjust = 0.15, yjust = 1,
           col = c(br_colors[1], br_colors[2]), lty = 1, lwd = 2, bty = "n", cex = 0.9)
  }
}

brplot_left_2_forecast <- function(x, y1, y1_forecast, legend_text = "", theme = "grid")
{
  y_limits <- c(min(y1, y1_forecast), max(y1, y1_forecast))
  
  if(theme == "grid")
  {
    
    par(mar = c(3,3,1,1), las = 1, col.main = "black", cex.axis = 0.9, cex.lab = 1, col = "black", cex.main = 1, family = "Calibri")
    plot(x, y1, axes = FALSE, xlab = NA, ylab = NA, type = "l", col = br_colors[1], lwd = 2, ylim = y_limits)
    axis(side = 2, tick = FALSE, col = NA, hadj = 0.5)
    
    # add the forecasted line
    lines(x, y1_forecast, xlab = NA, ylab = NA, type = "l", col = br_colors["green"], lwd = 2, lty = 3)
    grid(lty = 1, col = "lightgrey", ny = NULL, nx = 0)
    if(class(x) == "Date")
    {
      axis.Date(x = x, side = 1, tick = TRUE, col = NA, col.ticks = "grey")
    }else
    {
      axis(side = 1, tick = TRUE, col = NA, col.ticks = "grey")  
    }
    legend(x = min(x), y = max(y1, y1_forecast), legend = legend_text, xjust = 0.15, yjust = 1,
           col = c(br_colors[1], br_colors["green"]), lty = c(1, 3), lwd = 2, bty = "n", cex = 0.9) 
  }else if(theme == "box")
  {
    par(mar = c(3,3,1,1), las = 1, col.main = "black", cex.axis = 0.9, cex.lab = 1, col = "black", cex.main = 1, family = "Calibri")
    plot(x, y1, axes = TRUE, xlab = NA, ylab = NA, type = "l", col = br_colors[1], lwd = 2, ylim = y_limits, xaxt = NULL, yaxt = NULL)
    lines(x, y1_forecast, xlab = NA, ylab = NA, type = "l", col = br_colors["green"], lwd = 2, lty = 3)
    legend(x = min(x), y = max(y1, y2), legend = legend_text, xjust = 0.15, yjust = 1,
           col = c(br_colors[1], br_colors["green"]), lty = c(1,3), lwd = 2, bty = "n", cex = 0.9)
  }
}

brplot_error_ts <- function(x, error_ts, legend_text = "", theme = "grid", error_value = 0)
{
  # find the max absolute error that will be used for
  max_abs_error <- max(abs(error_ts), na.rm = TRUE)
  y_limits <- c(-max_abs_error, max_abs_error)
  
  if(theme == "grid")
  {
    par(mar = c(3,3,1,1), las = 1, col.main = "black", cex.axis = 0.9, cex.lab = 1, col = "black", cex.main = 1, family = "Calibri")
    plot(x, error_ts, axes = FALSE, xlab = NA, ylab = NA, type = "l", col = br_colors[1], lwd = 2, ylim = y_limits)
    axis(side = 2, tick = FALSE, col = NA, hadj = 0.5)
    
    # add line at 0, since usually the error term (e.g. of a regression) has a mean of zero
    grid(lty = 1, col = "lightgrey", ny = NULL, nx = 0)
    lines(x, rep(error_value, times = length(x)), xlab = NA, ylab = NA, type = "l", col = br_colors["black"], lwd = 1)
    if(class(x) == "Date")
    {
      axis.Date(x = x, side = 1, tick = TRUE, col = NA, col.ticks = "grey")
    }else
    {
      axis(side = 1, tick = TRUE, col = NA, col.ticks = "grey")  
    }
    legend(x = min(x), y = max_abs_error, legend = legend_text, xjust = 0.15, yjust = 1,
           col = c(br_colors[1]), lty = 1, lwd = 2, bty = "n", cex = 0.9) 
  }else if(theme == "box")
  {
    par(mar = c(3,3,1,1), las = 1, col.main = "black", cex.axis = 0.9, cex.lab = 1, col = "black", cex.main = 1, family = "Calibri")
    plot(x, error_ts, axes = TRUE, xlab = NA, ylab = NA, type = "l", col = br_colors[1], lwd = 2, ylim = y_limits, xaxt = NULL, yaxt = NULL)
    lines(x, rep(error_value, times = length(x)), xlab = NA, ylab = NA, type = "l", col = br_colors["black"], lwd = 1)
    legend(x = min(x), y = max_abs_error, legend = legend_text, xjust = 0.15, yjust = 1,
           col = c(br_colors[1]), lty = 1, lwd = 2, bty = "n", cex = 0.9)
  }
}

brplot_scatterplot <- function(x, y, xlab = "", ylab = "",
                               include_regression_line = TRUE, include_r_squared = TRUE, highlight_last_obs = TRUE,
                               theme = "grid")
{
  y_limits <- c(min(y, na.rm = TRUE), max(y, na.rm = TRUE))
  
  if(theme == "grid")
  {
    par(mar = c(4,4,1,1.5), las = 1, col.main = "black", cex.axis = 0.9, cex.lab = 1, col = "black", cex.main = 1, family = "Calibri")
    plot(x, y, axes = FALSE, xlab = NA, ylab = NA, col = br_colors[1], pch = 19, ylim = y_limits)
    axis(side = 2, tick = FALSE, col = NA, hadj = 0.5)
    
    grid(lty = 1, col = "lightgrey", ny = NULL, nx = 0)
    if(class(x) == "Date")
    {
      axis.Date(x = x, side = 1, tick = TRUE, col = NA, col.ticks = "grey")
    }else
    {
      axis(side = 1, tick = TRUE, col = NA, col.ticks = "grey")  
    }
    title(xlab = xlab, line = 2.2)
    title(ylab = ylab, line = 2.5)
    
    # include regression line
    if(include_regression_line == TRUE)
    {
      fitted_model <- lm(y~x)
      abline(fitted_model, col = br_colors["green"], lty = 3, lwd = 2)
    }
    
    # include R-Squared
    if(include_r_squared == TRUE)
    {
      fitted_model <- lm(y~x)
      r_squared <- round(summary(fitted_model)$adj.r.squared, 2)
      text(x = sort(x, decreasing = TRUE)[2], y = sort(y, decreasing = FALSE)[2], labels = bquote(R^2 == .(r_squared)), cex = 0.7)
    }
    
    # highlight last observation
    if(highlight_last_obs == TRUE)
    {
      points(x = x[length(x)], y = y[length(y)], col = br_colors["green"], pch = 19)
    }
    
  }else if(theme == "box")
  {
    par(mar = c(4,4,1,1.5), las = 1, col.main = "black", cex.axis = 0.9, cex.lab = 1, col = "black", cex.main = 1, family = "Calibri")
    plot(x, y, axes = TRUE, xlab = NA, ylab = NA, col = br_colors[1], pch = 19, ylim = y_limits)
    
    title(xlab = xlab, line = 2.2)
    title(ylab = ylab, line = 2.5)
    
    # include regression line
    if(include_regression_line == TRUE)
    {
      fitted_model <- lm(y~x)
      abline(fitted_model, col = br_colors["green"], lty = 3, lwd = 2)
    }
    
    # include R-Squared
    if(include_r_squared == TRUE)
    {
      fitted_model <- lm(y~x)
      r_squared <- round(summary(fitted_model)$adj.r.squared, 2)
      text(x = sort(x, decreasing = TRUE)[2], y = sort(y, decreasing = FALSE)[2], labels = bquote(R^2 == .(r_squared)), cex = 0.7)
    }
    
    # highlight last observation
    if(highlight_last_obs == TRUE)
    {
      points(x = x[length(x)], y = y[length(y)], col = br_colors["green"], pch = 19)
    }
    
  }
  
  # finally add legend
  if(include_regression_line == TRUE & highlight_last_obs == TRUE)
  {
    legend(x = min(x), y = max(y), legend = c("Fitted Line", "Latest Observation"), xjust = 0, yjust = 1,
           col = c(br_colors["green"], br_colors["green"]), lty = c(3, NULL), pch = c(NA, 19), bty = "n", cex = 0.8, lwd = c(2, NA))
  }else if(include_regression_line == TRUE & highlight_last_obs == FALSE)
  {
    legend(x = min(x), y = max(y), legend = c("Fitted Line"), xjust = 0, yjust = 1,
           col = c(br_colors["green"]), lty = c(3), pch = c(NA), bty = "n", cex = 0.8, lwd=2)
  }else if(include_regression_line == FALSE & highlight_last_obs == TRUE)
  {
    legend(x = min(x), y = max(y), legend = c("Latest Observation"), xjust = 0, yjust = 1,
           col = c(br_colors["green"]), lty = c(NULL), pch = c(19), bty = "n", cex = 0.8)
  }
  
}


brplot_histogram_1 <- function(x, legend_text, breaks = 10,
                               include_density_line = TRUE, overlay_normal = FALSE,
                               theme = "grid")
{
  if(theme == "grid")
  {
    par(mar = c(3,3,1,1), las = 1, col.main = "black", cex.axis = 0.9, cex.lab = 1, col = "black", cex.main = 1, family = "Calibri")
    hist(x, breaks = breaks, main = "", col = br_colors[1], border = br_colors["white"], axes = FALSE, freq = FALSE)
    axis(side = 1, col = NA, tick = TRUE, col.ticks = "grey")
    axis(side = 2, col = NA, tick = FALSE, hadj = 0.5)
    grid(lty = 1, col = "lightgrey", ny = NULL, nx = 0)
    hist(x, breaks = breaks, add = TRUE, main = "", col = br_colors[1], border = br_colors["white"], axes = FALSE, freq = FALSE)
  }else if(theme == "box")
  {
    par(mar = c(3,3,1,1), las = 1, col.main = "black", cex.axis = 0.9, cex.lab = 1, col = "black", cex.main = 1, family = "Calibri")
    hist(x, breaks = breaks, main = "", col = br_colors[1], border = br_colors["white"], axes = FALSE, freq = FALSE)
    box(which = "plot", lty = 1)
    axis(side = 1)
    axis(side = 2, hadj = 1)
  }

  # add density line
  if(include_density_line == TRUE)
  {
    lines(density(x), lty = 3, lwd = 2, col = br_colors["black"])
  }
  # add normal distribution density line
  if(overlay_normal == TRUE)
  {
    curve(dnorm(x, mean = mean(x, na.rm = TRUE), sd = sd(x, na.rm = TRUE)), add = TRUE,
          col = br_colors["yellow"], lty = 3, lwd = 2)
  }
  # add legend here
  legend("topright", legend = c(legend_text, paste(legend_text, " - Density", sep=""), "Normal Distribution"),
         col = c(br_colors[1], br_colors["black"], br_colors["yellow"]),
         lwd = c(5,2,2), lty = c(1,3,3), bty = "n", cex = 0.8, y.intersp = 1.2, inset = 0.07)
}


brplot_left_right <- function(x, y1, y2, y1_limits = c(), y2_limits = c(), legend_text = "", theme = "box")
{
  #theme can be "grid" or "box"
  # if no limits for the axes are given, then  calculate those ourselves
  if(length(y1_limits) != 2) y1_limits <- c(min(y1, na.rm = TRUE), max(y1, na.rm = TRUE))
  if(length(y2_limits) != 2) y2_limits <- c(min(y2, na.rm = TRUE), max(y2, na.rm = TRUE))
 
  # start plotting
  if(theme == "grid")
  {
     # par(mar = c(3,3,1,1), las = 1, col.main = "black", cex.axis = 0.9, cex.lab = 1, col = "black", cex.main = 1, family = "Calibri")
     # plot(x, y1, axes = FALSE, xlab = NA, ylab = NA, type = "l", col = br_colors[1], lwd = 2)
     # grid(lty = 1, col = "lightgrey", ny = NULL, nx = 0)
     # if(class(x) == "Date")
     # {
     #   axis.Date(x = x, side = 1, tick = TRUE, col = NA, col.ticks = "grey")
     # }else
     # {
     #   axis(side = 1, tick = TRUE, col = NA, col.ticks = "grey")  
     # }
     # axis(side = 2, tick = FALSE, col = NA, hadj = 0.5)
    
  }else if(theme == "box")
  {
    par(mar = c(2.5,3,1,3), las = 1, col.main = "black", cex.axis = 0.9, cex.lab = 1, col = "black", cex.main = 1, family = "Calibri")
    plot(x, y1, axes = TRUE, xlab = NA, ylab = NA, type = "l", col = br_colors[1], lwd = 2, xaxt = NULL, yaxt = NULL, ylim = y1_limits)
    
    # add legend before I go to the new plot
    legend(x = "topleft", legend = legend_text,
           col = c(br_colors[1], br_colors[2]), lty = 1, lwd = 2, bty = "n", cex = 0.9)
    
    par(new = T)
    plot(x, y2, axes = FALSE, xlab = NA, ylab = NA, type = "l", col = br_colors[2], lwd = 2, xaxt = NULL, yaxt = NULL, ylim = y2_limits)
    axis(side = 4)
  }
}


brplot_bands <- function(x, ts, upper_bound_ts, lower_bound_ts, legend_text = "",
                         only_future_bands = FALSE, num_of_forecast_points = NULL, shade_forecast_area = FALSE, theme = "box")
{
  y_limits = c(min(lower_bound_ts) - 0.3*sd(lower_bound_ts), max(upper_bound_ts) + 0.3*sd(upper_bound_ts))
  
  # determing the start point for the grey area
  if(!is.null(num_of_forecast_points))
  {
    start_forecast <- length(ts) - num_of_forecast_points
  }
  
  if(only_future_bands == FALSE)
  {
    
    if(theme == "box")
    {
      par(mar = c(2.5,3,1,1), las = 1, col.main = "black", cex.axis = 0.9, cex.lab = 1, col = "black", cex.main = 1, family = "Calibri")
      plot(x, ts, axes = TRUE, xlab = NA, ylab = NA, type = "l", col = br_colors["green"], lwd = 2, lty = 3, xaxt = NULL, yaxt = NULL, ylim = y_limits)
      lines(x, lower_bound_ts, col = br_colors_add_alpha(rgb_color = br_colors[1], alpha = 0.5), lty = 1, lwd = 1)
      lines(x, upper_bound_ts, col = br_colors_add_alpha(rgb_color = br_colors[1], alpha = 0.5), lty = 1, lwd = 1)
      polygon(x = c(x, rev(x)), c(upper_bound_ts, rev(lower_bound_ts)),
              col = br_colors_add_alpha(rgb_color = br_colors[1], alpha = 0.3), border = NA)
    }  
    
  }else if(only_future_bands == TRUE)
  {
    # calculate the starting point for the error bands and fill the upper/lower values with NAs before that
    upper_bound_ts[1:start_forecast-1] <- NA
    lower_bound_ts[1:start_forecast-1] <- NA
    
    upper_bound_ts[start_forecast] <- ts[start_forecast]
    lower_bound_ts[start_forecast] <- ts[start_forecast]
    
    par(mar = c(2.5,3,1,1), las = 1, col.main = "black", cex.axis = 0.9, cex.lab = 1, col = "black", cex.main = 1, family = "Calibri")
    plot(x, ts[c(1:start_forecast, rep(NA, times =num_of_forecast_points))], axes = TRUE, xlab = NA, ylab = NA, type = "l",
         col = br_colors[1], lwd = 2, lty = 1, xaxt = NULL, yaxt = NULL, ylim = y_limits)
    lines(x, y = c(rep(NA, times = length(ts) - num_of_forecast_points - 1), ts[(start_forecast):length(ts)]), col = br_colors["green"], lwd = 2, lty = 3)
    lines(x, lower_bound_ts, col = br_colors_add_alpha(rgb_color = br_colors[1], alpha = 0.5), lty = 1, lwd = 1)
    lines(x, upper_bound_ts, col = br_colors_add_alpha(rgb_color = br_colors[1], alpha = 0.5), lty = 1, lwd = 1)
    polygon(x = c(x, rev(x)), c(upper_bound_ts, rev(lower_bound_ts)),
            col = br_colors_add_alpha(rgb_color = br_colors[1], alpha = 0.3), border = NA)
    
  }
  
  # add grey shaded area
  if(shade_forecast_area == TRUE)
  {
    rect(xleft = x[start_forecast], xright = x[length(x)], ybottom = par("usr")[3], ytop = par("usr")[4],
         col = br_colors_add_alpha(rgb_color = br_colors["grey"], alpha = 0.1), border = NA)
  }
  
  # add legend
  if(only_future_bands == FALSE)
  {
    if(shade_forecast_area == FALSE)
    {
      legend("topleft", legend = c(legend_text), col = br_colors["green"], lty = 3, lwd = 2, bty = "n", inset = 0.02, cex = 0.9)
    }else
    {
      legend("topleft", legend = c(legend_text, "Forecast"), col = c(br_colors["green"], br_colors_add_alpha(br_colors["grey"], alpha = 0.1)),
             lty = c(3, 1), lwd = c(2, 6), bty = "n", inset = 0.02, cex = 0.9)
    }
  }else
  {
    legend("topleft", legend = c(legend_text, "Forecast"), col = c(br_colors[1], br_colors['green']), lty = c(1, 3), lwd = 2, bty = "n", inset = 0.02, cex = 0.9)
  }
}


brplot_simulation_lines <- function(x, time_series, ts_to_highlight, legend_text = c("", ""))
{
  # find the y_limits by checking the y
  max_per_row <- apply(X = time_series, MARGIN = 1, FUN = max, na.rm = TRUE)
  min_per_row <- apply(X = time_series, MARGIN = 1, FUN = min, na.rm = TRUE)
  
  y_max <- max(max_per_row) + 0.2*sd(max_per_row, na.rm = TRUE)
  y_min <- min(min_per_row) - 0.2*sd(min_per_row, na.rm = TRUE)
  
  par(mar = c(2.5,3,1,1), las = 1, col.main = "black", cex.axis = 0.9, cex.lab = 1, col = "black", cex.main = 1, family = "Calibri")
  
  # time_series should be a matrix or a dataframe
  # plot the first column in the time series
  plot(x, time_series[, 1], axes = TRUE, xlab = NA, ylab = NA, type = "l",
       col = br_colors_add_alpha(rgb_color = br_colors["grey"], alpha = 0.4), lwd = 1, lty = 1, xaxt = NULL, yaxt = NULL, ylim = c(y_min, y_max))
  
  # with the loop below add all the rest of the lines
  for(i in 2:ncol(time_series))
  {
    lines(x, time_series[, i], col = br_colors_add_alpha(rgb_color = br_colors["grey"], alpha = 0.4), lwd = 1, lty = 1)
    
  }
  
  # with the loop below highlight the time series we want
  for(item in ts_to_highlight)
  {
    lines(x, time_series[, item], col = br_colors_add_alpha(rgb_color = br_colors[1], alpha = 1), lwd = 2, lty = 1)
  }
  
  # add legend
  legend("topleft", legend = c(legend_text[1], legend_text[2]), col = c(br_colors[1], br_colors_add_alpha(rgb_color = br_colors["grey"], alpha = 0.4)),
         lty = c(1,1), bty = "n", lwd = c(2,1), cex = 0.9, x.intersp = 0.5)
}


brplot_barplot <- function(x, stacked_data, legend_text = rep("test", 10), y1_limits = NULL, main_title = "", x_axis_is_date = FALSE)
{
  par(mar = c(2.5,3,2,1), las = 1, col.main = "black", cex.axis = 0.8, cex.lab = 1, col = "black", cex.main = 1, family = "Calibri")
  
  num_variables <- nrow(stacked_data)
  # Check for blank legend text
  if(legend_text == "")
  {
    legend_text <- rownames(stacked_data)
  }
  # format the x axis labels if they are dates
  if(x_axis_is_date == TRUE)
  {
    x_labels <- as.Date(x)
  }

  barplot(stacked_data, col = br_colors[1:num_variables], space = c(0), border = NA, ylim = y1_limits, axes = FALSE)
  box()
  axis(side = 2)
  axis(side = 1, at = match(x = pretty(x_labels), x_labels), labels = pretty(x_labels))
  legend("topleft", legend = legend_text, fill = br_colors[1:num_variables], bty = "n", cex = 0.8, ncol = 4)
  title(main = main_title, adj = 0)
}









