#' Generate Hourly Time Series Data with Optional Diel/Sinusoidal Pattern
#'
#' This function creates synthetic data with an optional daily sinusoidal pattern,
#' with customizable peak and trough times and magnitudes.
#'
#' @param start_date Character string or Date object for the start date in "YYYY-MM-DD" format
#' @param end_date Character string or Date object for the end date in "YYYY-MM-DD" format
#' @param base_value Numeric base value around which the pattern varies
#' @param amplitude Numeric amplitude of the variation
#' @param peak_hour Numeric hour of the day (0-23) when the peak occurs (default 16, i.e., 4 PM)
#' @param trough_hour Numeric hour of the day (0-23) when the trough occurs (default 4, i.e., 4 AM)
#' @param noise_sd Standard deviation of random noise to add (default 0)
#' @param sinusoidal Logical, if TRUE uses sinusoidal pattern, if FALSE uses step function (default TRUE)
#' @return A data frame with timestamp and value columns
#'
cntdat_fun <- function(start_date, end_date, 
                                      base_value = 70, 
                                      amplitude = 20, 
                                      peak_hour = 16, 
                                      trough_hour = 4, 
                                      noise_sd = 0) {
  
  # Convert input dates to proper date objects if they're strings
  if (is.character(start_date)) start_date <- as.Date(start_date)
  if (is.character(end_date)) end_date <- as.Date(end_date)
  
  # Create sequence of hourly timestamps
  timestamps <- seq(from = as.POSIXct(paste(start_date, "00:00:00")), 
                    to = as.POSIXct(paste(end_date, "23:00:00")), 
                    by = "hour")
  
  # Calculate hour of day for each timestamp (0-23)
  hours <- hour(timestamps)
  
  # Calculate day number from start for trend
  days <- as.numeric(as.Date(as.Date(timestamps)) - start_date)
  
  # Calculate phase shift to align peak and trough with specified hours
  # Convert peak/trough hours to radians in a 24-hour cycle
  peak_hour_radians <- peak_hour * 2 * pi / 24
  
  # Phase shift calculation (in radians)
  # We subtract pi/2 because sine's natural peak is at pi/2 radians
  phase_shift <- peak_hour_radians - pi/2
  
  # Generate the sinusoidal pattern for each hour
  hour_radians <- hours * 2 * pi / 24
  pattern_component <- sin(hour_radians + phase_shift)
  
  # Calculate the diel pattern value
  values <- base_value + amplitude * pattern_component
  
  # Add random noise if specified
  if (noise_sd > 0) {
    noise <- rnorm(length(values), mean = 0, sd = noise_sd)
    noiseadd <- cumsum(rnorm(length(values), mean = 0, sd = 0.1))
    values <- values + noise + noiseadd
  }
  
  # Create data frame with timestamp and value
  out <- data.frame(timestamp = timestamps, value = values)
  
  return(out)
  
}
