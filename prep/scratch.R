library(tidyverse)
library(rnoaa)
library(lubridate)
library(scales)

stations <- c(
  'USC00086240' = 'Niceville',
  'USW00013884' = 'Crestview',
  'USC00013251' = 'Geneva'
)

# Set date range (adjust as needed - can now span multiple years)
start_date <- "1990-01-01"
end_date <- Sys.Date()

# Function to safely retrieve data for a single station across multiple years
get_station_data <- function(station_id, start_date, end_date, max_retries = 5) {
  tryCatch({
    cat("Retrieving data for station:", station_id, "from", start_date, "to", end_date, "\n")
    
    # Convert dates to Date objects
    start_dt <- as.Date(start_date)
    end_dt <- as.Date(end_date)
    
    # Create date ranges by year to handle API limitations
    years <- seq(year(start_dt), year(end_dt))
    all_station_data <- list()
    
    for (yr in years) {
      # Set year boundaries
      year_start <- max(start_dt, as.Date(paste0(yr, "-01-01")))
      year_end <- min(end_dt, as.Date(paste0(yr, "-12-31")))
      
      cat("  Fetching", yr, "data (", year_start, "to", year_end, ")\n")
      
      # Retry logic for this year
      year_data <- NULL
      retry_count <- 0
      
      while (is.null(year_data) && retry_count < max_retries) {
        if (retry_count > 0) {
          cat("    Retry", retry_count, "for", yr, "\n")
          # Exponential backoff: wait longer between retries
          Sys.sleep(2^retry_count)
        }
        
        retry_count <- retry_count + 1
        
        # Attempt to retrieve data for this year
        tryCatch({
          api_result <- ncdc(datasetid = 'GHCND',
                             stationid = paste0('GHCND:', station_id),
                             datatypeid = 'PRCP',
                             startdate = as.character(year_start),
                             enddate = as.character(year_end),
                             limit = 1000)
          
          if (!is.null(api_result$data) && nrow(api_result$data) > 0) {
            year_data <- api_result$data
            cat("    Retrieved", nrow(year_data), "records\n")
          } else {
            cat("    No data found for", yr, "\n")
            # If no data exists (not an error), break out of retry loop
            break
          }
          
        }, error = function(e) {
          cat("    Error on attempt", retry_count, "for", yr, ":", e$message, "\n")
          if (retry_count >= max_retries) {
            cat("    Maximum retries reached for", yr, ". Skipping this year.\n")
          }
        })
      }
      
      # Store data if we got it
      if (!is.null(year_data)) {
        all_station_data[[as.character(yr)]] <- year_data
      }
      
      # Add delay between years (even on success)
      Sys.sleep(0.5)
    }
    
    # Combine all years of data
    if (length(all_station_data) > 0) {
      combined_data <- bind_rows(all_station_data)
      cat("Total records for", station_id, ":", nrow(combined_data), "\n")
      cat("Successfully retrieved data for years:", paste(names(all_station_data), collapse = ", "), "\n\n")
      return(combined_data)
    } else {
      cat("No data found for station:", station_id, "\n\n")
      return(NULL)
    }
    
  }, error = function(e) {
    cat("Error retrieving data for station", station_id, ":", e$message, "\n\n")
    return(NULL)
  })
}
# Retrieve data for all stations
raindat <- map_dfr(names(stations), ~get_station_data(.x, start_date, end_date))

save(raindat, file = 'wq-dashboard/data/raindat.RData')

load('wq-dashboard/data/raindat.RData')

# Process the data
precip_data <- raindat %>%
  mutate(
    date = as.Date(date),
    # Convert precipitation from tenths of mm to inches
    precip_inches = value / 254,
    station = gsub('^GHCND:', '', station),
    station_name = factor(station, levels = names(stations), labels = stations),
    month = month(date, label = TRUE),
    year = year(date)
  ) %>%
  filter(!is.na(precip_inches)) %>%
  select(station, station_name, date, precip_inches, month, year)

# Plot 1: Time series of daily precipitation
p1 <- ggplot(precip_data, aes(x = date, y = precip_inches, color = station_name)) +
  geom_line(alpha = 0.7, size = 0.5) +
  geom_point(alpha = 0.6, size = 0.8) +
  labs(
    title = "Daily Precipitation Data",
    subtitle = paste("Data from", min(precip_data$date), "to", max(precip_data$date)),
    x = "Date",
    y = "Precipitation (inches)",
    color = "Station"
  ) +
  facet_wrap(~ station_name, ncol = 1) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    plot.title = element_text(size = 14, face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1)
  ) +
  scale_x_date(date_labels = "%b %Y", date_breaks = "1 year") +
  scale_y_continuous(labels = number_format(accuracy = 0.1))

print(p1)

# Plot 2: Monthly precipitation totals
monthly_precip <- precip_data %>%
  group_by(station_name, year, month) %>%
  summarise(
    monthly_total = sum(precip_inches, na.rm = TRUE),
    date = floor_date(first(date), "month"),
    .groups = 'drop'
  )

p2 <- ggplot(monthly_precip, aes(x = date, y = monthly_total, fill = station_name)) +
  geom_col(position = "dodge", alpha = 0.8) +
  labs(
    title = "Monthly Precipitation Totals",
    x = "Month",
    y = "Total Precipitation (inches)",
    fill = "Station"
  ) +
  facet_wrap(~station_name, ncol = 1) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1), 
    legend.position = 'none',
  ) +
  scale_x_date(date_labels = "%b %Y", date_breaks = "1 year") +
  scale_y_continuous(labels = number_format(accuracy = 0.1))

print(p2)

# Plot 3: Precipitation distribution by station
p3 <- ggplot(filter(precip_data, precip_inches > 0), 
             aes(x = station_name, y = precip_inches, fill = station_name)) +
  geom_boxplot(alpha = 0.7) +
  geom_jitter(alpha = 0.3, width = 0.2, size = 0.5) +
  labs(
    title = "Precipitation Distribution by Station",
    subtitle = "Only days with measurable precipitation shown",
    x = "Station",
    y = "Precipitation (inches)",
    fill = "Station"
  ) +
  theme_minimal() +
  theme(
    legend.position = "none",
    plot.title = element_text(size = 14, face = "bold")
  ) +
  scale_y_log10(labels = number_format(accuracy = 0.01))

print(p3)