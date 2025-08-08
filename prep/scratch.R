library(tidyverse)
library(scales)

load('wq-dashboard/data/raindat.RData')

# Plot 1: Time series of daily precipitation
p1 <- ggplot(raindat, aes(x = date, y = precip_inches, color = station_name)) +
  geom_line(alpha = 0.7, size = 0.5) +
  geom_point(alpha = 0.6, size = 0.8) +
  labs(
    title = "Daily Precipitation Data",
    subtitle = paste("Data from", min(raindat$date), "to", max(raindat$date)),
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
monthly_precip <- raindat %>%
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
p3 <- ggplot(filter(raindat, precip_inches > 0), 
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