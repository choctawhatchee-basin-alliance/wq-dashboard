library(plotly)
library(dplyr)

data(cntdat)

# Filter data for Little Red Fish, Station 1
filtered_data <- cntdat %>%
  filter(waterbody == "Little Red Fish", station == 1)

# Create salinity plot
plot_sal <- plot_ly(data = filtered_data,
                    x = ~timestamp,
                    y = ~sal_ppt,
                    type = "scatter",
                    mode = "lines",
                    name = "Salinity",
                    line = list(color = "blue")) %>%
  layout(
    title = "",
    xaxis = list(
      title = "",
      showticklabels = FALSE  # Hide x-axis labels for top plot
    ),
    yaxis = list(title = "Salinity (ppt)"),
    margin = list(l = 60, r = 40, t = 40, b = 10),
    showlegend = FALSE
  )

# Create temperature plot
plot_temp <- plot_ly(data = filtered_data,
                     x = ~timestamp,
                     y = ~temp_f,
                     type = "scatter",
                     mode = "lines",
                     name = "Temperature",
                     line = list(color = "red")) %>%
  layout(
    xaxis = list(title = "Time"),
    yaxis = list(title = "Temperature (°F)"),
    margin = list(l = 60, r = 40, t = 10, b = 40),
    showlegend = FALSE
  )

# Combine plots into a subplot with shared x-axis
combined_plot <- subplot(
    plot_sal, 
    plot_temp, 
    nrows = 2, 
    shareX = TRUE,
    heights = c(0.5, 0.5)
  ) %>%
  layout(
    title = list(
      text = "Little Red Fish (Station 1) - Time Series",
      y = 0.98
    )
  )

# Display the combined plot
combined_plot