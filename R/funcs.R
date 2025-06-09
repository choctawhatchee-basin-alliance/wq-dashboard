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

#' Create a base map for leafletproxy
#'
#' @param bnds input layer for bounding box
#'
#' @return An empty leaflet object
bsmap <- function(bnds){
  
  bnds <- sf::st_bbox(bnds)
  
  esri <- rev(grep("^Esri", leaflet::providers, value = TRUE))
  
  m <- leaflet::leaflet() %>%
    leaflet::fitBounds(bnds[['xmin']], bnds[['ymin']], bnds[['xmax']], bnds[['ymax']])
  
  for (provider in esri) {
    m <- m %>% leaflet::addProviderTiles(provider, group = provider)
  }
  
  out <- m %>%
    leaflet::addLayersControl(baseGroups = names(esri),
                              options = leaflet::layersControlOptions(collapsed = T),
                              position = 'topleft')
  
  return(out)
  
}

#' Function to update the map with summarized data by area
#' 
#' @param mapin Leaflet map object to update
#' @param alldat Data frame containing the data to summarize
#' @param cbawbid sf object containing wbids
#' @param stas sf object containing station geometries
#' @param summarize1 Character string indicating how to summarize the data ('WBID' or 'Station')
#' @param summstat1 Character string indicating the summary statistic to calculate (e.g., 'Mean', 'Median', 'Max', 'Min')
#' @param location1 Character string indicating the sample location (e.g., 'surface', 'bottom')
#' @param parameter1 Character string indicating the parameter to filter by
#' @param daterange1 Date range to filter the data
byareamap_fun <- function(mapin, alldat, cbawbid, stas, summarize1, summstat1, location1, parameter1, daterange1){
  
  # summarize data
  byareadat <- try(
    byareadat_fun(alldat, cbawbid, stas, summarize1, summstat1, location1, parameter1, daterange1),
    silent = T
  )
  
  # create map
  if(inherits(byareadat, 'try-error'))
    out <- mapin %>%
      leaflet::clearMarkers() |> 
      leaflet::clearShapes() |> 
      leaflet::clearControls()
  
  if(!inherits(byareadat, 'try-error')) {
    
    pal <- leaflet::colorNumeric(
      palette = "YlGnBu",
      domain = byareadat$val,
      na.color = "transparent"
    )
    
    lab <- meta |> 
      dplyr::filter(parameter == parameter1) |> 
      dplyr::pull(label) |> 
      unique()
    
    out <- mapin %>%
      leaflet::clearMarkers() |> 
      leaflet::clearShapes() |> 
      leaflet::clearControls()

    if(summarize1 == 'WBID')
      out <- out |> 
        leaflet::addPolygons(
          data = byareadat,
          fillColor = ~pal(val),
          fillOpacity = 0.7,
          color = "#666",
          weight = 1,
          highlightOptions = leaflet::highlightOptions(
            weight = 3,
            color = "#666",
            fillOpacity = 0.7,
            bringToFront = TRUE
          ),
          label = ~paste0(WBID, ", Value: ", round(val, 2)),
          labelOptions = leaflet::labelOptions(
            style = list("font-size" = "16px")
          ), 
          layerId = ~WBID
        )
    
    if(summarize1 == 'Station')
      out <- out |> 
        leaflet::addCircleMarkers(
          data = byareadat,
          radius = 7,
          fillColor = ~pal(val),
          fillOpacity = 0.7,
          color = "black",
          weight = 1,
          label = ~paste0(waterbody, " ", station, ", Value: ", round(val, 2)),
          labelOptions = leaflet::labelOptions(
            style = list("font-size" = "16px")
          ), 
          layerId = ~paste0(waterbody, "_", station)
        )
    
    # add legend
    out <- out |>
      leaflet::addLegend(
        pal = pal,
        values = byareadat$val,
        title = paste(lab, "<br>", summstat1),  # Dynamic title based on inputs
        position = "topright",
        opacity = 0.8,
        na.label = "No Data"
      )
    
  }
  
  return(out)
  
  
}

#' Function to summarize data by waterbody or station
#' 
#' @param alldat Data frame containing the data to summarize
#' @param cbawbid sf object containing wbids
#' @param stas sf object containing station geometries
#' @param summarize1 Character string indicating how to summarize the data ('WBID' or 'Station')
#' @param summstat1 Character string indicating the summary statistic to calculate (e.g., 'Mean', 'Median', 'Max', 'Min')
#' @param location1 Character string indicating the sample location (e.g., 'surface', 'bottom')
#' @param parameter1 Character string indicating the parameter to filter by
#' @param daterange1 Date range to filter the data
byareadat_fun <- function(alldat, cbawbid, stas, summarize1, summstat1, location1, parameter1, daterange1){

  dat <- alldat |> 
    dplyr::filter(
      parameter == parameter1 &
      date >= as.Date(daterange1[1]) & 
      date <= as.Date(daterange1[2]) & 
      location == location1
    ) |> 
    dplyr::select(waterbody, station, date, parameter, val) |> 
    dplyr::filter(!is.na(val))

  dat <- dplyr::left_join(dat, stas, by = c('waterbody', 'station')) |> 
    sf::st_as_sf()
  
  if (summarize1 == "WBID") {

    out <- dat |>  
      sf::st_drop_geometry() |> 
      dplyr::summarise(
        val = match.fun(tolower(summstat1))(val, na.rm = TRUE),
        .by = c(WBID)
      ) |> 
      dplyr::inner_join(cbawbid, by = 'WBID') |> 
      sf::st_as_sf() |> 
      dplyr::filter(!is.na(val))
    
  }
  
  if(summarize1 == 'Station'){
    
    out <- dat |> 
      dplyr::summarise(
        val = match.fun(tolower(summstat1))(val, na.rm = TRUE),
        .by = c(waterbody, station, geometry)
      ) |> 
      dplyr::filter(!is.na(val))
    
  }
  
  return(out)
  
}

byareaplo_fun <- function(shape_click, marker_click, alldat, stas, summarize1, summstat1, location1, parameter1, daterange1){
  
  toplo <- alldat |> 
    dplyr::filter(
      parameter == parameter1 &
        date >= as.Date(daterange1[1]) & 
        date <= as.Date(daterange1[2]) & 
        location == location1
    )
  
  if(!is.null(shape_click)){
    
    id <- shape_click$id
    ttl <- paste('WBID', id)
    
    toplo <- toplo |> 
      dplyr::inner_join(stas, by = c('waterbody', 'station')) |> 
      dplyr::filter(WBID %in% id) |> 
      dplyr::select(date, val) |> 
      dplyr::summarise(
        val = match.fun(tolower(summstat1))(val, na.rm = TRUE),
        .by = date
      )
    
  }
  
  if(!is.null(marker_click)){
 
    id <- marker_click$id
    waterbody <- sub("_(.*)", "", id)
    station <- sub(".*_(.*)", "\\1", id)
    ttl <- paste(waterbody, station)
    
    toplo <- toplo |> 
      dplyr::filter(waterbody == !!waterbody & station == !!station) |> 
      dplyr::select(date, val) |> 
      dplyr::summarise(
        val = match.fun(tolower(summstat1))(val, na.rm = TRUE),
        .by = date
      )
      
  }

  ylab <- meta |> 
    dplyr::filter(parameter == parameter1) |> 
    dplyr::pull(label) |> 
    unique()
  
  out <- plotly::plot_ly(
      data = toplo,
      x = ~date,
      y = ~val,
      type = 'scatter',
      mode = 'lines+markers',
      line = list(color = 'blue'),
      marker = list(size = 5),
      hoverinfo = 'text',
      text = ~paste("Date:", date, "<br>Value:", round(val, 2))
    ) |> 
    plotly::layout(
      title = ttl,
      xaxis = list(title = "Date", 
                   range = c(as.Date(daterange1[1]), as.Date(daterange1[2]))
                   ),
      yaxis = list(title = paste(summstat1, ylab))
    )
  
  return(out)
  
}
