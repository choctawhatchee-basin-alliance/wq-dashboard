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
#' @param stas sf object containing station geometries
#' @param summarize1 Character string indicating how to summarize the data ('WBID' or 'Station')
#' @param location1 Character string indicating the sample location (e.g., 'surface', 'bottom')
#' @param parameter1 Character string indicating the parameter to filter by
#' @param daterange1 Date range to filter the data
byareamap_fun <- function(mapin, alldat, stas, summarize1, location1, parameter1, daterange1){
  
  # summarize data
  byareadat <- try(
    byareadat_fun(alldat, stas, summarize1, location1, parameter1, daterange1),
    silent = T
  )

  # create map
  if(inherits(byareadat, 'try-error'))
    out <- mapin %>%
      leaflet::clearShapes() |> 
      leaflet::clearControls()
  
  if(!inherits(byareadat, 'try-error')) {
    
    pal <- leaflet::colorNumeric(
      palette = "YlGnBu",
      domain = byareadat$val,
      na.color = "transparent"
    )
    
    lab <- ylab_fun(parameter1, location1)
    
    out <- mapin %>%
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
            bringToFront = FALSE
          ),
          label = ~paste0(WBID, " (", stas, "), Value: ", round(val, 2)),
          labelOptions = leaflet::labelOptions(
            style = list("font-size" = "16px")
          ), 
          layerId = ~WBID
        )
    
    if(summarize1 == 'HUC12')
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
            bringToFront = FALSE
          ),
          label = ~paste0(huc12, " (", stas, "), Value: ", round(val, 2)),
          labelOptions = leaflet::labelOptions(
            style = list("font-size" = "16px")
          ), 
          layerId = ~huc12
        )
    
    # add legend
    out <- out |>
      leaflet::addLegend(
        pal = pal,
        values = byareadat$val,
        title = lab,
        position = "topright",
        opacity = 0.8,
        na.label = "No Data"
      )
    
  }
  
  return(out)
  
}

#' Function to summarize data by WBID or HUC12
#' 
#' @param alldat Data frame containing the data to summarize
#' @param stas sf object containing station geometries
#' @param summarize1 Character string indicating how to summarize the data ('WBID' or 'HUC12')
#' @param location1 Character string indicating the sample location (e.g., 'surface', 'bottom')
#' @param parameter1 Character string indicating the parameter to filter by
#' @param daterange1 Date range to filter the data
byareadat_fun <- function(alldat, stas, summarize1, location1, parameter1, daterange1){

  dat <- alldat |> 
    dplyr::filter(
      parameter == parameter1 &
      date >= as.Date(daterange1[1]) & 
      date <= as.Date(daterange1[2]) & 
      location == location1
    ) |> 
    dplyr::select(waterbody, station, date, parameter, val) |> 
    dplyr::filter(!is.na(val))
  dat <- dplyr::inner_join(stas, dat, by = c("waterbody", "station")) |> 
    dplyr::select(waterbody, station, date, parameter, val, WBID, huc12) |>  
    sf::st_set_geometry(NULL)
    
  if (summarize1 == "WBID") {

    out <- dat |> 
      tidyr::unite('stas', waterbody, station) |> 
      dplyr::summarise(
        val = mean(val, na.rm = TRUE),
        stas = length(unique(stas)),
        .by = c(WBID)
      ) |> 
      dplyr::mutate(
        stas = dplyr::case_when(
          stas == 1 ~ paste0(stas, " station"),
          stas > 1 ~ paste0(stas, " stations"),
          TRUE ~ "no stations"
        )
      ) |> 
      dplyr::inner_join(cbawbid, by = 'WBID') |> 
      sf::st_as_sf() |> 
      dplyr::filter(!is.na(val))
    
  }
  
  if (summarize1 == "HUC12") {
    
    out <- dat |>  
      sf::st_drop_geometry() |> 
      tidyr::unite('stas', waterbody, station) |> 
      dplyr::summarise(
        val = mean(val, na.rm = TRUE),
        stas = length(unique(stas)),
        .by = c(huc12)
      ) |> 
      dplyr::mutate(
        stas = dplyr::case_when(
          stas == 1 ~ paste0(stas, " station"),
          stas > 1 ~ paste0(stas, " stations"),
          TRUE ~ "no stations"
        )
      ) |> 
      dplyr::inner_join(cbahuc, by = 'huc12') |> 
      sf::st_as_sf() |> 
      dplyr::filter(!is.na(val))
    
  }
  
  return(out)
  
}

#' Function to create a time series plot for a selected area or station
#' 
#' @param sel Shape click event data from leaflet
#' @param alldat Data frame containing the water quality data to plo
#' @param stas sf object containing station geometries
#' @param nncdat Data frame containing the NNC data
#' @param location1 Character string indicating the sample location (e.g., 'surface', 'bottom')
#' @param parameter1 Character string indicating the parameter to filter by
#' @param daterange1 Date range to filter the data
byareaplo_fun <- function(sel, alldat, stas, nncdat, location1, parameter1, daterange1){
  
  toplo <- alldat |> 
    dplyr::filter(
      parameter == parameter1 &
        date >= as.Date(daterange1[1]) & 
        date <= as.Date(daterange1[2]) & 
        location == location1
    ) |> 
    dplyr::arrange(date)
  
  ylab <- ylab_fun(parameter1, location1)
  
  id <- sel$data$id
  
  # get nnc line if present
  chknnc <- nncdat |> 
    dplyr::filter(WBID %in% id & parameter %in% substr(parameter1, 1, 3)) |> 
    dplyr::select(WBID, parameter, value) |> 
    dplyr::distinct()
  
  if(id %in% stas$WBID){
    
    ttl <- paste('WBID', id)
    
    toplo <- toplo |> 
      dplyr::inner_join(stas, by = c('waterbody', 'station')) |> 
      dplyr::filter(WBID %in% id) |> 
      dplyr::select(date, val) |> 
      dplyr::summarise(
        avev = mean(val, na.rm = TRUE),
        .by = date
      )
    
  }
  
  if(id %in% cbahuc$huc12){
    
    ttl <- paste('HUC12', id)
    
    toplo <- toplo |> 
      dplyr::inner_join(stas, by = c('waterbody', 'station')) |> 
      dplyr::filter(huc12 %in% id) |> 
      dplyr::select(date, val) |> 
      dplyr::summarise(
        avev = mean(val, na.rm = TRUE),
        .by = date
      )
    
  }
  # Convert dates to milliseconds for Highcharts
  date_range_ms <- c(as.numeric(as.POSIXct(daterange1[1])) * 1000,
                     as.numeric(as.POSIXct(daterange1[2])) * 1000)
  
  # Create chart
  hc <- highcharter::highchart() |>
    highcharter::hc_chart(type = "line") |>
    highcharter::hc_title(text = ttl) |>
    highcharter::hc_xAxis(
      type = "datetime",
      min = date_range_ms[1],
      max = date_range_ms[2],
      title = list(text = "")
    ) |>
    highcharter::hc_yAxis(title = list(text = ylab)) |>
    highcharter::hc_legend(enabled = FALSE)
  
  # Add data series
  if(nrow(toplo) > 0) {
    hc <- hc |>
      highcharter::hc_add_series(
        data = toplo,
        type = "line",
        highcharter::hcaes(x = date, y = avev),
        color = "blue",
        marker = list(radius = 3, fillColor = "lightblue"),
        tooltip = list(
          pointFormatter = highcharter::JS("function() { return 'Value: ' + this.y.toFixed(2); }")
        )
      )
  }
  
  # Add NNC threshold line if present
  if(nrow(chknnc) == 1) {
    hc <- hc |>
      highcharter::hc_add_series(
        data = list(
          list(x = date_range_ms[1], y = chknnc$value),
          list(x = date_range_ms[2], y = chknnc$value)
        ),
        type = "line",
        color = "red",
        dashStyle = "Dash",
        showInLegend = FALSE,
        tooltip = list(
          headerFormat = '',
          pointFormatter = highcharter::JS("function() { return 'NNC Threshold: ' + this.y; }")
        )
      )
  }
  
  # # Set chart height and disable reflow (consistent with your first function)
  # hc <- hc |> 
  #   highcharter::hc_chart(height = 275) |> 
  #   highcharter::hc_chart(reflow = FALSE)
  # 
  # # Return as HTML div (consistent with your first function format)
  # out <- htmltools::div(
  #   style = "height: 275px; overflow: hidden;",
  #   hc
  # )
  # 
  out <- hc
  
  return(out)
  
  
}

#' Function to update the map with summarized data by area
#'
#' @param mapin Leaflet map object to update 
#' @param stas sf object containing station geometries
#' @param bystationdat Data frame containing the data to summarize and map
#' @param parameter Character string indicating the parameter to filter by
#' @param daterange2 Date range to filter the data
bystationmap_fun <- function(mapin, bystationdat, stas, parameter, daterange2){
  
  prm <- gsub("(^.*)\\_.*$", "\\1", parameter)
  loc <- gsub(".*\\_(.*)$", "\\1", parameter)
  
  tomap <- bystationdat |> 
    dplyr::filter(parameter == !!prm & location == !!loc) |> 
    dplyr::filter(
      date >= as.Date(daterange2[1]) & 
      date <= as.Date(daterange2[2])
    ) |>
    dplyr::summarise(
      val = mean(val, na.rm = T), 
      .by = c(waterbody, station, parameter, location)
    ) 
  
  out <- mapin

  pal <- leaflet::colorNumeric(
    palette = "YlGnBu",
    domain = tomap$val,
    na.color = "transparent"
  )
  
  lab <- ylab_fun(prm, loc)
  
  # create map
  if(nrow(tomap) == 0){
    
    out <- out %>%
      leaflet::clearMarkers() |> 
      leaflet::clearControls()
  
  }
  
  if(nrow(tomap) != 0) {

    tomap <- dplyr::inner_join(stas, tomap, by = c('waterbody', 'station'))
      
    out <- out |> 
      leaflet::clearMarkers() |> 
      leaflet::clearControls() |> 
      leaflet::addCircleMarkers(
        data = tomap,
        radius = 7,
        fillColor = ~pal(val),
        fillOpacity = 0.7,
        color = "#666",
        weight = 1,
        label = ~paste0(waterbody, " ", station, ", Mean Value: ", round(val, 2)),
        labelOptions = leaflet::labelOptions(
          style = list("font-size" = "16px")
        ),
        layerId = ~paste0(waterbody, "_", station)
      )
    
  }
  
  # add legend
  out <- out |>
    leaflet::addLegend(
      pal = pal,
      values = tomap$val,
      title = lab,
      position = "topright",
      opacity = 0.8,
      na.label = "No Data"
    )
  
  return(out)
  
}

#' Function to filter data by selected parameters and date range
#' 
#' @param alldat Data frame containing the water quality data to filter
#' @param parameter2a Character string indicating the first parameter to filter by
#' @param parameter2b Character string indicating the second parameter to filter by
bystationdat_fun <- function(alldat, parameter2a, parameter2b){
  
  prm2a <- gsub("(^.*)\\_.*$", "\\1", parameter2a)
  prm2b <- gsub("(^.*)\\_.*$", "\\1", parameter2b)
  loca <- gsub(".*\\_(.*)$", "\\1", parameter2a)
  locb <- gsub(".*\\_(.*)$", "\\1", parameter2b)
  
  out <- alldat |> 
    dplyr::filter((parameter == prm2a & location == loca) | 
                  (parameter == prm2b & location == locb)
                  ) |>
    dplyr::filter(!is.na(val))
    
  return(out)
  
}

#' Function to create a time series plot for a selected station
#' 
#' @param sel Selected station data from the input
#' @param bystationdat Data frame containing the water quality data to plot
#' @param nncdat Data frame containing the NNC data
#' @param summarize2 Character string indicating how to summarize the data ('day', 'week', 'month', 'year')
#' @param parameter2a Character string indicating the first parameter to filter by
#' @param parameter2b Character string indicating the second parameter to filter by
#' @param daterange2 Date range to filter the data
bystationplo_fun <- function(sel, bystationdat, nncdat, summarize2, parameter2a,
                             parameter2b, daterange2){
  
  waterbody <- gsub("(^.*)\\_.*$", "\\1", sel$id)
  station <- gsub(".*\\_(.*)$", "\\1", sel$id)
  prm2a <- gsub("(^.*)\\_.*$", "\\1", parameter2a)
  prm2b <- gsub("(^.*)\\_.*$", "\\1", parameter2b)
  loca <- gsub(".*\\_(.*)$", "\\1", parameter2a)
  locb <- gsub(".*\\_(.*)$", "\\1", parameter2b)
  
  if(summarize2 != 'day'){
    ylab1 <- ylab_fun(prm2a, loca)
    ylab2 <- ylab_fun(prm2b, locb)
  }
  if(summarize2 == 'day'){
    ylab1 <- ylab_fun(prm2a, loca, addmean = F)
    ylab2 <- ylab_fun(prm2b, locb, addmean = F)
  }
  
  toplo <- bystationdat |> 
    dplyr::filter(
      date >= as.Date(daterange2[1]) & 
        date <= as.Date(daterange2[2])
    ) |>
    dplyr::filter(waterbody == !!waterbody & station == !!station) |> 
    dplyr::rename(avev = val) |> 
    dplyr::mutate(
      date = lubridate::floor_date(date, summarize2),
    ) |> 
    dplyr::summarise(
      hivl = tryCatch(t.test(avev, conf.level = 0.95)$conf.int[2], silent = TRUE, error = function(e) NA),
      lovl = tryCatch(t.test(avev, conf.level = 0.95)$conf.int[1], silent = TRUE, error = function(e) NA),
      avev = mean(avev, na.rm = TRUE),
      .by = c(date, parameter, location)
    )
  
  toplo1 <- toplo |> 
    dplyr::filter(parameter == prm2a & location == loca) |> 
    dplyr::arrange(date)
  toplo2 <- toplo |>
    dplyr::filter(parameter == prm2b & location == locb) |> 
    dplyr::arrange(date)
  
  # Check for NNC thresholds
  nncchk1 <- nncdat |> 
    dplyr::filter(
      waterbody %in% !!waterbody & 
        station %in% !!station & 
        parameter %in% substr(prm2a, 1, 3)
    ) |> 
    dplyr::select(parameter, value) |> 
    dplyr::distinct()
  
  nncchk2 <- nncdat |> 
    dplyr::filter(
      waterbody %in% !!waterbody & 
        station %in% !!station & 
        parameter %in% substr(prm2b, 1, 3)
    ) |> 
    dplyr::select(parameter, value) |> 
    dplyr::distinct()
  
  # Convert dates to milliseconds for Highcharts
  date_range_ms <- c(as.numeric(as.POSIXct(daterange2[1])) * 1000,
                     as.numeric(as.POSIXct(daterange2[2])) * 1000)

  # Create first chart
  hc1 <- highcharter::highchart() |>
    highcharter::hc_chart(type = "line") |>
    highcharter::hc_title(text = paste(waterbody, station)) |>
    highcharter::hc_xAxis(
      type = "datetime",
      min = date_range_ms[1],
      max = date_range_ms[2],
      title = list(text = "")
    ) |>
    highcharter::hc_yAxis(title = list(text = ylab1)) |>
    highcharter::hc_legend(enabled = FALSE)
  
  # Add data series for first chart
  if(nrow(toplo1) > 0) {
    if(summarize2 != 'day') {
      # Add series with error bars
      hc1 <- hc1 |>
        highcharter::hc_add_series(
          data = toplo1,
          type = "line",
          highcharter::hcaes(x = date, y = avev),
          color = "blue",
          marker = list(radius = 3, fillColor = "lightblue"),
          tooltip = list(
            pointFormatter = highcharter::JS("function() { return 'Value: ' + this.y.toFixed(2); }")
          )
        ) |>
        highcharter::hc_add_series(
          data = toplo1,
          type = "errorbar",
          highcharter::hcaes(x = date, low = lovl, high = hivl),
          color = "blue",
          showInLegend = FALSE,
          tooltip = list(enabled = FALSE)
        )
    } else {
      # Add series without error bars
      hc1 <- hc1 |>
        highcharter::hc_add_series(
          data = toplo1,
          type = "line",
          highcharter::hcaes(x = date, y = avev),
          color = "blue",
          marker = list(radius = 3, fillColor = "lightblue"),
          tooltip = list(
            pointFormatter = highcharter::JS("function() { return 'Value: ' + this.y.toFixed(2); }")
          )
        )
    }
  }
  
  # Add NNC threshold line for first chart
  if(nrow(nncchk1) == 1) {
    hc1 <- hc1 |>
      highcharter::hc_add_series(
        data = list(
          list(x = date_range_ms[1], y = nncchk1$value),
          list(x = date_range_ms[2], y = nncchk1$value)
        ),
        type = "line",
        color = "red",
        dashStyle = "Dash",
        showInLegend = FALSE,
        tooltip = list(
          headerFormat = '',
          pointFormatter = highcharter::JS("function() { return 'NNC Threshold: ' + this.y; }")
        )
      )
  }
  
  # Create second chart
  hc2 <- highcharter::highchart() |>
    highcharter::hc_chart(type = "line") |>
    highcharter::hc_xAxis(
      type = "datetime",
      min = date_range_ms[1],
      max = date_range_ms[2],
      title = list(text = "")
    ) |>
    highcharter::hc_yAxis(title = list(text = ylab2)) |>
    highcharter::hc_legend(enabled = FALSE)
  
  # Add data series for second chart
  if(nrow(toplo2) > 0) {
    if(summarize2 != 'day') {
      # Add series with error bars
      hc2 <- hc2 |>
        highcharter::hc_add_series(
          data = toplo2,
          type = "line",
          highcharter::hcaes(x = date, y = avev),
          color = "darkgreen",
          marker = list(radius = 3, fillColor = "lightgreen"),
          tooltip = list(
            pointFormatter = highcharter::JS("function() { return 'Value: ' + this.y.toFixed(2); }")
          )
        ) |>
        highcharter::hc_add_series(
          data = toplo2,
          type = "errorbar",
          highcharter::hcaes(x = date, low = lovl, high = hivl),
          color = "darkgreen",
          showInLegend = FALSE,
          tooltip = list(enabled = FALSE)
        )
    } else {
      # Add series without error bars
      hc2 <- hc2 |>
        highcharter::hc_add_series(
          data = toplo2,
          type = "line",
          highcharter::hcaes(x = date, y = avev),
          color = "darkgreen",
          marker = list(radius = 3, fillColor = "lightgreen"),
          tooltip = list(
            pointFormatter = highcharter::JS("function() { return 'Value: ' + this.y.toFixed(2); }")
          )
        )
    }
  }
  
  # Add NNC threshold line for second chart
  if(nrow(nncchk2) == 1) {
    hc2 <- hc2 |>
      highcharter::hc_add_series(
        data = list(
          list(x = date_range_ms[1], y = nncchk2$value),
          list(x = date_range_ms[2], y = nncchk2$value)
        ),
        type = "line",
        color = "red",
        dashStyle = "Dash",
        showInLegend = FALSE,
        tooltip = list(
          headerFormat = '',
          pointFormatter = highcharter::JS("function() { return 'NNC Threshold: ' + this.y; }")
        )
      )
  }
  
  # Create combined chart using htmltools
  hc1 <- hc1 |> highcharter::hc_chart(height = 275) |> highcharter::hc_chart(reflow = FALSE) 
  hc2 <- hc2 |> highcharter::hc_chart(height = 275) |> highcharter::hc_chart(reflow = FALSE) 
  
  out <- htmltools::div(
    style = "height: 550px; overflow: hidden;",
    htmltools::div(
      style = "height: 275px; margin-bottom: 10px; overflow: hidden;",
      hc1
    ),
    htmltools::div(
      style = "height: 275px; overflow: hidden;",
      hc2
    )
  )
  
  return(out)
  
}

#' Add marker or shape highlight to area map selection
#'
#' @param mapsel1 Data frame containing the selected area or station's ID
addselareamap_fun <- function(mapsel1){

  if (!is.null(mapsel1)) {

    # wbid
    if(mapsel1$id %in% cbawbid$WBID)
      leaflet::leafletProxy("byareamap") |>
        leaflet::clearGroup("highlight") |>
        leaflet::addPolygons(
          data = cbawbid |> dplyr::filter(WBID == mapsel1$id), opacity = 1,
          group = "highlight", color = "black", weight = 6, fillOpacity = 0,
          options = leaflet::pathOptions(clickable = FALSE)
        )

    # huc12      
    if(mapsel1$id %in% cbahuc$huc12)
      leaflet::leafletProxy("byareamap") |>
        leaflet::clearGroup("highlight") |>
        leaflet::addPolygons(
          data = cbahuc |> dplyr::filter(huc12 == mapsel1$id), opacity = 1,
          group = "highlight", color = "black", weight = 6, fillOpacity = 0,
          options = leaflet::pathOptions(clickable = FALSE)
        )
    
    # station
    if(!mapsel1$id %in% cbawbid$WBID & !mapsel1$id %in% cbahuc$huc12)
      leaflet::leafletProxy("byareamap") |>
        leaflet::clearGroup("highlight") |>
        leaflet::addCircleMarkers(
          lng = mapsel1$lng, lat = mapsel1$lat,
          group = "highlight", radius = 8, color = "black",
          fillColor = '#007BC2', fillOpacity = 0, opacity = 1, weight = 6,
          options = leaflet::pathOptions(clickable = FALSE)
        )
      
    
  }
  
}

#' Add marker highlight to station map selection
#'
#' @param mapsel2 Data frame containing the selected station's longitude and latitude
addselstationmap_fun <- function(mapsel2){

  if (!is.null(mapsel2)) {
    
    leaflet::leafletProxy("bystationmap1") |>
      leaflet::clearGroup("highlight") |>
      leaflet::addCircleMarkers(
        lng = mapsel2$lng, lat = mapsel2$lat,
        group = "highlight", radius = 8, color = "black",
        fillColor = '#007BC2', fillOpacity = 0, opacity = 1, weight = 6,
        options = leaflet::pathOptions(clickable = FALSE)
      )
    
    leaflet::leafletProxy("bystationmap2") |>
      leaflet::clearGroup("highlight") |>
      leaflet::addCircleMarkers(
        lng = mapsel2$lng, lat = mapsel2$lat,
        group = "highlight", radius = 8, color = "black",
        fillColor = '#007BC2', fillOpacity = 0, opacity = 1, weight = 6,
        options = leaflet::pathOptions(clickable = FALSE)
      )
  }
  
}

#' Function to get the list of parameters for given dates
#' 
#' @param mapsel2 Data frame containing the selected station's ID
stationprmsel_fun <- function(daterange2){

  out <- alldat |> 
    dplyr::filter(date >= daterange2[1] & date <= daterange2[2]) |>
    dplyr::select(parameter, location) |> 
    dplyr::distinct() |> 
    dplyr::left_join(prmsdf, by = "parameter", relationship = 'many-to-many') |>
    dplyr::mutate(
      cnt = dplyr::n(), 
      .by = parameter
    ) |> 
    dplyr::mutate(
      location2 = dplyr::case_when(
        location == 'surf' & cnt == 2 ~ 'Surface',
        location == 'bott' ~ 'Bottom',
        location == 'surf' & cnt == 1 ~ '',
        TRUE ~ location
      )
    ) |>
    tidyr::unite(labelnouni, c(labelnouni, location2), sep = ": ") |>
    tidyr::unite(parameter, c(parameter, location), sep = "_") |>
    dplyr::mutate(
      labelnouni = gsub('\\:\\s$', '', labelnouni)
    ) |> 
    dplyr::arrange(labelnouni, .locale = 'en')
  
  out <- setNames(out$parameter, out$labelnouni)
  
  return(out)
  
}

#' Function to create a reactable table for displaying data to download
#' 
#' @param dldat Data frame containing the data to display in the table
dldattab_fun <- function(dldat){
  
  out <- reactable::reactable(dldat,
                              defaultColDef = reactable::colDef(
                                footerStyle = list(fontWeight = "bold"),
                                format = reactable::colFormat(digits = 3, separators = F),
                                resizable = TRUE, 
                                align = 'left'
                              ),
                              columns = list(
                                notes = reactable::colDef(show = F)
                              ),
                              filterable = F,
                              defaultPageSize = 15
  )
  
  return(out)
  
}

#' Function to generate a list of available dates for selection
#' 
#' @param meta Data frame containing metadata with date ranges
#' @param location Optional character string to filter by location
#' @param parameter Optional character string to filter by parameter
datechoice_fun <- function(alldat, location = unique(alldat$location), parameter = unique(alldat$parameter), 
                           waterbody = unique(alldat$waterbody)){

  dtrng <- alldat |> 
    dplyr::filter(
      parameter %in% !!parameter &
      location %in% !!location &
      waterbody %in% !!waterbody
      ) |> 
    dplyr::pull(date) |> 
    range()
  
  dtchc <- seq.Date(from = lubridate::ceiling_date(dtrng[1], 'month'), 
                    to = lubridate::floor_date(dtrng[2], 'month'), by = "month")
  
  dtchc <- unique(c(dtrng[1], dtchc, dtrng[2]))
  
  return(dtchc)
  
}

#' Function to generate a y-axis label based on parameter and location
#' 
#' @param prm Character string indicating the parameter to filter by
#' @param loc Character string indicating the sample location (e.g., 'surf', 'bott')
#' 
#' @details Will return the parameter name, units, and location.  Location will only be included if parameter has both surface and bottom measurements.
ylab_fun <- function(prm, loc, addmean = TRUE){

  ymeta <- meta |> 
    dplyr::filter(parameter == !!prm) 
  
  chklocs <- ymeta |>
    dplyr::pull(location) |> 
    unique()
  
  loclb <- ifelse(
    length(chklocs) == 1, '', 
    ifelse(
      loc == 'surf', ': Surface', ': Bottom'
    )
  )
  
  meanlb <- ''
  if(addmean)
    meanlb <- 'Mean '
  
  out <- ymeta |> 
    dplyr::pull(label) |> 
    unique()
  out <- paste0(meanlb, out, loclb)
  
  return(out)
  
}