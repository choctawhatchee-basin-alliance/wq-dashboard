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
  hours <- lubridate::hour(timestamps)
  
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

#' Function to summarize data by WBID or Station
#' 
#' @param alldat Data frame containing the data to summarize
#' @param stas sf object containing station geometries
#' @param summarize1 Character string indicating how to summarize the data ('WBID' or 'Station')
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
    dplyr::select(waterbody, station, date, parameter, val, WBID, WATERBODY_NAME) |> 
    sf::st_set_geometry(NULL)
  
  if (summarize1 == "WBID") {
    
    out <- dat |>
      tidyr::unite('stas', waterbody, station) |> 
      dplyr::summarise(
        val = mean(val, na.rm = TRUE),
        stas = length(unique(stas)),
        .by = c(WBID, WATERBODY_NAME)
      ) |> 
      dplyr::mutate(
        stas = dplyr::case_when(
          stas == 1 ~ paste0(stas, " station"),
          stas > 1 ~ paste0(stas, " stations"),
          TRUE ~ "no stations"
        )
      ) |> 
      dplyr::inner_join(cbawbid, by = c('WBID', 'WATERBODY_NAME')) |> 
      sf::st_as_sf() |> 
      dplyr::filter(!is.na(val))
    
  }
  
  if (summarize1 == "Station") {
    
    out <- dat |>  
      dplyr::summarise(
        val = mean(val, na.rm = TRUE),
        .by = c(waterbody, station)
      ) |> 
      dplyr::left_join(stas, by = c('waterbody', 'station')) |> 
      dplyr::select(-name, -WBID, -datestr, -dateend) |>
      tidyr::unite('stas', waterbody, station, sep = '_', remove = F) |> 
      sf::st_as_sf() |> 
      dplyr::filter(!is.na(val))
    
  }
  
  return(out)
  
}

#' Function to update the map with summarized data by area
#' 
#' @param mapin Leaflet map object to update
#' @param byareadat Data frame returned by byareadat_fun
#' @param summarize1 Character string indicating how to summarize the data ('WBID' or 'Station')
#' @param parameter1 Character string indicating the parameter to filter by
#' @param location1 Character string indicating the sample location (e.g., 'surface', 'bottom')
byareamap_fun <- function(mapin, byareadat, summarize1, parameter1, location1){
  
  # create map
  if(inherits(byareadat, 'try-error'))
    out <- mapin %>%
      leaflet::clearShapes() |> 
      leaflet::clearMarkers() |> 
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
      leaflet::clearMarkers() |> 
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
        label = ~ lapply(paste0(WATERBODY_NAME, ' - ', WBID, "<br>Value: ", round(val, 2), " (", stas , ")"), HTML),
        labelOptions = leaflet::labelOptions(
          style = list("font-size" = "16px")
        ), 
        layerId = ~ WBID
      )
    
    if(summarize1 == 'Station')
      out <- out |> 
      leaflet::addCircleMarkers(
        data = byareadat,
        radius = 7,
        fillColor = ~pal(val),
        fillOpacity = 0.7,
        color = "#666",
        weight = 1,
        label = ~paste0(waterbody, ' ', station, ", Value: ", round(val, 2)),
        labelOptions = leaflet::labelOptions(
          style = list("font-size" = "16px")
        ),
        layerId = ~paste0(stas)
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

#' Function to create a time series plot for a selected area or station
#' 
#' @param shape_click Shape click event data from leaflet
#' @param marker_click Marker click event data from leaflet
#' @param alldat Data frame containing the water quality data to plo
#' @param stas sf object containing station geometries
#' @param nncdat Data frame containing the NNC data
#' @param location1 Character string indicating the sample location (e.g., 'surface', 'bottom')
#' @param parameter1 Character string indicating the parameter to filter by
#' @param daterange1 Date range to filter the data
#' @param showtrnd1 Logical indicating whether to show trend lines
byareaplo_fun <- function(shape_click, marker_click, alldat, stas, nncdat, location1, parameter1, daterange1, showtrnd1){
  
  toplo <- alldat |> 
    dplyr::filter(
      parameter == parameter1 &
        date >= as.Date(daterange1[1]) & 
        date <= as.Date(daterange1[2]) & 
        location == location1
    ) |> 
    dplyr::arrange(date)
  
  if(!is.null(marker_click))
    ylab <- ylab_fun(parameter1, location1, addmean = F)
  if(!is.null(shape_click))
    ylab <- ylab_fun(parameter1, location1)
  
  if(!is.null(shape_click)){
    
    id <- shape_click$id
    
    toplo <- toplo |> 
      dplyr::inner_join(stas, by = c('waterbody', 'station')) |> 
      dplyr::filter(WBID %in% !!id) |> 
      dplyr::select(date, val) |> 
      dplyr::summarise(
        avev = mean(val, na.rm = TRUE),
        .by = date
      )
    
    ttl <- stas |> 
      dplyr::filter(WBID %in% id) |> 
      dplyr::pull(WATERBODY_NAME) |> 
      unique()
    ttl <- paste(ttl, "-", id)
    
    # get nnc line if present
    chknnc <- nncdat |> 
      dplyr::filter(WBID %in% id & parameter %in% substr(parameter1, 1, 3)) |> 
      dplyr::select(WBID, parameter, value) |> 
      dplyr::distinct()
    
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
        avev = mean(val, na.rm = TRUE),
        .by = date
      )
    
    # get nnc line if present
    chknnc <- nncdat |> 
      dplyr::filter(waterbody %in% !!waterbody & station %in% !!station & parameter %in% substr(parameter1, 1, 3)) |> 
      dplyr::select(waterbody, station, parameter, value) |> 
      dplyr::distinct()
    
  }
  
  # Convert dates to milliseconds for Highcharts
  date_range_ms <- c(as.numeric(as.POSIXct(daterange1[1])) * 1000,
                     as.numeric(as.POSIXct(daterange1[2])) * 1000)
  
  # Create chart
  hc <- highcharter::highchart() |>
    highcharter::hc_chart(type = "line") |>
    highcharter::hc_add_theme(
      highcharter::hc_theme(chart = list(backgroundColor = 'white'))
    ) |> 
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
        enableMouseTracking = FALSE
      ) |> 
      highcharter::hc_add_annotation(
        labels = list(
          list(
            point = list(
              xAxis = 0,  # Specify which axis
              yAxis = 0,  # Specify which axis
              x = date_range_ms[1],
              y = chknnc$value
            ),
            y = 0,
            text = paste0("NNC Threshold: ", chknnc$value),
            backgroundColor = rgb(0, 0, 0, 0),
            borderColor = rgb(0, 0, 0, 0),
            style = list(
              color = "red",
              fontSize = "12px",
              fontWeight = "normal"
            )
          )
        )
      )
  }
  
  # trend lines
  if(showtrnd1 != 'none' & nrow(toplo) > 1){
    
    if(showtrnd1 == 'complete'){
      
      mod <- lm(avev ~ date, data = toplo)
      
      # Create a sequence of dates for the trend line based on the summary period
      trend_data <- data.frame(
        date = toplo$date,
        avev = predict(mod)
      )
      
    }
    
    if(showtrnd1 == 'ten years'){
      maxdt <- max(toplo$date, na.rm = T)
      rng <- c(maxdt - lubridate::years(9), maxdt)
      if(rng[1] < min(toplo$date, na.rm = T)){
        trend_data <- data.frame(
          date = NA, 
          avev = NA
        )
        
      } else {
        tomod <- toplo |> 
          dplyr::filter(date >= rng[1] & date <= rng[2])
        mod <- lm(avev ~ date, data = tomod)
        
        # Create a sequence of dates for the trend line based on the summary period
        trend_data <- data.frame(
          date = tomod$date,
          avev = predict(mod)
        )
      }
    }
    
    # add to chart
    hc <- hc |>
      highcharter::hc_add_series(
        data = trend_data,
        type = "line",
        highcharter::hcaes(x = date, y = avev),
        color = "dodgerblue",
        dashStyle = "Dash",
        marker = list(enabled = FALSE),
        tooltip = list(
          pointFormatter = highcharter::JS("function() { return 'Trend: ' + this.y.toFixed(2); }")
        )
      )
    
  }
  
  hc <- hc |> 
    highcharter::hc_chart(height = 265) |> 
    highcharter::hc_chart(reflow = F) |> 
    highcharter::hc_exporting(
      enabled = TRUE,
      filename = "myplot",
      buttons = list(
        contextButton = list(
          menuItems = c("viewFullscreen", "downloadPNG", "downloadCSV")
        )
      )
    )
  
  out <- htmltools::div(
    style = "height: 295px; overflow: hidden;",
    
    # Styled title
    htmltools::h5(
      style = "text-align: center; margin: 0 0 10px 0; padding: 5px; color: #333; font-family: Arial, sans-serif;",
      ttl
    ),
    
    # Charts with equal heights
    htmltools::div(
      style = "height: 265px; margin-bottom: 0px; overflow: hidden;",
      hc
    )
    
  )
  
  return(out)
  
}

#' Function to create a gauge chart for a selected area
#' 
#' @param shape_click Shape click event data from leaflet
#' @param marker_click Marker click event data from leaflet
#' @param byareadat Data frame containing the summarized data by area
#' @param nncdat Data frame containing the NNC data
#' @param parameter1 Character string indicating the parameter to filter by
byareagauge_fun <- function(shape_click, marker_click, byareadat, nncdat, parameter1){
  
  if(!is.null(shape_click)){
    
    id <- shape_click$id
    id <- gsub("^.*\\s\\-\\s", "", id)
    
    curval<- byareadat |> 
      dplyr::filter(WBID %in% id)
    
    chknnc <- nncdat |> 
      dplyr::filter(WBID %in% id & parameter %in% substr(parameter1, 1, 3)) |> 
      dplyr::select(WBID, parameter, value) |> 
      dplyr::distinct()
    
  }
  
  if(!is.null(marker_click)){
    
    id <- marker_click$id
    waterbody <- sub("_(.*)", "", id)
    station <- sub(".*_(.*)", "\\1", id)
    
    curval<- byareadat |> 
      dplyr::filter(stas %in% id) 
    
    chknnc <- nncdat |> 
      dplyr::filter(waterbody %in% !!waterbody & station %in% !!station & parameter %in% substr(parameter1, 1, 3)) |> 
      dplyr::select(waterbody, station, parameter, value) |> 
      dplyr::distinct()
    
  }
  
  curval <- curval |> 
    dplyr::pull(val)
  
  minv <- 0
  maxv <- max(byareadat$val, na.rm = T)
  
  thrshval <- NULL
  
  if(nrow(chknnc) == 1){
    thrshval <- chknnc$value
    maxv <- max(maxv, thrshval)
  }
  
  units <- meta |>
    dplyr::filter(parameter %in% parameter1) |> 
    dplyr::pull(label) |> 
    unique()
  units <- gsub('^.*\\((.*)\\)$', '\\1', units)
  
  # get color
  pal <- leaflet::colorNumeric(
    palette = "YlGnBu",
    domain = range(byareadat$val, na.rm = T),
    na.color = "transparent"
  )
  col <- pal(curval)
  
  out <- highcharter::highchart() |>
    highcharter::hc_chart(type = "solidgauge") |>
    highcharter::hc_add_theme(
      highcharter::hc_theme(chart = list(backgroundColor = 'white'))
    ) |> 
    highcharter::hc_pane(
      center = c("50%", "85%"),
      size = "140%",
      startAngle = -90,
      endAngle = 90,
      background = list(
        backgroundColor = '#FFF',
        innerRadius = "60%",
        outerRadius = "100%",
        shape = "arc"
      )
    ) |>
    highcharter::hc_tooltip(enabled = FALSE) |>
    highcharter::hc_yAxis(
      min = minv,
      max = maxv,
      stops = list(list(1.0, col)),
      minorTickLength = 0,
      tickLength = 10,                                    
      title = list(y = -70),
      plotLines = list(
        list(
          value = thrshval,
          color = "#FF0000",
          width = 3,
          zIndex = 5
        )
      )
    ) |>
    highcharter::hc_plotOptions(
      solidgauge = list(
        dataLabels = list(
          y = 5,
          borderWidth = 0,
          useHTML = TRUE
        )
      )
    ) |>
    highcharter::hc_add_series(
      name = "Value",
      data = list(round(curval, 2)),
      color = col,
      dataLabels = list(
        format = paste0('<div style="text-align:center">',
                        '<span style="font-size:25px">{y}</span><br/>',
                        '<span style="font-size:16px;opacity:0.4">Time Series Mean (', units, ')</span>',
                        '</div>')
      ),
      tooltip = list(
        valueSuffix = paste(" ", units)
      )
    ) |>
    highcharter::hc_credits(enabled = FALSE) |>
    highcharter::hc_exporting(
      enabled = TRUE,
      filename = "mygauge",
      buttons = list(
        contextButton = list(
          menuItems = c("viewFullscreen", "downloadPNG", "downloadCSV")
        )
      )
    ) |> 
    highcharter::hc_chart(height = 220) |> 
    highcharter::hc_chart(reflow = F)
  
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

#' Function to update the map with summarized data by area
#'
#' @param mapin Leaflet map object to update 
#' @param stas sf object containing station geometries
#' @param bystationdat Data frame containing the data to summarize and map
#' @param parameter Character string indicating the parameter to filter by
#' @param daterange2 Date range to filter the data
bystationmap_fun <- function(mapin, bystationdat, stas, parameter, daterange2){
  
  if(parameter != 'rain'){

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

  }

  if(parameter == 'rain'){

    tomap <- raindat |> 
      dplyr::filter(
        date >= as.Date(daterange2[1]) & 
          date <= as.Date(daterange2[2])
      ) |>
      dplyr::mutate(
        date = lubridate::floor_date(date, 'month')
      ) |> 
      dplyr::summarise(
        val = sum(precip_inches, na.rm = T), 
        .by = c(station, name, date)
      ) |>
      dplyr::summarise(
        val = mean(val, na.rm = T), 
        .by =c(station, name)
      )
    
    out <- mapin
    
    pal <- leaflet::colorNumeric(
      palette = "Blues",
      domain = tomap$val,
      na.color = "transparent"
    )
    
    lab <- 'Mean monthly rainfall (inches)'

  }

  # create map
  if(nrow(tomap) == 0){
    
    out <- out %>%
      leaflet::clearMarkers() |> 
      leaflet::clearControls()
    
  }
  
  if(nrow(tomap) != 0 && parameter != 'rain') {
    
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
  
  if(nrow(tomap) != 0 && parameter == 'rain') {
    
    tomap <- dplyr::inner_join(rainstas, tomap, by = c('station', 'name'))
    
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
        label = ~paste0(name, " ", station, ", Mean Value: ", round(val, 2)),
        labelOptions = leaflet::labelOptions(
          style = list("font-size" = "16px")
        ),
        layerId = ~paste0(name, "_", station)
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

#' Function to create a time series plot for a selected station
#' 
#' @param sela Selected station data from the left map
#' @param selb Selected station data from the right map
#' @param bystationdat Data frame containing the water quality data to plot
#' @param nncdat Data frame containing the NNC data
#' @param summarize2 Character string indicating how to summarize the data ('day', 'year')
#' #' @param showtrnd Logical indicating whether to show trend lines (default FALSE)
#' @param parameter2a Character string indicating the first parameter to filter by
#' @param parameter2b Character string indicating the second parameter to filter by
#' @param daterange2 Date range to filter the data
bystationplo_fun <- function(sela, selb, bystationdat, nncdat, summarize2, showtrnd2, parameter2a,
                             parameter2b, daterange2){

  waterbodya <- gsub("(^.*)\\_.*$", "\\1", sela$id)
  stationa <- gsub(".*\\_(.*)$", "\\1", sela$id)
  waterbodyb <- gsub("(^.*)\\_.*$", "\\1", selb$id)
  stationb <- gsub(".*\\_(.*)$", "\\1", selb$id)
  prm2a <- gsub("(^.*)\\_.*$", "\\1", parameter2a)
  prm2b <- gsub("(^.*)\\_.*$", "\\1", parameter2b)
  loca <- gsub(".*\\_(.*)$", "\\1", parameter2a)
  locb <- gsub(".*\\_(.*)$", "\\1", parameter2b)

  rainstations <- unique(raindat$name)
  
  cond_a <- length(waterbodya) > 0 && length(stationa) > 0
  cond_b <- length(waterbodyb) > 0 && length(stationb) > 0
  
  toplo <- bystationdat |> 
    dplyr::filter(
      date >= as.Date(daterange2[1]) & 
        date <= as.Date(daterange2[2])
    )
  
  # filter rainfall data for top plot if selected
  toploraina <- NULL
  if(cond_a && waterbodya %in% rainstations)
    toploraina <- raindat |> 
      dplyr::filter(name %in% waterbodya) |> 
      dplyr::mutate(
        parameter = 'rain', 
        location = 'surf'
      ) |> 
      dplyr::select(waterbody = name, station, date, parameter, location, avev = precip_inches)
  
  # filter rainfall data for bottom plot if selected
  toplorainb <- NULL
  if(cond_b && waterbodyb %in% rainstations)
    toplorainb <- raindat |> 
      dplyr::filter(name %in% waterbodyb) |> 
      dplyr::mutate(
        parameter = 'rain', 
        location = 'surf'
      ) |> 
      dplyr::select(waterbody = name, station, date, parameter, location, avev = precip_inches)
  
  if(cond_a || cond_b)
    toplo <- toplo |>
      dplyr::filter(
        (if(cond_a) waterbody == !!waterbodya & station == !!stationa else FALSE) |
        (if(cond_b) waterbody == !!waterbodyb & station == !!stationb else FALSE)
      )

  toplo <- toplo |> 
    dplyr::rename(avev = val) |> 
    dplyr::select(waterbody, station, date, parameter, location, avev) |> 
    dplyr::bind_rows(toploraina, toplorainb)
  
  if(summarize2 == 'day'){
    ylab1 <- ylab_fun(prm2a, loca, addmean = F)
    ylab2 <- ylab_fun(prm2b, locb, addmean = F)
  }
  
  if(summarize2 != 'day'){
    
    ylab1 <- ylab_fun(prm2a, loca)
    ylab2 <- ylab_fun(prm2b, locb)
    
    if(summarize2 == 'year')
      toplo <-  toplo |> 
        dplyr::mutate(
          date = lubridate::floor_date(date, summarize2),
        ) 
    
    if(summarize2 %in% c('wet season', 'dry season'))
      toplo <- toplo |> 
        dplyr::mutate(
          mo = lubridate::month(date), 
          wetdry = as.character(factor(
            ifelse(mo %in% c(6, 7, 8, 9), "wet", "dry"),
            levels = c("wet", "dry")
          )), 
          wetdrygrp = rep(seq_along(rle(wetdry)$lengths), rle(wetdry)$lengths)
        ) |>
        dplyr::mutate(
          date = lubridate::floor_date(min(date), 'month'), 
          .by = c(waterbody, station, wetdrygrp)
        ) |> 
        dplyr::filter(wetdry == gsub('\\sseason$', '', summarize2))
      
    if(summarize2 %in% c('winter', 'spring', 'summer', 'fall', 'season')){
      toplo <- toplo |>
        dplyr::mutate(
          mo = lubridate::month(date), 
          season = as.character(factor(
            ifelse(mo %in% c(12, 1, 2), "winter",
                  ifelse(mo %in% c(3, 4, 5), "spring",
                          ifelse(mo %in% c(6, 7, 8), "summer", "fall"))),
            levels = c("winter", "spring", "summer", "fall")
          )), 
          seasongrp = rep(seq_along(rle(season)$lengths), rle(season)$lengths)
        ) |> 
        dplyr::mutate(
          date = lubridate::floor_date(min(date), 'month'), 
          .by = c(waterbody, station, seasongrp)
        )
      
      if(summarize2 != 'season')
        tplo <- toplo |>  
          dplyr::filter(season == summarize2)

    }

    toplo <- toplo |> 
      dplyr::group_nest(parameter) |> 
      dplyr::mutate(
        data = purrr::pmap(list(parameter, data), function(parameter, data){
          if(parameter == 'rain')
            out <- data |> 
              dplyr::summarise(
                hivl = NA_real_, 
                lovl = NA_real_,
                avev = sum(avev, na.rm = T),
                .by = c(waterbody, station, date, location)
              )
          if(parameter != 'rain')
            out <- data |> 
              dplyr::summarise(
                hivl = tryCatch(t.test(avev, conf.level = 0.95)$conf.int[2], silent = TRUE, error = function(e) NA), 
                lovl = tryCatch(t.test(avev, conf.level = 0.95)$conf.int[1], silent = TRUE, error = function(e) NA),
                avev = mean(avev, na.rm = T),
                .by = c(waterbody, station, date, location)
              )
          return(out)
        })
      ) |> 
      tidyr::unnest('data')

  }
  
  if(prm2a == 'rain')
    loca <- 'surf'
  if(prm2b == 'rain')
    locb <- 'surf'
  
  toplo1 <- toplo |> 
    dplyr::filter(parameter == prm2a & location == loca) |> 
    dplyr::filter(
      if(cond_a) waterbody %in% !!waterbodya & station %in% !!stationa else FALSE
    ) |> 
    dplyr::arrange(date)
  toplo2 <- toplo |>
    dplyr::filter(parameter == prm2b & location == locb) |> 
    dplyr::filter(
      if(cond_b) waterbody %in% !!waterbodyb & station %in% !!stationb else FALSE
    ) |> 
    dplyr::arrange(date)
  
  # Check for NNC thresholds
  nncchk1 <- nncdat |> 
    dplyr::filter(
      if(cond_a) waterbody %in% !!waterbodya & station %in% !!stationa & parameter %in% substr(prm2a, 1, 3) else FALSE
    ) |> 
    dplyr::select(parameter, value) |> 
    dplyr::distinct()
  
  nncchk2 <- nncdat |> 
    dplyr::filter(
      if(cond_a) waterbody %in% !!waterbodyb & station %in% !!stationb & parameter %in% substr(prm2b, 1, 3) else FALSE
    ) |> 
    dplyr::select(parameter, value) |> 
    dplyr::distinct()
  
  # Convert dates to milliseconds for Highcharts
  date_range_ms <- c(as.numeric(as.POSIXct(daterange2[1])) * 1000,
                     as.numeric(as.POSIXct(daterange2[2])) * 1000)
  
  # Create combined chart using htmltools
  hc1 <- bystationplohc_fun(toplo1, toplo2, nncchk1, showtrnd2, date_range_ms, ylab1, summarize2)
  hc2 <- bystationplohc_fun(toplo2, toplo1, nncchk2, showtrnd2, date_range_ms, ylab2, summarize2)

  out <- htmltools::div(
    style = "height: 550px; overflow: hidden;",
    
    # Styled title
    htmltools::h5(
      style = "text-align: center; margin: 0 0 10px 0; padding: 5px; color: #333; font-family: Arial, sans-serif;",
      if(cond_a)
        paste(waterbodya, stationa)
    ),
    
    # Charts with equal heights
    htmltools::div(
      style = "height: 230px; margin-bottom: 10px; overflow: hidden;",
      hc1
    ),
    
    htmltools::h5(
      style = "text-align: center; margin: 0 0 10px 0; padding: 5px; color: #333; font-family: Arial, sans-serif;",
      if(cond_b)
        paste(waterbodyb, stationb)
    ),

    htmltools::div(
      style = "height: 230px; margin-bottom: 10px; overflow: hidden;",
      hc2
    )
  )
  
  return(out)
  
}

#' Helper function to create highcharts for bystationareaplo_fun
#' 
#' @param toplo Data frame containing the summarized data for the chart
#' @param toplo Data frame containing the summarize data for the other chart, used if \code{showtrnd} is not "none"
#' @param nncchk Data frame containing the NNC threshold data
#' @param showtrnd2 Character string option for showing the trend line
#' @param date_range_ms Numeric vector containing the start and end dates in milliseconds
#' @param ylab Character string for the y-axis label
#' @param summarize2 Character string indicating how to summarize the data ('day', 'year', etc)
bystationplohc_fun <- function(toplo, toplosupp, nncchk, showtrnd2, date_range_ms, ylab, summarize2){
  
  # Create first chart
  hc <- highcharter::highchart() |>
    highcharter::hc_chart(type = "line") |>
    highcharter::hc_add_theme(
      highcharter::hc_theme(chart = list(backgroundColor = 'white'))
    ) |> 
    highcharter::hc_xAxis(
      type = "datetime",
      min = date_range_ms[1],
      max = date_range_ms[2],
      title = list(text = "")
    ) |>
    highcharter::hc_yAxis(title = list(text = ylab)) |>
    highcharter::hc_legend(enabled = FALSE)
  
  # Add data series for first chart
  if(nrow(toplo) > 0) {
    if(summarize2 != 'day') {
      # Add series with error bars
      hc <- hc |>
        highcharter::hc_add_series(
          data = toplo,
          type = "errorbar",
          highcharter::hcaes(x = date, low = lovl, high = hivl),
          color = "blue",
          showInLegend = FALSE,
          tooltip = list(
            pointFormat = "Range: {point.low:.2f} - {point.high:.2f}"
          )
        ) |> 
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
    } else {
      # Add series without error bars
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
  }
  
  # Add NNC threshold line
  if(nrow(nncchk) == 1 & nrow(toplo) > 0) {
    hc <- hc |>
      highcharter::hc_add_series(
        data = list(
          list(x = date_range_ms[1], y = nncchk$value),
          list(x = date_range_ms[2], y = nncchk$value)
        ),
        type = "line",
        color = "red",
        dashStyle = "Dash",
        showInLegend = FALSE,
        enableMouseTracking = FALSE
      ) |> 
      highcharter::hc_add_annotation(
        labels = list(
          list(
            point = list(
              xAxis = 0,  # Specify which axis
              yAxis = 0,  # Specify which axis
              x = date_range_ms[1],
              y = nncchk$value
            ),
            y = 0,
            text = paste0("NNC Threshold: ", nncchk$value),
            backgroundColor = rgb(0, 0, 0, 0),
            borderColor = rgb(0, 0, 0, 0),
            style = list(
              color = "red",
              fontSize = "12px",
              fontWeight = "normal"
            )
          )
        )
      )
  }
  
  if(showtrnd2 != 'none' & nrow(toplo) > 1){
    
    if(showtrnd2 == 'shortest' & nrow(toplosupp) == 0)
      showtrnd2 <- 'complete'
    
    if(showtrnd2 == 'complete'){
      
      mod <- lm(avev ~ date, data = toplo)
      
      # Create a sequence of dates for the trend line based on the summary period
      trend_data <- data.frame(
        date = toplo$date,
        avev = predict(mod)
      )
      
    }
    
    if(showtrnd2 == 'shortest'){
      
      rng1 <- range(toplo$date, na.rm = T)
      rng2 <- range(toplosupp$date, na.rm = T)
      rng <- c(diff(rng1), diff(rng2))
      rng <- list(rng1, rng2)[[which.min(rng)]]
      
      tomod <- toplo |> 
        dplyr::filter(date >= rng[1] & date <= rng[2])
      mod <- lm(avev ~ date, data = tomod)
      
      # Create a sequence of dates for the trend line based on the summary period
      trend_data <- data.frame(
        date = tomod$date,
        avev = predict(mod)
      )
      
    }
    
    if(showtrnd2 == 'ten years'){
      maxdt <- max(toplo$date, na.rm = T)
      rng <- c(maxdt - lubridate::years(9), maxdt)
      if(rng[1] < min(toplo$date, na.rm = T)){
        trend_data <- data.frame(
          date = NA, 
          avev = NA
        )
          
      } else {
        tomod <- toplo |> 
          dplyr::filter(date >= rng[1] & date <= rng[2])
        mod <- lm(avev ~ date, data = tomod)
        
        # Create a sequence of dates for the trend line based on the summary period
        trend_data <- data.frame(
          date = tomod$date,
          avev = predict(mod)
        )
      }
    }
    
    # add to chart
    hc <- hc |>
      highcharter::hc_add_series(
        data = trend_data,
        type = "line",
        highcharter::hcaes(x = date, y = avev),
        color = "dodgerblue",
        dashStyle = "Dash",
        marker = list(enabled = FALSE),
        tooltip = list(
          pointFormatter = highcharter::JS("function() { return 'Trend: ' + this.y.toFixed(2); }")
        )
      )
    
  }
  
  out <- hc |> 
    highcharter::hc_chart(height = 230) |> 
    highcharter::hc_chart(reflow = FALSE) |> 
    highcharter::hc_exporting(
      enabled = TRUE,
      filename = "myplot",
      buttons = list(
        contextButton = list(
          menuItems = c("viewFullscreen", "downloadPNG", "downloadCSV")
        )
      )
    )
  
  return(out)
  
}

#' #' Function to summarize continuous data by station
#' #' 
#' #' @param cntdat Data frame containing the continuous data to summarize
#' #' @param stas sf object containing station geometries
#' #' @param parameter3 Character string indicating the parameter to filter by
#' #' @param daterange3 Date range to filter the data
#' bycntdat_fun <- function(cntdat, stas, parameter3, daterange3){
#'   
#'   out <- cntdat |> 
#'     dplyr::select(waterbody, station, timestamp, val = contains(parameter3)) |> 
#'     dplyr::filter(
#'       timestamp >= as.Date(daterange3[1]) & 
#'         timestamp <= as.Date(daterange3[2]) 
#'     ) |>  
#'     dplyr::summarise(
#'       val = mean(val, na.rm = TRUE),
#'       .by = c(waterbody, station)
#'     ) |> 
#'     dplyr::left_join(stas, by = c('waterbody', 'station')) |> 
#'     dplyr::select(-name, -WBID, -datestr, -dateend) |>
#'     tidyr::unite('stas', waterbody, station, sep = '_', remove = F) |> 
#'     sf::st_as_sf() |> 
#'     dplyr::filter(!is.na(val))
#'   
#'   return(out)
#'   
#' }
#' 
#' #' Function to update the continuous map with summarized data
#' #' 
#' #' @param mapin Leaflet map object to update
#' #' @param bycntdat Data frame returned by bycntdat_fun
#' #' @param parameter1 Character string indicating the parameter to filter by
#' bycntmap_fun <- function(mapin, bycntdat, parameter3){
#'   
#'   # create map
#'   if(inherits(bycntdat, 'try-error'))
#'     out <- mapin %>%
#'       leaflet::clearShapes() |> 
#'       leaflet::clearMarkers() |> 
#'       leaflet::clearControls()
#'   
#'   if(!inherits(bycntdat, 'try-error')) {
#'     
#'     pal <- leaflet::colorNumeric(
#'       palette = "YlGnBu",
#'       domain = bycntdat$val,
#'       na.color = "transparent"
#'     )
#'     
#'     lab <- ylab_fun(parameter3, NULL)
#'     
#'     out <- mapin %>%
#'       leaflet::clearShapes() |> 
#'       leaflet::clearMarkers() |> 
#'       leaflet::clearControls()
#'     
#'     out <- out |> 
#'       leaflet::addCircleMarkers(
#'         data = bycntdat,
#'         radius = 7,
#'         fillColor = ~pal(val),
#'         fillOpacity = 0.7,
#'         color = "#666",
#'         weight = 1,
#'         label = ~paste0(waterbody, ' ', station, ", Value: ", round(val, 2)),
#'         labelOptions = leaflet::labelOptions(
#'           style = list("font-size" = "16px")
#'         ),
#'         layerId = ~paste0(stas)
#'       )
#'     
#'     # add legend
#'     out <- out |>
#'       leaflet::addLegend(
#'         pal = pal,
#'         values = bycntdat$val,
#'         title = lab,
#'         position = "topright",
#'         opacity = 0.8,
#'         na.label = "No Data"
#'       )
#'     
#'   }
#'   
#'   return(out)
#'   
#' }
#' 
#' #' Function to create a time series plot for a selected station
#' #' 
#' #' @param sel Selected station data from the input
#' #' @param cntdat Data frame containing the continuous water quality data
#' #' @param parameter3 Character string indicating the parameter to filter by
#' #' @param daterange3 Date range to filter the data
#' #' @param summarize3 Character string indicating how to summarize the data ('none', 'day', 'week', 'quarter' 'year')
#' #' #' @param showtrnd Logical indicating whether to show trend lines (default FALSE)
#' bycntplo_fun <- function(sel, cntdat, parameter3, daterange3, summarize3, 
#'                          showtrnd){
#'   
#'   waterbody <- gsub("(^.*)\\_.*$", "\\1", sel$id)
#'   station <- gsub(".*\\_(.*)$", "\\1", sel$id)
#'   prm3 <- gsub("(^.*)\\_.*$", "\\1", parameter3)
#' 
#'   toplo <- cntdat |> 
#'     dplyr::select(waterbody, station, date = timestamp, avev = contains(parameter3)) |> 
#'     dplyr::filter(
#'       date >= as.Date(daterange3[1]) & 
#'         date <= as.Date(daterange3[2]) &
#'         waterbody == !!waterbody &
#'         station == !!station
#'     ) |> 
#'     dplyr::arrange(date)
#' 
#'   if(summarize3 == 'none')
#'     ylab <- ylab_fun(prm3, NULL, addmean = F)
#'   
#'   if(summarize3 != 'none'){
#'     
#'     ylab <- ylab_fun(prm3, NULL, addmean = T)
#'     
#'     toplo <-  toplo |> 
#'       dplyr::mutate(
#'         date = lubridate::floor_date(date, summarize3),
#'       ) |> 
#'       dplyr::summarise(
#'         hivl = tryCatch(t.test(avev, conf.level = 0.95)$conf.int[2], silent = TRUE, error = function(e) NA),
#'         lovl = tryCatch(t.test(avev, conf.level = 0.95)$conf.int[1], silent = TRUE, error = function(e) NA),
#'         avev = mean(avev, na.rm = TRUE),
#'         .by = c(date)
#'       )
#'     
#'   }
#'   
#'   # Convert dates to milliseconds for Highcharts
#'   date_range_ms <- c(as.numeric(as.POSIXct(daterange3[1])) * 1000,
#'                      as.numeric(as.POSIXct(daterange3[2])) * 1000)
#' 
#'   # Create chart
#'   hc <- highcharter::highchart() |>
#'     highcharter::hc_chart(type = "line") |>
#'     highcharter::hc_xAxis(
#'       type = "datetime",
#'       min = date_range_ms[1],
#'       max = date_range_ms[2],
#'       title = list(text = "")
#'     ) |>
#'     highcharter::hc_yAxis(title = list(text = ylab)) |>
#'     highcharter::hc_legend(enabled = FALSE)
#'   
#'   # Add data series for first chart
#'   if(nrow(toplo) > 0) {
#'     
#'     toplo <- toplo |> 
#'       dplyr::mutate(date = as.numeric(as.POSIXct(date)) * 1000)
#'     if(summarize3 != 'none') {
#'       # Add series with error bars
#'       hc <- hc |>
#'         highcharter::hc_add_series(
#'           data = toplo,
#'           type = "errorbar",
#'           highcharter::hcaes(x = date, low = lovl, high = hivl),
#'           color = "blue",
#'           showInLegend = FALSE,
#'           tooltip = list(enabled = FALSE)
#'         ) |> 
#'         highcharter::hc_add_series(
#'           data = toplo,
#'           type = "line",
#'           highcharter::hcaes(x = date, y = avev),
#'           color = "blue",
#'           marker = list(radius = 3, fillColor = "lightblue"),
#'           tooltip = list(
#'             pointFormatter = highcharter::JS("function() { return 'Value: ' + this.y.toFixed(2); }")
#'           )
#'         )
#'     } else {
#'       # Add series without error bars
#'       hc <- hc |>
#'         highcharter::hc_add_series(
#'           data = toplo,
#'           type = "line",
#'           highcharter::hcaes(x = date, y = avev),
#'           color = "blue",
#'           # marker = list(radius = 3, fillColor = "lightblue"),
#'           tooltip = list(
#'             pointFormatter = highcharter::JS("function() { return 'Value: ' + this.y.toFixed(2); }")
#'           )
#'         )
#'     }
#'   }
#'   
#'   if(showtrnd & nrow(toplo) > 1){
#'     
#'     mod <- lm(avev ~ date, data = toplo)
#'     
#'     # Create a sequence of dates for the trend line based on the summary period
#'     trend_data <- data.frame(
#'       date = toplo$date,
#'       avev = predict(mod)
#'     )
#'     
#'     # add to chart
#'     hc <- hc |>
#'       highcharter::hc_add_series(
#'         data = trend_data,
#'         type = "line",
#'         highcharter::hcaes(x = date, y = avev),
#'         color = "dodgerblue",
#'         dashStyle = "Dash",
#'         marker = list(enabled = FALSE),
#'         tooltip = list(
#'           pointFormatter = highcharter::JS("function() { return 'Trend: ' + this.y.toFixed(2); }")
#'         )
#'       )
#'     
#'   }
#'   
#'   hc <- hc |> highcharter::hc_chart(height = 250) |> highcharter::hc_chart(reflow = FALSE) 
#'   
#'   out <- htmltools::div(
#'     style = "height: 300px; overflow: hidden;",
#'     
#'     # Styled title
#'     htmltools::h5(
#'       style = "text-align: center; margin: 0 0 10px 0; padding: 5px; color: #333; font-family: Arial, sans-serif;",
#'       paste(waterbody, station)
#'     ),
#'     
#'     # chart
#'     htmltools::div(
#'       style = "height: 275px; margin-bottom: 10px; overflow: hidden;",
#'       hc
#'     )
#'     
#'   )
#'   
#'   return(out)
#'   
#' }
#' 
#' #' Function to create a gauge chart for continuous data
#' #' 
#' #' @param marker_click Marker click event data from leaflet
#' #' @param bycntdat Data frame containing the summarized continuous data
#' #' @param parameter3 Character string indicating the parameter to filter by
#' bycntgauge_fun <- function(marker_click, bycntdat, parameter3){
#' 
#'   id <- marker_click$id
#'   waterbody <- sub("_(.*)", "", id)
#'   station <- sub(".*_(.*)", "\\1", id)
#' 
#'   curval <- bycntdat |>
#'     dplyr::filter(stas %in% id) |> 
#'     dplyr::pull(val) |>
#'     round(2)
#' 
#'   minv <- 0 #round(min(bycntdat$val, na.rm = T), 2)
#'   maxv <- round(max(bycntdat$val, na.rm = T), 2)
#' 
#'   units <- meta |>
#'     dplyr::filter(parameter %in% parameter3) |>
#'     dplyr::pull(label) |>
#'     unique()
#'   units <- gsub('^.*\\((.*)\\)$', '\\1', units)
#' 
#'   # Create color stops for gradient
#'   vals <- seq(0, 1, length.out = 11)
#'   pal <- leaflet::colorNumeric(
#'     palette = "YlGnBu",
#'     domain = vals,
#'     na.color = "transparent"
#'   )
#'   cols <- pal(vals)
#'   color_stops <- lapply(seq_along(cols), function(i) {
#'     list(vals[i], cols[i])
#'   })
#' 
#'   out <- highcharter::highchart() |>
#'     highcharter::hc_chart(type = "solidgauge") |>
#'     highcharter::hc_pane(
#'       center = c("50%", "85%"),
#'       size = "140%",
#'       startAngle = -90,
#'       endAngle = 90,
#'       background = list(
#'         backgroundColor = '#FFF',
#'         innerRadius = "60%",
#'         outerRadius = "100%",
#'         shape = "arc"
#'       )
#'     ) |>
#'     highcharter::hc_tooltip(enabled = FALSE) |>
#'     highcharter::hc_yAxis(
#'       min = minv,
#'       max = maxv,
#'       stops = color_stops,
#'       minorTickLength = 0,
#'       tickLength = 10,
#'       title = list(y = -70)
#'     ) |>
#'     highcharter::hc_plotOptions(
#'       solidgauge = list(
#'         dataLabels = list(
#'           y = 5,
#'           borderWidth = 0,
#'           useHTML = TRUE
#'         )
#'       )
#'     ) |>
#'     highcharter::hc_add_series(
#'       name = "Value",
#'       data = list(curval),
#'       dataLabels = list(
#'         format = paste0('<div style="text-align:center">',
#'                         '<span style="font-size:25px">{y}</span><br/>',
#'                         '<span style="font-size:16px;opacity:0.4">Time Series Mean (', units, ')</span>',
#'                         '</div>')
#'       ),
#'       tooltip = list(
#'         valueSuffix = paste(" ", units)
#'       )
#'     ) |>
#'     highcharter::hc_credits(enabled = FALSE) |>
#'     highcharter::hc_exporting(enabled = FALSE) |>
#'     highcharter::hc_chart(height = 210) |>
#'     highcharter::hc_chart(reflow = F)
#' 
#'   return(out)
#'   
#' }

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
    
    # station
    if(!mapsel1$id %in% cbawbid$WBID)
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
#' @param mapsel2a Data frame containing the selected station's longitude and latitude for the left map
#' @param mapsel2b Data frame containing the selected station's longitude and latitude for the right map
addselstationmap_fun <- function(mapsel2a = NULL, mapsel2b = NULL){
  
  if (!is.null(mapsel2a)) {
    
    leaflet::leafletProxy("bystationmap1") |>
      leaflet::clearGroup("highlight") |>
      leaflet::addCircleMarkers(
        lng = mapsel2a$data$lng, lat = mapsel2a$data$lat,
        group = "highlight", radius = 8, color = "black",
        fillColor = '#007BC2', fillOpacity = 0, opacity = 1, weight = 6,
        options = leaflet::pathOptions(clickable = FALSE)
      )
    
  }

  if(!is.null(mapsel2b)){

    leaflet::leafletProxy("bystationmap2") |>
      leaflet::clearGroup("highlight") |>
      leaflet::addCircleMarkers(
        lng = mapsel2b$data$lng, lat = mapsel2b$data$lat,
        group = "highlight", radius = 8, color = "black",
        fillColor = '#007BC2', fillOpacity = 0, opacity = 1, weight = 6,
        options = leaflet::pathOptions(clickable = FALSE)
      )
    
  }
  
}

#' Add marker highlight to continuous map selection
#'
#' @param mapsel3 Data frame containing the selected area or station's ID
addselcntmap_fun <- function(mapsel3){
  
  if (!is.null(mapsel3)) {
    
    leaflet::leafletProxy("bycntmap") |>
      leaflet::clearGroup("highlight") |>
      leaflet::addCircleMarkers(
        lng = mapsel3$lng, lat = mapsel3$lat,
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
  
  # parameters
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

  # rain
  rainout <- raindat |> 
    dplyr::filter(date >= daterange2[1] & date <= daterange2[2])

  if(nrow(rainout) > 0)
    out <- c(out, 'Rainfall' = 'rain')
  
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
  
  if(!is.null(loc) && !is.null(prm)){
    if(prm == 'rain' && loc == 'rain'){
      out <- "Cumulative rainfall (inches)"
      return(out)
    }
  }

  ymeta <- meta |> 
    dplyr::filter(parameter == !!prm) 
  
  chklocs <- ymeta |>
    dplyr::pull(location) |> 
    unique()
  
  loclb <- NULL
  if(!is.null(loc))
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

#' Function to get rain data for a single station across multiple years
#' 
#' @param station_id Character string indicating the station ID
#' @param start_date Character string indicating the start date in "YYYY-MM-DD" format
#' @param end_date Character string indicating the end date in "YYYY-MM-DD" format
#' @param max_retries Integer indicating the maximum number of retries for API requests
#' @param noaa_key Character NOAA API key
#' 
#' @return daily rainfall in tenths of mm for the station
getstatrain_fun <- function(station_id, start_date, end_date, max_retries = 5, noaa_key) {
  
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
          api_result <- rnoaa::ncdc(datasetid = 'GHCND',
                             stationid = paste0('GHCND:', station_id),
                             datatypeid = 'PRCP',
                             startdate = as.character(year_start),
                             enddate = as.character(year_end),
                             limit = 1000, 
                            token = noaa_key)
          
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
      combined_data <- dplyr::bind_rows(all_station_data)
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

#' Function to get rain data for multiple stations across multiple years
#' 
#' @param stations List of stations for retrieving rain data, names are stations ids and elements are station names
#' @param start_date Character string indicating the start date in "YYYY-MM-DD" format
#' @param end_date Character string indicating the end date in "YYYY-MM-DD" format
#' @param max_retries Integer indicating the maximum number of retries for API requests
#' @param noaa_key Character NOAA API key
#' 
#' @return daily rainfall in inches for each station
getallrain_fun <- function(stations, start_date, end_date, max_retries = 5, noaa_key){

  # Retrieve data for all stations
  raindat <- purrr::map_dfr(names(stations), ~getstatrain_fun(.x, start_date, end_date, max_retries, noaa_key))

  # Process the data
  out <- raindat |>
    dplyr::mutate(
      date = as.Date(date),
      precip_inches = value / 254, # convert to inches
      station = gsub('^GHCND:', '', station),
      name = factor(station, levels = names(stations), labels = stations)
    ) |> 
    dplyr::filter(!is.na(precip_inches)) |> 
    dplyr::select(station, name, date, precip_inches)

  return(out)

}