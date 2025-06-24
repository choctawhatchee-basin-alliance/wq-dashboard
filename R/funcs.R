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
    
    lab <- meta |> 
      dplyr::filter(parameter == parameter1) |> 
      dplyr::pull(label) |> 
      unique()
    
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
        title = paste("Mean", lab),
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
  
  ylab <- meta |> 
    dplyr::filter(parameter == parameter1) |> 
    dplyr::pull(label) |> 
    unique()
  ylab <- paste("Mean", ylab)
  
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

  out <- plotly::plot_ly(
      data = toplo,
      x = ~date,
      y = ~avev,
      type = 'scatter',
      mode = 'lines+markers',
      line = list(color = 'blue'),
      marker = list(size = 5),
      hoverinfo = 'text',
      text = ~paste("Date:", date, "<br>Value:", round(avev, 2))
    )  |> 
    plotly::layout(
      title = ttl,
      xaxis = list(title = "", 
                   range = c(as.Date(daterange1[1]), as.Date(daterange1[2]))
                   ),
      yaxis = list(title = ylab)
    )


  if(nrow(chknnc) == 1)
    out <- out |> 
      plotly::add_trace(
        x = c(as.Date(daterange1[1]), as.Date(daterange1[2])),
        y = chknnc$value,
        type = 'scatter',
        mode = 'lines',
        line = list(color = 'black', dash = 'dash'),
        name = 'NNC Threshold',
        hoverinfo = 'text',
        showlegend = FALSE,
        text = paste("NNC Threshold:", chknnc$value),
        inherit = F
      )
  
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
  
  lab <- meta |> 
    dplyr::filter(parameter == !!prm) |>
    dplyr::filter(location == !!loc) |> 
    dplyr::pull(label) |> 
    unique()
  
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
      title = paste("Mean", lab),
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
  localb <- ifelse(loca == 'surf', 'Surface', 'Bottom')
  locblb <- ifelse(locb == 'surf', 'Surface', 'Bottom')
  
  ylab1 <- names(stationprmsel)[which(stationprmsel == parameter2a)]
  ylab2 <- names(stationprmsel)[which(stationprmsel == parameter2b)]

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
  
  if(summarize2 != 'day'){
    
    p1 <- plotly::plot_ly(
        data = toplo1,
        x = ~date,
        y = ~avev,
        error_y = list(
          array = ~hivl - avev,
          color = 'blue'
        ),
        type = 'scatter',
        mode = 'lines+markers',
        line = list(color = 'blue'),
        marker = list(size = 5, color = "cornflowerblue"),
        hoverinfo = 'text',
        text = ~paste("Date:", date, "<br>Value:", round(avev, 2))
      ) 
      
    p2 <- plotly::plot_ly(
        data = toplo2,
        x = ~date,
        y = ~avev,
        error_y = list(
          array = ~hivl - lovl,
          color = 'red'
        ),
        type = 'scatter',
        mode = 'lines+markers',
        line = list(color = 'red'),
        marker = list(size = 5, color = 'coral'),
        hoverinfo = 'text',
        text = ~paste("Date:", date, "<br>Value:", round(avev, 2))
      ) 
    
  }
  
  if(summarize2 == 'day'){
    
    p1 <- plotly::plot_ly(
      data = toplo1,
      x = ~date,
      y = ~avev,
      type = 'scatter',
      mode = 'lines+markers',
      line = list(color = 'blue'),
      marker = list(size = 5, color = "cornflowerblue"),
      hoverinfo = 'text',
      text = ~paste("Date:", date, "<br>Value:", round(avev, 2))
    ) 
    
    p2 <- plotly::plot_ly(
      data = toplo2,
      x = ~date,
      y = ~avev,
      type = 'scatter',
      mode = 'lines+markers',
      line = list(color = 'red'),
      marker = list(size = 5, color = 'coral'),
      hoverinfo = 'text',
      text = ~paste("Date:", date, "<br>Value:", round(avev, 2))
    ) 
    
  }
  
  p1 <- p1 |> 
    plotly::layout(
      title = paste(waterbody, station),
      xaxis = list(title = "", 
                   range = c(as.Date(daterange2[1]), as.Date(daterange2[2]))
                   ),
      yaxis = list(title = ylab1), 
      showlegend = F
    )
  p2 <- p2 |> 
    plotly::layout(
      xaxis = list(title = "", 
                   range = c(as.Date(daterange2[1]), as.Date(daterange2[2]))
                   ),
      yaxis = list(title = ylab2),
      showlegend = F
    )
  
  # chk nnc p1
  nncchk1 <- nncdat |> 
    dplyr::filter(
      waterbody %in% !!waterbody & 
      station %in% !!station & 
      parameter %in% substr(prm2a, 1, 3)
    ) |> 
    dplyr::select(parameter, value) |> 
    dplyr::distinct()
  if(nrow(nncchk1) == 1)
    p1 <- p1 |> 
      plotly::add_trace(
        x = c(as.Date(daterange2[1]), as.Date(daterange2[2])),
        y = nncchk1$value,
        type = 'scatter',
        mode = 'lines',
        line = list(color = 'black', dash = 'dash'),
        name = 'NNC Threshold',
        hoverinfo = 'text',
        showlegend = FALSE,
        text = paste("NNC Threshold:", nncchk1$value),
        inherit = F
      )
    
  # chk nnc p2
  nncchk2 <- nncdat |> 
    dplyr::filter(
      waterbody %in% !!waterbody & 
      station %in% !!station & 
      parameter %in% substr(prm2b, 1, 3)
    ) |> 
    dplyr::select(parameter, value) |> 
    dplyr::distinct()
  if(nrow(nncchk2) == 1)
    p2 <- p2 |> 
      plotly::add_trace(
        x = c(as.Date(daterange2[1]), as.Date(daterange2[2])),
        y = nncchk2$value,
        type = 'scatter',
        mode = 'lines',
        line = list(color = 'black', dash = 'dash'),
        name = 'NNC Threshold',
        hoverinfo = 'text',
        showlegend = FALSE,
        text = paste("NNC Threshold:", nncchk2$value),
        inherit = F
      )
  
  out <- plotly::subplot(p1, p2, nrows = 2, shareX = TRUE, titleY = TRUE)
  
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
    tidyr::unite(label, c(label, location2), sep = ": ") |>
    tidyr::unite(parameter, c(parameter, location), sep = "_") |>
    dplyr::mutate(
      label = gsub('\\:\\s$', '', label)
    ) |> 
    dplyr::arrange(label, .locale = 'en')
  
  out <- setNames(out$parameter, out$label)
  
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