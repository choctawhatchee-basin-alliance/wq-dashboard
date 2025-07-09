library(shiny)
library(bslib)

source('R/funcs.R')
source('R/global.R')

ui <- page_navbar(
  title = "CBA WATER QUALITY DASHBOARD",
  id = "main-nav",
  
  # Add logo
  nav_item(
    tags$img(src = "heron.png", height = "30px", style = "margin-right: 10px;")
  ),

  # styles and download spinner
  header = tagList(
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "styles.css"),
      tags$script(HTML("
      $(document).on('click', '#dwnld', function() {
        $(this).prop('disabled', true).html('<span class=\"spinner-border spinner-border-sm\" role=\"status\"></span> Downloading...');
      });
      Shiny.addCustomMessageHandler('reset_download_button', function(message) {
        $('#dwnld').prop('disabled', false).html('<i class=\"fa fa-download\"></i> Download data');
      });
      $(document).ready(function() {
        $('.navbar-brand').css('cursor', 'pointer').on('click', function() {
          $('a[data-value=\"overview\"]').tab('show');
          Shiny.setInputValue('main-nav', 'overview');
          setTimeout(function() {
            $('a[data-value=\"about\"]').tab('show');
          }, 100);
        });
      });
      "))
    )
  ),
  
  # First nav item - Overview
  nav_panel(
    title = "OVERVIEW",
    value = 'overview',
    navset_card_underline(
      full_screen = TRUE,
      height = '100%',
      nav_panel(
        title = "ABOUT",
        value = 'about',
        class = 'card-scroll',
        layout_sidebar(
          shiny::includeMarkdown('www/overview.md'),
          sidebar = sidebar(
            position = 'right',
            width = '500px',
            layout_column_wrap(
              width = "100%",
              height = "100%",
              style = "padding: 5px",
              valbx
            )
          )
        )
      ),
      nav_panel(
        title = 'METHODS', 
        class = 'card-scroll',
        shiny::includeMarkdown('www/methods.md')
      )
    )
  ),
  
  # Second nav item - by area
  nav_panel(
    title = "1 BY AREA",
    value = 'byarea',
    navset_card_underline(
      full_screen = TRUE,
      height = '100%',
      nav_panel(
        title = "DATA",
        class = 'card-scroll',
        layout_sidebar(
          sidebar = sidebar(
            title = "Controls",
            border_radius = FALSE, 
            fillable = TRUE,
            width = "600px",
            open = "desktop",
            radioButtons("summarize1", 
                         label = bslib::popover(
                           trigger = list(
                             "Summarize By:",
                             icon("info-circle")
                           ),
                           HTML('click an area on the map to view summary info on the right')
                         ),
                         choices = c("WBID", "HUC12"), 
                         selected = "WBID"), 
            selectInput("parameter1", "Select Parameter:", choices = prms), 
            uiOutput("location1"),
            uiOutput("daterange1")
          ),
          layout_sidebar(
            border = FALSE,
            leaflet::leafletOutput('byareamap', height = "100%"),
            sidebar = sidebar(
              id = "byareasidebar",
              htmltools::div(
                style = "height: 625px; overflow: hidden;",
                htmltools::div(
                  style = "height: 350px; margin-bottom: 10px; overflow: hidden;",
                  highcharter::highchartOutput('byareaplo'),
                ),
                htmltools::div(
                  style = "height: 275px; overflow: hidden;",
                  highcharter::highchartOutput("byareagauge"),
                )
              ),
              width = "50%",
              position = "right",
              open = FALSE
            )
          )
        )
      ), 
      nav_panel(
        title = "HOW TO USE",
        class = 'card-scroll',
        shiny::includeMarkdown('www/byareahowto.md')
        )
      )
      
  ),
  
  #####
  # Third nav item - by station with dual maps
  nav_panel(
    title = "2 PARAMETER COMPARISON",
    value = 'bystation',
    navset_card_underline(
      full_screen = TRUE,
      height = '100%',
      nav_panel(
        title = "DATA",
        class = 'card-scroll',
        layout_sidebar(
          border = FALSE,
          sliderTextInput("daterange2", 
                          label = bslib::popover(
                            trigger = list(
                              "Select Date Range:",
                              icon("info-circle")
                            ),
                            HTML('click a station on the map to view summary info on the right')
                          ), 
                          choices = dtchc, selected = range(dtchc), width = '50%'),
          div(
            style = "height: calc(100vh - 400px); display: flex; flex-direction: column;",
            div(
              style = "flex: 1; display: flex; gap: 10px; margin-bottom: 10px;",
              div(
                style = "flex: 1;",
                uiOutput('parameter2a'),
                leaflet::leafletOutput('bystationmap1', height = "100%")
              ),
              div(
                style = "flex: 1;",
                uiOutput('parameter2b'),
                leaflet::leafletOutput('bystationmap2', height = "100%")
              )
            )
          ),
          width = '60%', 
          position = 'left',
          open = TRUE,
          sidebar = sidebar(
            id = "bystationsidebar",
            selectInput("summarize2", "Summarize By:", 
                        choices = c("day", "week", "month", "year"), 
                        selected = "day"),
            uiOutput('bystationplo'),
            border_radius = FALSE, 
            fillable = FALSE,
            width = "40%",
            open = FALSE,
            position = 'right'
          ),
        )
      ), 
      nav_panel(
        title = "HOW TO USE",
        class = 'card-scroll',
        shiny::includeMarkdown('www/parmcomphowto.md')
      )
    )
    
  ),
  
  #####
  # Fourth nav item - continuous data
  nav_panel(
    title = "3 CONTINUOUS DATA",
    value = 'continuousdata',
    navset_card_underline(
      full_screen = TRUE,
      height = '100%',
      nav_panel(
        title = "DATA",
        class = 'card-scroll',
        layout_sidebar(
          sidebar = sidebar(
            title = "Controls",
            border_radius = FALSE, 
            fillable = TRUE,
            width = "600px",
            open = "desktop",
            selectInput("variable3", "Select Variable:", choices = c("Option 1", "Option 2", "Option 3"))
          ),
          layout_sidebar(
            border = FALSE,
            "middle content",
            sidebar = sidebar(
              "right content",
              width = "50%",
              position = "right",
              open = FALSE
            )
          )
        )
      ), 
      nav_panel(
        title = "HOW TO USE",
        class = 'card-scroll',
        NULL
      )
    )
    
  ),
  
  ####### fifth nav item
  # Fifth nav item - download
  nav_panel(
    title = "4 DOWNLOAD",
    value = 'download',
    layout_sidebar(
      sidebar = sidebar(
        title = "Controls:",
        border_radius = FALSE, 
        fillable = TRUE,
        width = "600px",
        open = "desktop",
        pickerInput("waterbody4", "Select Waterbody:", 
          choices = wtbds, selected = wtbds, multiple = T,
          options = list(`actions-box` = TRUE, style = "btn-outline-secondary btn-sm")),
        pickerInput("parameter4", "Select Parameter:", choices = prms, selected = prms, multiple = T,
                    options = list(`actions-box` = TRUE, style = "btn-outline-secondary btn-sm")),
        uiOutput("location4"),
        uiOutput("daterange4")
      ),
      border = FALSE,
      downloadButton('dwnld', 'Download data', class = "btn-primary"),
      shinyWidgets::addSpinner(reactable::reactableOutput('dltabout'), spin = "circle", color = "#007bff")
    )
    
  ),
  
  #####
  # Sixth nav item - oyster map
  nav_panel(
    title = "5 OYSTER HABITAT SUITABILITY", 
    value = 'oyster',
    div(
      style = "height: 80vh;", # Use viewport height for responsive sizing
      tags$iframe(
        src = "https://experience.arcgis.com/experience/5c8a799166174fcb9c4d17b7d6132b1d/",
        width = "100%",
        height = "100%",
        frameborder = "0",
        scrolling = "auto"
      )
    )
  ),
  
  # Navbar configuration
  nav_spacer(),
  nav_item(
    tags$a(
      href = "https://github.com/choctawhatchee-basin-alliance/wq-dashboard",
      target = "_blank",
      "Source Code"
    )
  )
)

#####
server <- function(input, output, session) {
  
  #####
  # reactives
    
  # byareamap update

  # debounced inputs to prevent cascade firing for by area
  inputs_debounced1 <- reactive({
    list(
      summarize1 = input$summarize1,
      parameter1 = input$parameter1,
      location1 = input$location1,
      daterange1 = input$daterange1
    )
  }) %>% debounce(500)  # 500ms delay
  
  # data to map and gauge - using debounced inputs
  byareadat <- reactive({
    
    inputs <- inputs_debounced1()
    
    req(inputs$location1)  
    req(inputs$daterange1)
    
    try(byareadat_fun(alldat, stas, inputs$summarize1, inputs$location1, 
                      inputs$parameter1, inputs$daterange1), 
        silent = T)
  }) 
  
  # by area map initialize
  observeEvent(list(byareadat(), inputs_debounced1()$summarize1, 
                    inputs_debounced1()$parameter1, inputs_debounced1()$location1, 
                    input$`main-nav`), {
                      
    if(input$`main-nav` == 'byarea'){
      
      req(byareadat())
      inputs <- inputs_debounced1()
      req(inputs$location1)
      
      byareamap_fun(byareamap_proxy, byareadat(), inputs$summarize1, 
                    inputs$parameter1, inputs$location1)
      
      req(map_sel1())
      
      addselareamap_fun(map_sel1()$data)
      
    }
    
  }, ignoreInit = FALSE) 
  
  # for area map clicks
  map_sel1 <- reactiveVal(NULL)
  
  # reset map selection when summarize1 changes
  observeEvent(input$summarize1, {
    map_sel1(NULL)
  }, ignoreInit = TRUE)
  
  # handle marker clicks  
  observeEvent(input$byareamap_shape_click, {
    map_sel1(list(type = "marker", data = input$byareamap_shape_click))
  })
  
  # add highlight of shape to byareamap if map_sel1
  observe({
    
    req(map_sel1())
    
    addselareamap_fun(map_sel1()$data)
    
  })
  
  # by area plot - using debounced inputs
  byareaplo <- reactive({

    req(map_sel1())
    inputs <- inputs_debounced1()
    req(inputs$location1)
    req(inputs$daterange1)
    
    sel <- map_sel1()
    
    out <- byareaplo_fun(sel, alldat, stas, nncdat,
                         inputs$location1, inputs$parameter1,
                         inputs$daterange1)
    
    return(out)    
    
  })
  
  # by area gauge - using debounced inputs
  byareagauge <- reactive({
    
    req(byareadat())
    req(map_sel1())
    
    sel <- map_sel1()
    inputs <- inputs_debounced1()
    
    out <- byareagauge_fun(sel, byareadat(), nncdat, inputs$parameter1)
    
    return(out)  
    
  })
  
  # toggle areamap open sidebar, polygon
  observeEvent(input$byareamap_shape_click, {
    sidebar_toggle("byareasidebar", open = TRUE)
  })
  
  # toggle areamap close sidebar, summarize1 changes
  observeEvent(input$summarize1, {
    sidebar_toggle("byareasidebar", open = FALSE)
  })
  
  # station map
  
  # parameters to select
  dtprmsel <- reactive({

    stationprmsel_fun(input$daterange2)
    
  })
  
  # retain last parameter selection on daterange2 change
  observeEvent(input$daterange2, {
    
    req(input$parameter2a)
    req(input$parameter2b)
    
    dtprmsel <- stationprmsel_fun(input$daterange2)
    
    # Get current selection
    curparameter2a <- input$parameter2a
    curparameter2b <- input$parameter2b
    
    choices <- dtprmsel

    # update choices but preserve selection if it exists
    if (!is.null(curparameter2a)) {
      if (!curparameter2a %in% dtprmsel) {
        curparameter2a <- stationprmsel[stationprmsel %in% curparameter2a]
        names(curparameter2a) <- paste(names(curparameter2a), "(not in date range)")
        choices <- c(curparameter2a, choices)
      }
      updateSelectInput(session, "parameter2a",
                        choices = choices,
                        selected = curparameter2a)
      
    } else {
      updateSelectInput(session, "parameter2a",
                        choices = choices)
    }
    
    if(!is.null(curparameter2b)) {
      if (!curparameter2b %in% dtprmsel) {
        curparameter2b <- stationprmsel[stationprmsel %in% curparameter2b]
        names(curparameter2b) <- paste(names(curparameter2b), "(not in date range)")
        choices <- c(curparameter2b, choices)
      }
      updateSelectInput(session, "parameter2b",
                        choices = choices,
                        selected = curparameter2b)
    } else {
      updateSelectInput(session, "parameter2b",
                        choices = choices)
    }
    
  })
  
  # station data
  bystationdat <- reactive({

    req(input$parameter2a)
    req(input$parameter2b)

    out <- bystationdat_fun(alldat, input$parameter2a, input$parameter2b)

    return(out)
    
  })
  
  # shared reactive values for map synchronization
  last_sync_time <- reactiveVal(Sys.time())
  sync_in_progress <- reactiveVal(FALSE)
  
  # Initialize both maps
  observeEvent(list(bystationdat(), input$daterange2, input$parameter2a, input$parameter2b, input$`main-nav`), {
    
    if(input$`main-nav` == 'bystation'){

      req(input$parameter2a)
      req(input$parameter2b)
      
      # Initialize both maps with the same data
      bystationmap_fun(bystationmap1_proxy, bystationdat(), stas, input$parameter2a, input$daterange2)
      bystationmap_fun(bystationmap2_proxy, bystationdat(), stas, input$parameter2b, input$daterange2)
      
      req(map_sel2())
      
      # Highlight selected station on both maps
      addselstationmap_fun(map_sel2()$data)
      
    }
    
  }, ignoreInit = FALSE) |> debounce(500)  # 500ms delay
  
  # Station selection reactive
  map_sel2 <- reactiveVal(NULL)
  
  # Handle marker clicks from map 1
  observeEvent(input$bystationmap1_marker_click, {
    if (!is.null(input$bystationmap1_marker_click)) {
      map_sel2(list(type = "marker", data = input$bystationmap1_marker_click))
    }
  })
  
  # Handle marker clicks from map 2
  observeEvent(input$bystationmap2_marker_click, {
    if (!is.null(input$bystationmap2_marker_click)) {
      map_sel2(list(type = "marker", data = input$bystationmap2_marker_click))
    }
  })
  
  # Synchronize map view when map 1 center/zoom changes
  observeEvent(list(input$bystationmap1_center, input$bystationmap1_zoom), {
    # Add time-based throttling to prevent loops
    current_time <- Sys.time()
    if (!sync_in_progress() && 
        !is.null(input$bystationmap1_center) && 
        !is.null(input$bystationmap1_zoom) &&
        as.numeric(current_time - last_sync_time()) > 0.5) {  # 0.5 second throttle
      
      sync_in_progress(TRUE)
      last_sync_time(current_time)
      
      leaflet::leafletProxy("bystationmap2") %>%
        leaflet::setView(
          lng = input$bystationmap1_center$lng, 
          lat = input$bystationmap1_center$lat, 
          zoom = input$bystationmap1_zoom
        )
      
      # Use reactive timer to reset sync flag
      invalidateLater(200)
      sync_in_progress(FALSE)
    }
  }, ignoreInit = TRUE)
  
  # Synchronize map view when map 2 center/zoom changes
  observeEvent(list(input$bystationmap2_center, input$bystationmap2_zoom), {
    # Add time-based throttling to prevent loops
    current_time <- Sys.time()
    if (!sync_in_progress() && 
        !is.null(input$bystationmap2_center) && 
        !is.null(input$bystationmap2_zoom) &&
        as.numeric(current_time - last_sync_time()) > 0.5) {  # 0.5 second throttle
      
      sync_in_progress(TRUE)
      last_sync_time(current_time)
      
      leaflet::leafletProxy("bystationmap1") %>%
        leaflet::setView(
          lng = input$bystationmap2_center$lng, 
          lat = input$bystationmap2_center$lat, 
          zoom = input$bystationmap2_zoom
        )
      
      # Use reactive timer to reset sync flag
      invalidateLater(200)
      sync_in_progress(FALSE)
    }
  }, ignoreInit = TRUE)
  
  # Highlight selected station on both maps
  observe({
    req(map_sel2())
    addselstationmap_fun(map_sel2()$data)
  }) |> debounce(500)
  
  # Plot for parameter 1
  bystationplo <- reactive({
    
    req(map_sel2())
    req(bystationdat())
    req(input$parameter2a)
    req(input$parameter2b)
    sel <- map_sel2()$data

    out <- bystationplo_fun(sel, bystationdat(), nncdat, input$summarize2, input$parameter2a, input$parameter2b, input$daterange2)  
    
    return(out)
    
  }) |> debounce(1000)
  
  # Toggle sidebar when marker is clicked
  observeEvent(map_sel2(), {
    sidebar_toggle("bystationsidebar", open = TRUE)
  })
  
  # data to download
  dldat <- reactive({
    
    req(input$daterange4)
    
    waterbody4 <- input$waterbody4
    parameter4 <- input$parameter4
    daterange4 <- input$daterange4

    out <- alldat |> 
      dplyr::filter(
        waterbody %in% waterbody4 &
        parameter %in% parameter4 & 
        date >= daterange4[1] & 
        date <= daterange4[2]
        ) |> 
      dplyr::select(-type)
    
    return(out)
    
  })
  
  # reactable table
  dltab <- reactive({
    
    dldat <- dldat()

    out <- dldattab_fun(dldat)
    
    return(out)
    
  })
  
  #####
  # output
  
  output$daterange1 <- renderUI({
    
    req(input$location1)
    
    # inputs
    location1 <- input$location1 
    parameter1 <- input$parameter1
    
    dtchc <- try(datechoice_fun(alldat, location1, parameter1), silent = T)
    
    sliderTextInput("daterange1", "Select Date Range:",
                choices = dtchc, selected = range(dtchc))
    
  })
  
  output$location1 <- renderUI({
    
    parameter1 <- input$parameter1
    
    locsin <- meta |> 
      dplyr::filter(parameter == parameter1) |>
      dplyr::pull(location) |> 
      unique()

    locsel <- locs[locs %in% locsin] 

    out <- selectInput('location1', "Sample location:", choices = locsel)
    
    if(length(locsel) > 1) {
      out
    } else {
      div(style = "display: none;", out)
    }
    
  })
  
  output$parameter2a <- renderUI({
  
    req(dtprmsel())
    
    selectInput('parameter2a', "Select Parameter One:", choices = dtprmsel())
    
  })
  
  output$parameter2b <- renderUI({
    
    req(dtprmsel())
    
    selectInput('parameter2b', "Select Parameter Two:", choices = dtprmsel())
    
  })
  
  output$daterange4 <- renderUI({

    # inputs
    waterbody4 <- input$waterbody4
    parameter4 <- input$parameter4
    
    dtchc <- try(datechoice_fun(alldat, parameter = parameter4, waterbody = waterbody4), silent = T)
    
    if(inherits(dtchc, "try-error")) {
      div(style = "display: none;")
    } else {
      sliderTextInput("daterange4", "Select Date Range:", 
                      choices = dtchc, selected = range(dtchc))
    }
    
  })
  
  output$byareamap <- leaflet::renderLeaflet(bsmap(stas))
  byareamap_proxy <- leaflet::leafletProxy("byareamap")
  output$byareaplo <- highcharter::renderHighchart(byareaplo())
  output$byareagauge <- highcharter::renderHighchart(byareagauge())
  
  output$bystationmap1 <- leaflet::renderLeaflet(bsmap(stas))
  output$bystationmap2 <- leaflet::renderLeaflet(bsmap(stas))
  
  bystationmap1_proxy <- leaflet::leafletProxy("bystationmap1")
  bystationmap2_proxy <- leaflet::leafletProxy("bystationmap2")
  
  output$bystationplo <- renderUI(bystationplo())

  # download table
  output$dltabout <- reactable::renderReactable(dltab())
  
  # download handler
  output$dwnld <- downloadHandler(
    filename = function(){'downloaddata.csv'},
    content = function(file){
      
      # inputs
      dldat <- dldat()
      
      write.csv(dldat, file, quote = T, row.names = F)
      
      session$sendCustomMessage("reset_download_button", "")
      
    }
  )
  
}

shinyApp(ui, server)