library(shiny)
library(bslib)

# Source required files (commented out as we don't have access to them)
source(here::here('R/global.R'))
source(here::here('R/funcs.R'))

ui <- page_navbar(
  title = "CBA WATER QUALITY DASHBOARD",
  id = "main-nav",

  # Add logo
  nav_item(
    tags$img(src = "logo.png", height = "30px", style = "margin-right: 10px;")
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
        class = 'card-scroll',
        layout_sidebar(
          shiny::includeMarkdown('doc/overview.md'),
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
            width = "400px",
            open = "desktop",
            radioButtons("summarize1", "Summarize By:", choices = c("WBID", "Station"), selected = "WBID"), 
            selectInput("parameter1", "Select Parameter:", choices = prms), 
            uiOutput("location1"),
            selectInput('summstat1', "Summarize as:", choices = c("Mean", "Median", "Max", "Min")),
            uiOutput("daterange1")
          ),
          layout_sidebar(
            border = FALSE,
            leaflet::leafletOutput('byareamap', height = "100%"),
            sidebar = sidebar(
              id = "byareasidebar",
              plotly::plotlyOutput('byareaplo', height = "calc(100vh - 300px)"),
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
  
  #####
  # Third nav item - by station
  nav_panel(
    title = "2 BY STATION",
    value = 'bystation',
    navset_card_underline(
      full_screen = TRUE,
      height = '100%',
      nav_panel(
        title = "DATA",
        class = 'card-scroll',
        layout_sidebar(
          border = FALSE,
          sliderInput("daterange2", "Select Date Range:", 
                      min = dtrng[1], max = dtrng[2], 
                      value = dtrng, timeFormat = "%Y-%m-%d", width = '50%'),
          leaflet::leafletOutput('bystationmap', height = "calc(100vh - 370px)"),
          width = '50%', 
          position = 'left',
          open = TRUE,
          sidebar = sidebar(
            id = "bystationsidebar",
            div(
              style = "display: flex; align-items: center; gap: 1rem;",
              uiOutput('parameter2a'),
              uiOutput('parameter2b'),
              uiOutput('summarize2'),
            ),
            plotly::plotlyOutput('bystationplo', height = "calc(100vh - 300px)"),
            border_radius = FALSE, 
            fillable = TRUE,
            width = "50%",
            open = FALSE,
            position = 'right'
          ),
        )
      ), 
      nav_panel(
        title = "HOW TO USE",
        class = 'card-scroll',
        NULL
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
            width = "400px",
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
  
  #####
  # Fifth nav item - download
  nav_panel(
    title = "DOWNLOAD",
    value = 'download',
    layout_sidebar(
      sidebar = sidebar(
        title = "Controls",
        border_radius = FALSE, 
        fillable = TRUE,
        width = "400px",
        open = "desktop",
        selectInput("variable4", "Select Variable:", choices = c("Option 1", "Option 2", "Option 3"))
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
  
  # reactive value to prevent double firing
  values1 <- reactiveValues(
    updating = FALSE
  )
  
  values2 <- reactiveValues(
    updating = FALSE
  )
  
  # by area map initialize
  observeEvent(list(input$summarize1, input$parameter1, input$location1, input$summstat1, input$daterange1, input$`main-nav`), {
    
    if(input$`main-nav` == 'byarea' && !values1$updating){
      
      req(input$location1)
      req(input$daterange1)
      
      # Set flag to prevent concurrent updates
      values1$updating <- TRUE
      
      byareamap_fun(byareamap_proxy, alldat, cbawbid, stas, input$summarize1, input$summstat1, 
                    input$location1, input$parameter1, input$daterange1)
      
      # reset flag after brief delay
      later::later(function() {
        values1$updating <- FALSE
      }, 0.1)  # 100ms delay
      
      req(map_sel1())
      
      addselareamap_fun(map_sel1()$data, cbawbid)
      
    }
    
  }, ignoreInit = FALSE) 
  
  # for area map clicks
  map_sel1 <- reactiveVal(NULL)
  
  # handle shape clicks
  observeEvent(input$byareamap_shape_click, {
    if (!is.null(input$byareamap_shape_click)) {
      map_sel1(list(type = "shape", data = input$byareamap_shape_click))
    }
  })
  
  # handle marker clicks  
  observeEvent(input$byareamap_marker_click, {
    if (!is.null(input$byareamap_marker_click)) {
      map_sel1(list(type = "marker", data = input$byareamap_marker_click))
    }
  })
  
  # add highlight of shape to byareamap if map_sel1
  observe({
    
    req(map_sel1())
    
    addselareamap_fun(map_sel1()$data, cbawbid)
 
  })
  
  # by area plot
  byareaplo <- reactive({
    
    req(map_sel1())
    
    sel <- map_sel1()
    
    if (sel$type == "shape") {
      shape_click <- sel$data
      marker_click <- NULL
    } else {
      shape_click <- NULL
      marker_click <- sel$data
    }
    
    # Set flag to prevent concurrent updates
    values2$updating <- TRUE
    
    out <- byareaplo_fun(shape_click, marker_click, alldat, stas, 
              input$summarize1, input$summstat1, input$location1, input$parameter1,
              input$daterange1)
    
    # reset flag after brief delay
    later::later(function() {
      values2$updating <- FALSE
    }, 0.1)  # 100ms delay
    
    return(out)    
    
  })
  
  # toggle areamap open sidebar, polygon or marker
  observeEvent(input$byareamap_shape_click, {
    sidebar_toggle("byareasidebar", open = TRUE)
  })
  observeEvent(input$byareamap_marker_click, {
    sidebar_toggle("byareasidebar", open = TRUE)
  })
  
  # station map
  
  # by station map initialize
  observeEvent(list(input$daterange2, input$`main-nav`), {
    
    if(input$`main-nav` == 'bystation'){
      
      bystationmap_fun(bystationmap_proxy, stas, input$daterange2)
      
      req(map_sel2())
      
      addselstationmap_fun(map_sel2()$data)
      
    }
    
  }, ignoreInit = FALSE)
  
  # for station map clicks
  map_sel2 <- reactiveVal(NULL)
  
  # handle shape clicks
  observeEvent(input$bystationmap_marker_click, {
    if (!is.null(input$bystationmap_marker_click)) {
      map_sel2(list(type = "marker", data = input$bystationmap_marker_click))
    }
  })
  
  # add highlight of marker to bystationmap if map_sel2
  observe({
    
    req(map_sel2())
    
    addselstationmap_fun(map_sel2()$data)
    
  })
  
  # reactive for parameter station selections
  stationprmsel <- reactive({
    
    req(map_sel2())

    stationprmsel_fun(map_sel2()$data)
    
  })
  
  # by station plot
  bystationplo <- reactive({
    
    req(map_sel2())
    req(input$parameter2a)
    req(input$parameter2b)
    
    sel <- map_sel2()$data

    out <- bystationplo_fun(sel, alldat,
                         input$summarize2, input$parameter2a,
                         input$parameter2b, input$daterange2)
    
    return(out)    
    
  })
  
  # toggle stationmap open sidebar, marker
  observeEvent(input$bystationmap_marker_click, {
    sidebar_toggle("bystationsidebar", open = TRUE)
  })
  
  #####
  # output
  
  output$daterange1 <- renderUI({
    
    req(input$location1)
    
    # inputs
    location1 <- input$location1 
    parameter1 <- input$parameter1
    
    dtrng <- meta |> 
      dplyr::filter(location == location1 & parameter == parameter1) |> 
      dplyr::select(datestr, dateend)
    dtrng <- range(c(dtrng$datestr, dtrng$dateend))
    
    sliderInput("daterange1", "Select Date Range:", 
                min = dtrng[1], max = dtrng[2], 
                value = dtrng, timeFormat = "%Y-%m-%d")
    
  })
  
  output$location1 <- renderUI({
    
    parameter1 <- input$parameter1
    
    locsin <- meta |> 
      dplyr::filter(parameter == parameter1) |>
      dplyr::pull(location) |> 
      unique()

    locsel <- locs[locs %in% locsin] 

    selectInput('location1', "Sample location:", choices = locsel)
    
  })
  
  output$parameter2a <- renderUI({
    
    req(stationprmsel())
    
    selectInput('parameter2a', "Parameter 1:", choices = stationprmsel())
    
  })
  
  output$parameter2b <- renderUI({
    
    req(stationprmsel())
    
    selectInput('parameter2b', "Parameter 2:", choices = stationprmsel())
    
  })
  
  output$summarize2 <- renderUI({
    
    req(map_sel2())
    
    selectInput("summarize2", "Summarize By:", 
                choices = c("day", "week", "month", "year"), 
                selected = "day")
    
  })
  
  output$byareamap <- leaflet::renderLeaflet(bsmap(stas))
  byareamap_proxy <- leaflet::leafletProxy("byareamap")
  output$byareaplo <- plotly::renderPlotly(byareaplo())
  
  output$bystationmap <- leaflet::renderLeaflet(bsmap(stas))
  bystationmap_proxy <- leaflet::leafletProxy("bystationmap")
  output$bystationplo <- plotly::renderPlotly(bystationplo())
  
}

shinyApp(ui, server)