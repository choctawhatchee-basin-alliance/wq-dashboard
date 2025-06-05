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
            
          # Main content area with side-by-side visualizations
          layout_sidebar(
            sidebar = sidebar(
              "right content",
              width = "50%",
              position = "right",
              open = FALSE
            ),
            border = FALSE,
            leaflet::leafletOutput('byareamap', height = "100%")
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
          sidebar = sidebar(
            title = "Controls",
            border_radius = FALSE, 
            fillable = TRUE,
            width = "400px",
            open = "desktop",
            
            selectInput("variable2", "Select Variable:", choices = c("Option 1", "Option 2", "Option 3"))
            
          ),
          
        # Main content area with side-by-side visualizations
        layout_sidebar(
          sidebar = sidebar(
            "right content",
            width = "50%",
            position = "right",
            open = FALSE
          ),
          border = FALSE,
          "middle content"
          
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
          
          # Main content area with side-by-side visualizations
          layout_sidebar(
            sidebar = sidebar(
              "right content",
              width = "50%",
              position = "right",
              open = FALSE
            ),
            border = FALSE,
            "middle content"
            
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
      
      # Main content area with side-by-side visualizations
      layout_sidebar(
        sidebar = sidebar(
          "right content",
          width = "50%",
          position = "right",
          open = FALSE
        ),
        border = FALSE,
        "middle content"
        
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
  values <- reactiveValues(
    updating = FALSE
  )
  
  # Single observer for all inputs
  observeEvent(list(input$summarize1, input$parameter1, input$location1, input$summstat1, input$daterange1, input$`main-nav`), {
    
    if(input$`main-nav` == 'byarea' && !values$updating){
      
      req(input$location1)
      req(input$daterange1)
      
      # Set flag to prevent concurrent updates
      values$updating <- TRUE
      
      byareamap_fun(byareamap_proxy, alldat, cbawbid, stas, input$summarize1, input$summstat1, 
                    input$location1, input$parameter1, input$daterange1)
      
      # reset flag after brief delay
      later::later(function() {
        values$updating <- FALSE
      }, 0.1)  # 100ms delay
      
    }
    
  }, ignoreInit = FALSE) 
  
  #####
  # output
  
  output$location1 <- renderUI({
    
    parameter1 <- input$parameter1
    
    locsin <- meta |> 
      dplyr::filter(parameter == parameter1) |>
      dplyr::pull(location) |> 
      unique()

    locsel <- locs[locs %in% locsin] 

    selectInput('location1', "Sample location:", choices = locsel)
    
  })
  
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
  
  output$byareamap <- leaflet::renderLeaflet(bsmap(stas))
  byareamap_proxy <- leaflet::leafletProxy("byareamap")

}

shinyApp(ui, server)