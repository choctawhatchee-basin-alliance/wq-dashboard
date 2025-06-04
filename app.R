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
        shiny::includeMarkdown('www/methods.md')
      )
    )
  ),
  
  # Second nav item - by area
  nav_panel(
    title = "1 BY AREA",
    value = 'byarea',
    layout_sidebar(
      sidebar = sidebar(
        title = "Controls",
        border_radius = FALSE, 
        fillable = TRUE,
        width = "400px",
        open = "desktop",
        
          radioButtons("summarize1", "Summarize By:", choices = c("WBID", "Station"), selected = "WBID"), 
          selectInput('summstat1', "Summarize as:", choices = c("Mean", "Median", "Max", "Min")),
          selectInput('location1', "Sample location:", choices = list('Surface' = 'surf', 'Bottom' = 'bott')),
          selectInput("parameter1", "Select Parameter:", choices = prms), 
          sliderInput("daterange1", "Select Date Range:", min = dtrng[1], max = dtrng[2], value = dtrng, 
                     timeFormat = "%Y-%m-%d")
          
        ),
        
        # Main content area with side-by-side visualizations
        layout_sidebar(
          sidebar = sidebar(
            "right contents",
            width = "50%",
            position = "right",
            open = FALSE
          ),
          border = FALSE,
          leaflet::leafletOutput('byareamap', height = "100%")
        )
      )
      
  ),
  
  #####
  # Third nav item - by station
  nav_panel(
    title = "2 BY STATION",
    value = 'bystation',
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
          "right contents",
          width = "50%",
          position = "right",
          open = FALSE
        ),
        border = FALSE,
        "middle content"
        
      )
    )
    
  ),
  
  #####
  # Fourth nav item - continuous data
  nav_panel(
    title = "3 CONTINUOUS DATA",
    value = 'continuousdata',
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
          "right contents",
          width = "50%",
          position = "right",
          open = FALSE
        ),
        border = FALSE,
        "middle content"
        
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
          "right contents",
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
  observeEvent(list(input$summarize1, input$summstat1, input$location1, input$parameter1, input$daterange1, input$`main-nav`), {
    
    if(input$`main-nav` == 'byarea'){
      
      # inputs
      byareadat <- try(byareadat_fun(alldat, 
                                 cbawbid, 
                                 stas, 
                                 input$summarize1, 
                                 input$summstat1, 
                                 input$location1, 
                                 input$parameter1, 
                                 input$daterange1
      ))
      
      byareamap_fun(byareamap_proxy, byareadat, input$summarize1)
      
    }
   
  })
  
  #####
  # output
  
  output$byareamap <- leaflet::renderLeaflet(bsmap(stas))
  byareamap_proxy <- leaflet::leafletProxy("byareamap")

}

shinyApp(ui, server)