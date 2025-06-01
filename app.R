library(shiny)
library(bslib)

# Source required files (commented out as we don't have access to them)
# source(here::here('R/global.R'))
source(here::here('R/funcs.R'))

ui <- page_navbar(
  title = "CBA WATER QUALITY DASHBOARD",
  id = "main-nav",
  # bg = '#00806E',
  
  # Add logo
  nav_item(
    tags$img(src = "logo.jpg", height = "30px", style = "margin-right: 10px;")
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
        class = 'card-scroll'#,
        # shiny::includeMarkdown('www/overview.md')   
      ),
      nav_panel(
        title = 'METHODS'#, 
        # shiny::includeMarkdown('www/methods.md')
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
        width = "300px",
        open = "desktop",
        
          selectInput("variable", "Select Variable:", choices = c("Option 1", "Option 2", "Option 3"))
          
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

server <- function(input, output, session) {
  
 
  
}

shinyApp(ui, server)