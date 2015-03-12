library(shiny)

# Define UI for application
shinyUI(fluidPage(
  
  # Application title
  headerPanel("Spatially referenced seagrass depth of colonization"),
  
  # Sidebar with a slider input for number of observations
  sidebarPanel(
  
    width = 3,
    
    selectInput("segment", 
                label = h4("Select segment"), 
                choices = list(
                  "Big Bend 820 (2006)" = "820",
                  "Choctawhatchee Bay 303 (2007)" = "303",
                  "Indian River Lagoon 1502 (2009)" = "1502",
                  "Old Tampa Bay 902 (2010)" = "2010_902"
                  ),
                  selected = "2010_902"),
    
    numericInput("grid_spc", 
                 label = h4("Grid spacing (dec. deg.)"), 
                 min=0.005, 
                 max=0.1, 
                 value=0.02, step = 0.001),
    numericInput("grid_seed", 
                 label = h4("Random seed"), 
                 min=1, 
                 max=5000, 
                 value=123, step = 1),
    
    uiOutput("reserveControls"),

    checkboxInput("point_lab", 
                  label = "Label points as numbers",
                  value = T),
    
    numericInput("radius", 
                 label = h4('Radius (dec. deg.)'), 
                 min=0, 
                 max=2, 
                 value=0.02, step = 0.01),
    
    selectInput("show_all", 
            label = h4("Show all estimates"), 
            choices = list(
              "No" = "nope",
              "Minimum depth of colonization" = "z_cmin",
              "Median depth of colonization" = "z_cmed",
              "Maximum depth of colonization" = "z_cmax"
              ),
              selected = "nope"),
    
    checkboxInput("show_krige", 
              label = "Interpolate smooth surface",
              value = F),

    submitButton("Submit")
    
  ),
  
  # Show a plot of the generated distribution
  mainPanel(
    plotOutput("simplot", width = "100%")
  )
  
))