library(shiny)

# Define UI for application
shinyUI(pageWithSidebar(
  
  # Application title
  headerPanel("Spatially referenced seagrass depth of colonization"),
  
  # Sidebar with a slider input for number of observations
  sidebarPanel(
    
    selectInput("segment", 
                label = h3("Select segment"), 
                choices = list(
                  "Old Tampa Bay 902" = "902", 
                  "Big Bend 820" = "820"),
                  selected = "902"),
    
    numericInput("grid_spc", 
                 label = h3("Grid spacing (dec. deg.)"), 
                 min=0.005, 
                 max=0.1, 
                 value=0.02, step = 0.001),
    numericInput("grid_seed", 
                 label = h3("Random seed"), 
                 min=1, 
                 max=5000, 
                 value=1234, step = 1),
    
    uiOutput("reserveControls"),

    numericInput("radius", 
                 label = h3('Radius (dec. deg.)'), 
                 min=0, 
                 max=2, 
                 value=0.04, step = 0.01),
    
  	numericInput("thresh",
  							label = h3('Slope threshold\n(Proportion of all)'), 
                 min=0.01, 
                 max=0.5, 
                 value=0.1, step = 0.01),
   
  	
    h3("Show all estimates"), 
    
    checkboxInput("show_all", 
                  label = '',
                  value = FALSE)
    
#     submitButton("Submit")
    
  ),
  
  # Show a plot of the generated distribution
  mainPanel(
    plotOutput("simplot", width = "100%")
  )
))