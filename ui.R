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
                  "Big Bend 820 (2006)" = "820",
                  "Choctawhatchee Bay 303 (2007)" = "303",
                  "Indian River Lagoon 1502 (2009)" = "1502",
                  "Old Tampa Bay 902 (1988)" = "1988_902",
                  "Old Tampa Bay 902 (1990)" = "1990_902",
                  "Old Tampa Bay 902 (1992)" = "1992_902",
                  "Old Tampa Bay 902 (1994)" = "1994_902",
                  "Old Tampa Bay 902 (1996)" = "1996_902",
                  "Old Tampa Bay 902 (1999)" = "1999_902",
                  "Old Tampa Bay 902 (2001)" = "2001_902",
                  "Old Tampa Bay 902 (2004)" = "2004_902",
                  "Old Tampa Bay 902 (2006)" = "2006_902",
                  "Old Tampa Bay 902 (2008)" = "2008_902",
                  "Old Tampa Bay 902 (2010)" = "2010_902"
                  ),
                  selected = "820"),
    
    numericInput("grid_spc", 
                 label = h3("Grid spacing (dec. deg.)"), 
                 min=0.005, 
                 max=0.1, 
                 value=0.02, step = 0.001),
    numericInput("grid_seed", 
                 label = h3("Random seed"), 
                 min=1, 
                 max=5000, 
                 value=123, step = 1),
    
    uiOutput("reserveControls"),

    checkboxInput("point_lab", 
                  label = "Label points as numbers",
                  value = F),
    
    numericInput("radius", 
                 label = h3('Radius (dec. deg.)'), 
                 min=0, 
                 max=2, 
                 value=0.02, step = 0.01),
    
    selectInput("show_all", 
            label = h3("Show all estimates"), 
            choices = list(
              "No" = "nope",
              "Depth with maximum seagrass" = "sg_max",
              "Median depth of colonization" = "doc_med",
              "Maximum depth of colonization" = "doc_max"
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