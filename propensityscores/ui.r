require(shiny)
require(ggvis)

data.casted.dummies <- read.table("data.casted.dummies")
subclasses <- read.table("subclasses")

shinyUI(
  
  fluidPage(
    
    titlePanel("Exploring NJ/PA Minimum Wage Data"),
    
    sidebarLayout(
      
      sidebarPanel(
        sliderInput("slider", "# of subclasses", min = 2, max = 10, value = 2, step = 1)
        ),
      
      mainPanel(
        uiOutput("ggvis_ui"),
        ggvisOutput("ggvis")
        )
      
      )
    
    )
  
  )

