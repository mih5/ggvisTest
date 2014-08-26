require(shiny)
require(ggvis)
data.casted <- read.table("data_casted")

shinyUI(
  
  fluidPage(
    
    titlePanel("Exploring NJ/PA Minimum Wage Data"),
    
    sidebarLayout(
      
      sidebarPanel(
        p("Data from Card and Krueger's", a("study", href = "http://davidcard.berkeley.edu/papers/njmin-aer.pdf"), "on whether an increase in the minimum wage affects employment.  In 1992, New Jersey decided to raise its minimum wage by 80 cents while its neighbor Pennsylvania kept its wage the same.  371 fast-food restaurants were surveyed about the number of people employed before and after the minimum wage was raised, as well as their starting wage."),
        selectInput("select.variableX", "X variable:", choices = names(data.casted)[4:6]),
        selectInput("select.variableY", "Y variable:", choices = names(data.casted)[4:6]),
        selectInput("group.by", "Group by:", choices = names(data.casted)[2:3]),
        p()
        ),
      
      mainPanel(
        uiOutput("ggvis_ui"),
        ggvisOutput("ggvis")
        )
      
      )
    
    )
  
  )

