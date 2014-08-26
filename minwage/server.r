require(shiny)
require(ggvis)
data.casted <- read.table("data_casted")

shinyServer(function(input,output){
     
  observe({
    
      #as.name only works within "prop" context
    
        data.casted %>% 
          ggvis(prop("x", as.name(input$select.variableX)), prop("y", as.name(input$select.variableY))) %>%
          layer_points(prop("fill", as.name(input$group.by)), opacity := 0.4) %>%
          #can't add straight lines! as of yet
          bind_shiny("ggvis", "ggvis_ui")
  })
  }
)