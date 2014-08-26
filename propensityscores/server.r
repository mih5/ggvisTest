require(shiny)
require(ggvis)

source("utility.R")
data.casted.dummies <- read.table("data.casted.dummies")



shinyServer(function(input,output){
  

  #find subclasses for 2-10 subclasses
  subclasses <- matrix(nrow = 9, ncol = observations)
  for(i in 2:10){
    subclass.breaks <- quantile(ps, c(seq(1/i,1-1/i,1/i)) )
    subclass <- rep(length(subclass.breaks)+1, length(ps))
    for (j in length(subclass.breaks):1){
      subclass[ps <= subclass.breaks[j]] = j
    }
    subclasses[i-1,] <- subclass
  }
  
  t_statistics <- welchTStats(data.casted.dummies[,c("EmploymentPre", "WagePre","Burger_King", "KFC", "Roys", "Wendys")], data.casted.dummies$state=="New Jersey")
     
  observe({
    
    t_statistics_subclasses <- welchTStats(data.casted.dummies[,c("EmploymentPre", "WagePre","Burger_King", "KFC", "Roys", "Wendys")], data.casted.dummies$state=="New Jersey", subclass=subclasses[input$slider,])
    
    data <- data.frame(tstats = t(t_statistics_subclasses), covariates = names(t_statistics)) 
    names(data) <- c("tstats","covariates")
    
    print(data)
    
    data %>% 
      ggvis(x = ~tstats, y = ~covariates) %>% 
      group_by(covariates) %>% layer_points() %>% 
      #overrides scale limits
      scale_numeric("x", domain = c(-3, 3)) %>%
      add_axis("y", title = "Covariates", 
               properties = axis_props(labels = list(fontSize=14), 
                                       title = list(fontSize=14, dy=-30))) %>% 
      add_axis("x", title = "Standardized Difference in Covariate Means", 
               properties = axis_props(labels = list(fontSize=14), 
                                       title = list(fontSize=14, dy=30))) %>%
      bind_shiny("ggvis", "ggvis_ui")
      
  })
  }
)