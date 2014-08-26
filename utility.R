##########################
# SOME UTILITY FUNCTIONS #
##########################


##trimIterate
##function that trims the dataset so that there are no controls above treated and no treated below controls
##using linear propensity scores
trimIterate = function(model,data,W){
  ##trimIterate takes the following parameters:
  ##model: a model object from variable section
  ##data: the data without the outcomes
  ##W: the assignment vector
  
  condition = TRUE
  iter = 0
  
  while(condition){
    
    iter = iter +1
    print(paste("iteration:",iter))
    
    ## get linear propensity scores
    ps=predict(model)
    
    ##get which observations are controls below treated or treated above controls
    controls.below.treated = ps < min(ps[W==1])
    treated.above.controls = ps > max(ps[W==0])
    
    
    ##check condition
    if((sum(controls.below.treated)+sum(treated.above.controls)<1)||iter>10){
      condition=FALSE
    }
    else{
      ##trim data and outcomes
      exclude=(1-controls.below.treated)&(1-treated.above.controls)
      data = data[exclude,]
      W = W[exclude]
      
      ##refit model
      model = glm(formula(model),data=data,family="binomial")
    }
    
  }
  
  return(data)
}

# plotCovariateBalance
# function which plots the "covariate balance" between the treatment and control groups
# a way of assessing how well our "design" balances covariates
# This function is an adaptation of 


# functions for calculating welch t-statistics

diffMeans <-  function(values,group_indicators) {
  (mean(values[group_indicators==1])-mean(values[group_indicators==0]))}

welchStandardError <- function(values, group_indicators) {
  sqrt(var(values[group_indicators==0],na.rm=TRUE)/sum(group_indicators==0)+
         var(values[group_indicators==1],na.rm=TRUE)/sum(group_indicators==1)) 
}

welchTStat <- function(values,group_indicators) {
  (mean(values[group_indicators==1])-mean(values[group_indicators==0]))/welchStandardError(values, group_indicators)
}

welchTStats <- function(data, group_indicators, subclass = NULL) {
  var_names = names(data)
  print(class(data))
  value_matrix = data
  if(is.null(subclass)){
  result = apply(value_matrix, MARGIN=2, welchTStat, group_indicators=group_indicators)
  return(result)
  }
  else{
    J = length(table(subclass))
    K = dim(value_matrix)[2]
    n = dim(value_matrix)[1]
    denominators = apply(value_matrix, MARGIN = 2, welchStandardError, group_indicators = group_indicators)
    diffs = matrix(NA, nrow=J, ncol=K)
    for (j in 1:J) for (k in 1:K) diffs[j,k] = diffMeans(value_matrix[subclass==j,k], group_indicators[subclass==j])
    weights = table(subclass)/n
    overall.diffs = weights%*%diffs
    result = overall.diffs/denominators
    return(result)
  }
}











plotCovariateBalance <- function(value_matrix, group_indicators, subclass = NULL){
  if (is.null(subclass)){
    t_statistics <- welchTStats(value_matrix, group_indicators)
    data.frame(t_statistics, covariates = names(t_statistics)) %>% 
      ggvis(x = ~t_statistics, y = ~covariates) %>% 
      group_by(covariates) %>% layer_points() %>% 
      add_axis("y", title = "Covariates", 
               properties = axis_props(labels = list(fontSize=14), 
                                       title = list(fontSize=14, dy=-30))) %>% 
      add_axis("x", title = "Standardized Difference in Covariate Means", 
               properties = axis_props(labels = list(fontSize=14), 
                                       title = list(fontSize=14, dy=30))) 
  }
  if (!is.null(subclass)){
    
  }
} 
