###########################
# TESTING GGVIS OUT USING #
# MINIMUM WAGE DATA       #
###########################

#SOURCE UTILITY FUNCTIONS
source("~/GitHub/ggvisTest/utility.R")
#READ-IN DATA
data <- read.csv("~/GitHub/ggvisTest/Minimum Wage - Sheet1.csv")
#IMPORT GGVIS
require(ggvis)
require(ggplot2)
require(reshape2)
require(dplyr)

#ADD ID COLUMN TO DATA

data <- cbind(id = seq(1:nrow(data)), data)
observations <- dim(data)[1]

#DESCRIPTION OF RAW DATA
# NJ.PA, indicator for whether or not restaurant is in NJ
# EmploymentPost, EmploymentPre, how many people employed at restaurant at beginning and end of study period
# WagePre, wage before 
# Burger King, KFC, Roys, Wendys, type of restaurant

#TIDY THE DATA
#I wish to know about the measured variables EmploymentPost, EmploymentPre, and Wage broken up by state and restaurant
data.melted.temp <- melt(data, id.vars = c("id","NJ.PA","BurgerKing","KFC","Roys","Wendys")) 
head(data.melted.temp)

#merge restaurant indicators into restaurant variable
restaurantPicker <- function(x) c("Burger King", "KFC", "Roys", "Wendys")[x%*%c(1,2,3,4)]
restaurant <- apply(data.melted.temp[,c("BurgerKing", "KFC", "Roys", "Wendys")], MARGIN =1, restaurantPicker)

data.melted <- cbind(id = data.melted.temp$id, state = ifelse(data.melted.temp$NJ.PA==1, "New Jersey", "Pennsylvania"), restaurant, data.melted.temp[,c("variable", "value")])

head(data.melted)

data.casted <- dcast(data.melted, id + state + restaurant ~ variable)



########################
# EXPLORATORY ANALYSIS #
########################

variableSelect <- input_select(choices=c("# of people employed" = "EmploymentPre", "# of people employed" = "EmploymentPost", "# of people employed" = "WagePre"))


#############################
# EXAMINE COVARIATE BALANCE #
#############################

head(data.casted)
summary(data.casted$restaurant)

#create dummy variables
data.casted.dummies <- mutate(data.casted, Burger_King = ifelse(restaurant == "Burger King", 1, 0),
       KFC = ifelse(restaurant == "KFC", 1, 0),
       Roys = ifelse(restaurant == "Roys", 1, 0),
       Wendys = ifelse(restaurant == "Wendys", 1, 0))
head(data.casted.dummies)

#calculate t-statistics

#uses strange "axis_props" convention
#also has specification of properties similar to svg css - dx, dy, etc.
#geom abline doesn't exist

t_statistics <- welchTStats(data.casted.dummies[,c("EmploymentPre", "WagePre","Burger_King", "KFC", "Roys", "Wendys")], data.casted.dummies$state=="New Jersey")
data.frame(t_statistics, covariates = names(t_statistics)) %>% 
  ggvis(x = ~t_statistics, y = ~covariates) %>% 
  group_by(covariates) %>% layer_points() %>% 
  add_axis("y", title = "Covariates", 
           properties = axis_props(labels = list(fontSize=14), 
                                   title = list(fontSize=14, dy=-30))) %>% 
  add_axis("x", title = "Standardized Difference in Covariate Means", 
           properties = axis_props(labels = list(fontSize=14), 
                                   title = list(fontSize=14, dy=30))) 

##################
# "DESIGN PHASE" #
##################

#This section is known as the "design phase" 
#because we attempt to balance the covariates
#between the treatment and control groups,
#much like we would in an actual experiment.


## Interaction terms and quadratics
fullmodel <- glm(state ~ .^2 + I(WagePre^2) + I(EmploymentPre^2),data.casted.dummies[,-1],family="binomial")
ps.model = step(fullmodel, 
                scope = list(lower = ~EmploymentPre+WagePre+Burger_King+KFC+Roys, upper = ~.)) 

#stepwise regression is generally bad practice, but there we're not doing inference on propensity score, just using it to balance covariates
ps = predict(ps.model, type="response") 
#gets the propensity scores for each unit, based on the model


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

head(subclasses)

welchTStats(data.casted.dummies[,c("EmploymentPre", "WagePre","Burger_King", "KFC", "Roys", "Wendys")], data.casted.dummies$state=="New Jersey")

welchTStats(data.casted.dummies[,c("EmploymentPre", "WagePre","Burger_King", "KFC", "Roys", "Wendys")], data.casted.dummies$state=="New Jersey", subclass=subclasses[2,])

t_statistics_subclasses <- welchTStats(data.casted.dummies[,c("EmploymentPre", "WagePre","Burger_King", "KFC", "Roys", "Wendys")], data.casted.dummies$state=="New Jersey", subclass=subclasses[4,])

data.frame(t_statistics_subclasses=t(t_statistics_subclasses), covariates = names(t_statistics)) %>% 
  ggvis(x = ~t_statistics_subclasses, y = ~covariates) %>% 
  group_by(covariates) %>% layer_points() %>% 
  #overrides scale limits
  scale_numeric("x", domain = c(-3, 3)) %>%
  add_axis("y", title = "Covariates", 
           properties = axis_props(labels = list(fontSize=14), 
                                   title = list(fontSize=14, dy=-30))) %>% 
  add_axis("x", title = "Standardized Difference in Covariate Means", 
           properties = axis_props(labels = list(fontSize=14), 
                                   title = list(fontSize=14, dy=30))) 


