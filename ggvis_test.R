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
require(reshape2)

##################
# "DESIGN PHASE" #
##################

#This section is known as the "design phase" 
#because we attempt to balance the covariates
#between the treatment and control groups,
#much like we would in an actual experiment.
#Of course, in an actual experiment,
#we don't know the outcomes, so we also remove them.

# Remove outcomes
data.without.outcomes <- data[,-2]

# ESTIMATE PROPENSITY SCORES

# fit logistic regression with interaction terms and quadratics
fullmodel <- glm(NJ.PA ~ .^2 + I(WagePre^2) + I(EmploymentPre^2),data.without.outcomes,family="binomial")
# stepwise regression is used here because overfitting is not a concern
# our objective is to use propensity scores to balance the covariates
# finding the best possible (overfitted) model allows us to balance covariates most effectively
ps.model <- step(fullmodel, 
                scope = list(lower = ~EmploymentPre+WagePre+BurgerKing+KFC+Roys, upper = ~.)) #stepwise regression to 
ps <- predict(ps.model, type="response") #gets the propensity scores for each unit, based on the model
data.without.outcomes <- cbind(data.without.outcomes,ps, treatment = ifelse(data.without.outcomes$NJ.PA==1,"New Jersey","Pennsylvania"))

# PLOT PROPENSITY SCORES OF TREATMENT AND CONTROL GROUPS
# does group? work? can use in ggplot2
#turns out you can use add_data?
subset(data.without.outcomes, subset = NJ.PA==0)  %>% ggvis(~ps)%>% layer_densities(stroke := "blue", fill := "none")%>%add_data(subset(data.without.outcomes, subset = NJ.PA==1)) %>% layer_densities(stroke :="red", fill := "none") 

#NOTE: VERY PICKY ABOUT WHAT GETS PASSED TO ARGUMENTS
#MUST BE CATEGORICAL FOR GROUP BY TO WORK!
ggvis(data=data.without.outcomes, x = ~ps) %>% group_by(treatment) %>% layer_densities(fill := "none", stroke = ~treatment)
#so it appears that there is good balance

#TRIM DATA
data.trimmed=trimIterate(ps.model,data,data$NJ.PA)

# OBTAIN NEW PROPENSITY SCORE ESTIMATES
data.trimmed.model = glm(formula(ps.model),data.trimmed,family="binomial")
ps.trimmed = predict(data.trimmed.model)

# REBIND PROPENSITY SCORES AND TREATMENT INDICATOR TO DATA
data.trimmed <- cbind(data.trimmed,ps = ps.trimmed, treatment = ifelse(data.trimmed$NJ.PA==1,"New Jersey","Pennsylvania"))

ggvis(data=data.trimmed, x = ~ps) %>% 
  group_by(treatment) %>% 
  layer_densities(fill := "none", stroke = ~treatment) %>% 
  add_axis("x", title = "Linear Propensity Scores")


selectedData <- input_select(choices=c("Untrimmed Data" = data.without.outcomes, "Trimmed Data" = data.trimmed))


cbind(data.trimmed[,c("treatment","ps")],id="trimmed")

ggvis(data=data.trimmed, x = ~ps) %>% 
  group_by(treatment) %>% 
  layer_densities(fill := "none", stroke = ~treatment) %>% 
  add_axis("x", title = "Linear Propensity Scores")







