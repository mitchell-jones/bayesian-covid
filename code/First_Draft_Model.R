set_cmdstan_path("C:/Users/Mitchell Gaming PC/Documents/.cmdstanr/cmdstan-2.28.2/cmdstan-2.28.2")
setwd("C:/Users/Mitchell Gaming PC/OneDrive - Visual Risk IQ, LLC/School Files/Spring 2022/Bayesian Stats/Actual Semester Project/Datasources")
library(rethinking)
library(dagitty)
library(readr)

### Loading the Data
data <- read_csv('First_Draft_Data.csv')

### Drawing the DAG
g<- dagitty('dag {
bb="0,0,1,1"
"Proximity to Other States" [latent,pos="0.551,0.143"]
"incident rate" [outcome,pos="0.847,0.370"]
"proportion of >18 at risk" [pos="0.555,0.629"]
"status of vaccine mandate" [exposure,pos="0.304,0.375"]
"Proximity to Other States" -> "incident rate"
"proportion of >18 at risk" -> "incident rate"
"proportion of >18 at risk" -> "status of vaccine mandate"
"status of vaccine mandate" -> "incident rate"
}')
plot(g)

### Based on DAG, Initial Model (Without Distance/Covariance)
colnames(data)

dat <- list(
  R = data['At-risk adults, as a share of all adults ages 18 and older'],
  I = data['Incident_Rate'],
  V = data['Any Mandate in Place?'])

### Justifying Priors
## Outcome:
# Normally distributed Incident Rate (Number of Covid-19 Cases per 1000 population)

## At Risk Adults:
# Justified by prior scientific study of at-risk individuals in the United States
# by possession of 1 or more chronic conditions. Found that ~50% of Americans suffer,
# according to (https://www.cdc.gov/pcd/issues/2020/20_0130.htm#T1_down)

## Vaccine Mandates
# Index variable.
# 1: 'No' Mandate.
# 2: 'Prohibited' legally from creating a mandate in this state
# 3: 'Yes' that a mandate has been enacted

### Initial Model Fit to Data (Will Sample Priors from Here)
m1 <- ulam( 
  alist(
    I ~ dnorm( mu , sigma ),
    mu <- a + bR*R[i],
    a ~ dnorm( 0 , 1 ),
    bR ~ dnorm( 0 , 1),
    sigma ~ dexp(1)
  ), data=dat , chains=4 , log_lik=TRUE )

## Confused about setting priors for coeffiencets - does ULAM and Stan standardize data automatically?
