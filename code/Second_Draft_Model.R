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

### Standardizing the data
data$R <- standardize(data['At-risk adults, as a share of all adults ages 18 and older'])
data$I <- standardize(data['Incident_Rate'])
data$V <- ifelse(data['Any Mandate in Place?']=="Yes", 1 , ifelse(data['Any Mandate in Place?']=="Prohibited" , 3 ,  2))

### Based on DAG, Initial Model (Without Distance/Covariance)
colnames(data)

dat <- list(
  R = data$R,
  I = data$I,
  V = as.integer(data$V))

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
    mu <- a[V],
    a[V] ~ dnorm( 0 , 1 ),
    sigma ~ dexp(1)
  ), data=dat , chains=4 , log_lik=TRUE )

# Prior Predictive Check
prior <- extract.prior(m1)
prior$yes<-prior$a[,1]
prior$no<-prior$a[,2]
prior$prohibited<-prior$a[,3]
dens(prior$yes, lwd=4, col="blue", xlab="Total Effect of Mandate", xlim = c(-3, 3))
abline( v=0, lty=3)
dens(prior$no, lwd=4, col="red", add = TRUE)
dens(prior$prohibited, lwd=4, col="dark green", add = TRUE)
legend("topleft",
       c("Yes","No","Prohibited"),
       fill=c("blue","red","dark green"))

# Implication of this prior:
# Width:
# Index: expectation that regardless of 

# Question: Could we set different priors for each level of the index variable?


# Look at Trace and Rank Plots for diagnostic concerns 
traceplot(m1)

trankplot(m1)

precis(m1, depth = 2)

postcheck(m1)

# Visualizing Total Effect of Mandate
post1 <- extract.samples(m1)
post1$yes<-post1$a[,1]
post1$no<-post1$a[,2]
post1$prohibited<-post1$a[,3]
dens(post1$yes, lwd=4, col="blue", xlab="Total Effect of Mandate", xlim = c(-1.5, 1.5))
abline( v=0, lty=3)
dens(post1$no, lwd=4, col="red", add = TRUE)
dens(post1$prohibited, lwd=4, col="dark green", add = TRUE)
legend("topleft",
       c("Yes","No","Prohibited"),
       fill=c("blue","red","dark green"))


# Direct Effect of Mandate on Incident Rate (Still a work in progress)
m2 <- ulam( 
  alist(
    I ~ dnorm( mu , sigma ),
    mu <- a[V] + bR*R,
    a[V] ~ dnorm( 0 , 1 ),
    bR ~ dlnorm( 0 , 1),
    sigma ~ dexp(1)
  ), data=dat , chains=4 , log_lik=TRUE )

# Explanation of Prior:


# Direct Effect Prior Predictive Check

# Fit Diagnostics

# This needs to be converted into visualizing the linear regression
# not just density of intercepts.
post2 <- extract.samples(m2)
post2$yes<-post2$a[,1]
post2$no<-post2$a[,2]
post2$prohibited<-post2$a[,3]
dens(post2$yes, lwd=4, col=2, xlab="Total Effect of Yes Mandate")
abline( v=0, lty=3)
dens(post2$no, lwd=4, col=2, xlab="Total Effect of No Mandate")
abline( v=0, lty=3)
dens(post2$prohibited, lwd=4, col=2, xlab="Total Effect of Prohibited Mandate")
abline( v=0, lty=3)

postcheck(m2)

# Notes from Talking to Ryan
# Can set priors from common knowledge - but not necessary.
# Want to see how wide priors should be (after mean-centering).
# Choose three different priors and show different posteriors & can come to conclusion that width of priors
#       doesn't have an effect on the result of the model. But setting this width can have very different results

# then add in spatial covariation and see if it has an effect.