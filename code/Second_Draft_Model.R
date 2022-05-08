set_cmdstan_path("C:/Users/Mitchell Gaming PC/Documents/.cmdstanr/cmdstan-2.28.2/cmdstan-2.28.2")
setwd("C:/Users/Mitchell Gaming PC/OneDrive - Visual Risk IQ, LLC/School Files/Spring 2022/Bayesian Stats/Actual Semester Project/Datasources")
library(rethinking)
library(dagitty)
library(readr)
set.seed(42)
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
  ), data=dat , chains=4)

# Prior Predictive Check
prior <- extract.prior(m1)
prior$yes<-prior$a[,1]
prior$no<-prior$a[,2]
prior$prohibited<-prior$a[,3]
dens(prior$yes, lwd=4, col="blue", xlab="Total Effect of Mandate", xlim = c(-3, 3))
abline( v=0, lty=3)
dens(prior$no, lwd=4, col="red", add = TRUE)
dens(prior$prohibited, lwd=4, col="dark green", add = TRUE)
title('Prior for Total Effect of Mandate')
legend("topleft",
       c("Yes","No","Prohibited"),
       fill=c("blue","red","dark green"))

# Implication of this prior:
# Width:
# Index: expectation that regardless of 

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
title("Posterior: Total Effect of Mandate")
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
  ), data=dat , chains=4)

# Visualizing Prior
prior <- extract.prior(m2)
# set up the plot dimensions
plot( NULL , xlim=c(0,1) , ylim=c(0.5,1.5) ,
      xlab="At-Risk Population" , ylab="Incident Rate" )
# draw 50 lines from the prior
title('Prior for Direct Effect of Mandate')
risk_seq <- seq( from=-0.1 , to=1.1 , length.out=30 )
vaccine_seq <-  rep(1, 30)
mu <- link( m2 , post=prior , data= data.frame(R=risk_seq, V = vaccine_seq))
for ( i in 1:50 ) lines( risk_seq , mu[i,] , col=col.alpha("black",0.3) )

# Prior Predictive Check:
# Unreasonably strong associations in m2, for bR. Going to decrease sigma.

m2.1 <- ulam( 
  alist(
    I ~ dnorm( mu , sigma ),
    mu <- a[V] + bR*R,
    a[V] ~ dnorm( 0 , 1 ),
    bR ~ dlnorm( 0 , .5),
    sigma ~ dexp(1)
  ), data=dat , chains=4)

# Visualizing Prior for model 2.1
prior <- extract.prior(m2.1)
# set up the plot dimensions
plot( NULL , xlim=c(-2,2) , ylim=c(-2,2) ,
      xlab="At-Risk Population" , ylab="Incident Rate" )
title('Priors for Direct Effect of Mandate (v2)')
# draw 50 lines from the prior
risk_seq <- seq( from=-2.5 , to=2.5 , length.out=30 )
vaccine_seq <-  rep(1, 30)
mu <- link( m2.1 , post=prior , data= data.frame(R=risk_seq, V = vaccine_seq))
for ( i in 1:50 ) lines( risk_seq , mu[i,] , col=col.alpha("black",0.3) )

# Fit Diagnostics
traceplot(m2.1)

trankplot(m2.1)

precis(m2.1, depth = 2)

postcheck(m2.1)


# Code for just plotting all together
plot( data$R , data$I , pch=16 , col='black' ,
      xlab="At-Risk Population" , ylab="Incident Rate" ,
      xlim=c(-2,2), ylim = c(-3,3))

# Plotting Posterior all together
risk_seq <- seq( from=-2 , to=2 , length.out=30 )

plot( data[which(data$V == 1),]$R , data[which(data$V == 1),]$I , pch=16 , col='red' ,
      xlab="At-Risk Population" , ylab="Incident Rate" ,
      xlim=c(-2,2), ylim = c(-3,3))

points(data[which(data$V == 2),]$R , data[which(data$V == 2),]$I , pch=16 , col='dark green' )

points(data[which(data$V == 3),]$R , data[which(data$V == 3),]$I , pch=16 , col='blue' )

title('Incident Rate for Mandate Levels')

# Draw Yes Lines
mu_yes <- link( m2.1 , data=data.frame( V= 3, R=risk_seq ) )
mu_yes_mean <- apply( mu , 2 , mean )
lines( risk_seq , mu_yes_mean , lwd=2, col = 'blue')
#mu__yes_ci <- apply( mu , 2 , PI , prob=0.97 )
#shade( mu_yes_ci , risk_seq , col=col.alpha(rangi2,0.3) )

# Draw No Line
mu_no <- link( m2.1 , data=data.frame( V=1 , R=risk_seq ) )
mu_no_mean <- apply( mu_no , 2 , mean )
lines( risk_seq , mu_no_mean , lwd=2, col = 'red' )

# Draw Prohibited Line
mu_prohibited <- link( m2.1 , data=data.frame( V=2 , R=risk_seq ) )
mu_prohibited_mean <- apply( mu_prohibited , 2 , mean )
lines( risk_seq , mu_prohibited_mean , lwd=2, col = 'dark green')

# Draw Legend
legend("topleft",
       c("Yes","No","Prohibited"),
       fill=c("blue","red","dark green"))



## Plot Individual Posteriors in Separate Graphs
risk_seq <- seq( from=-2 , to=2 , length.out=30 )
plot( data[which(data$V == 1),]$R , data[which(data$V == 1),]$I , pch=16 , col='black' ,
      xlab="At-Risk Population" , ylab="Incident Rate" ,
      xlim=c(-2,2), ylim = c(-3,3))
title('Incident Rate for Mandate Level = No')
lines( risk_seq , mu_no_mean , lwd=2, col = 'red' )
legend("topleft",
       c("No"),
       fill=c("red"))

plot( data[which(data$V == 2),]$R , data[which(data$V == 2),]$I , pch=16 , col='black' ,
      xlab="At-Risk Population" , ylab="Incident Rate" ,
      xlim=c(-2,2), ylim = c(-3,3))
title('Incident Rate for Mandate Level = Prohibited')
lines( risk_seq , mu_prohibited_mean , lwd=2, col = 'dark green')
legend("topleft",
       c("Prohibited"),
       fill=c("dark green"))

plot( data[which(data$V == 3),]$R , data[which(data$V == 3),]$I , pch=16 , col='black' ,
      xlab="At-Risk Population" , ylab="Incident Rate" ,
      xlim=c(-2,2), ylim = c(-3,3))
title('Incident Rate for Mandate Level = Yes')
lines( risk_seq , mu_yes_mean , lwd=2, col = 'blue')
legend("topleft",
       c("Yes"),
       fill=c("blue"))