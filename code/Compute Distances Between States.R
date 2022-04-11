install.packages('geodist')

library(geodist)

setwd("C:/Users/Mitchell Gaming PC/OneDrive - Visual Risk IQ, LLC/School Files/Spring 2022/Bayesian Stats/Actual Semester Project/Datasources")

coordinates <- read.csv('State Center Coordinates.csv')

distances <- geodist(coordinates, measure = 'geodesic')

df <- data.frame(distances)
colnames(df) <- coordinates$State
rownames(df) <- coordinates$State

write.csv(df, "C:/Users/Mitchell Gaming PC/OneDrive - Visual Risk IQ, LLC/School Files/Spring 2022/Bayesian Stats/Actual Semester Project/Datasources/Distances From States.csv", row.names = TRUE)
