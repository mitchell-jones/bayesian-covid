setwd("C:/Users/Mitchell Gaming PC/OneDrive - Visual Risk IQ, LLC/School Files/Spring 2022/Bayesian Stats/Actual Semester Project/Datasources")

## Loading At Risk Population Data

library(readr)
Population_at_Risk <- read_csv("Population at Risk.csv", 
                               skip = 2)
View(Population_at_Risk)
Population_at_Risk <- Population_at_Risk[which(Population_at_Risk$Location != 'United States'),]
Population_at_Risk <- Population_at_Risk[c("Location","At-risk adults, as a share of all adults ages 18 and older")]
Population_at_Risk <- na.omit(Population_at_Risk)


## Loading vaccine Mandate Status as of 2/10
Covid_19_Vaccination_Mandates_as_of_Feb_10 <- read_csv("Covid-19 Vaccination Mandates as of Feb 10.csv", 
                                                       skip = 2)
View(Covid_19_Vaccination_Mandates_as_of_Feb_10)

Covid_19_Vaccination_Mandates_as_of_Feb_10 <- Covid_19_Vaccination_Mandates_as_of_Feb_10[which(Covid_19_Vaccination_Mandates_as_of_Feb_10$Location != 'United States'),]
Covid_19_Vaccination_Mandates_as_of_Feb_10 <- Covid_19_Vaccination_Mandates_as_of_Feb_10[c("Location","Any Mandate in Place?")]
Covid_19_Vaccination_Mandates_as_of_Feb_10 <- na.omit(Covid_19_Vaccination_Mandates_as_of_Feb_10)

## Loading Dependent Variables
Dependent_Vars <- read_csv("Dependent Vars.csv")
Dependent_Vars <- Dependent_Vars[c("Province_State","Incident_Rate")]
colnames(Dependent_Vars) <- c('Location', 'Incident_Rate')

df1 <- merge(Covid_19_Vaccination_Mandates_as_of_Feb_10, Population_at_Risk)
first_draft_data <- merge(df1, Dependent_Vars)
write.csv(first_draft_data, "C:/Users/Mitchell Gaming PC/OneDrive - Visual Risk IQ, LLC/School Files/Spring 2022/Bayesian Stats/Actual Semester Project/Datasources/First_Draft_Data.csv", row.names = FALSE)
