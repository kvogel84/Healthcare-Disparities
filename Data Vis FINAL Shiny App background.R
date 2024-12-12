library(readxl)

Data <- read_excel("C:/Users/KPVog/OneDrive/Documents/ER visit risks for Shiny App.xlsx")

library(dplyr)

library(ggplot2)

ER_Visit <- Data %>% filter(`Measure Names`== "Estimate")

library(ggplot2)

ER_2019 <- ER_Visit %>% 
  filter(Year == 2019), 

ER_2019a <- ER_2019 %>% select(-Outcome, -Suppressed, -`Type of ED Visit`, -`Measure Names`)

ER_Visitrisk <- ER_Visit %>% select(-Outcome, -Suppressed, -`Type of ED Visit`, -`Measure Names`)

ER_Visitrisk$`Characteristics_Characteristic Levels` <- interaction(ER_Visitrisk$Characteristics, ER_2019a$`Characteristic Levels`)

ggplot(ER_2019a, aes(x = 'Characteristics', y = 'Measure Values', color = "Characteristic Level")) + 
  geom_point(size = 4) 


labs(title = "ER vists based on Race, Income, and Age", 
     x = "Characteristics", y = "Rate of ED Visits per 100,000 Population") 

# Correcting the column name reference
ER_2019a$`Characteristics_Characteristic Levels` <- interaction(ER_2019a$Characteristics, ER_2019a$`Characteristic Levels`)

ggplot(ER_2019a, aes(x = 'Characteristics', y = 'Measure Values', color = "Characteristics_Characteristic Level")) + 
  geom_point(size = 4) 

print(ER_2019sub)

structure(ER_2019sub)


# Create Grouped Bar Chart
ggplot(ER_2019a, aes(x = Characteristics, y = `Measure Values`, fill = 'Characteristic Levels')) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Plot",
       x = "Category", y = "Value") +
  theme_minimal()


ggplot(ER_Visitrisk, aes(x = 'Year', y = 'Measure Values', color = "Characteristics_Characteristic Level")) + 
  geom_point() 

ER2019_age <- Data %>% filter(Year == 2019, Characteristics == "Patient Age Group") + 

ER2019_age1 <- ER2019_age %>% select(-"Type of ED Visit", -Outcome, -"Measure Names")

library(ggplot2)

ggplot(ER2019_age1, aes(x="Characteristic Levels", y="Measure Values"))

# Assuming ER2019_age1 is your dataset

install.packages("plotly")
library(plotly)

p <- ggplot(ER2019_age1, aes(x = `Characteristic Levels`, y = `Measure Values`)) +
  geom_bar(stat = "identity", fill = "lightblue") +
  labs(title = "ER Visits by Age Group in 2019", x = "Age Group", y = "ER Visits Per 100,000 Population") +
  theme_classic()

ggplotly(p)

ER2019_Income <- Data %>% filter(Year == 2019, Characteristics == "Patient Community-level Income")

ER2019_income1 <- ER2019_Income %>% select(-"Type of ED Visit", -Outcome, -"Measure Names")

ggplot(ER2019_income1, aes(x = `Characteristic Levels`, y = `Measure Values`)) +
  geom_bar(stat = "identity", fill = "lightgreen") +
  labs(title = "Bar Plot", x = "Characteristic Levels", y = "Measure Values") +
  theme_minimal()

ER2019_race <- Data %>% filter(Year == 2019, Characteristics == "Patient Race/Ethnicity")

ER2019_race1 <- ER2019_race %>% select(-"Type of ED Visit", -Outcome, -"Measure Names")

ggplot(ER2019_race1, aes(x = `Characteristic Levels`, y = `Measure Values`)) +
  geom_bar(stat = "identity", fill = "pink") +
  labs(title = "Bar Plot", x = "Characteristic Levels", y = "Measure Values") +
  theme_minimal()

############

ER2020_age <- Data %>% filter(Year == 2020, Characteristics == "Patient Age Group")  
  
  ER2020_age1 <- ER2020_age %>% select(-"Type of ED Visit", -Outcome, -"Measure Names")

ggplot(ER2020_age1, aes(x = `Characteristic Levels`, y = `Measure Values`)) +
  geom_bar(stat = "identity", fill = "lightblue") +
  labs(title = "Bar Plot", x = "Characteristic Levels", y = "Measure Values") +
  theme_minimal()

ER2020_Income <- Data %>% filter(Year == 2020, Characteristics == "Patient Community-level Income")

ER2020_income1 <- ER2020_Income %>% select(-"Type of ED Visit", -Outcome, -"Measure Names")

ggplot(ER2020_income1, aes(x = `Characteristic Levels`, y = `Measure Values`)) +
  geom_bar(stat = "identity", fill = "lightgreen") +
  labs(title = "Bar Plot", x = "Characteristic Levels", y = "Measure Values") +
  theme_minimal()

ER2020_race <- Data %>% filter(Year == 2020, Characteristics == "Patient Race/Ethnicity")

ER2020_race1 <- ER2020_race %>% select(-"Type of ED Visit", -Outcome, -"Measure Names")

ggplot(ER2020_race1, aes(x = `Characteristic Levels`, y = `Measure Values`)) +
  geom_bar(stat = "identity", fill = "pink") +
  labs(title = "Bar Plot", x = "Characteristic Levels", y = "Measure Values") +
  theme_minimal()

##############

ER2021_age <- Data %>% filter(Year == 2021, Characteristics == "Patient Age Group")  

ER2021_age1 <- ER2020_age %>% select(-"Type of ED Visit", -Outcome, -"Measure Names")

ggplot(ER2021_age1, aes(x = `Characteristic Levels`, y = `Measure Values`)) +
  geom_bar(stat = "identity", fill = "lightblue") +
  labs(title = "Bar Plot", x = "Characteristic Levels", y = "Measure Values") +
  theme_minimal()

ER2021_Income <- Data %>% filter(Year == 2021, Characteristics == "Patient Community-level Income")

ER2021_income1 <- ER2021_Income %>% select(-"Type of ED Visit", -Outcome, -"Measure Names")

ggplot(ER2021_income1, aes(x = `Characteristic Levels`, y = `Measure Values`)) +
  geom_bar(stat = "identity", fill = "lightgreen") +
  labs(title = "Bar Plot", x = "Characteristic Levels", y = "Measure Values") +
  theme_minimal()

ER2021_race <- Data %>% filter(Year == 2021, Characteristics == "Patient Race/Ethnicity")

ER2021_race1 <- ER2021_race %>% select(-"Type of ED Visit", -Outcome, -"Measure Names")

ggplot(ER2021_race1, aes(x = `Characteristic Levels`, y = `Measure Values`)) +
  geom_bar(stat = "identity", fill = "pink") +
  labs(title = "Bar Plot", x = "Characteristic Levels", y = "Measure Values") +
  theme_minimal()


  
  
  
  
  
  
  
  