# BRFSS2015-Data-Analysis
Behavioral Risk Factor Surveillance System Data Analysis in R

# What is BRFSS?
According to Kaggle.com, BRFSS, short for Behavioral Risk Factor Surveillance System, The objective of the BRFSS is to collect uniform, state-specific data on preventive health practices and risk
behaviors that are linked to chronic diseases, injuries, and preventable infectious diseases in the adult population.
Factors assessed by the BRFSS include tobacco use, health care coverage, HIV/AIDS knowledge or prevention,
physical activity, and fruit and vegetable consumption. Data are collected from a random sample of adults (one per
household) through a telephone survey.

The Behavioral Risk Factor Surveillance System (BRFSS) is the nation's premier system of health-related telephone surveys that collect state data about U.S. residents 
regarding their health-related risk behaviors, chronic health conditions, and use of preventive services. Established in 1984 with 15 states, BRFSS now collects data 
in all 50 states as well as the District of Columbia and three U.S. territories. BRFSS completes more than 400,000 adult interviews each year, making it the largest 
continuously conducted health survey system in the world.

# The Data Analysis Process
Using the R language and RStudio along with a few CRAN packages (including tidyverse), I performed the following in my analysis:
1. Imported csv dataset into RStudio
2. conducted Exploratory Data Analysis (EDA) 
3. Explored the relationship between Marital Status and Minutes of Total Physical Activity Per Week variables
4. Data Visualizations
5. Built model to predict Minutes of Total Physical Activity Per Week by Martial Status using linear regression 
6. Ran an Analysis of Variance (ANOVA) test to compare Minutes of Total Physical Activity Per Week across the five groups of Marital Status
7. Ran a Post-Hoc test to tell which group means were different
8. Ran another linear regression with the added Total Fruits Consumed Per Day variable and used R-squared and AIC to conclude which model was the better model
9. Selected four categorical variables to explore
10. Performed Data Visualizations with ggplot2
11. Ran basic descriptive statistics on the four variables
12. Chose one of the four variables to predict using multinomial logistical regression
