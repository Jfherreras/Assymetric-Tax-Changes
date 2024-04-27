################################################################################
######################### Assymetrical Taxation  ###############################
##################### Juan Felipe Herrera Sarmiento ############################
################################################################################

# This code produces: database construction from raw data
################################################################################

rm(list = ls())

# load libraries

library(readxl)
library(dplyr)
#library(xlsx)
library(mFilter)
library(tidyverse)

Corporate_tax_rate <- read_excel("G:/Mi unidad/1 Drive Escritorio/Investigaciones/Assymetric impacts of income taxation/Analysis/Raw/Corporate_tax_rate.xlsx")
Corporate_tax_rev_gdp <- read_excel("G:/Mi unidad/1 Drive Escritorio/Investigaciones/Assymetric impacts of income taxation/Analysis/Raw/Corporate_tax_rev_gdp.xlsx")
Corporate_tax_rev_prof_gdp <- read_excel("G:/Mi unidad/1 Drive Escritorio/Investigaciones/Assymetric impacts of income taxation/Analysis/Raw/Corporate_tax_rev_prof_gdp.xlsx")
GDP <- read_excel("G:/Mi unidad/1 Drive Escritorio/Investigaciones/Assymetric impacts of income taxation/Analysis/Raw/GDP.xlsx")
Unemployment <- read_excel("G:/Mi unidad/1 Drive Escritorio/Investigaciones/Assymetric impacts of income taxation/Analysis/Raw/Unemployment.xlsx")
Gini <- read_excel("G:/Mi unidad/1 Drive Escritorio/Investigaciones/Assymetric impacts of income taxation/Analysis/Raw/Gini.xlsx")
Inequality_income <- read_excel("G:/Mi unidad/1 Drive Escritorio/Investigaciones/Assymetric impacts of income taxation/Analysis/Raw/Income_Inequality.xlsx")
Inequality_wealth <- read_excel("G:/Mi unidad/1 Drive Escritorio/Investigaciones/Assymetric impacts of income taxation/Analysis/Raw/Wealth_Inequality.xlsx")
Inflation <- read_excel("G:/Mi unidad/1 Drive Escritorio/Investigaciones/Assymetric impacts of income taxation/Analysis/Raw/Inflation.xlsx")
Profit <- read_excel("G:/Mi unidad/1 Drive Escritorio/Investigaciones/Assymetric impacts of income taxation/Analysis/Raw/Profit_rate.xlsx")
Wages <- read_excel("G:/Mi unidad/1 Drive Escritorio/Investigaciones/Assymetric impacts of income taxation/Analysis/Raw/Wages.xlsx")

# Merge datasets
Dataset = GDP %>% full_join(Corporate_tax_rate, by=c("Country","Year"))
Dataset = Dataset %>% full_join(Corporate_tax_rev_gdp, by=c("Country","Year"))
Dataset = Dataset %>% full_join(Corporate_tax_rev_prof_gdp, by=c("Country","Year"))
Dataset = Dataset %>% full_join(Unemployment, by=c("Country","Year"))
Dataset = Dataset %>% full_join(Gini, by=c("Country","Year"))
Dataset = Dataset %>% full_join(Inequality_income, by=c("Country","Year"))
Dataset = Dataset %>% full_join(Inequality_wealth, by=c("Country","Year"))
Dataset = Dataset %>% full_join(Inflation, by=c("Country","Year"))
Dataset = Dataset %>% full_join(Profit, by=c("Country","Year"))
Dataset = Dataset %>% full_join(Wages, by=c("Country","Year"))
# Data processing
Growth = Dataset %>% 
  dplyr::select(Country,GDP) %>%
  group_by(Country) %>%
  mutate(growth = 100*c(NA,diff(GDP))/lag(GDP, 1))
Dataset[,"Growth"] = Growth[,3]
# Drop year 1999
Dataset = subset(Dataset, Year != 1999)

# Keep only the Database
rm(list = setdiff(ls(),"Dataset"))

# save dataset
#write.xlsx(Dataset,"G:/Mi unidad/1 Drive Escritorio/Investigaciones/Assymetric impacts of income taxation/Analysis/Raw/Database.xlsx")
write.csv(Dataset,"G:/Mi unidad/1 Drive Escritorio/Investigaciones/Assymetric impacts of income taxation/Analysis/Raw/Database.csv")
