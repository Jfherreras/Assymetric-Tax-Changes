################################################################################
######################### Assymetrical Taxation  ###############################
##################### Juan Felipe Herrera Sarmiento ############################
################################################################################

# This code produces: database construction from raw data for semi-structural model
################################################################################

rm(list = ls())
# Libraries
library(readxl)
##### General stuff 

# list for all variables
list = c("AUS","AUT","BEL","CAN","CHL","COL","CRI","CZE","DNK","EST","FIN","FRA",
         "DEU","GRC","HUN","ISL","IRL","ISR","ITA","JPN","KOR","LVA","LTU","LUX",
         "MEX","NLD","NZL","NOR","POL","PRT","SVK","SVN","ESP","SWE","CHE","TUR",
         "GBR","USA","WLD")

list_countries = c(AUS,AUT,BEL,CAN,CHL,COL,CRI,CZE,DNK,EST,FIN,FRA,
                   DEU,GRC,HUN,ISL,IRL,ISR,ITA,JPN,KOR,LVA,LTU,LUX,
                   MEX,NLD,NZL,NOR,POL,PRT,SVK,SVN,ESP,SWE,CHE,TUR,
                   GBR,USA,WLD)

# Years 2000-2021
years = 41:62

# Headers function
header.true <- function(df) {
  names(df) <- as.character(unlist(df[1,]))
  df[-1,]
}

# CPI
CPI <- read_excel("G:/Mi unidad/1 Drive Escritorio/Investigaciones/Assymetric impacts of income taxation/Analysis/Raw/API_FP.CPI.TOTL.ZG_DS2_en_excel_v2_4701305.xls", 
                  range = "A4:BN270")

CPI = CPI[ -c(1,3:4) ]

CPI = as.data.frame(t(CPI), make.names = T)

CPI = header.true(CPI)
CPI = CPI[years,]
CPI = CPI[,list]

# Economic growth
Growth <- read_excel("G:/Mi unidad/1 Drive Escritorio/Investigaciones/Assymetric impacts of income taxation/Analysis/Raw/API_NY.GDP.MKTP.KD.ZG_DS2_en_excel_v2_4701280.xls", 
                     range = "A4:BN270")

Growth = Growth[ -c(1,3:4) ]

Growth = as.data.frame(t(Growth), make.names = T)

Growth = header.true(Growth)
Growth = Growth[years,]
Growth = Growth[,list]

# Unemployment
Unem <- read_excel("G:/Mi unidad/1 Drive Escritorio/Investigaciones/Assymetric impacts of income taxation/Analysis/Raw/API_SL.UEM.TOTL.ZS_DS2_en_excel_v2_4700566.xls", 
                   range = "A4:BN270")

Unem = Unem[ -c(1,3:4) ]

Unem = as.data.frame(t(Unem), make.names = T)

Unem = header.true(Unem)
Unem = Unem[years,]
Unem = Unem[,list]

# Exchange Rate
EXR <- read_excel("G:/Mi unidad/1 Drive Escritorio/Investigaciones/Assymetric impacts of income taxation/Analysis/Raw/API_PA.NUS.FCRF_DS2_en_excel_v2_4700603.xls", 
                  range = "A4:BN270")

EXR = EXR[ -c(1,3:4) ]

EXR = as.data.frame(t(EXR), make.names = T)

EXR = header.true(EXR)
EXR = EXR[years,]
EXR = EXR[,list]

# Interest Rate
MPR <- read_excel("G:/Mi unidad/1 Drive Escritorio/Investigaciones/Assymetric impacts of income taxation/Analysis/Raw/Interest Rates.xlsx", 
                  range = "C7:Z45")
MPR = as.data.frame(t(MPR))
MPR = header.true(MPR)

# Premium risk
#Prem = 

# Save databases 
#for (i in list){
# 1. Merge a dataset for each variable
#  assign(as.vector(paste0('db_', i)), CPI[,i])
# Growth
#db_i[,2] = Growth[,i]
#  db_[i] = as.data.frame(db_[i])
#}
setwd("G:/Mi unidad/1 Drive Escritorio/Investigaciones/Assymetric impacts of income taxation/Analysis/Codes/Multivariate Filter/data")
for (i in list){
  # 1. Create a temporal object.
  temp <- CPI[,i]
  # 2. Add another variable.
  #temp["Growth"] = Growth[,i]
  #temp$Growth = Growth$i
  # .
  #temp$Unem = Unem$i
  # .
  temp = as.data.frame(temp)
  # N. Add another variable.
  # N + 1. Save database.
  assign(paste0('db_', i), temp)
  write.csv(paste0('db_',i))
              }

for (i in list){
  temp = Growth[,i]
  temp = as.data.frame(temp) 
  assign(paste0('db_', i), temp)
}
CPI$AUS