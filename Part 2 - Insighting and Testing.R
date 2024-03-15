install.packages("leaps")
install.packages("readr")
install.packages("readxl")
install.packages("lubricate")
install.packages("zoo")
install.packages("car")
library(readxl)
library(dplyr)
library(lubridate)
library(zoo)
library(tidyr)
library(leaps)
library(car)


# Reading individual files
df1 <- read.csv("C://Users//Rahul//Downloads//2016_brooklyn.csv", skip=4)
df2 <- read.csv("C://Users//Rahul//Downloads//2017_brooklyn.csv", skip=4)
df3 <- read.csv("C://Users//Rahul//Downloads//2018_brooklyn.csv", skip=4)
df4 <- read.csv("C://Users//Rahul//Downloads//2019_brooklyn.csv", skip=4)
df5 <- read.csv("C://Users//Rahul//Downloads//2020_brooklyn.csv", skip=6)


# Changing column names of imported files
colnames(df1) = c('borough','neighborhood','bldclasscat','taxclasscurr','block','lot','easement','bldclasscurr','address','aptnum','zip','resunits','comunits','totunits','landsqft','grosssqft','yrbuilt','taxclasssale','bldclasssale','price','date')
colnames(df2) = c('borough','neighborhood','bldclasscat','taxclasscurr','block','lot','easement','bldclasscurr','address','aptnum','zip','resunits','comunits','totunits','landsqft','grosssqft','yrbuilt','taxclasssale','bldclasssale','price','date') 
colnames(df3) = c('borough','neighborhood','bldclasscat','taxclasscurr','block','lot','easement','bldclasscurr','address','aptnum','zip','resunits','comunits','totunits','landsqft','grosssqft','yrbuilt','taxclasssale','bldclasssale','price','date') 
colnames(df4) = c('borough','neighborhood','bldclasscat','taxclasscurr','block','lot','easement','bldclasscurr','address','aptnum','zip','resunits','comunits','totunits','landsqft','grosssqft','yrbuilt','taxclasssale','bldclasssale','price','date') 
colnames(df5) = c('borough','neighborhood','bldclasscat','taxclasscurr','block','lot','easement','bldclasscurr','address','aptnum','zip','resunits','comunits','totunits','landsqft','grosssqft','yrbuilt','taxclasssale','bldclasssale','price','date')

# Trimming Whitespaces from the dataframe
df <- rbind(df1, df2, df3, df4, df5)
feat <- lapply(df, trimws)
df <- bind_rows(feat)

df$price <- gsub("[$]", "", df$price)
df$price <- gsub(",", "", df$price)
df$price <- gsub("-", "", df$price)
df$price <- as.numeric(df$price)

df$landsqft <- gsub(",", "", df$landsqft)
df$landsqft <- as.numeric(df$landsqft)

df$grosssqft <- gsub(",", "", df$grosssqft)
df$grosssqft <- as.numeric(df$grosssqft) 

df$totunits[which(df$totunits == " -   ")] <- NA
df$totunits <- as.numeric(df$totunits)

df$comunits[which(brooklyn_df$comunits == " -   ")] <- NA
df$comunits <- as.numeric(brooklyn_df$comunits)

df$resunits[which(df$resunits == " -   ")] <- NA
df$resunits <- as.numeric(df$resunits)

df$bldclasscurr[which(df$bldclasscurr == "  ")] <- NA
df$bldclasscurr[which(df$bldclasscurr == " ")] <- NA
df$bldclasscurr[which(df$bldclasscurr == "")] <- NA

df$taxclasscurr[which(df$taxclasscurr == "  ")] <-  NA
df$taxclasscurr[which(df$taxclasscurr == " ")] <-  NA
df$taxclasscurr[which(df$taxclasscurr == "")] <-  NA

df <- df[!is.na(df$price),]
df <- df[df$price >100 & df$price!=25500000 & df$price!=16745121, ]
df <- df[as.numeric(df$yrbuilt)!= 0, ]
df <- df[df$grosssqft > 0, ]
df <- df[df$totunits == '1' & df$resunits == '1', ]

dfA <- df[grepl("A", df$bldclasssale),]
dfR <- df[grepl("R", df$bldclasssale),]
df <- rbind(dfA,dfR)

df$date <- as.Date(df$date, format = "%m/%d/%y")
df$qtr <- quarters(df$date)
df$qtr <- gsub("Q", "", df$qtr)
df$year <- year(df$date)
df$day <- day(df$date)
df$month <- month(df$date)
df$yr_qrt <- paste(as.numeric(df$year), df$qtr, sep = "_")

df$zip <- as.numeric(df$zip)

df <- df %>%
  mutate(neighb_cat = case_when(
    between(zip, 11201, 11224) ~ "brooklyn_h1",
    between(zip, 11225, 11249) ~ "brooklyn_h2",
    TRUE ~ "Other"))

df$bldclasscat_modify <- substr(df$bldclasscat, 1, 2)

df <- df %>% select(-easement, -borough, -resunits, -totunits, -comunits, -aptnum,
                    -neighborhood, -address, -taxclasscurr, -bldclasscurr, -bldclasscat)

# Converting Features to their respective Data types
df$block <- as.numeric(df$block)
df$lot <- as.numeric(df$lot)
df$zip <- as.numeric(df$zip)
df$yrbuilt <- as.numeric(df$yrbuilt)
#df$bldclasscat <- as.factor(df$bldclasscat)
df$bldclasssale <- as.factor(df$bldclasssale)
df$yr_qrt <- as.factor(df$yr_qrt)
df$neighb_cat <- as.factor(df$neighb_cat)
df$landsqft <- as.numeric(df$landsqft)
df$grosssqft <- as.numeric(df$grosssqft)
df$taxclasssale <- as.numeric(df$taxclasssale)
df$price <- as.numeric(df$price)
df$month <- as.numeric(df$month)
df$day <- as.numeric(df$day)
df$year <- as.numeric(df$year)
df$qtr <- as.numeric(df$qtr)
df$bldclasscat_modify <- as.numeric(df$bldclasscat_modify)


model1 <- lm(price ~ block + lot + zip + bldclasscat_modify + yr_qrt  + bldclasssale
             + landsqft + grosssqft + taxclasssale + neighb_cat, data = df)
summary(model1) #R2: 04691, Adj R2: 0.4674
rmse <- sqrt(mean((df$price - predict(model1, newdata=df))^2))
rmse #$650k


modelf <- lm(price ~ grosssqft*taxclasssale + log(block)*lot*zip + landsqft*log(yrbuilt) + neighb_cat*bldclasssale
             + qtr + (bldclasscat_modify) + date , data = df)
summary(modelf) #0.526, 612k
rmse <- sqrt(mean((df$price - predict(modelf, newdata=df))^2))
rmse 



#write.csv(df, "C://Users//Rahul//Downloads//brooklyn_v2.csv", row.names = FALSE)
