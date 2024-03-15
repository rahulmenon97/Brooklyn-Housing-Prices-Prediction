install.packages("leaps")
install.packages("readr")
install.packages("readxl")
install.packages("lubricate")
install.packages("zoo")
install.packages("car")
install.packages("ggplot2")
library(readxl)
library(dplyr)
library(lubridate)
library(zoo)
library(tidyr)
library(leaps)
library(car)
library(ggplot2)


# Reading individual files
df1 <- read.csv("C://Users//Rahul//Downloads//2016_brooklyn.csv", skip=4)
df2 <- read.csv("C://Users//Rahul//Downloads//2017_brooklyn.csv", skip=4)
df3 <- read.csv("C://Users//Rahul//Downloads//2018_brooklyn.csv", skip=4)
df4 <- read.csv("C://Users//Rahul//Downloads//2019_brooklyn.csv", skip=4)
df5 <- read.csv("C://Users//Rahul//Downloads//2020_brooklyn.csv", skip=6)
df5 <- df5[-1,] 


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

# Cleaning Price Variable (Dependent Variable)
clean_price_col <- function(dfs, col) {
  dfs[[col]] <- gsub(",+", "", dfs[[col]])
  dfs[[col]] <- gsub("\\$+", "", dfs[[col]])
  return(dfs)
}
df <- clean_price_col(df, "price")


# Cleaning Gross & Land Square Footage Variable
clean_landgrosssqrt_col <- function(dfs, col) {
  dfs[[col]] <- gsub("[,]", "", dfs[[col]])
  dfs[[col]] <- gsub(",+", "", dfs[[col]])
  return(dfs)
}
df <- clean_landgrosssqrt_col(df, "grosssqft")
df <- clean_landgrosssqrt_col(df, "landsqft")


# Filtering out data where Total & Residential Units are 1
df <- df[df$totunits == '1' & df$resunits == '1', ]
df <- df[df$grosssqft > 0, ]

# Removing Outliers from Price
df <- df[!is.na(df$price),]
df$price <- as.numeric(df$price)
df <- df[df$price>240000 & df$price<6500000, ] #best 24k, 6.5M, new best 6.35


# Filtering out data with Building Class Sales with A & R
dfA <- df[grepl("A", df$bldclasssale),]
dfR <- df[grepl("R", df$bldclasssale),]
df <- rbind(dfA,dfR)

# Treating Date Feature
df$date <- as.Date(df$date, format = "%m/%d/%y")
df$qtr <- quarters(df$date)
df$qtr <- gsub("Q", "", df$qtr)
df$year <- year(df$date)
df$day <- day(df$date)
df$month <- month(df$date)
df$yr_qrt <- paste(as.numeric(df$year), df$qtr, sep = "_")

# Treating Neighborhood Feature
north_b <- c("BAY RIDGE", "BEDFORD STUYVESANT", "BOROUGH PARK", "BROOKLYN HEIGHTS", "BUSH TERMINAL", "BUSHWICK",
                         "CARROLL GARDENS", "CLINTON HILL", "COBBLE HILL", "COBBLE HILL-WEST", "CROWN HEIGHTS",
                         "DOWNTOWN-FULTON FERRY", "DOWNTOWN-FULTON MALL", "DOWNTOWN-METROTECH", "DYKER HEIGHTS", "FORT GREENE",
                         "GOWANUS", "GREENPOINT", "KENSINGTON", "MADISON", "NAVY YARD", "PARK SLOPE", "PARK SLOPE SOUTH",
                         "PROSPECT HEIGHTS", "RED HOOK", "WILLIAMSBURG-CENTRAL", "WILLIAMSBURG-EAST", "WILLIAMSBURG-NORTH",
                         "WILLIAMSBURG-SOUTH", "WINDSOR TERRACE", "WYCKOFF HEIGHTS")

south_b <- c("BATH BEACH", "BENSONHURST", "BERGEN BEACH", "BRIGHTON BEACH", "CANARSIE", "DITMAS PARK",
                         "FLATBUSH-CENTRAL", "FLATBUSH-EAST", "FLATBUSH-LEFFERTS GARDEN", "FLATBUSH-NORTH", "FLATLANDS",
                         "GERRITSEN BEACH", "GRAVESEND", "MANHATTAN BEACH", "MARINE PARK", "MIDWOOD", "MILL BASIN",
                         "OCEAN HILL", "OCEAN PARKWAY-NORTH", "OCEAN PARKWAY-SOUTH", "OLD MILL BASIN", "SEAGATE", "SHEEPSHEAD BAY",
                         "SPRING CREEK", "CONEY ISLAND")

east_b <- c("BROWNSVILLE", "CANARSIE", "CYPRESS HILLS", "EAST NEW YORK", "NEW LOTS", "OCEAN HILL", "SPRING CREEK")

west_b <- c("GOWANUS", "GREENPOINT", "RED HOOK", "SUNSET PARK")

# Assign categories to the data frame
df$neighborhood[df$neighborhood %in% north_b] <- "North"
df$neighborhood[df$neighborhood %in% south_b] <- "South"
df$neighborhood[df$neighborhood %in% east_b] <- "East"
df$neighborhood[df$neighborhood %in% west_b] <- "West"

# Treating ZIP Feature
Central = c(11213, 11216, 11225, 11233, 11226, 11218)
Eastern = c(11212, 11236, 11207, 11208, 11239, 11203)
Northern = c(11206, 11221, 11237, 11222, 11211, 11249)
Northwestern = c(11201, 11217, 11238, 11205, 11251, 11231, 11215)
Southern = c(11234, 11224, 11229, 11235, 11210, 11230, 11223)
Southwestern = c(11209, 11220, 11204, 11214, 11219, 11228, 11232)

data_mod$locality = as.factor(ifelse(data_mod$zip %in% Central, 'Central',
                                     ifelse(data_mod$zip %in% Eastern, 'Eastern',
                                            ifelse(data_mod$zip %in% Northern, 'Northern',
                                                   ifelse(data_mod$zip %in% Northwestern, 'Northwestern',
                                                          ifelse(data_mod$zip %in% Southern, 'Southern',
                                                                 ifelse(data_mod$zip %in% Southwestern, 'Southwestern',"")))))))


# Treating Block Feature
df$block <- as.numeric(df$block)
df$block_cat <- cut(df$block, breaks = 10, labels = c("b1", "b2", "b3", "b4", "b5", "b6",
                                                      "b7", "b8", "b9", "b10"))

# Treating Lot Feature
df$lot <- as.numeric(df$lot)
df$lot_cat <- cut(df$lot, breaks = 10, labels = c("lot1", "lot2", "lot3", "lot4", "lot5", "lot6", 
                                                  "lot7", "lot8", "lot9", "lot10"))

# Treating Building Class Category
df$bldclasscat_modify <- substr(df$bldclasscat, 1, 2)


# Removing Irrelevant Features
df <- df %>% select(-easement, -borough, -resun its, -totunits, -comunits, -aptnum,
                     -address, -taxclasscurr, -bldclasscurr, -bldclasscat, -taxclasscurr)

# Checking the Data types of the Features
str(df)

# Converting Features to their respective Data types
df$block <- as.numeric(df$block)
df$lot <- as.numeric(df$lot)
df$zip <- as.numeric(df$zip)
df$yrbuilt <- as.numeric(df$yrbuilt)
df$bldclasssale <- as.factor(df$bldclasssale)
df$yr_qrt <- as.factor(df$yr_qrt)
df$landsqft <- as.numeric(df$landsqft)
df$grosssqft <- as.numeric(df$grosssqft)
df$taxclasssale <- as.numeric(df$taxclasssale)
df$price <- as.numeric(df$price)
df$month <- as.numeric(df$month)
df$day <- as.numeric(df$day)
df$year <- as.numeric(df$year)
df$qtr <- as.numeric(df$qtr)
df$bldclasscat_modify <- as.numeric(df$bldclasscat_modify)
df$zip_cat <- as.numeric(unclass(df$zip_cat))
df$block_cat <- as.numeric(unclass(df$block_cat))
df$neighborhood <- as.factor(df$neighborhood)
dflot_cat <- as.numeric(unclass(df$lot_cat))

#df <- df[df$landsqft<127377, ]
#df <- df[df$landsqft!=1, ]

## Exploratory Data Analysis

# Block Analysis
hist(df$block, breaks = 50, main = 'block dist', xlab = 'Block')
hist(log(df$block), breaks = 150, main = 'Log Block Distribution', xlab = 'Log - Block')
hist(sqrt(df$block), breaks = 150, main = 'Square Root Block Distribution', xlab = 'Square Root - Block')
plot(df$block, df$price, xlab='Block', ylab='Price', main = 'Price vs Block')

# Lot Analysis
hist(df$lot, breaks = 50, main = 'block dist', xlab = 'Lot')
hist(log(df$lot), breaks = 150, main = 'Log Gross Lot Distribution', xlab = 'Log - Lot')
hist(sqrt(df$lot), breaks = 150, main = 'Square root Lot Distribution', xlab = 'Square Root - Lot')
plot(df$lot, df$price)

# Gross Square Feet Analysis
hist(df$grosssqft, breaks = 50, main = 'Gross Sqrt Ft dist', xlab = 'Gross Square Feet')
hist(log(df$grosssqft), breaks = 150, main = 'Log Gross Sqrt Ft Distribution', xlab = 'Log Gross Square Feet')
hist(sqrt(df$grosssqft), breaks = 150, main = 'Sqrt Gross Sqrt Ft Distribution', xlab = 'Square root Gross Square Feet')
plot(df$grosssqft, df$price, xlab = 'Gross Sqrt Ft', ylab = 'Price', main='Price vs Gross Sqrt Ft')
# Grosssqrt is positively correlated with price

# Land Square Feet Analysis
hist(df$landsqft, breaks = 50, main = 'Land Square Feet Distribution', xlab = 'Land Square Feet')
hist(log(df$landsqft), breaks = 150, main = 'Log Land Square Feet Distribution', xlab = 'Log Land Square Feet')
hist(sqrt(df$landsqft), breaks = 150, main = 'Sqrt Land Square Feet Distribution', xlab = 'Square root Land Square Feet')
plot(df$landsqft, df$price, xlab='Land Sqrt Ft', ylab='Price', main='Price vs Land Sqrt Ft')
plot(df$landsqft, df$grosssqft, xlab='Land Sqrt Ft', ylab='Gross Sqrt Ft', main='Gross Sqrt Ft vs Land Sqrt Ft')

# Neighborhood Analysis
ggplot(df, aes(x = neighb_cat)) +
  geom_bar(stat = "count") +
  labs(title = "Count Plot of Neighborhood Category", x = "Neighborhood Category", y = "Count")

# Zip Analysis
hist(df$zip, breaks = 50, main = 'Zip Distribution', xlab = 'Zip')
hist(log(df$zip), breaks = 50, main = 'Log Zip Distribution', xlab = 'Zip - Log')
hist(sqrt(df$zip), breaks = 50, main = 'Sqrt Zip Distribution', xlab = 'Zip - Sqrt')
plot(df$zip, df$price, main="Zip & Price", xlab="Zip", ylab="Price", pch=19)

# Price Analysis
hist(df$price, breaks = 50, main = 'Price Distribution', xlab = 'Zip')
hist(log(df$price), breaks = 50, main = 'Log Price Distribution', xlab = 'Price - Log')
hist(sqrt(df$price), breaks = 50, main = 'Sqrt Price Distribution', xlab = 'Price - Sqrt')

# Correlation Check
plot(df$lot, df$zip)
plot(df$bldclasscat, df$bldclasssale)
plot(df$taxclasssale, df$bldclasssale)

df_final <- df[df$neighb_cat!='low_end_neigh' & df$neighb_cat!='others', ]

## Modelling
model1 <- lm(price ~ block + lot + zip + yr_qrt  + bldclasssale
             + landsqft + grosssqft + taxclasssale + neighborhood, data = df)
summary(model1) #R2: 0.6147, Adj R2: 0.6136
rmse <- sqrt(mean((df$price - predict(model1, newdata=df))^2))
rmse #$532k

#----------

model2 <- lm(price ~ block + lot + zip + yr_qrt
             + landsqft + grosssqft + neighborhood, data = df)
summary(model2) #R2: 04691, Adj R2: 0.4674
rmse <- sqrt(mean((df$price - predict(model2, newdata=df))^2))
rmse #$661k

#----------
model3 <- lm(price ~ block + sqrt(lot) + sqrt(zip) + yr_qrt
             + landsqft + grosssqft , data = df)
summary(model3) #R2: 04691, Adj R2: 0.4674
rmse <- sqrt(mean((df$price - predict(model3, newdata=df))^2))
rmse

#----------
model4 <- lm(price ~ block + lot + zip  + yr_qrt  + yr_qrt
             + landsqft + grosssqft, data = df)
summary(model4) #R2: 04691, Adj R2: 0.4674
rmse <- sqrt(mean((df$price - predict(model4, newdata=df))^2))
rmse

#----------
model5 <- lm(price ~ block + lot + bldclasscat + yr_qrt
             + landsqft + grosssqft + neighb_cat, data = df)
summary(model5) #R2: 04691, Adj R2: 0.4674
rmse <- sqrt(mean((df$price - predict(model5, newdata=df))^2))
rmse

#----------
model6 <- lm(price ~ block + lot + bldclasscat + yr_qrt
             + grosssqft + neighb_cat, data = df)
summary(model6) #R2: 04691, Adj R2: 0.4674
rmse <- sqrt(mean((df$price - predict(model6, newdata=df))^2))
rmse

#----------Best as of now
model7 <- lm(price ~ grosssqft + log(lot) + log(block) + taxclasssale + landsqft + yrbuilt + yr_qrt + zip + neighborhood
             + bldclasssale , data = df)
summary(model7) #R2: 04691, Adj R2: 0.4674
rmse <- sqrt(mean((df$price - predict(model7, newdata=df))^2))
rmse

#--------------
model8 <- lm(price ~ grosssqft + log(lot) + log(block) + taxclasssale + landsqft + yrbuilt + neighborhood*bldclasssale
             + bldclasscat_modify + zip, data = df)
summary(model8) #R2: 04691, Adj R2: 0.4674
rmse <- sqrt(mean((df$price - predict(model8, newdata=df))^2))
rmse

#--------------
model9 <- lm(price ~ (grosssqft*landsqft) + (log(lot)*log(block)*zip) + (bldclasssale+taxclasssale)
             + (bldclasscat_modify*neighborhood)  +(year*qtr*month*day), data = df)
summary(model9) #R2: 04691, Adj R2: 0.4674
rmse <- sqrt(mean((df$price - predict(model9, newdata=df))^2))
rmse

#---------------
#df[["grosssqft"]] <- MASS::boxcox(df[["grosssqft"]])

model9 <- lm(price ~ (grosssqft*landsqft) + (lot*log(block)*zip*yrbuilt) + neighborhood
             + (bldclasscat_modify*taxclasssale) +(year*qtr*month*day), data = df)
summary(model9) #R2: 04691, Adj R2: 0.4674
rmse <- sqrt(mean((df$price - predict(model9, newdata=df))^2))
rmse

#----------
model10 <- lm(price ~ grosssqft*taxclasssale + log(block)*lot*zip + sqrt(landsqft)*neighborhood + bldclasssale
               + (year*qtr*bldclasscat_modify) , data = df)
summary(model10) #0.526, 612k
rmse <- sqrt(mean((df$price - predict(model10, newdata=df))^2))
rmse

#best as of now
#-------------
model11 <- lm(price ~ block*lot + zip_cat*block_cat + grosssqft*log(block)*neighborhood
              + sqrt(landsqft) + bldclasssale, data = df)
summary(model11) #0.6072
rmse <- sqrt(mean((df$price - predict(model11, newdata=df))^2))
rmse #458k
sqrt(mean(model11$residuals^2))

df
#-------------

model12 <- lm(price ~ grosssqft + neighborhood*zip_cat*year + bldclasssale + 
              ,data = df)
summary(model12) #0.6072
rmse <- sqrt(mean((df$price - predict(model12, newdata=df))^2))
rmse#458k
sqrt(mean(model12$residuals^2))
