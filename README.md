# seasadjust
My code to seasonally adjust monthly unemployment rate data, California (2000-2015)

library(zoo)
library(fpp)
library(plyr)
library(reshape2)

#Merging and Seasonally Adjusting BLS Data
BLS2000 <- read.csv("/Users/torresj2/Downloads/CA Unemployment BLS/BLS 2000 - County Unemployment.csv")
BLS2001 <- read.csv("/Users/torresj2/Downloads/CA Unemployment BLS/BLS 2001 - County Unemployment.csv")
BLS2002 <- read.csv("/Users/torresj2/Downloads/CA Unemployment BLS/BLS 2002 - County Unemployment.csv")
BLS2003 <- read.csv("/Users/torresj2/Downloads/CA Unemployment BLS/BLS 2003 - County Unemployment.csv")
BLS2004 <- read.csv("/Users/torresj2/Downloads/CA Unemployment BLS/BLS 2004 - County Unemployment.csv")
BLS2005 <- read.csv("/Users/torresj2/Downloads/CA Unemployment BLS/BLS 2005 - County Unemployment.csv")
BLS2006 <- read.csv("/Users/torresj2/Downloads/CA Unemployment BLS/BLS 2006 - County Unemployment.csv")
BLS2007 <- read.csv("/Users/torresj2/Downloads/CA Unemployment BLS/BLS 2007 - County Unemployment.csv")
BLS2008 <- read.csv("/Users/torresj2/Downloads/CA Unemployment BLS/BLS 2008 - County Unemployment.csv")
BLS2009 <- read.csv("/Users/torresj2/Downloads/CA Unemployment BLS/BLS 2009 - County Unemployment.csv")
BLS2010 <- read.csv("/Users/torresj2/Downloads/CA Unemployment BLS/BLS 2010 - County Unemployment.csv")
BLS2011 <- read.csv("/Users/torresj2/Downloads/CA Unemployment BLS/BLS 2011 - County Unemployment.csv")
BLS2012 <- read.csv("/Users/torresj2/Downloads/CA Unemployment BLS/BLS 2012 - County Unemployment.csv")
BLS2013 <- read.csv("/Users/torresj2/Downloads/CA Unemployment BLS/BLS 2013 - County Unemployment.csv")
BLS2014 <- read.csv("/Users/torresj2/Downloads/CA Unemployment BLS/BLS 2014 - County Unemployment.csv")
BLS2015 <- read.csv("/Users/torresj2/Downloads/CA Unemployment BLS/BLS 2015 - County Unemployment.csv")

BLSall <- dplyr::bind_rows(BLS2000, BLS2001, BLS2002, BLS2003, BLS2004,
                           BLS2005, BLS2006, BLS2007, BLS2008, BLS2009,
                           BLS2010, BLS2011, BLS2012, BLS2013, BLS2014, BLS2015)

#Limiting the dataset to necessary variables, creating date variable, and 
BLSall <- dplyr::filter(BLSall, desc_short == "Unemployment Rate")
BLSall <- dplyr::mutate(BLSall, fips = county)
BLSall <- dplyr::select(BLSall, fips, year, month, quantity)
BLSall$Date <- as.yearmon(paste(BLSall$year, BLSall$month), "%Y %m")

#splitting into a list of data.frames by fips/county
BLSall_split <- split(BLSall, BLSall$fips)

#creating a function to turn all data.frames into timeseries data
tsonly <- function(x) {
  ts_x <- ts(x$quantity, frequency = 12, start = c(2000, 1)) 	
}

tsconvert1 <- lapply(BLSall_split, tsonly)

#checking out the unadjusted plot for one county
unadjustp <- plot(tsconvert1$"6011")

#Now adjusting
tsadjust <- function(x) {
  ts_x <- ts(x$quantity, frequency = 12, start = c(2000, 1)) 	
  decomp <- decompose(ts_x)
  adjust <- ts_x - decomp$seasonal
}

tsconvert2 <- lapply(BLSall_split, tsadjust)

#check out the adjusted plot for the same county
adjustp <- plot(tsconvert2$"6011")

#looking side-by-side
par(mfrow=c(1, 2))
plot(tsconvert1$"6011")
plot(tsconvert2$"6011")

#Now converting the tsconvert into a list of dataframes (rather than a list of timeseries)
dfs <- function(x) {
  data.frame(date=as.Date(index(x)), Y = melt(x)$value)
}

tsconvertnew <- lapply(tsconvert2, dfs)

#Now bringing each of the lists into data.frames
BLSnew <- as.data.frame(bind_rows(BLSall_split))
tscon <- as.data.frame(bind_rows(tsconvertnew))

#Merging BLS unadjusted and adjusted data
BLSwhole <- bind_cols(BLSnew, tscon)

#Cleaning up a bit
BLSwhole <- dplyr::mutate(BLSwhole, adjusted = Y)
BLSwhole <- dplyr::mutate(BLSwhole, unadjusted = quantity)
BLSwhole <- dplyr::select(BLSwhole, fips, year, month, unadjusted, adjusted)

#Spitting out as a .csv file
write.csv(BLSwhole, "BLSwhole.csv")

