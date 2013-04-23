#define utility functions for data quality indicators

count.na <- function(x) sum(is.na(x))
all.na <- function(x, y) max(count.na(x), count.na(y))==length(y)
rmse <- function(x=x, y=y) sqrt(mean((x-y)^2))
r2 <- function(x=x, y=y, ...) {if(!all.na(x, y)) summary(lm(y~x,...))$r.squared else NA}
intercept <- function(x=x, y=y, ...) {if(!all.na(x,y)) coef(lm(y~x,...))["(Intercept)"] else NA}
slope <- function(x=x, y=y, ...) {if(!all.na(x,y)) coef(lm(y~x,...))["x"] else NA}

#install.packages("plyr")
#install.packages("data.table")

## test and sample application
require(plyr)
require(data.table)

# test data
#setwd('/Users/lmwang/Dropbox/smartdata/04152013')
#load("ALL.Rda")
#head(ALL, 10)

units <- read.table("housing_units.csv", sep=",", header=T)
units.dt <- data.table(units, key = "Year")
head(units.dt, 10)

## compute DQI with pluggable indicators
#solution 1
#ddply(units.dt[, ':='(x=RLIS_MFH, y=ACS_MFH)], .(Year), 
#      summarise, rmse=sqrt(mean((x-y)^2)))      

#solution 2
#units.dt[, ':='(x=RLIS_MFH, y=ACS_MFH)]
#units.dt[, rmse:=sqrt(mean((x-y)^2)), by="Year"]
##units.dt[, lapply(.SD, function(x, y) sqrt(mean((x-y)^2))), by="Year"]

dqi <- ddply(units.dt, .(Year), summarise, 
      mfh.rmse=rmse(x=RLIS_MFH, y=ACS_MFH),
      mfh.r2=r2(x=RLIS_MFH, y=ACS_MFH),
      mfh.intercept=intercept(x=RLIS_MFH, y=ACS_MFH),
      mfh.slope=slope(x=RLIS_MFH, y=ACS_MFH)
      )
print(dqi)
