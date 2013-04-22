count.na <- function(x) sum(is.na(x))
all.na <- function(x, y) max(count.na(x), count.na(y))==length(y)
rmse <- function(x=x, y=y) sqrt(mean((x-y)^2))
r2 <- function(x=x, y=y, ...) {if(!all.na(x, y)) summary(lm(y~x,...))$r.squared else 0.0}
intercept <- function(x=x, y=y, ...) {if(!all.na(x,y)) coef(lm(y~x,...))["(Intercept)"] else 0.0}
slope <- function(x=x, y=y, ...) {if(!all.na(x,y)) coef(lm(y~x,...))["x"] else 0.0}

setwd('/Users/lmwang/Dropbox/smartdata/04152013')
load("ALL.Rda")
head(ALL, 10)

install.packages("plyr")
install.packages("data.table")

require("plyr")
library(data.table)
all.dt <- data.table(ALL, key = "Year")
head(all.dt, 10)

#solution 1
#ddply(all.dt[, ':='(x=RLIS_MFH, y=ACS_MFH)], .(Year), 
#      summarise, rmse=sqrt(mean((x-y)^2)))      

#solution 2
#all.dt[, ':='(x=RLIS_MFH, y=ACS_MFH)]
#all.dt[, rmse:=sqrt(mean((x-y)^2)), by="Year"]
##all.dt[, lapply(.SD, function(x, y) sqrt(mean((x-y)^2))), by="Year"]

ddply(all.dt, .(Year), summarise, 
      mfh.rmse=rmse(x=RLIS_MFH, y=ACS_MFH),
      mfh.r2=r2(x=RLIS_MFH, y=ACS_MFH),
      mfh.intercept=intercept(x=RLIS_MFH, y=ACS_MFH),
      mfh.slope=slope(x=RLIS_MFH, y=ACS_MFH)
      )
