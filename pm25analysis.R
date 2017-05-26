## Find the data on GitHub
## https://github.com/rdpeng/courses/tree/master/04_ExploratoryAnalysis/CaseStudy

## THE QUESTION:
## Has fine particle pollution in the U.S. decreased from 1999 to 2012?

## Unzip the data and move this file into the unzipped folder "pm25_data"

## Launch Terminal and view the first few lines of the file 
## (file too big to preview on Mac in Finder)

## Admins-MBP:~ admin$ cd Programming/class/pm25_data
## Admins-MBP:pm25_data admin$ ls
## RD_501_88101_1999-0.txt	RD_501_88101_2012-0.txt
## Admins-MBP:pm25_data admin$ head RD_501_88101_1999-0.txt
## # RD|Action Code|State Code|County Code|Site ID|Parameter|POC|Sample Duration|Unit|Method|Date|Start Time|Sample Value|Null Data Code|Sampling Frequency|Monitor Protocol (MP) ID|Qualifier - 1|Qualifier - 2|Qualifier - 3|Qualifier - 4|Qualifier - 5|Qualifier - 6|Qualifier - 7|Qualifier - 8|Qualifier - 9|Qualifier - 10|Alternate Method Detectable Limit|Uncertainty
## # RC|Action Code|State Code|County Code|Site ID|Parameter|POC|Unit|Method|Year|Period|Number of Samples|Composite Type|Sample Value|Monitor Protocol (MP) ID|Qualifier - 1|Qualifier - 2|Qualifier - 3|Qualifier - 4|Qualifier - 5|Qualifier - 6|Qualifier - 7|Qualifier - 8|Qualifier - 9|Qualifier - 10|Alternate Method Detectable Limit|Uncertainty
## RD|I|01|027|0001|88101|1|7|105|120|19990103|00:00||AS|3|||||||||||||
## RD|I|01|027|0001|88101|1|7|105|120|19990106|00:00||AS|3|||||||||||||
## RD|I|01|027|0001|88101|1|7|105|120|19990109|00:00||AS|3|||||||||||||
## RD|I|01|027|0001|88101|1|7|105|120|19990112|00:00|8.841||3|||||||||||||
## RD|I|01|027|0001|88101|1|7|105|120|19990115|00:00|14.92||3|||||||||||||
## RD|I|01|027|0001|88101|1|7|105|120|19990118|00:00|3.878||3|||||||||||||
## RD|I|01|027|0001|88101|1|7|105|120|19990121|00:00|9.042||3|||||||||||||
## RD|I|01|027|0001|88101|1|7|105|120|19990124|00:00|5.464||3|||||||||||||

## Notice the column names have hashtags, separators are vertical lines, and missing data is a missing string

## Open R and change the working directory
## setwd("~/Programming/class/pm25_data")

## Read in data from 1999 into a data frame called pm0
pm0 <- read.table("RD_501_88101_1999-0.txt", comment.char = "#", header = FALSE, sep = "|", na.strings = "")

## Check the dimensions and first 6 lines of pm0
dim(pm0)
head(pm0)

## Read in a single line to retrieve variable names and store in object
cnames <- readLines("RD_501_88101_1999-0.txt", 1)
print(cnames)
## [1] "# RD|Action Code|State Code|County Code|Site ID|Parameter|POC|Sample Duration|Unit|Method|Date|Start Time|Sample Value|Null Data Code|Sampling Frequency|Monitor Protocol (MP) ID|Qualifier - 1|Qualifier - 2|Qualifier - 3|Qualifier - 4|Qualifier - 5|Qualifier - 6|Qualifier - 7|Qualifier - 8|Qualifier - 9|Qualifier - 10|Alternate Method Detectable Limit|Uncertainty"

## Split the string object into a character list and view
cnames <- strsplit(cnames, "|", fixed = TRUE)
print(cnames)

## Make the variable names of the list valid by replacing spaces with periods 
## and assign that to the variable names of pm0
names(pm0) <- make.names(cnames[[1]])
head(pm0)

## Extract only the column containing pm2.5 values and create object x0
x0 <- pm0$Sample.Value

## Check to make sure it's of numeric class
class(x0)
## [1] "numeric"

## Structure
str(x0)
## num [1:117421] NA NA NA 8.84 14.92 ...

## Summary to see mean, NA's, quantiles, etc.
summary(x0)
## Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
## 0.00    7.20   11.50   13.74   17.90  157.10   13217 

## Are missing values important here?
## is.na(x0) creates a logical vector of TRUEs(1) and FALSEs(0). When you take the mean, 
## you are adding all the 1's and dividing that by the length of the logical vector.
mean(is.na(x0))  
## [1] 0.1125608 Meaning 11% of our pm2.5 values are missing from the 1999 dataset! (13217/117421)

## Read in data from 2012 into a dataframe called pm1 and extract the pm2.5 values into x1
pm1 <- read.table("RD_501_88101_2012-0.txt", comment.char = "#", header = FALSE, sep = "|", na.strings = "", nrow = 1304290)
names(pm1) <- make.names(cnames[[1]])
head(pm1)
dim(pm1)
x1 <- pm1$Sample.Value
class(x1)

## Size of 2012 data frame
print(object.size(pm1),units="Mb")
## 144.3 Mb

## Does the median appear to decrease from 1999 to 2012? Yes...
summary(x0)
## Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
## 0.00    7.20   11.50   13.74   17.90  157.10   13217 
summary(x1)
##   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
## -10.00    4.00    7.63    9.14   12.00  909.00   73133 

## However, we have a negative value, but we can't have negative mass.

## Percent of missing data in 2012 pm2.5 values?
mean(is.na(x1))  
## [1] 0.05607125 Only 5.6% data missing in the 2012 dataset

## Make a boxplot of both 1999 and 2012
boxplot(x0, x1)
## Use a base 10 log scale for a better view (warning messages because of negative values)
boxplot(log10(x0), log10(x1))

## Create a logical vector of negative values in the 2012 pm2.5 values (NA's are introduced)
negative <- x1 < 0

## How many values are TRUE (less than 0)?
sum(negative, na.rm = T)
## [1] 26474

## What proportion of values are negative?
mean(negative, na.rm = T)
## [1] 0.0215034   (2%)

## Create a vector of dates from the 2012 dataset
dates <- pm1$Date

## Check the structure... it's an integer vector
str(dates)
## int [1:1304287] 20120101 20120104 20120107 20120110 20120113 20120116 20120119 20120122 20120125 20120128 ...

## Convert to date class
dates <- as.Date(as.character(dates), "%Y%m%d")
str(dates)
##  Date[1:1304287], format: "2012-01-01" "2012-01-04" "2012-01-07" "2012-01-10" "2012-01-13" "2012-01-16" ...

## Let's view the histogram of dates to see where data occurs
hist(dates, "month")  
## The density of observations take place January through June, then drop off for summer

## Histogram of density of negative values
hist(dates[negative], "month")
## Most negative values in Jan-Jun
## Maybe there are more negative values in the colder months because it's harder to measure, thus errors

## Let's plot a subset for one monitor that existed in 1999 and 2012
## This will help us control for possible changes in monitoring locations

## Find a monitor for New York State that exists in both datasets
## Subset out the rows where State.Code = 36, and pluck out only the county and id columns
site0 <- unique(subset(pm0, State.Code == 36, c(County.Code, Site.ID)))
## Do the same for both datasets, use 'unique' to remove duplicates
site1 <- unique(subset(pm1, State.Code == 36, c(County.Code, Site.ID)))

## Check out the new data frames
head(site0)
##       County.Code Site.ID
## 65873           1       5
## 65995           1      12
## 66056           5      73
## 66075           5      80
## 66136           5      83
## 66197           5     110

## Paste together the county and id numbers to make new character vectors
site0 <- paste(site0[,1], site0[,2], sep = ".")
site1 <- paste(site1[,1], site1[,2], sep = ".")

str(site0)
## chr [1:33] "1.5" "1.12" "5.73" "5.80" "5.83" "5.110" "13.11" "27.1004" "29.2" "29.5" ...
str(site1)
## chr [1:18] "1.5" "1.12" "5.80" "5.133" "13.11" "29.5" "31.3" "47.122" "55.1007" "61.79" ...

## Where do the county and id combinations intersect? Which exist in both sets?
both <- intersect(site0, site1)
print(both)
## [1] "1.5"     "1.12"    "5.80"    "13.11"   "29.5"    "31.3"    "63.2008" "67.1015" "85.55"   "101.3" 
## There are 10 monitoring locations in New York that exist in both datasets

## Find how many observations available at each monitor
## Add a new column to each dataset that contains the new monitor codes we pasted together earlier
pm0$county.site <- with(pm0, paste(County.Code, Site.ID, sep = "."))
pm1$county.site <- with(pm1, paste(County.Code, Site.ID, sep = "."))

## Subset only New York observations that only share monitor sites in both
cnt0 <- subset(pm0, State.Code == 36 & county.site %in% both)
cnt1 <- subset(pm1, State.Code == 36 & county.site %in% both)

## Verify datasets are only in New York and share identical unique sites
head(cnt0)
head(cnt1)
identical(unique(cnt0$county.site),unique(cnt1$county.site))
## [1] TRUE

## split into groups by monitor and count the observations
sapply(split(cnt0, cnt0$county.site), nrow)
## 1.12     1.5   101.3   13.11    29.5    31.3    5.80 63.2008 67.1015   85.55 
##   61     122     152      61      61     183      61     122     122       7 
sapply(split(cnt1, cnt1$county.site), nrow)
## 1.12     1.5   101.3   13.11    29.5    31.3    5.80 63.2008 67.1015   85.55 
##   31      64      31      31      33      15      31      30      31      31

## Create subsets choosing only New York, county 63, and side ID 2008
pm1sub <- subset(pm1, State.Code == 36 & County.Code == 63 & Site.ID == 2008)
pm0sub <- subset(pm0, State.Code == 36 & County.Code == 63 & Site.ID == 2008)

## Check dimensions
dim(pm1sub)
##  30 29
dim(pm0sub)
##  122  29

## Plot data for 2012 as a time series

## Extract the dates and pm2.5 values from the subset of 2012 New York monitors
dates1 <- as.Date(as.character(pm1sub$Date), "%Y%m%d")
x1sub <- pm1sub$Sample.Value

## Plot dates and pm2.5 values
plot(dates1, x1sub)

## Plot dates and pm2.5 values for 1999
dates0 <- as.Date(as.character(pm0sub$Date), "%Y%m%d")
x0sub <- pm0sub$Sample.Value
plot(dates0, x0sub)

## Now plot data for both years in same panel

## Set parameters for side-by-side (1 row, 2 cols) comparison and set margins
par(mfrow = c(1, 2), mar = c(4, 4, 2, 1))

## Plot first panel with tiny circles for 1999
plot(dates0, x0sub, pch = 20)

## Add a horizontal line representing the median
abline(h = median(x0sub, na.rm = T))

## Plot the second panel with tiny circles for 2012 and the median line
plot(dates1, x1sub, pch = 20)  
abline(h = median(x1sub, na.rm = T))
## Whoa! Different ranges!!!! 
## y-axis in 1999 goes to 40, y-axis goes to 14 in 2012
## Also, different dates

## Find global range
rng <- range(x0sub, x1sub, na.rm = T)
rng
## [1]  3.0 40.1

## Now plot with new y-axis limits
par(mfrow = c(1, 2), mar = c(4, 4, 2, 1))
plot(dates0, x0sub, pch = 20, ylim = rng)
abline(h = median(x0sub, na.rm = T))
plot(dates1, x1sub, pch = 20, ylim = rng)
abline(h = median(x1sub, na.rm = T))
## Median levels are coming down, and so are the extremes

## Show state-wide means and make a plot showing trend
head(pm0)

## Use tapply to compute average pm2.5 value BY state for 1999
mn0 <- with(pm0, tapply(Sample.Value, State.Code, mean, na.rm = T))

str(mn0)
##  num [1:53(1d)] 19.96 6.67 10.8 15.68 17.66 ...
##  - attr(*, "dimnames")=List of 1
##  ..$ : chr [1:53] "1" "2" "4" "5" ...

summary(mn0)
##  Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
## 4.862   9.519  12.310  12.410  15.640  19.960 

## Create vector of means by state for 2012 data
mn1 <- with(pm1, tapply(Sample.Value, State.Code, mean, na.rm = T))
str(mn1)
summary(mn1)
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##   4.006   7.355   8.729   8.759  10.610  11.990 

## Make separate data frames for states / years
d0 <- data.frame(state = names(mn0), mean = mn0)
d1 <- data.frame(state = names(mn1), mean = mn1)
mrg <- merge(d0, d1, by = "state")
dim(mrg)
## [1] 52  3

head(mrg)
##   state    mean.x    mean.y
## 1     1 19.956391 10.126190
## 2    10 14.492895 11.236059
## 3    11 15.786507 11.991697
## 4    12 11.137139  8.239690
## 5    13 19.943240 11.321364
## 6    15  4.861821  8.749336

## Finally, plot the state means and connect the lines
par(mfrow = c(1, 1))
with(mrg, plot(rep(1999, 52), mrg[, 2], xlim = c(1998, 2013)))
with(mrg, points(rep(2012, 52), mrg[, 3]))
segments(rep(1999, 52), mrg[, 2], rep(2012, 52), mrg[, 3])
