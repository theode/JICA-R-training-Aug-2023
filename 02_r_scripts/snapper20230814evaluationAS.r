#################################################
################ growth curve ###################
############## Von-Bertalanffy ##################
#################################################
#the parameters of von-Bertalanffy equation
#Linf: theoretical maximum length 
#K : growth coefficient
#t0 : theoretical age at zero length

library(here)

#In this exercise, we use growth equation of yellowtail snapper 
# (See "Age and Growth of Yellowtail Snapper, Ocyurus chrysurus, from the Southeastern United States",
#Authors: Garcia, Eden R.; Potts, Jennifer C.; Rulifson, Roger A.; Manooch, Charles S.
#Source: Bulletin of Marine Science, Volume 72, Number 3, May 2003, pp.909-921(13)
#Publisher: University of Miami-Rosenstiel School of Marine, Atmospheric & Earth Science )

age <- 1 # The number "1" is assigned at the object "age"
TLage1 <- 607.7*(1-exp(-0.17*(age+1.88))) # growth curve / growth equation
TLage1 # Length at age 1


#### Assume that length data of age 1-13 fish have been collected ####
allage <- read.csv(here("00_raw_data", "20230814.csv"), header=TRUE) #age-length data, "header=T" means the line 1 in the csv file is the header (the names if each column)
summary(allage) # show summary of the data 
head(allage) # show the upper 5 lines of the data
tail(allage) # show the lower 5 lines of the data




######## R has 2 types of function for making graph
# 1. generic functions to draw plots and ggplot functions
hist(allage$length)
# 2. geom_histogram (one of ggplot functions) is more convenient for making histograms
#install.packages("ggplot2",dependencies = T)
library(ggplot2) # load and attach add-on tools of making graphs into this R session
gg <- ggplot(data=allage, aes(x=length, fill=as.factor(age))) + geom_histogram(position = "identity", alpha=0.8)
plot(gg)
ggsave(here("03_plots", "length.comps.png"),width=5,height=5) #width and height are specified in inches

# plot growth curve
# use generic functions "plot" and "points"
age <- c(1:13) # predict the length at each age 1-13
TLmean <- 607.7*(1-exp(-0.17*(age+1.88))) # growth curve of yellowtail snapper
plot(TLmean, type="l") # draw a line plot of growth curve
points(x=allage$age, y=allage$length) # add points of observations

# Assume the growth curve estimated by Garcia et al is true
# estimate growth curve from observed data
# observed data is saved in the object "allage"
library(FSA) # Fishery Stock Assessment methods
library(FSAdata)
library(nlstools)
f.starts <- vbStarts(length~age,data=allage)# calculate appropriate initial values
growth <- nls(length ~ Linf*(1-exp(-K*(age-t0))), data=allage, start=f.starts)# non-linear regression
# response variable ~ explanatory variable (the same as simple linear regression)
#Linf, K, t0 are the parameters of von-Bertalanffy equation
overview(growth) # overview the result of regression

# using the estimates of parameter, draw the line of the estimated curve in blue
TLest <- 627.4 *(1-exp(-0.16 *(age + 2.007)))
points(TLest, type="l",col="blue")

# some of observations screen out...
# we can specify the upper limit of y-axis

# set y-axis range "ylim" from 0 to the maximum value of observed length
plot(TLmean, type="l", ylim=c(0,max(allage$length))) # draw a line plot of growth curve
points(x=allage$age, y=allage$length) # add points of observations
points(TLest, type="l",col="blue") # draw a blue line of the estimated curve





####### Evaluation (small test) #######
# 1. Load a csv file "allage2.csv" and make a histogram of length data, color by age
# 2. Estimate growth curve
# 3. Make a graph of estimated curve (black line) and observation (black point), age 1-10, lower limit of y-axis must be 0, do not screen out

# Step 1:
# Load "allage2.csv"
allage2 <- read.csv(here("00_raw_data", "allage2.csv"), header=TRUE)

# Make histogram
ggplot(data = allage2, aes(x = length, fill = as.factor(age))) +
  geom_histogram(position = "identity")

# Step 2:
# Estimate growth curve
f.starts <- vbStarts(length~age,data=allage2)# calculate appropriate initial values
growth <- nls(length ~ Linf*(1-exp(-K*(age-t0))), data=allage2, start=f.starts)
overview(growth)

# Step 3: Make a graph of estimated curve (black line) and observation (black point), age 1-10, lower limit of y-axis must be 0, do not screen out

age2 <- c(1:10)

TLmean <- 607.7*(1-exp(-0.17*(age2+1.88))) # growth curve 
plot(TLmean, type="l", ylim = c(0, 600)) # draw a line plot of growth curve
points(x=allage2$age, y=allage2$length) # add points of observations


# using the estimates of parameter, draw the line of the estimated curve in blue
TLest <- 627.4 *(1-exp(-0.16 *(age2 + 2.007)))
points(TLest, type="l",col="blue")
