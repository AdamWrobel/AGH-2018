library(dplyr)
library(MASS)
library(ggplot2)

# Function that moves a window of a given width over all the data
# and calibrates the model using the data in the window. Then it uses
# this calibration to forecast the market movement on a given horizon.
rollWindow<-function(data, width,horizon)
{
  from<-min(data$Date)  #find a first day/last day in the data
  to<-max(data$Date)
  
  
  
  rdf<-data.frame() # new empty data frame that will contain the results
  while (now<to)
  {
    # use  'filter(data, condition)' to get the data in the time window
    # use  'lm(y~x,data=DATA)' to get the the linear fit  
    # use 'coefficients(regression result)' to get the fitted params value
    #      a=coeff[2],
    #      b=coeff[1].
    
    newrow<-data.frame( Day =   ,Predicted_Price=      , Coefficient1=     , .....        ) # creates a new row of data containing all the infos you would like to put in
    rdf<-rbind(rdf,newrow) # add new row to the data
    
    now<-now+1 # next day
  }
  rdf<-left_join(data,rdf,by="Day") # if you add the Day number to rdf you can use it join two data frames together
  rdf
}


df<-read.csv("./Data01.dat",TRUE," ")  # read the data file
# df<-read.csv("./Data02.dat",TRUE," ")
# df<-read.csv("./Data03.dat",TRUE," ")

# use 'mutate(data,newVariable=oldVariable1*+-/^oldVariable2)' combined with 'lag(Variable)'
# to calculate the relative difference between i-th Price and i-1 Price. Why?
# Linear regression does not run on Date, add a Day counter variable

regdata<-rollWindow(         ) # use the function defined by us
regdata<-regdata[complete.cases(regdata),] # filter the rows containig any 'NAs'

# use ggplot to draw whatever is interesting for us.
(regdata%>%ggplot(aes(x,y))+geom_point()+geom_line(aes(x,y2),color="Red"))%>%print

# use 'fitdistr(data,"distribution_name")' to fit gaussian to the relevant data
#        mean = fitResults$estimate[[1]], sd = fitResults$estimate[[2]]

# generate data.frame with fitted distribution using 'qnorm(seq(from=0, to = 1, by = 0.001), mean = , sd = )'
# using ggplot's geom_histogram(....), geom_density(....) plot the calculated data versus modelled one 
