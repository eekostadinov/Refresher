## This Rscript contains all the codes from the corresponding chapter of the ebook in raw form.
## They are intended to make it easy to replicate the analysis on your own computer given that 
## you have already installed R and RStudio (refer to ebook for details).
## For explanation of codes, refer to the ebook.

## To replicate the analysis in the book, download the Rscripts and the Excel spreadsheets in a 
## folder on your computer. 

## Each line of code can be evaluated within your RStudio by having your cursor on the 
## line and pressing Ctrl + Enter (Windows or Linux) or Cmd + Enter (Mac)


## Next set the working directory to this folder, using the setwd() function.
## To set the working directory, note that the path should be given as a string (enclosed in "") as
## an argument to setwd(). 

#### If you are using Unix based architecture (Mac or Linux) then simply 
#### write the path naturally. For example, if the path is /home/user/Desktop then simply type

setwd("/home/user/Desktop")

#### If you are using Windows machine, then \ characters should be escaped. For example, 
#### suppose that you place files in the folder C:\Users\user\Documents\Refresher. Then type

setwd("C:\\Users\\user\\Documents\\Refresher")


## ----installpackages, eval=FALSE------------------------------------------------------------------
install.packages("readxl")
install.packages("dplyr")
install.packages("ggplot2")
install.packages("forcats")


## -------------------------------------------------------------------------------------------------
library(readxl)
library(dplyr)
library(ggplot2)
library(forcats)


## -------------------------------------------------------------------------------------------------
gst <- read_xlsx("data_ch2.xlsx", sheet="gst")
head(gst)


## -------------------------------------------------------------------------------------------------
gst$Date<-as.Date(with(gst,paste(Year,Month,"01",sep="-")),"%Y-%m-%d")


## -------------------------------------------------------------------------------------------------
ggplot(gst, aes(x=Date, y=TemperatureAnomaly)) +
  geom_line(col="blue") +
  geom_hline(yintercept=0, lty="dashed", col="black", linewidth=1) +
  geom_segment(y=0,yend=0, x=as.Date("1951-01-01"), xend=as.Date("1980-12-01"), col="red", linewidth=1) +
  labs(title="Time series line plot", subtitle = "Average global monthly temperature anomalies") +
  xlab("Time") + ylab("Degrees relative to 1951-1980 average")


## -------------------------------------------------------------------------------------------------
# Create new data frame based on the original one, and create a variable for the decade
gst.decade <- gst %>% mutate(Decade = floor(Year/10)*10)

# Create a new variable which equals the mean monthly anomaly for all months in each decade
gst.decade <- gst.decade %>% group_by(Decade) %>% mutate(aveTempAnomaly = mean(TemperatureAnomaly))

# Keep only one observation per decade
gst.decade <- gst.decade %>% group_by(Decade) %>% filter(row_number() == 1)

# Remove observations for the 2020-2029 decade, as sample is smaller
gst.decade <- subset(gst.decade, Decade<2020)

# Report a table of average temperatures by decade 
tab <- cbind(gst.decade$Decade, gst.decade$aveTempAnomaly)

tab <- gst.decade %>% select(c("Decade", "aveTempAnomaly")) 
tab


## -------------------------------------------------------------------------------------------------
ggplot(gst.decade, aes(x=Decade, y=aveTempAnomaly)) +
  geom_point(col="blue") +
  geom_line(col="blue")+
  geom_hline(yintercept=0, lty="dashed", col="black", linewidth=1) +
  geom_segment(y=0,yend=0, x=1950, xend=1980, col="red", linewidth=1) +
  labs(title="Time series line plot", subtitle = "Decade averages of average global monthly temperature anomalies") +
  xlab("Time") + ylab("Degrees relative to 1951-1980 average")


## -------------------------------------------------------------------------------------------------
ggplot(gst.decade, aes(x=as.factor(Decade), y=aveTempAnomaly)) +
  geom_bar(stat="identity", fill="blue")+
  geom_hline(yintercept=0, lty="dashed", col="black", linewidth=1) +
  geom_segment(y=0,yend=0, x=10.5, xend=13.5, col="red", linewidth=1) +
  labs(title="Bar chart", subtitle = "Decade averages of average global monthly temperature anomalies") +
  xlab("Time") + ylab("Degrees relative to 1951-1980 average")




## -------------------------------------------------------------------------------------------------
# Clear R environment
rm(list = ls())


## -------------------------------------------------------------------------------------------------
antarctic <- read_xlsx("data_ch2.xlsx", sheet="ice")
head(antarctic)


## -------------------------------------------------------------------------------------------------
ggplot(antarctic, aes(x=Time, y=Mass)) + geom_line(col="blue") +
  labs(title="Time series line plot", subtitle = "Change in Antarctica ice mass") +
  xlab("Time") + ylab("Gigatonnes, Change relative to 2002")


## -------------------------------------------------------------------------------------------------
# Clear R environment
rm(list = ls())


## -------------------------------------------------------------------------------------------------
sea <- read_xlsx("data_ch2.xlsx", sheet="sea")
head(sea)


## -------------------------------------------------------------------------------------------------
ggplot(sea, aes(x=Time, y=GMSL)) + geom_line(col="blue") +
  labs(title="Time series line plot", subtitle = "Global mean sea level change") +
  xlab("Time") + ylab("mm, Change relative to 1993")


## -------------------------------------------------------------------------------------------------
# Clear R environment
rm(list = ls())


## -------------------------------------------------------------------------------------------------
co2df <- read_xlsx("data_ch2.xlsx", sheet = "co2")
co2df$Time <- as.Date(co2df$Time)
head(co2df)


## -------------------------------------------------------------------------------------------------
ggplot(co2df, aes(x=Time, y=co2)) + geom_line(col="blue")+
  labs(title="Time series line plot", subtitle = "Atmospheric CO2 levels measured at Mauna Loa Observatory") +
  xlab("Time") + ylab("CO2, parts per million")


## -------------------------------------------------------------------------------------------------
# Clear R environment
rm(list = ls())


## -------------------------------------------------------------------------------------------------
co2world <- read_xlsx("data_ch2.xlsx", sheet = "co2emissions")
head(co2world)


## -------------------------------------------------------------------------------------------------
co2world$co2 <- co2world$co2/1000000000 


## -------------------------------------------------------------------------------------------------
ggplot(co2world, aes(x=Year, y=co2)) + geom_line(col="blue")+
  labs(title="Time series line plot", subtitle = "Global CO2 emissions from fossil fuels") +
  xlab("Year") + ylab("Billions of tonnes")


## -------------------------------------------------------------------------------------------------
# Clear R environment
rm(list=ls())


## -------------------------------------------------------------------------------------------------
gnico22019 <- read_xlsx("data_ch2.xlsx", sheet = "gni_co2_pc_2019")
head(gnico22019)

## -------------------------------------------------------------------------------------------------
ggplot(gnico22019, aes(x=log(GNIpc), y=log(CO2pc))) + geom_point(col="blue")+
  labs(title="Scatterplot", subtitle = "GNI p.c. (PPP) and CO2 emissions p.c., 2019") +
  xlab("log(International Dollars per capita)") + ylab("log(Metric tonnes per capita)")


## -------------------------------------------------------------------------------------------------
# Clear R environment
rm(list = ls())


## -------------------------------------------------------------------------------------------------
uktemp <- read_xlsx("data_ch2.xlsx", sheet = "uktemp")
head(uktemp)


## -------------------------------------------------------------------------------------------------
ggplot(uktemp, aes(x=Year, y=AnnualMeanTemperature)) + 
  geom_line(col="blue") + 
  labs(title="Time series line plot", subtitle = "Average annual temperature in UK") +
  xlab("Time") + ylab("Degrees")


## -------------------------------------------------------------------------------------------------
mean5180 <- mean(subset(uktemp, Year>=1951 & Year<=1980)$AnnualMeanTemperature)
uktemp$AnnualAnomaly <- uktemp$AnnualMeanTemperature - mean5180


## -------------------------------------------------------------------------------------------------
ggplot(uktemp, aes(x=Year, y=AnnualAnomaly)) + 
  geom_line(col="blue") +
  geom_hline(yintercept=0, lty="dashed", col="black", linewidth=1) +
  geom_segment(y=0,yend=0, x=1951, xend=1980, col="red", linewidth=1) +
  labs(title="Time series line plot", subtitle = "Average annual temperature anomalies, UK") +
  xlab("Time") + ylab("Degrees relative to 1951-1980 average")


## -------------------------------------------------------------------------------------------------
# Create a variable measuring decade
uktemp.decade <- uktemp %>% mutate(Decade = floor(Year/10)*10)

# Create variable aveTempAnomaly equal to the average of annual temperatures for the corresponding decade.
uktemp.decade <- uktemp.decade %>% group_by(Decade) %>% mutate(aveTempAnomaly = mean(AnnualAnomaly))

# Keep single observation for decade
uktemp.decade <- uktemp.decade %>% group_by(Decade) %>% filter(row_number() == 1)

# Remove observations for "incomplete" decades
uktemp.decade <- subset(uktemp.decade, Decade<2020&Decade>1880)

# Plot 10-year average temperatures against corresponding decades 
ggplot(uktemp.decade, aes(x=Decade, y=aveTempAnomaly)) +
  geom_point(col="blue") +
  geom_line(col="blue")+
  geom_hline(yintercept=0, lty="dashed", col="black", linewidth=1) +
  geom_segment(y=0,yend=0, x=1950, xend=1980, col="red", linewidth=1) +
  labs(title="Time series line plot", subtitle = "Decade averages of average annual UK temperature anomalies") +
  xlab("Time") + ylab("Degrees relative to 1951-1980 average")


## -------------------------------------------------------------------------------------------------
# Clear R environment
rm(list = ls())


## -------------------------------------------------------------------------------------------------
ukrain <- read_xlsx("data_ch2.xlsx", sheet = "ukrain")
head(ukrain)


## -------------------------------------------------------------------------------------------------
ggplot(ukrain, aes(x=Year, y=Rain)) + 
  geom_line(col="blue") + 
  labs(title="Time series line plot", subtitle = "Average annual rainfall in UK") +
  xlab("Time") + ylab("Millimeters")


## -------------------------------------------------------------------------------------------------
# Calucalte mean annual rainfall for 1951-1980
mean5180 <- mean(subset(ukrain, Year>=1951 & Year<=1980)$Rain)

# Calculate annual anomaly as difference between actual annual rainfall and the 1951-1980 average
ukrain$AnnualAnomaly <- ukrain$Rain - mean5180

# Plot annual anomalies against year 
ggplot(ukrain, aes(x=Year, y=AnnualAnomaly)) + 
  geom_line(col="blue") +
  geom_hline(yintercept=0, lty="dashed", col="black", linewidth=1) +
  geom_segment(y=0,yend=0, x=1951, xend=1980, col="red", linewidth=1) +
  labs(title="Time series line plot", subtitle = "Annual UK rain anomalies") +
  xlab("Time") + ylab("Millimeters relative to 1951-1980 average")


## -------------------------------------------------------------------------------------------------
# Create a variable measuring the decade of observation
ukrain.decade <- ukrain %>% mutate(Decade = floor(Year/10)*10)

# Create a variable measuring the average anomaly per decade
ukrain.decade <- ukrain.decade %>% group_by(Decade) %>% mutate(aveAnomaly = mean(AnnualAnomaly))

# Keep single observation per decade
ukrain.decade <- ukrain.decade %>% group_by(Decade) %>% filter(row_number() == 1)

# Remove incomplete decades
ukrain.decade <- subset(ukrain.decade, Decade>1830&Decade<2020)


# Plot
ggplot(ukrain.decade, aes(x=Decade, y=aveAnomaly)) +
  geom_point(col="blue") +
  geom_line(col="blue")+
  geom_hline(yintercept=0, lty="dashed", col="black", linewidth=1) +
  geom_segment(y=0,yend=0, x=1950, xend=1980, col="red", linewidth=1) +
  labs(title="Time series line plot", subtitle = "Decade averages of annual rain anomalies, UK") +
  xlab("Time") + ylab("Millimeters relative to 1951-1980 average")


## -------------------------------------------------------------------------------------------------
# Clear R environment
rm(list = ls())


## -------------------------------------------------------------------------------------------------
ukco2 <- read_xlsx("data_ch2.xlsx", sheet = "ukco2")
head(ukco2)


## -------------------------------------------------------------------------------------------------
ukco2$co2 <- ukco2$co2/1000000
ukco2$co2cons <- ukco2$co2cons/1000000


## -------------------------------------------------------------------------------------------------
ggplot(ukco2, aes(x=Year, y=co2)) + 
  geom_line(col="blue") + 
  labs(title="Time series line plot", subtitle = "Annual production-based CO2 emissions, UK") +
  xlab("Time") + ylab("Millions of tonnes")


## -------------------------------------------------------------------------------------------------
ggplot(ukco2, aes(x=Year)) + 
  geom_line(aes(y=co2,col="Production")) + 
  geom_line(aes(y=co2cons,col="Consumption")) + 
  labs(title="Time series line plot", subtitle = "Annual CO2 emissions, UK", col = "Measure") +
  xlab("Time") + ylab("Millions of tonnes") + 
  scale_color_manual(values=c("red", "blue"))


## -------------------------------------------------------------------------------------------------
ggplot(ukco2, aes(x=Year)) + 
  geom_line(aes(y=co2,col="Production")) + 
  geom_line(aes(y=co2cons,col="Consumption")) + 
  labs(title="Time series line plot", subtitle = "Annual CO2 emissions, UK", col = "Measure") +
  xlab("Time") + ylab("Millions of tonnes") + 
  xlim(c(1988,2022)) +
  scale_color_manual(values=c("red", "blue"))

