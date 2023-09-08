## ----setup, include=FALSE-------------------------------------------------------------------------
knitr::opts_chunk$set(echo = TRUE, warning=FALSE, message = FALSE)
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


## Install packages if not already installed
install.packages("readxl")
install.packages("dplyr")
install.packages("ggplot2")
install.packages("tidyr")



## Load packages
library(readxl)
library(dplyr)
library(ggplot2)
library(tidyr)


## -------------------------------------------------------------------------------------------------
data <- read_excel("data_ch0.xlsx", sheet="mini")

data


## ---- rows.print=12-------------------------------------------------------------------------------
long.data <- data %>% 
  pivot_longer(
    cols = `2019`:`2021`, 
    names_to = "Year",
    values_to = "GNIpc"
  )

long.data


## -------------------------------------------------------------------------------------------------
wide.data <- long.data %>%
  pivot_wider(names_from = Year, values_from = GNIpc)

wide.data


## -------------------------------------------------------------------------------------------------
cs.data <- subset(long.data, Year==2021)

cs.data


## -------------------------------------------------------------------------------------------------
ts.data <- subset(long.data, Country=="UK")

ts.data


## -------------------------------------------------------------------------------------------------
summary(cs.data$GNIpc)


## -------------------------------------------------------------------------------------------------
summary(cs.data$Region)


## -------------------------------------------------------------------------------------------------
cs.data$Region <- as.factor(cs.data$Region) 


## -------------------------------------------------------------------------------------------------
summary(cs.data$Region)


## -------------------------------------------------------------------------------------------------
csdata <- read_excel("data_ch0.xlsx", sheet="csdata")

csdata


## -------------------------------------------------------------------------------------------------
hist(csdata$GNIpc)


## -------------------------------------------------------------------------------------------------
ggplot(csdata, aes(x=GNIpc)) + 
  geom_histogram()


## -------------------------------------------------------------------------------------------------
ggplot(csdata, aes(x=GNIpc)) + 
  geom_histogram(bins=60, color="black", fill="red", alpha=0.5)+
  xlab("GNI per capita, PPP (2019)") + ylab("Number of countries") +
  labs(title="Histogram",
       subtitle="Distribution of real GNI per capita across countries, 2019")

