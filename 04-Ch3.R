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
install.packages("pracma")
install.packages("scales")
## 
## 


## -------------------------------------------------------------------------------------------------
library(readxl)
library(ggplot2)
library(scales)
library(dplyr)
library(forcats)
library(pracma)


## -------------------------------------------------------------------------------------------------
wid2017 <- read_xlsx("data_ch3.xlsx", sheet = "WID2017")
wid2017$Country <- as.factor(wid2017$Country)
head(wid2017)


## -------------------------------------------------------------------------------------------------
summary(wid2017)


## -------------------------------------------------------------------------------------------------
ggplot(wid2017) + 
  geom_bar(aes(x=fct_reorder(Country, gini), y=gini), stat="identity", fill="red", col="black") +
  theme(axis.text.y= element_text(size=6)) + 
  xlab("") + ylab("Gini coefficient") +
  labs(title = "Gini coefficients across countries", 
       subtitle = "Pre-tax income from all sources") +
  coord_flip()


## -------------------------------------------------------------------------------------------------
ggplot(wid2017) + 
  geom_bar(aes(x=fct_reorder(Country, s10), y=s10), stat="identity", fill="red", col="black") +
  theme(axis.text.y= element_text(size=6)) + 
  xlab("") + ylab("Top 10 % income share") +
  labs(title = "Top 10% income shares across countries", 
       subtitle = "Pre-tax income from all sources") +
  coord_flip()


## -------------------------------------------------------------------------------------------------
ggplot(wid2017) + 
  geom_bar(aes(x=fct_reorder(Country, s1), y=s1), stat="identity", fill="red", col="black") +
  theme(axis.text.y= element_text(size=6)) + 
  xlab("") + ylab("Top 1% income share") +
  labs(title = "Top 1% income shares across countries", 
       subtitle = "Pre-tax income from all sources") +
  coord_flip()


## -------------------------------------------------------------------------------------------------
ggplot(wid2017) + 
  geom_bar(aes(x=fct_reorder(Country, p90p50), y=p90p50), stat="identity", fill="red", col="black") +
  theme(axis.text.y= element_text(size=6)) + 
  xlab("") + ylab("P90P50 ratio") +
  labs(title = "P90P50 ratio across countries", 
       subtitle = "Pre-tax income from all sources") +
  coord_flip()


## -------------------------------------------------------------------------------------------------
ggplot(subset(wid2017, !is.na(p50p10))) + 
  geom_bar(aes(x=fct_reorder(Country, p50p10), y=p50p10), stat="identity", fill="red", col="black") +
  theme(axis.text.y= element_text(size=6)) + 
  xlab("") + ylab("P50P10 ratio") +
  labs(title = "P50P10 ratio across countries", 
       subtitle = "Pre-tax income from all sources") +
  coord_flip()


## -------------------------------------------------------------------------------------------------
ggplot(wid2017, aes(x=gini)) + 
  geom_point(aes(y=s10, col="Top 10%")) +
  geom_point(aes(y=s1, col="Top 1%")) +
  geom_point(aes(y=s0.1, col="Top 0.1%")) +
  xlab("Gini coefficient") +
  ylab("Measure") +
  labs(title = "Scatterplot", subtitle = "Association between Gini coefficient and top income shares,  countries", col = "Income share")


## -------------------------------------------------------------------------------------------------
ggplot(wid2017, aes(x=gini)) +
  geom_point(aes(y=p50p10, col="P50P10")) +
  geom_point(aes(y=p90p50, col="P90P50")) +
  xlab("Gini coefficient") +
  ylab("Measure") +
  labs(title = "Scatterplot", subtitle = "Association between Gini coefficient and percentile ratios, countries", col = "Measure")


## -------------------------------------------------------------------------------------------------
panel <- read_xlsx("data_ch3.xlsx", sheet = "WIDpanel")
panel$Country <- as.factor(panel$Country)
head(panel)


## -------------------------------------------------------------------------------------------------
levels(panel$Country)


## -------------------------------------------------------------------------------------------------
ggplot(panel, aes(x=Year, y=gini, col=Country)) + 
  geom_line() + 
  geom_point() +
  scale_color_brewer(palette = "Dark2") +
  xlab("") + ylab("Gini coefficient") +
  labs(title = "Time series line plot", subtitle = "Gini coefficient over time")


## -------------------------------------------------------------------------------------------------
ggplot(panel, aes(x=Year, y=s10, col=Country)) + 
  geom_line() + 
  geom_point() +
  scale_color_brewer(palette = "Dark2") +
  xlab("") + ylab("Top 10% income share") +
  labs(title = "Time series line plot", subtitle = "Top 10% income share over time")


## -------------------------------------------------------------------------------------------------
ggplot(panel, aes(x=Year, y=s1, col=Country)) + 
  geom_line() + 
  geom_point() +
  scale_color_brewer(palette = "Dark2") +
  xlab("") + ylab("Top 1% income share") +
  labs(title = "Time series line plot", subtitle = "Top 1% income share over time")

