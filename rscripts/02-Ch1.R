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
install.packages("tidyr")
install.packages("dplyr")
install.packages("ggplot2")
install.packages("forcats")
install.packages("stringr")
install.packages("maps")
install.packages("knitr")


## -----------------------------------------------------
library(readxl)
library(tidyr)
library(dplyr)
library(ggplot2)
library(forcats)
library(stringr)
library(maps)
library(knitr)



## -------------------------------------------------------------------------------------------------
# Load full panel dataset in long form
longdata <- read_excel("data_ch1.xlsx", sheet = "gnipcppp_long")
head(longdata)


## -------------------------------------------------------------------------------------------------
rm(list = ls())


## -------------------------------------------------------------------------------------------------
# Load full panel dataset in long form
longdata <- read_excel("data_ch1.xlsx", sheet = "gnipcppp_long")

# Obtain a cross-section for Year 2019 only, and allocate to dataframe `csdata`

csdata <- subset(longdata, Year==2019)

# Prepare by declaring `Country`, `Code` and `Region` to be factor (or categorical) variables

csdata$Code <- as.factor(csdata$Code)
csdata$Region <- as.factor(csdata$Region)
csdata$Country <- as.factor(csdata$Country)

# Remove full panel dataset from memory
rm(longdata)


## -------------------------------------------------------------------------------------------------
csdata <- csdata %>% arrange(desc(rGNIpc))

csdata


## -------------------------------------------------------------------------------------------------
kable(summary(csdata$Region))



## -------------------------------------------------------------------------------------------------
summary(csdata$rGNIpc)


## -------------------------------------------------------------------------------------------------
# Load world map data from server
world <- map_data("world")

# Merge world map data with GNI data

gnimap.data <- inner_join(world, csdata, by=c("region"="Country"))

# Set a simple map theme

plainmap <- theme(
  axis.text = element_blank(),
  axis.line = element_blank(),
  axis.ticks = element_blank(),
  panel.border = element_blank(),
  panel.grid = element_blank(),
  axis.title = element_blank(),
  panel.background = element_rect(fill = "white"),
  plot.title = element_text(hjust = 0.5)
)


## -------------------------------------------------------------------------------------------------

ggplot(data = gnimap.data, mapping = aes(x = long, y = lat, group = group)) + 
  coord_fixed(1.3) +
  geom_polygon(aes(fill = rGNIpc)) +
  scale_fill_distiller(palette ="Blues", direction = -1, name="international $") + 
  ggtitle("GNI per capita, PPP (2019)") + plainmap


## -------------------------------------------------------------------------------------------------
ggplot(data = gnimap.data, mapping = aes(x = long, y = lat, group = group)) + 
  coord_fixed(1.3) +
  geom_polygon(aes(fill = Region)) +
  ggtitle("Regions by World Bank classification") + plainmap 


## -------------------------------------------------------------------------------------------------
# Clean up

rm(gnimap.data, plainmap, world,longdata)



## -------------------------------------------------------------------------------------------------
csdata <- subset(csdata, !is.na(rGNIpc))



## -------------------------------------------------------------------------------------------------
ggplot(csdata, aes(x=rGNIpc)) + 
  geom_histogram(bins=60, color="black", fill="red", alpha=0.5)+
  xlab("GNI per capita, PPP (2019)") + ylab("Number of countries") +
  labs(title="Histogram",
       subtitle="Distribution of real GNI per capita across countries, 2019")


## -------------------------------------------------------------------------------------------------
quantile(csdata$rGNIpc,seq(0.1,0.9,0.1))


## -------------------------------------------------------------------------------------------------

# Create a variable rGNIpc.rank representing the quantile rank of each country in the distribution of GNI p.c.

csdata <- csdata %>% mutate(rGNIpc.rank = rank(rGNIpc)/length(rGNIpc))

# (Not essential) Sort country codes by GNI pc 

csdata$Code <- fct_reorder(csdata$Code,csdata$rGNIpc)



## -------------------------------------------------------------------------------------------------


ggplot(csdata, aes(y=rGNIpc, x=rGNIpc.rank)) + 
  geom_line() +
  xlab("Quantile rank") + ylab("International dollars") + labs(title="Empirical quantile function", subtitle = "GNI per capita (PPP), 2019")



## -------------------------------------------------------------------------------------------------
ggplot(csdata, aes(y=rGNIpc, x=rGNIpc.rank)) + 
  geom_point(col="tomato") +
  geom_segment(aes(x=rGNIpc.rank, xend=rGNIpc.rank, y=0, yend=rGNIpc), col="tomato") +
  geom_text(aes(label=Code), check_overlap = TRUE, angle=90, hjust=-0.5, size=2) + 
  xlab("Quantile rank") + ylab("International dollars") + labs(title="Empirical quantile function", subtitle = "GNI per capita (PPP), 2019")



## -------------------------------------------------------------------------------------------------
ggplot(csdata, aes(y=log(rGNIpc), x=rGNIpc.rank)) + 
  geom_point(col="tomato") +
  geom_segment(aes(x=rGNIpc.rank, xend=rGNIpc.rank, y=4, yend=log(rGNIpc)), col="tomato") +
  geom_text(aes(label=Code), check_overlap = TRUE, angle=90, hjust=-0.5, size=2) + 
  xlab("Quantile rank") + ylab("International dollars") + labs(title="Empirical quantile function", subtitle = "GNI per capita (PPP), 2019")



## -------------------------------------------------------------------------------------------------
ggplot(csdata, aes(y=rGNIpc, x=rGNIpc.rank, col=Region)) + 
  geom_point() +
  geom_segment(aes(x=rGNIpc.rank, xend=rGNIpc.rank, y=0, yend=rGNIpc)) +
  geom_text(aes(label=Code), check_overlap = TRUE, angle=90, hjust=-0.5, size=2) + 
  xlab("Quantile rank") + ylab("International dollars") + labs(title="Empirical quantile function", subtitle = "GNI per capita (PPP), 2019") +   theme(legend.position = c(0.16, 0.7))



## -------------------------------------------------------------------------------------------------
# Rearrange region in order of the median GNI pc.
csdata$Region <- fct_reorder(csdata$Region,csdata$rGNIpc, .fun = median)


## -------------------------------------------------------------------------------------------------
ggplot(csdata, aes(x=rGNIpc, fill=Region)) + 
  geom_histogram(bins=10,color="black", alpha=0.9)+
  labs(title="Histograms",
       subtitle="Distribution of GNI per capita within regions, 2019") +
  xlab("GNI per capita, PPP (2019)") + ylab("Number of countries") +
  facet_wrap(Region~., scales="free_y", strip.position = "bottom")+theme(legend.position = "none")


## -------------------------------------------------------------------------------------------------
ggplot(csdata, aes(x=Region, y=rGNIpc)) + 
  geom_point(aes(col=Region), show.legend=F) +
  labs(title="Dot plot", subtitle="GNI per capita grouped by regions, 2019") + 
  ylab("International $") + xlab("") +  coord_flip()


## -------------------------------------------------------------------------------------------------
csdata %>% group_by(Region) %>% summarize(median=median(rGNIpc), mean=mean(rGNIpc), sd=sd(rGNIpc))



## -------------------------------------------------------------------------------------------------
ggplot(csdata, aes(y=rGNIpc, x=Region)) + 
  geom_bar(stat='summary', fun='median', aes(fill=Region), show.legend=F) +   
  labs(title="Bar chart",subtitle="Median GNI per capita (PPP), 2019") + 
  xlab("") + ylab("International $") +
  coord_flip()


## -------------------------------------------------------------------------------------------------
ggplot(csdata, aes(x=Region, y=rGNIpc)) + 
  geom_boxplot(aes(fill=Region), show.legend=F) +
  labs(title="Box plot", subtitle="GNI per capita (PPP), 2019") + 
  xlab("Region") + ylab("International $") + coord_flip()


## -------------------------------------------------------------------------------------------------
csdata <- csdata %>% arrange(rGNIpc)
csdata <- csdata %>% mutate(qrank.rGNIpc=ntile(rGNIpc,5))
csdata$qrank.rGNIpc <- as.factor(csdata$qrank.rGNIpc)
levels(csdata$qrank.rGNIpc) = c("0-20%", "20-40%", "40-60%", "60-80%", "80-100%")


## -------------------------------------------------------------------------------------------------
tab <- table(csdata$Region, csdata$qrank.rGNIpc)
kable(tab)


## -------------------------------------------------------------------------------------------------
kable(prop.table(tab), digits=2)



## -------------------------------------------------------------------------------------------------
kable(prop.table(tab,1), digits=2)



## -------------------------------------------------------------------------------------------------
kable(prop.table(tab,2))



## -------------------------------------------------------------------------------------------------
ggplot(csdata, aes(x=Region, fill=qrank.rGNIpc)) + 
  geom_bar(position='dodge', color="black") +
  labs(title="Bar chart",subtitle="Distribution of countries across GNI quintiles by region, 2019") + 
  xlab("GNI per capita (PPP) Quintile") + ylab("Number of countries") 


## -------------------------------------------------------------------------------------------------
ggplot(csdata, aes(fill=Region, x=qrank.rGNIpc), na.rm = T) + 
  geom_bar(position='dodge', color="black") +
  labs(title="Bar chart",subtitle="Distribution of countries across regions by GNI quintile, 2019") + 
  xlab("GNI per capita (PPP) Quintile") + ylab("Number of countries") 


## -------------------------------------------------------------------------------------------------
rm(csdata, tab)


## -------------------------------------------------------------------------------------------------
csdata.all <- read_excel("data_ch1.xlsx", sheet="cs2019_multi")
head(csdata.all)


## -------------------------------------------------------------------------------------------------
ggplot(csdata.all,aes(x=rGNIpc, y=LifeExpectancy)) +
  geom_point()


## -------------------------------------------------------------------------------------------------
ggplot(csdata.all,aes(x=log(rGNIpc), y=log(LifeExpectancy))) +
  geom_point()


## -------------------------------------------------------------------------------------------------
ggplot(csdata.all,aes(x=log(rGNIpc), y=log(LifeExpectancy))) +
  geom_point(col="red") + xlab("log(GNI per capita, PPP)") +
  ylab("log(Life Expectancy)") + labs(title="Scatterplot", subtitle="GNI per capita and life expecancy, 2019")


## -------------------------------------------------------------------------------------------------
ggplot(csdata.all,aes(x=log(rGNIpc), y=log(LifeExpectancy),label=Country)) +
  geom_text(col="red", check_overlap=TRUE, size=3) + xlab("log(GNI per capita, PPP)") +
  ylab("log(Life Expectancy)") + labs(title="Scatterplot", subtitle="GNI per capita and life expecancy, 2019")


## -------------------------------------------------------------------------------------------------
cor(csdata.all$rGNIpc, csdata.all$LifeExpectancy,use="complete.obs")



## -------------------------------------------------------------------------------------------------
cor(log(csdata.all$rGNIpc), log(csdata.all$LifeExpectancy),use="complete.obs")



## -------------------------------------------------------------------------------------------------
ggplot(csdata.all,aes(x=log(rGNIpc), y=log(CO2pc), label=Country)) +
  geom_point(col="tomato2") + xlab("log(GNI per capita)") +
  ylab("log(CO2 emissions per capita)") + labs(title="Scatterplot", subtitle="GNI per capita and CO2 emissions")

## -------------------------------------------------------------------------------------------------
cor(log(csdata.all$rGNIpc), log(csdata.all$CO2pc),use="complete.obs")



## -------------------------------------------------------------------------------------------------
rm(csdata.all)


## -------------------------------------------------------------------------------------------------
ukgdp <- read_excel("data_ch1.xlsx", sheet="uk_gdppc_qr")
ukgdp$date <- as.Date(ukgdp$date)

head(ukgdp)



## -------------------------------------------------------------------------------------------------
ggplot(ukgdp, aes(x=date, y=gdppc)) +
  geom_line() 


## -------------------------------------------------------------------------------------------------
ggplot(ukgdp, aes(x=date, y=gdppc)) +
  geom_line(col="red", size=1) +
  labs(title="Time-series line plot", subtitle = "Real GDP per capita, quarterly, UK") +
  xlab("") + ylab("2022 £s") 


## -------------------------------------------------------------------------------------------------
ggplot(ukgdp, aes(x=date, y=log(gdppc))) +
  geom_line(col="red" ,size=1) +
  labs(title="Time-series line plot", subtitle = "Real GDP per capita, quarterly, UK") +
  xlab("") + ylab("log(2022 £s)")


## -------------------------------------------------------------------------------------------------
avegr.preGR<-(ukgdp$gdppc[ukgdp$date=="2007-01-01"]/ukgdp$gdppc[ukgdp$date=="1955-01-01"])^(1/((2007-1955)*4))-1


## -------------------------------------------------------------------------------------------------
ukgdp$cfgdppc <- ukgdp$gdppc[ukgdp$date=="1955-01-01"]

for (i in 1:length(ukgdp$date)){
  ukgdp$cfgdppc[i]=ukgdp$cfgdppc[1]*(1+avegr.preGR)^(i-1)
}


## -------------------------------------------------------------------------------------------------
ggplot(ukgdp) +
  geom_line(aes(x=date, y=gdppc, linetype="Actual"), size=1, col="red") +
  geom_line(aes(x=date, y=cfgdppc, linetype="Pre-2007 trend"), alpha=0.4, size=1, col="blue") +
  labs(title="Time-series line plot", subtitle = "Real GDP per capita, quarterly, UK") +
  xlab("") + ylab("2022 £s") + labs(linetype="") 


## -------------------------------------------------------------------------------------------------
ggplot(ukgdp) +
  geom_line(aes(x=date, y=log(gdppc), linetype="Actual"),size=1, col="red") +
  geom_line(aes(x=date, y=log(cfgdppc), linetype="Pre-2007 trend"), alpha=0.4, col="blue", size=1) +
  labs(title="Time-series line plot", subtitle = "Real GDP per capita, quarterly, UK") +
  xlab("") + ylab("log(2022 £s)") + labs(linetype="") 


## -------------------------------------------------------------------------------------------------
recs <- read_excel("data_ch1.xlsx", sheet="recs")
recs$rec_st <- as.Date(recs$rec_st)
recs$rec_en <- as.Date(recs$rec_en)

recs


## -------------------------------------------------------------------------------------------------
ggplot(ukgdp) +
  geom_line(aes(x=date, y=gdppc), col="red", size=1) +
  labs(title="Time-series line plot", subtitle = "Real GDP per capita, quarterly, UK") +
  xlab("") + ylab("2022 £s") +
  geom_rect(data=recs,aes(xmin=rec_st, xmax=rec_en, ymin=-Inf, ymax=+Inf), fill='tomato', alpha=0.2)


## -------------------------------------------------------------------------------------------------
ukgdp <- ukgdp %>% mutate(gdppc.gr = (gdppc - lag(gdppc,4))/lag(gdppc,4))


## -------------------------------------------------------------------------------------------------
head(ukgdp)


## -------------------------------------------------------------------------------------------------
ggplot(ukgdp) +
  geom_line(aes(x=date, y=gdppc.gr), col="red") +
  labs(title="Time-series line plot", subtitle = "Real GDP growth rate, quarterly, UK") +
  xlab("") + ylab("Percentage change on year ago") +
  geom_rect(data=recs,aes(xmin=rec_st, xmax=rec_en, ymin=-Inf, ymax=+Inf), fill='blue', alpha=0.2)


## -------------------------------------------------------------------------------------------------
ggplot(subset(ukgdp,date<"2020-01-01")) +
  geom_line(aes(x=date, y=gdppc.gr), col="red") +
  labs(title="Time-series line plot", subtitle = "Real GDP growth rate, quarterly, UK") +
  xlab("") + ylab("Percentage change on year ago") +
  geom_rect(data=recs[1:7,],aes(xmin=rec_st, xmax=rec_en, ymin=-Inf, ymax=+Inf), fill='blue', alpha=0.2)


## -------------------------------------------------------------------------------------------------
ggplot(ukgdp, aes(x=gdppc.gr)) +
  geom_histogram(col="black", fill="red", binwidth=0.01)+
  labs(title="Histogram", subtitle = "Distribution of growth rates") +
  ylab("Number of quarters") + xlab("Percentage change on year ago") 


## -------------------------------------------------------------------------------------------------
ukgdp$year <- as.numeric(format(ukgdp$date, format="%Y"))
ukgdp <- ukgdp %>% mutate(decade = floor(year / 10) * 10)
ukgdp$decade <- as.factor(ukgdp$decade)
levels(ukgdp$decade) <- c("1950s", "1960s", "1970s", "1980s", "1990s", "2000s", "2010s", "2020s")

ukgdp.trunc <- subset(ukgdp,year<2020&year>1959)

ggplot(ukgdp.trunc, aes(x=decade)) +
  geom_boxplot(aes(y=gdppc.gr, col=decade)) +
  coord_flip() +
  geom_hline(aes(yintercept=mean(gdppc.gr, na.rm=TRUE), col="Average"), linetype="dashed", size=1)+
  labs(title="Box plot", subtitle = "Distribution of growth rates by decade") +
  xlab("Decade") + ylab("Percentage change on year ago") + theme(legend.position="False")


## -------------------------------------------------------------------------------------------------
rm(ukgdp,ukgdp.decade, avegr.preGR, i, ukgdp.trunc, recs)


## -------------------------------------------------------------------------------------------------
longdata <- read_excel("data_ch1.xlsx", sheet = "gnipcppp_long")

longdata$Code <- as.factor(longdata$Code)
longdata$Region <- as.factor(longdata$Region)
longdata$Country <- as.factor(longdata$Country)


## -------------------------------------------------------------------------------------------------
head(longdata)


## -------------------------------------------------------------------------------------------------
df <- subset(longdata, Country == "UK"|
         Country == "USA"|
         Country == "Germany"|
         Country == "France"|
         Country == "Japan"|
           Country == "Ireland"|Country == "Italy"|Country=="Canada")

ggplot(df, aes(x=Year, y=log(rGNIpc), col=Country)) +
  geom_line(linewidth=1) +
  xlab("Year") + 
  ylab("log(GNI per capita, PPP)") +
  labs(title = "Time series plot", subtitle="Evolution of GNI per capita across a set of developed economies")


## -------------------------------------------------------------------------------------------------
df <- subset(longdata, Country == "USA"|
               Country == "South Korea"|
               Country == "Singapore"|
               Country == "China"|
               Country == "Myanmar"|
               Country == "Indonesia"|Country == "Vietnam")

ggplot(df, aes(x=Year, y=log(rGNIpc), col=Country)) +
  geom_line(size=1)+
  xlab("Year") + 
  ylab("log(GNI per capita, PPP)") +
  labs(title = "Time series plot", subtitle="Evolution of GNI per capita across a set of developping economies")


## -------------------------------------------------------------------------------------------------
df <- subset(longdata, Country == "USA"|
               Country == "Democratic Republic of the Congo"|
               Country == "Zimbabwe"|
               Country == "Niger"|
               Country == "Somalia")

ggplot(df, aes(x=Year, y=log(rGNIpc), col=Country)) +
  geom_line(size=1)+
  xlab("Year") + 
  ylab("log(GNI per capita, PPP)") +
  labs(title = "Time series plot", subtitle="Evolution of GNI per capita across a set of developed economies")


## -------------------------------------------------------------------------------------------------
longdata<-longdata %>% group_by(Country) %>% mutate(rGNIpc.gr = (rGNIpc[Year==2019]/rGNIpc[Year==1973])^(1/46)-1)


## -------------------------------------------------------------------------------------------------
df<-longdata %>% group_by(Country) %>% filter(Year==1973&!is.na(rGNIpc.gr))


## -------------------------------------------------------------------------------------------------
df %>% arrange(rGNIpc.gr)


## -------------------------------------------------------------------------------------------------
ggplot(df,aes(x=rGNIpc.gr,y=fct_reorder(Country,rGNIpc.gr))) +
  geom_bar(stat="identity", aes(fill=Region)) +
  xlab("Average annual growth rate, 1973-2019") + 
  ylab("Country") +
  labs(title = "Bar chart", subtitle="Countries ranked by average growth rate of GNI per capita") + theme(axis.text.y = element_text(size=4))


## -------------------------------------------------------------------------------------------------
ggplot(df,aes(x=log(rGNIpc),y=(rGNIpc.gr), color=Region)) +
  geom_point() +
  xlab("log(GNI per capita, 1973)") + 
  ylab("Annual growth rate, 1973 - 2019") +
  labs(title = "Scatter plot", subtitle="Initial per capita income and subsequent growth rates") 

#+ facet_wrap(Region~., scales="free")


## -------------------------------------------------------------------------------------------------
cor(log(df$rGNIpc),df$rGNIpc.gr)


## -------------------------------------------------------------------------------------------------
df2 <- subset(df, Region!="Sub-Saharan Africa"&Region !="Middle East and North Africa"&Region !="Latin America and Caribbean")

ggplot(df2,aes(x=log(rGNIpc),y=(rGNIpc.gr), color=Region)) +
  geom_point() +
  xlab("log(GNI per capita, 1973)") + 
  ylab("Annual growth rate, 1973 - 2019") +
  labs(title = "Scatter plot", subtitle="Initial per capita income and subsequent growth rates") 


## -------------------------------------------------------------------------------------------------
cor(log(df2$rGNIpc),df2$rGNIpc.gr)

