getwd()
setwd("/Users/evelynxu/Documents/workplace_RStudio")
library(tidyverse)
library(mice)
library(plot.matrix)
library(VIM)
library(lubridate)
library(zoo)
library(scales)
library(dplyr)
library(ggplot2)
library(Rmisc)
library(ggridges)
library(gridExtra)

options(scipen = 3)

census <- read.csv("COVID-19_cases_plus_census.csv")
case_tx <- read.csv("COVID-19_cases_TX.csv")
mobility <- read.csv("Global_Mobility_Report.csv")
income <- read.csv("income.csv")
hospital <- read.csv("hospital.csv")
case <- read.csv("case.csv")

# Format
case_tx$state <- factor(case_tx$state)
case_tx$county_fips_code <- factor(case_tx$county_fips_code)
case_tx$state_fips_code <- factor(case_tx$state_fips_code)
case_tx$date <- as.Date(case_tx$date)

case_tx <- as_tibble(case_tx)

dim(mobility)
mobility <- mobility %>% mutate_if(is.character, factor)
summary(mobility)
head(mobility)

mobility_Dallas <- mobility %>% filter(sub_region_1 == "Texas" & sub_region_2 == "Dallas County")
dim(mobility_Dallas)

mobility_Dallas
mobility_Dallas$date <- as.Date(mobility_Dallas$date)
summary(mobility_Dallas)

# Make character factors for analysis
census <- census %>% mutate_if(is.character,factor)
dim(census)
case_tx <- case_tx %>% mutate_if(is.character,factor)
dim(case_tx)


# rows without NA
case_tx[!complete.cases(case_tx),]
census[!complete.cases(census),]

# count rows without NA
count(census[!complete.cases(census),])
count(case_tx[!complete.cases(case_tx),])

# how many NA
sum(is.na(case_tx))
sum(is.na(census))

matrixplot(case_tx)
matrixplot(census)

# clean case_tx with incorrect deaths data(94350 -> 94343)
clean <-function(){
  case_tx<-
    case_tx %>%
    arrange(county_name,date)%>%
    mutate(deaths_diff = deaths - lag(deaths,default = first(deaths))) %>%
    mutate(confirmed_cases_diff = confirmed_cases - lag(confirmed_cases,default = first(confirmed_cases)))
  
  y <- case_tx %>%filter(deaths_diff < 0,deaths != 0)
  while (nrow(y) != 0) {
    case_tx <-
      case_tx %>%
      filter(deaths_diff >= 0 | deaths == 0)
    
    case_tx<-
      case_tx %>%
      arrange(county_name,date)%>%
      mutate(deaths_diff = deaths - lag(deaths,default = first(deaths))) %>%
      mutate(confirmed_cases_diff = confirmed_cases - lag(confirmed_cases,default = first(confirmed_cases)))
    y <- case_tx %>%filter(deaths_diff < 0,deaths != 0)
  }
  case_tx
}
case_tx <- clean()

# select Dallas data
case_Dallas <- case_tx %>% 
  filter(county_name == "Dallas County" & state == "TX")
dim(case_Dallas)

# group and sum by date
case_Dallas_monthly <- case_Dallas %>%
  mutate(month = as.yearmon(case_Dallas$date,"%b %Y")) %>%
  group_by(month) %>%
  summarize_if(is.numeric,sum)

# group and sum by date
case_tx_amount <- case_tx %>% 
  group_by(date) %>% 
  summarize_if(is.numeric,sum)

case_Dallas_monthly
case_tx_amount

death_avg <- c(0,0,0,0,0,0,rollmean(case_Dallas$deaths,7)) 
death_avg

confirmed_avg <- c(0,0,0,0,0,0,rollmean(case_Dallas$confirmed_cases,7)) 
confirmed_avg

case_Dallas_avg <- case_Dallas %>% 
  mutate(death_avg = death_avg)  %>% 
  mutate(confirmed_avg = confirmed_avg) 


# Mobility
p1 <- ggplot(mobility_Dallas, mapping = aes(x = date, y = retail_and_recreation_percent_change_from_baseline),fill = 'blue') + 
  #geom_point() +
  geom_ridgeline(aes(x = date,height = retail_and_recreation_percent_change_from_baseline,y=0),min_height=-80)+
  geom_line(color = 'cornflowerblue') +
  geom_smooth(color= 'orange') +
  scale_x_date(date_breaks = "2 month",date_labels = "%b %Y")+
  scale_y_continuous(limits = c(-80,80),breaks = (seq(-80,80,40)), labels = percent_format(scale = 1))+
  labs(title = "Retail & Recreation", x = 'Date',y = 'Compared to Baseline') +
  theme(panel.grid.minor.x = element_blank(),panel.grid.major.x = element_blank())+
  theme(text=element_text(size=15))
  
p2 <- ggplot(mobility_Dallas, mapping = aes(x = date, y = grocery_and_pharmacy_percent_change_from_baseline),fill = 'blue') + 
  #geom_point() +
  geom_ridgeline(aes(x = date,height = grocery_and_pharmacy_percent_change_from_baseline,y=0),min_height=-80)+
  geom_line(color = 'cornflowerblue') +
  geom_smooth(color= 'orange') +
  scale_x_date(date_breaks = "2 month",date_labels = "%b %Y")+
  scale_y_continuous(limits = c(-80,80),breaks = (seq(-80,80,40)), labels = percent_format(scale = 1))+
  labs(title = "Grocery & Pharmacy", x = 'Date',y = 'Compared to Baseline') +
  theme(panel.grid.minor.x = element_blank(),panel.grid.major.x = element_blank())+
  theme(text=element_text(size=15))

p3 <- ggplot(mobility_Dallas, mapping = aes(x = date, y = parks_percent_change_from_baseline),fill = 'blue') + 
  #geom_point() +
  geom_ridgeline(aes(x = date,height = parks_percent_change_from_baseline,y=0),min_height=-80)+
  geom_line(color = 'cornflowerblue') +
  geom_smooth(color= 'orange') +
  scale_x_date(date_breaks = "2 month",date_labels = "%b %Y")+
  scale_y_continuous(limits = c(-80,80),breaks = (seq(-80,80,40)), labels = percent_format(scale = 1))+
  labs(title = "Parks", x = 'Date',y = 'Compared to Baseline') +
  theme(panel.grid.minor.x = element_blank(),panel.grid.major.x = element_blank())+
  theme(text=element_text(size=15))

p4 <- ggplot(mobility_Dallas, mapping = aes(x = date, y = transit_stations_percent_change_from_baseline),fill = 'blue') + 
  #geom_point() +
  geom_ridgeline(aes(x = date,height = transit_stations_percent_change_from_baseline,y=0),min_height=-80)+
  geom_line(color = 'cornflowerblue') +
  geom_smooth(color= 'orange') +
  scale_x_date(date_breaks = "2 month",date_labels = "%b %Y")+
  scale_y_continuous(limits = c(-80,80),breaks = (seq(-80,80,40)), labels = percent_format(scale = 1))+
  labs(title = "Transit stations", x = 'Date',y = 'Compared to Baseline') +
  theme(panel.grid.minor.x = element_blank(),panel.grid.major.x = element_blank())+
  theme(text=element_text(size=15))

p5 <- ggplot(mobility_Dallas, mapping = aes(x = date, y = workplaces_percent_change_from_baseline),fill = 'blue') + 
  #geom_point() +
  geom_ridgeline(aes(x = date,height = workplaces_percent_change_from_baseline,y=0),min_height=-80)+
  geom_line(color = 'cornflowerblue') +
  geom_smooth(color= 'orange') +
  scale_x_date(date_breaks = "2 month",date_labels = "%b %Y")+
  scale_y_continuous(limits = c(-80,80),breaks = (seq(-80,80,40)), labels = percent_format(scale = 1))+
  labs(title = "Workplaces", x = 'Date',y = 'Compared to Baseline') +
  theme(panel.grid.minor.x = element_blank(),panel.grid.major.x = element_blank())+
  theme(text=element_text(size=15))

p6 <- ggplot(mobility_Dallas, mapping = aes(x = date, y = residential_percent_change_from_baseline),fill = 'blue') + 
  #geom_point() +
  geom_ridgeline(aes(x = date,height = residential_percent_change_from_baseline,y=0),min_height=-80)+
  geom_line(color = 'cornflowerblue') +
  geom_smooth(color= 'orange') +
  scale_x_date(date_breaks = "2 month",date_labels = "%b %Y")+
  scale_y_continuous(limits = c(-80,80),breaks = (seq(-80,80,40)), labels = percent_format(scale = 1))+
  labs(title = "Residential", x = 'Date',y = 'Compared to Baseline') +
  theme(panel.grid.minor.x = element_blank(),panel.grid.major.x = element_blank())+
  theme(text=element_text(size=15))

grid.arrange(p1,p3,p2,p4,p5,p6, ncol=2, nrow=3, top = "Mobility of Dallas")



# select Top 5 counties
counties_top <- c("Harris County","Dallas County","Tarrant County","Bexar County","El Paso County")
top_5 <- case_tx %>% filter(county_name %in% counties_top)


# Deaths of Top 5 Counties(Lines)
line_counties_deaths<- ggplot(top_5,aes(x = date,y = deaths,group = county_name,colour = county_name)) + 
  scale_color_brewer(palette="Dark2")+
  geom_line(size = 1,alpha = I(7/10), show.legend=F,linetype = 'dashed') +
  geom_smooth(size = 1.2)+
  scale_x_date(date_breaks = "2 month",date_labels = "%b %Y")+
  labs(title = "Deaths of Top 5 Counties",x = "Date",y = "Deaths",color = "County")+
  theme(text=element_text(size=20))
  
line_counties_deaths

# Confirmed Cases of Top 5 Counties(Lines)
line_counties_confirmed<- ggplot(top_5,aes(x = date,y = confirmed_cases,group = county_name,colour = county_name)) + 
  scale_color_brewer(palette="Dark2")+
  geom_line(size = 1,alpha = I(7/10), show.legend=F,linetype = 'dashed') +
  geom_smooth(size = 1.2)+
  scale_x_date(date_breaks = "2 month",date_labels = "%b %Y")+
  labs(title = "Confirmed Cases of Top 5 Counties",x = "Date",y = "Confirmed",color = "County")+
  theme(text=element_text(size=20))

line_counties_confirmed

# Confirmed Cases of Top 5 Counties(boxplot)
boxplot_top5_confirmed <- ggplot(top_5,aes(x = county_name,y = confirmed_cases))+
  geom_boxplot()+
  labs(x = "Date",y = "Confirmed Cases")+
  theme(text=element_text(size=20))
boxplot_top5_confirmed

# Deathsof Top 5 Counties(boxplot)
boxplot_top5_death <- ggplot(top_5,aes(x = county_name,y = deaths))+
  geom_boxplot()+
  labs(x = "Date",y = "Deaths")+
  theme(text=element_text(size=20))
boxplot_top5_death

# Mobility of Dallas(boxplot)
boxplot_mobility_dallas <- ggplot(mobility_Dallas,aes(x = date,y = workplaces_percent_change_from_baseline))+
  geom_boxplot()+
  labs(x = "Date",y = "Workplaces")+
  theme(text=element_text(size=20))
boxplot_mobility_dallas
plot_data <- layer_data(boxplot_mobility_dallas)

# Average of Deaths in 7 Days of Texas(Line)
line_tx_avg_deaths<- ggplot(case_Dallas_avg,aes(x = date,y = death_avg,group = 1)) + 
  geom_line(aes(colour = 'Deaths'),size = 1,linetype = "dashed")+
  geom_smooth(aes(color = 'Smooth line of Deaths'),size = 1.2)+
  scale_x_date(date_breaks = "2 month",date_labels = "%b %Y")+
  labs(title = 'Average of Deaths in 7 Days of Dallas',x="Date",y="Confirmed Cases")+
  scale_colour_manual("",values = c("Deaths" = "cornflowerblue","Smooth line of Deaths" = "orange"))+
  theme(text=element_text(size=20))
line_tx_avg_deaths

# Average of Confirmed in 7 Days of Texas(Line)
line_tx_avg_confirm<- ggplot(case_Dallas_avg,aes(x = date,y = confirmed_avg,group = 1)) + 
  geom_line(aes(colour = 'Confirmed cases'),size = 1,linetype = "dashed")+
  geom_smooth(aes(color = 'Smooth line of Confirmed cases'),size = 1.2)+
  scale_x_date(date_breaks = "2 month",date_labels = "%b %Y")+
  labs(title = 'Average of Confirmed Cases in 7 Days of Dallas',x="Date",y="Confirmed Cases")+
  scale_colour_manual("",values = c("Confirmed cases" = "skyblue","Smooth line of Confirmed cases" = "chocolate3"))+
  theme(text=element_text(size=20))
line_tx_avg_confirm

# Average of Deaths & Confirmed in 7 Days of Texas(Line)
line_tx_avg<- ggplot(case_Dallas_avg) + 
  geom_smooth(aes(x = date,y = death_avg,color = 'Smooth line of Deaths'),size = 1.2)+
  geom_line(aes(x = date,y = death_avg,colour = 'Deaths'),size = 1,linetype = "dashed")+
  
  geom_smooth(aes(x = date, y = confirmed_avg,color = 'Smooth line of Confirmed cases'),size = 1.2)+
  geom_line(aes(x = date, y = confirmed_avg,colour = 'Confirmed cases'),size = 1,linetype = "dashed")+
  
  scale_x_date(date_breaks = "2 month",date_labels = "%b %Y")+
  labs(title = 'Average of Deaths & Confirmed Cases in 7 Days of Dallas',x="Date",y="Confirmed Cases")+
  scale_colour_manual("Lines",values = c("Confirmed cases" = "skyblue",
                                         "Smooth line of Confirmed cases" = "chocolate3",
                                         "Deaths" = "cornflowerblue",
                                         "Smooth line of Deaths" = "orange"))+
  theme(text=element_text(size=20))
line_tx_avg



### Texas/Dallas deaths increasing rate graph
# Method 1
death_increasing_rate <- 100*(case_Dallas_avg$death_avg - lag(case_Dallas_avg$death_avg,default = first(case_Dallas_avg$death_avg)))/mean(case_Dallas_avg$deaths)
dallas_death_increasing_rate <- case_Dallas_avg %>%
  mutate(death_increasing_rate = death_increasing_rate,
  location = "Dallas")%>%
  filter(!is.na(death_increasing_rate) & !is.infinite(death_increasing_rate))

# Method 2
dallas_death_increasing_rate <- case_Dallas %>%
  mutate(death_increasing_rate = 
           100*(deaths - lag(deaths, 
                         default = first(deaths)
                         ))/mean(deaths),
         location = "Dallas") %>%
  filter(!is.na(death_increasing_rate) & !is.infinite(death_increasing_rate))


dallas_death_increasing_rate %>% 
  ggplot(aes(as.Date(x = date), y = death_increasing_rate, group = location, colour = location)) + 
  geom_line(linetype = 'dashed') + 
  geom_smooth(formula = y ~ x, method = "loess", alpha = 0.1,color = "cornflowerblue") +
  scale_color_manual("Location",values = c("Dallas" = "orange")) +
  scale_x_date(date_breaks = '1 months',date_labels = '%b %Y') +
  scale_y_continuous(breaks = seq(0, 20, 1), labels = percent_format(scale = 1)) +
  theme(
    text = element_text(size = 20),
    axis.text.x = element_text(angle = 30)) +
  labs(
    title = "Death Increasing Rate of Dallas",
    x = "Date",
    y = "Death Case Increasing Rate")




### Texas/Dallas confirmed cases increasing rate graph
# Method 1
confirmed_cases_increasing_rate <- 100*(case_Dallas_avg$confirmed_avg - lag(case_Dallas_avg$confirmed_avg,default = first(case_Dallas_avg$confirmed_avg)))/mean(case_Dallas_avg$confirmed_cases)
dallas_case_increasing_rate <- case_Dallas_avg %>%
  mutate(confirmed_cases_increasing_rate = confirmed_cases_increasing_rate,
         location = "Dallas")%>%
  filter(!is.na(confirmed_cases_increasing_rate) & !is.infinite(confirmed_cases_increasing_rate))
# Method 2
dallas_case_increasing_rate <- case_Dallas %>%
  mutate(confirmed_cases_increasing_rate = 
           100*(confirmed_cases - lag(confirmed_cases, 
                             default = first(confirmed_cases)
           ))/mean(confirmed_cases),
         location = "Dallas") %>%
  filter(!is.na(confirmed_cases_increasing_rate) & !is.infinite(confirmed_cases_increasing_rate))

dallas_case_increasing_rate %>% 
  ggplot(aes(as.Date(x = date), y = confirmed_cases_increasing_rate, group = location, colour = location)) + 
  geom_line(linetype = 'dashed') + 
  geom_smooth(formula = y ~ x, method = "loess", alpha = 0.1,color = "cornflowerblue") +
  scale_color_manual("Location",values = c("Dallas" = "orange")) +
  scale_x_date(date_breaks = '1 months',date_labels = '%b %Y') +
  scale_y_continuous(breaks = seq(0, 50, 10), labels = percent_format(scale = 1)) +
  theme(
    text = element_text(size = 20),
    axis.text.x = element_text(angle = 30)) +
  labs(
    title = "Confirmed Case Increasing Rate of Dallas",
    x = "Date",
    y = "Confirmed Case Increasing Rate")



# Merge a new form for bar chart
bar_tx_dallas <- rbind(case_Dallas_monthly %>% mutate(pos = "Dallas"),
                       case_tx_monthly %>% mutate(pos = "TX"))
bar_tx_dallas


# Deaths of Texas & Dallas
ggplot(bar_tx_dallas) + 
  geom_bar(aes(x = month,y = deaths,fill = pos),stat = 'identity',position = 'dodge')+
  labs(title = "Monthly Deaths of Texas & Dallas",x="Month",y="Deaths")+
  theme(axis.text.x = element_text(angle=45))+
  scale_x_continuous(breaks=as.numeric(bar_tx_dallas$month), labels=format(bar_tx_dallas$month,"%b %Y"))+
  scale_fill_manual("Position",values = c("orange", "cornflowerblue"))+

  
  geom_smooth(data =case_tx_monthly, aes(x = month,y = deaths,group = 1,color = "tx_smooth"),size = 1)+
  geom_smooth(data =case_Dallas_monthly, aes(x = month,y = deaths, group = 1,color = "dallas_smooth"),size = 1)+

  geom_line(data = case_tx_monthly, aes(x = month,y = deaths,group = 1,color = "tx"),size = 1,linetype = "dashed")+
  geom_line(data = case_Dallas_monthly, aes(x = month,y = deaths, group = 1,color = "dallas"),size = 1,linetype = "dashed")+
  scale_colour_manual("Lines",values = c("chocolate2","darkorange","skyblue"," steelblue"))+
  theme(text=element_text(size=20))

  
# Monthly of Texas & Dallas
ggplot(bar_tx_dallas) + 
  geom_bar(aes(x = month,y = confirmed_cases,fill = pos),stat = 'identity',position = 'dodge')+
  labs(title = "Monthly Confirmed Cases of Texas & Dallas",x="Month",y="Confirmed Cases")+
  theme(axis.text.x = element_text(angle=45))+
  scale_x_continuous(breaks=as.numeric(bar_tx_dallas$month), labels=format(bar_tx_dallas$month,"%b %Y"))+
  scale_fill_manual("Position",values = c("orange", "cornflowerblue"))+
  
  geom_smooth(data =case_tx_monthly, aes(x = month,y = confirmed_cases,group = 1,color = "tx_smooth"),size = 1)+
  geom_smooth(data =case_Dallas_monthly, aes(x = month,y = confirmed_cases, group = 1,color = "dallas_smooth"),size = 1)+
  
  geom_line(data = case_tx_monthly, aes(x = month,y = confirmed_cases,group = 1,color = "tx"),size = 1,linetype = "dashed")+
  geom_line(data = case_Dallas_monthly, aes(x = month,y = confirmed_cases, group = 1,color = "dallas"),size = 1,linetype = "dashed")+
  scale_colour_manual("Lines",values = c("chocolate2","darkorange","skyblue"," steelblue"))+
  theme(text=element_text(size=20))



# script
summary(census)
summary(case_tx)
summary(mobility)

try(case_tx)
try(mobility)
summary(income)
summary(hospital)
try(income)
try(hospital)

census
case_tx <- as_tibble(case_tx)
census <- as_tibble(census)
case_tx

place_mobility <- mobility %>% select(retail_and_recreation_percent_change_from_baseline,grocery_and_pharmacy_percent_change_from_baseline,parks_percent_change_from_baseline,transit_stations_percent_change_from_baseline,workplaces_percent_change_from_baseline,residential_percent_change_from_baseline)
list <-which(rowSums(is.na(place_mobility)) > 0)
list
hafu_NA <- mobility[list,]
hafu_NA

case_tx$confirmed_cases %>% is.na() %>% as.integer() %>% sum()
case_tx$deaths

levels(case_tx$county_name)
levels(case_tx$date)
# theme(panel.grid =element_blank()) +    ## 删去网格线
# theme(axis.text = element_blank())   ## 删去所有刻度标签

line_case_dallas<- ggplot(case_Dallas,aes(x = date,y = confirmed_cases,group = 1)) + 
  geom_line(aes(colour = 'Confirmed cases'),size = 1,linetype = "dashed") +
  geom_smooth(aes(color = 'Smooth line of Confirmed cases'),size = 1.2)+
  scale_colour_manual("",values = c("Confirmed cases" = "skyblue","Smooth line of Confirmed cases" = "chocolate3"))+
  scale_x_date(date_breaks = "2 month",date_labels = "%b %Y")+
  labs(title = "Dallas Confirmed Cases",x = "Date",y = "Confirmed Cases")+
  theme(text=element_text(size=20))

line_case_tx <- ggplot(case_tx_amount,aes(x = date,y = deaths,group = 1)) +
  geom_line(aes(colour = 'Deaths'),size = 1,linetype = "dashed")+
  geom_smooth(aes(color = 'Smooth line of Deaths'),size = 1.2)+
  scale_colour_manual("",values = c("Deaths" = "cornflowerblue","Smooth line of Deaths" = "orange"))+
  scale_x_date(date_breaks = "2 month",date_labels = "%b %Y")+
  labs(title = "Dallas Deaths",x = "Date",y = "Deaths")+
  theme(text=element_text(size=20))

line_case_dallas
line_case_tx

# case_tx_monthly$month <- as.yearmon(case_tx_monthly$month,"%m/%y")

case_tx_monthly <- case_tx_amount %>%
  mutate(month = as.yearmon(case_tx_amount$date,"%b %Y")) %>%
  group_by(month) %>%
  summarize_if(is.numeric,sum)

case_tx_monthly

line_case_tx_monthly <- ggplot(case_tx_monthly,aes(x = month,y = confirmed_cases,group = 1)) +
  geom_line() +
  geom_smooth() +
  xlab("Date") +
  ylab("Confirmed Case")
line_case_tx_monthly

