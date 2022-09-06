#Yiwen Xu
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


#Kehan Zhang
library("xlsx")
library(scales)
library(ggplot2)
library(tidyverse)
library(dplyr)
library(zoo)
library(lubridate)

cases_us <- read.csv("case.csv", 1)
counties_info <- read.csv("COVID-19_cases_plus_census.csv", 1)
hospital <- read.csv("hospital.csv", 1)
income <- read.csv("income.csv", 1)
mobility <- read.csv("Global_Mobility_Report 2.csv", 1)


# get number of hospital in each county
selected_hospital <- hospital %>%
  group_by(county_name) %>%
  mutate(count = 1) %>%
  summarise(num_hospital = sum(count)) %>%
  mutate(county_name = county_name %>% str_to_lower())


# get number of employment in each county
selected_income <- income %>%
  filter(Year == max(Year)) %>%
  mutate(
    total_employment = Total_employment,
    state = str_split(GeoName, ',', simplify = T)[, 2] %>%
      str_replace(' ', '') %>%
      str_replace('\\*', ''),
    county_name = GeoName %>%
      str_to_lower() %>%
      str_replace(',.*', '')) %>%
  select(county_name, state, total_employment)


# get first case date
selected_first_case <- cases_us %>%
  filter(!county_name == "Statewide Unallocated" & confirmed_cases != 0) %>%
  group_by(county_name) %>%
  filter(date == min(date)) %>%
  mutate(
    first_case_reported_date = date,
    county_name = county_name %>%
      str_to_lower() %>% 
      str_replace('\\s+county\\s*$', '')) %>%
  select(county_name, state, first_case_reported_date)


# get social distance response date
selected_mobility <- mobility %>%
  filter(country_region_code == "US") %>%
  filter(sub_region_1 != '' & sub_region_2 != '') %>%
  mutate(county_name = sub_region_2 %>%
           str_to_lower() %>% 
           str_replace('\\s+county\\s*$', ''),
         recreation = retail_and_recreation_percent_change_from_baseline,
         transportation = transit_stations_percent_change_from_baseline,
         workplace = workplaces_percent_change_from_baseline,
         residential = residential_percent_change_from_baseline) %>%
  select(date, county_name, recreation, transportation, workplace, residential) %>%
  filter(!is.na(recreation) & !is.na(transportation) & !is.na(workplace) & !is.na(residential)) %>%
  mutate(social_distance = ifelse(recreation < 0 & transportation < 0 & workplace < 0 & residential > 0, 1, 0)) %>%
  mutate(days = c(0, 0, 0, 0, 0, 0, 0, 0, 0, rollsum(social_distance, 10))) %>%
  filter(days == 10) %>%
  group_by(county_name) %>%
  filter(date == min(date)) %>%
  mutate(social_distance_response_date = as.Date(date) + 10) %>%
  select(county_name, social_distance_response_date)


# get confirmed cases/deaths
selected_cases_death <- cases_us %>%
  filter(date == max(date)) %>%
  mutate(county_name = county_name %>% 
           str_to_lower() %>% 
           str_replace('\\s+county\\s*$', '')) %>%
  select(county_name, state, confirmed_cases, deaths)

# final raw table  
counties_info <- counties_info %>%
  select(county_name, state, total_pop, median_income) %>%
  mutate(county_name = county_name %>% 
           str_to_lower() %>% 
           str_replace('\\s+county\\s*$', '')) %>%
  left_join(selected_cases_death) %>%
  left_join(selected_income) %>%
  left_join(selected_hospital) %>%
  left_join(selected_first_case) %>%
  left_join(selected_mobility) 

# final table
counties_info <- counties_info %>%
  mutate(response_days = ymd(social_distance_response_date) - ymd(first_case_reported_date),
         infection_rate = confirmed_cases / total_pop * 100,
         death_rate = deaths / confirmed_cases * 100,
         hospital_per_1000_people = num_hospital / total_pop * 1000, 
         employment_rate = total_employment / total_pop)

# county_hospital_number_bar_graph
counties_info %>%
  ggplot(aes(x = num_hospital)) +
  geom_bar() +
  theme(text = element_text(size = 20)) +
  scale_x_continuous(limits = c(0, 60), breaks = seq(0, 60, 5)) +
  labs(title = "Number of hospitals of Counties in US",
       x = "Number of Hospital",
       y = "Number of Counties")

# county_response_days_bar_graph
counties_info %>%
  ggplot(aes(x = response_days)) +
  geom_bar() +
  theme(text = element_text(size = 20)) +
  scale_x_continuous(limits = c(0, 40), breaks = seq(0, 40, 2)) +
  labs(title = "Response Days of Counties in US",
       x = "Days of Response",
       y = "Number of Counties")

# county_first_case_date_bar_graph
counties_info %>%
  ggplot(aes(x = as.Date(first_case_reported_date))) +
  geom_bar() +
  theme(text = element_text(size = 20)) +
  scale_x_date(date_breaks = "1 month",date_labels = "%b %Y") +
  labs(title = "Date of First Case Reported of Counties in US",
       x = "Date of First Case Reported",
       y = "Number of Counties")

# county_response_date_bar_graph
counties_info %>%
  filter(social_distance_response_date < "2020-06-01") %>%
  ggplot(aes(x = as.Date(social_distance_response_date))) +
  geom_bar() +
  theme(text = element_text(size = 20)) +
  scale_x_date(date_breaks = "1 month",date_labels = "%b %Y") +
  labs(title = "Response Date of Counties in US",
       x = "Response Date",
       y = "Number of Counties")

# hospital_per_1000_people_death_rate_relation_point_graph
counties_info %>% 
  ggplot(aes(x = hospital_per_1000_people, y = death_rate)) + 
  geom_point() +
  scale_x_continuous(limits = c(0, 15), breaks = seq(0, 15, 1)) +
  scale_y_continuous(labels = percent_format(scale = 1)) +
  theme(text = element_text(size = 20)) +
  labs(
    title = "Relationship between Hospital per 1000 People and Death Rate",
    x = "Number of Hospital per 1000 People",
    y = "Death Rate")

# median_income_infection_rate_relation_line_graph
counties_info %>% 
  ggplot(aes(x = median_income, y = infection_rate)) + 
  geom_point() +
  scale_y_continuous(limits = c(0, 60), breaks = seq(0, 60, 5), labels = percent_format(scale = 1)) +
  geom_smooth(formula = y ~ x, method = "loess") +
  theme(text = element_text(size = 20)) +
  labs(
    title = "Relationship between Median Income and Infection Rate",
    x = "Median Income",
    y = "Infection Rate")


#Kehan Zhang plot
library("xlsx")
library(scales)
library(ggplot2)
library(tidyverse)
library(dplyr)
library(scales)




### Texas county case/death bar graph
options(scipen = 1)
tx <- read.csv("COVID-19_cases_TX.csv", 1)
summary(tx)

tx_county_case_death <- tx %>% 
  filter(date == max(date)) %>% 
  select(county_name, confirmed_cases, deaths) %>% 
  mutate(death_rate = percent(deaths / confirmed_cases, 0.01)) %>% 
  arrange(desc(confirmed_cases))

tx_county_cases <- tx_county_case_death %>%
  select(county_name, confirmed_cases) %>%
  mutate(type = "confirmed_cases") %>%
  rename(number = confirmed_cases)

tx_county_death <- tx_county_case_death %>%
  select(county_name, deaths) %>%
  mutate(type = "deaths") %>%
  rename(number = deaths)

tx_county_case_death_top_10 <- rbind(tx_county_cases[1:10,], tx_county_death[1:10,])

tx_county_case_death_top_10[1:20,] %>% 
  ggplot(aes(x = number, y = reorder(county_name, number), fill = type)) + 
  # theme(axis.text.x = element_text(angle = 45)) +
  geom_bar(stat="identity", position="dodge") +
  # guides(fill = guide_legend(reverse=TRUE)) +
  scale_fill_manual(values = c("cornflowerblue","orange")) +
  theme(text = element_text(size = 20)) +
  labs(
    title = "Number of Confirmed Cases and Deaths of Top 10 Counties in Texas",
    x = "Number of confirmed cases and deaths",
    y = "County")




### Texas/Dallas death rate with time line graph
dallas_death_rate <- tx %>%
  filter(county_name == "Dallas County") %>%
  mutate(death_rate = deaths / confirmed_cases * 100, location = "Dallas") %>%
  filter(!is.na(death_rate)) %>%
  select(date, death_rate, location)

texas_death_rate <- tx %>%
  group_by(date) %>%
  summarise(confirmed_cases = sum(confirmed_cases), deaths = sum(deaths)) %>%
  mutate(death_rate = deaths/ confirmed_cases * 100, location = "Texas") %>%
  filter(!is.na(death_rate)) %>%
  select(date, death_rate, location)

texas_dallas_death_rate <- rbind(dallas_death_rate, texas_death_rate)

texas_dallas_death_rate %>% 
  ggplot(aes(as.Date(x = date), y = death_rate, group = location, colour = location)) + 
  #scale_color_brewer(palette="Dark2") +
  geom_line() + 
  geom_smooth(formula = y ~ x, method = "loess") +
  scale_color_manual("Location",values = c("Dallas" = "orange","Texas" = "cornflowerblue")) +
  scale_x_date(date_breaks = '1 months',date_labels = '%b %Y') +
  scale_y_continuous(breaks = seq(0, 3, 0.2), labels = percent_format(scale = 1)) +
  theme(
    text = element_text(size = 20),
    axis.text.x = element_text(angle = 30)) +
  labs(
    title = "Death Rate of Dallas and Texas",
    x = "Date",
    y = "Death Rate")




### Texas county confirmed case top 5 death rate line graph
tx_county_top_5_death_rate <- tx %>%
  filter(county_name %in% tx_county_cases[1:5,"county_name"]) %>%
  mutate(death_rate = deaths / confirmed_cases * 100) %>%
  filter(!is.na(death_rate)) %>%
  filter(death_rate < quantile(death_rate, 0.9999)) %>%
  select(county_name, date, death_rate)

tx_county_top_5_death_rate %>% 
  ggplot(aes(x = as.Date(date), y = death_rate, group = county_name, colour = county_name)) + 
  scale_color_brewer(palette="Dark2") +
  geom_line(alpha = 0.5) + 
  geom_smooth(formula = y ~ x, method = "loess", size = 0.7) +
  scale_x_date(date_breaks = "1 month",date_labels = "%b %Y") +
  scale_y_continuous(breaks = seq(0, 6, 0.25), labels = percent_format(scale = 1)) +
  theme(
    text = element_text(size = 20),
    axis.text.x = element_text(angle = 30)) +
  labs(
    title = "Death Rate of Top 5 Counties in Texas",
    x = "Date",
    y = "Death Rate",
    color = "County")




### Texas/Dallas confirmed cases increasing rate graph
dallas_case_increasing_rate <- tx %>%
  filter(county_name == "Dallas County") %>%
  select(date, confirmed_cases, deaths) %>%
  mutate(confirmed_cases_increasing_rate = 
           confirmed_cases / lag(confirmed_cases, default = first(confirmed_cases)),
         location = "Dallas") %>%
  filter(!is.na(confirmed_cases_increasing_rate) & !is.infinite(confirmed_cases_increasing_rate))

texas_case_increasing_rate <- tx %>%
  group_by(date) %>%
  summarise(confirmed_cases = sum(confirmed_cases), deaths = sum(deaths)) %>%
  mutate(confirmed_cases_increasing_rate = 
           confirmed_cases / lag(confirmed_cases, default = first(confirmed_cases)),
         location = "Texas") %>%
  filter(!is.na(confirmed_cases_increasing_rate) & !is.infinite(confirmed_cases_increasing_rate))

texas_dallas_case_increasing_rate <- rbind(dallas_case_increasing_rate, texas_case_increasing_rate)

texas_dallas_case_increasing_rate %>% 
  ggplot(aes(as.Date(x = date), y = confirmed_cases_increasing_rate, group = location, colour = location)) + 
  geom_line() + 
  #geom_smooth(formula = y ~ x, method = "loess", alpha = 0.1) +
  scale_color_manual("Location",values = c("Dallas" = "orange","Texas" = "cornflowerblue")) +
  scale_x_date(date_breaks = '1 months',date_labels = '%b %Y') +
  scale_y_continuous(breaks = seq(0, 4, 0.2), labels = percent_format(scale = 1)) +
  theme(
    text = element_text(size = 20),
    axis.text.x = element_text(angle = 30)) +
  labs(
    title = "Confirmed Case Increasing Rate of Dallas and Texas",
    x = "Date",
    y = "Confirmed Case Increasing Rate")




### Texas/Dallas death increasing rate line graph
# remove abnormal data: e.g. death decease with date
tx_abnormal_data_cleaned <- tx
clean_abnormal_data <- function(){
  tx_abnormal_data_cleaned <- tx_abnormal_data_cleaned %>%
    arrange(county_name, date) %>%
    mutate(deaths_diff = deaths - lag(deaths, default = first(deaths))) 
  
  abnormal <- tx_abnormal_data_cleaned %>%
    filter(deaths_diff < 0,deaths != 0)
  
  while (nrow(abnormal) != 0) {
    tx_abnormal_data_cleaned <- tx_abnormal_data_cleaned %>%
      filter(deaths_diff >= 0 | deaths == 0)
    
    tx_abnormal_data_cleaned <- tx_abnormal_data_cleaned %>%
      mutate(deaths_diff = deaths - lag(deaths, default = first(deaths))) 
    
    abnormal <- tx_abnormal_data_cleaned %>%
      filter(deaths_diff < 0,deaths != 0)
  }
  tx_abnormal_data_cleaned
}

# get clean data
tx_abnormal_data_cleaned <- clean_abnormal_data()

dallas_death_increasing_rate <- tx %>%
  filter(county_name == "Dallas County") %>%
  select(date, confirmed_cases, deaths) %>%
  mutate(death_increasing_rate = 
           deaths / lag(deaths, default = first(deaths)),
         location = "Dallas") %>%
  filter(!is.na(death_increasing_rate) & !is.infinite(death_increasing_rate))

texas_death_increasing_rate <- tx %>%
  group_by(date) %>%
  summarise(confirmed_cases = sum(confirmed_cases), deaths = sum(deaths)) %>%
  mutate(death_increasing_rate = 
           deaths / lag(deaths, default = first(deaths)),
         location = "Texas") %>%
  filter(!is.na(death_increasing_rate) & !is.infinite(death_increasing_rate))

texas_dallas_death_increasing_rate <- rbind(dallas_death_increasing_rate, texas_death_increasing_rate)

texas_dallas_death_increasing_rate %>% 
  ggplot(aes(as.Date(x = date), y = death_increasing_rate, group = location, colour = location)) + 
  geom_line() + 
  #geom_smooth(formula = y ~ x, method = "loess", alpha = 0.1) +
  scale_color_manual("Location",values = c("Dallas" = "orange","Texas" = "cornflowerblue")) +
  scale_x_date(date_breaks = '1 months',date_labels = '%b %Y') +
  scale_y_continuous(breaks = seq(0, 4, 0.2), labels = percent_format(scale = 1)) +
  theme(
    text = element_text(size = 20),
    axis.text.x = element_text(angle = 30)) +
  labs(
    title = "Death Increasing Rate of Dallas and Texas",
    x = "Date",
    y = "Death Increasing Rate")




### Dallas death increasing rate(population) line graph
dallas_death_increasing_rate_population <- tx_abnormal_data_cleaned %>%
  filter(county_name == "Dallas County") %>%
  select(date, confirmed_cases, deaths) %>%
  mutate(
    total_pop = 2552213,
    death_increasing_rate = 
      (deaths - lag(deaths, default = first(deaths))) / total_pop * 1000) %>%
  filter(!is.na(death_increasing_rate) & !is.infinite(death_increasing_rate))

dallas_death_increasing_rate_population %>% 
  ggplot(aes(as.Date(x = date), y = death_increasing_rate)) + 
  geom_line() + 
  #geom_smooth(formula = y ~ x, method = "loess", alpha = 0.1) +
  scale_x_date(date_breaks = '1 months',date_labels = '%b %Y') +
  scale_y_continuous(breaks = seq(0, 4, 0.2), labels = percent_format(scale = 1)) +
  theme(
    text = element_text(size = 20),
    axis.text.x = element_text(angle = 30)) +
  labs(
    title = "Death Increasing Rate of Dallas and Texas",
    x = "Date",
    y = "Death Increasing Rate")




### US case/death bar graph
options(scipen = 2)
us <- read.csv("COVID-19_cases_plus_census.csv", 1)
summary(us)

#final_us <- final_us[complete.cases(final_us),]
us_state_case_death <- us %>% 
  #filter(date == max(date)) %>% 
  select(county_name, state, confirmed_cases, deaths) %>% 
  mutate(death_rate = percent(deaths / confirmed_cases, 0.01)) %>%
  group_by(state) %>% 
  summarize(confirmed_cases = sum(confirmed_cases,na.rm = T), deaths = sum(deaths,na.rm = T)) %>%
  arrange(desc(confirmed_cases))

us_state_case <- us_state_case_death %>%
  select(state, confirmed_cases) %>%
  mutate(type = "confirmed_cases") %>%
  rename(number = confirmed_cases)

us_state_death <- us_state_case_death %>%
  select(state, deaths) %>%
  mutate(type = "deaths") %>%
  rename(number = deaths)

us_state_case_death_top_10 <- rbind(us_state_case[1:10,], us_state_death[1:10,])

us_state_case_death_top_10[1:20,] %>% 
  ggplot(aes(x = number, y = reorder(state, number), fill = type)) + 
  geom_bar(stat="identity", position="dodge") +
  scale_fill_manual(values = c("cornflowerblue","orange")) +
  theme(text = element_text(size = 20)) +
  labs(
    title = "Number of Confirmed Cases and Deaths of Top 10 States in the US",
    x = "Number of confirmed cases and deaths",
    y = "State")




### Texas county cases/death/death rate per 1000 map
tx_county_case_death_death_rate <- us %>%
  mutate_if(is.character, factor) %>%
  filter(state == "TX") %>%
  filter(confirmed_cases > 100) %>% 
  arrange(desc(confirmed_cases)) %>%    
  select(county_name, confirmed_cases, deaths, total_pop) %>%
  mutate(
    cases_per_1000 = confirmed_cases/total_pop * 1000, 
    deaths_per_1000 = deaths/total_pop * 1000, 
    death_per_case = deaths/confirmed_cases,
    county = county_name %>% 
      str_to_lower() %>% 
      str_replace('\\s+county\\s*$', ''))

tx_county_map <- as_tibble(map_data("county")) %>%
  filter(region == "texas") %>%
  rename(c(county = subregion)) %>%
  left_join(tx_county_case_death_death_rate %>% 
              select(c(county, cases_per_1000, deaths_per_1000, death_per_case)))

## case map
tx_county_map %>%
  ggplot(aes(long, lat, label = county)) + 
  geom_polygon(aes(group = group, fill = cases_per_1000)) +
  # geom_text_repel(data = counties_TX %>% filter(complete.cases(.)) %>% group_by(county) %>% 
  # summarize(long = mean(long), lat = mean(lat)) %>% mutate(county = str_to_title(county))) +
  coord_quickmap() + 
  scale_fill_gradient(low="yellow", high="red") +
  theme(text = element_text(size = 20)) +
  labs(
    title = "COVID-19 Cases per 1000 People of Counties in Texas", 
    subtitle = "Only counties reporting 100+ cases",
    x = "Longitude",
    y = "Latitude")

## death map
tx_county_map %>%
  ggplot(aes(long, lat, label = county)) + 
  geom_polygon(aes(group = group, fill = deaths_per_1000)) +
  coord_quickmap() + 
  scale_fill_gradient(low="yellow", high="red") +
  theme(text = element_text(size = 20)) +
  labs(
    title = "COVID-19 Deaths per 1000 People of Counties in Texas",
    subtitle = "Only counties reporting 100+ cases",
    x = "Longitude",
    y = "Latitude")

## death rate map
tx_county_map %>%
  ggplot(aes(long, lat, label = county)) + 
  geom_polygon(aes(group = group, fill = death_per_case)) +
  coord_quickmap() + 
  scale_fill_gradient(low="yellow", high="red") +
  theme(text = element_text(size = 20)) +
  labs(
    title = "COVID-19 Deaths Rate of Counties in Texas", 
    subtitle = "Only counties reporting 100+ cases",
    x = "Longitude",
    y = "Latitude")




### US state cases/death per 1000000 map
us_state_case_death_death_rate <- us %>% 
  mutate_if(is.character, factor) %>%
  select(state, confirmed_cases, deaths, total_pop) %>% 
  group_by(state) %>% 
  summarize(confirmed_cases = sum(confirmed_cases,na.rm = T), 
            deaths = sum(deaths,na.rm = T),
            total_pop = sum(total_pop, na.rm = T)) %>%
  arrange(desc(confirmed_cases)) %>%
  mutate(
    cases_per_1000000 = confirmed_cases/total_pop * 1000000, 
    deaths_per_1000000 = deaths/total_pop * 1000000, 
    death_per_case = deaths/confirmed_cases)

us_state_map <- as_tibble(map_data("state")) %>%
  mutate(state = state.abb[match(region,str_to_lower(state.name))]) %>%
  left_join(us_state_case_death_death_rate %>% 
              select(c(state, cases_per_1000000, deaths_per_1000000, death_per_case)))

## case map
us_state_map %>%
  ggplot(aes(long, lat, label = state)) + 
  geom_polygon(aes(group = group, fill = cases_per_1000000)) +
  coord_quickmap() + 
  scale_fill_gradient(low="yellow", high="red") +
  theme(text = element_text(size = 20)) +
  labs(title = "COVID-19 Cases per 1000000 People of States",
       x = "Longitude",
       y = "Latitude")

## death map
us_state_map %>%
  ggplot(aes(long, lat, label = state)) + 
  geom_polygon(aes(group = group, fill = deaths_per_1000000)) +
  coord_quickmap() + 
  scale_fill_gradient(low="yellow", high="red") +
  theme(text = element_text(size = 20)) +
  labs(title = "COVID-19 Deaths per 1000000 People of States",
       x = "Longitude",
       y = "Latitude")

## death rate map
us_state_map %>%
  ggplot(aes(long, lat, label = state)) + 
  geom_polygon(aes(group = group, fill = death_per_case)) +
  coord_quickmap() + 
  scale_fill_gradient(low="yellow", high="red") +
  theme(text = element_text(size = 20)) +
  labs(title = "COVID-19 Death Rate of States",
       x = "Longitude",
       y = "Latitude")

#ShuangLiang
#TX data summary
library("ggpubr")
library("tidyverse")
library("ggplot2")
library("ggrepel")
library("ggcorrplot")
library("DT")
getwd()
setwd("/Users/shuangliang/Documents/SMU/CS7331 Datamining/project1")
cases <- read_csv("COVID-19_cases_plus_census.csv")
#cases
#summary(cases)

cases <- cases %>% mutate_if(is.character, factor)
#dim(cases)
cases_TX <- cases %>% filter(state == "TX")
#dim(cases_TX)
#summary(cases_TX[,1:10])

sum(cases_TX$male_pop)

sum(cases_TX$female_pop)

temp_cases_TX_male_age <- cases_TX %>% filter(confirmed_cases > 100) %>% 
  #  arrange(desc(confirmed_cases)) %>%    
  select(male_under_5, male_5_to_9, male_10_to_14, male_15_to_17, male_18_to_19, male_20,male_21,male_22_to_24,male_25_to_29,male_30_to_34,male_35_to_39, male_40_to_44,male_45_to_49,male_50_to_54,male_55_to_59,male_60_61,male_62_64,male_65_to_66,male_67_to_69,male_70_to_74,male_75_to_79,male_80_to_84,male_85_and_over)

sumlist <- c(2,4,9,11,13,15,19,21,23)
starter_index <- 1
age_after_sum <- data.frame()
#age_after_sum

for (i in 1:length(sumlist)){
  temp_sum <- 0
  for(j in starter_index:sumlist[i]){
    #print(j)
    temp_sum <- temp_sum + temp_cases_TX_male_age[j]
  }
  print(temp_sum)
  temp_cases_TX_male_age[,i+23] <- temp_sum
  starter_index <- sumlist[i] + 1
}
age_after_sum_male <- (temp_cases_TX_male_age[24:32])
summary(age_after_sum_male)
age <- c('0 ~ 9', '10 ~ 17', '18 ~ 29','30 ~ 39', '40 ~ 49','50 ~ 59','60 ~ 69','70 ~ 79','80 +')
#age <- c('male9', 'male17', 'male29','male39', 'male49','male59','male69','male79','male_80_and_over')

colnames(age_after_sum_male) <- age
age_after_sum_male$county_name <- cases_TX %>% filter(confirmed_cases > 100)%>%select(county_name)
summary(age_after_sum_male)
for (i in 1:9){
  print(var(age_after_sum_male[i]))
}



temp_cases_TX_female_age <- cases_TX %>% filter(confirmed_cases > 100) %>% 
  select(female_under_5, female_5_to_9, female_10_to_14, female_15_to_17, female_18_to_19, female_20,female_21,female_22_to_24,female_25_to_29,female_30_to_34,female_35_to_39, female_40_to_44,female_45_to_49,female_50_to_54,female_55_to_59,female_60_to_61,female_62_to_64,female_65_to_66,female_67_to_69,female_70_to_74,female_75_to_79,female_80_to_84,female_85_and_over)
sumlist <- c(2,4,9,11,13,15,19,21,23)
starter_index <- 1
age_after_sum <- data.frame()
#age_after_sum

for (i in 1:length(sumlist)){
  temp_sum <- 0
  for(j in starter_index:sumlist[i]){
    #print(j)
    temp_sum <- temp_sum + temp_cases_TX_female_age[j]
  }
  print(temp_sum)
  temp_cases_TX_female_age[,i+23] <- temp_sum
  starter_index <- sumlist[i] + 1
}
age_after_sum_female <- (temp_cases_TX_female_age[24:32])
summary(age_after_sum_female)
df <- var(age_after_sum_female)
write.csv(df,file = "BirthData.csv")
age <- c('0 ~ 9', '10 ~ 17', '18 ~ 29','30 ~ 39', '40 ~ 49','50 ~ 59','60 ~ 69','70 ~ 79','80 +')
#age <- c('male9', 'male17', 'male29','male39', 'male49','male59','male69','male79','male_80_and_over')

colnames(age_after_sum_female) <- age
age_after_sum_female$county_name <- cases_TX %>% filter(confirmed_cases > 100)%>%select(county_name)

age_after_sum_male$sum <- rowSums(age_after_sum_male[,1:9])
age_after_sum_male
age_after_sum_female$sum<- rowSums(age_after_sum_female[,1:9])
age_after_sum_female
sum(age_after_sum_male$sum,age_after_sum_female$sum)


temp_cases_TX_income <- cases_TX %>% filter(confirmed_cases > 100) %>% 
  #  arrange(desc(confirmed_cases)) %>%    
  select(income_less_10000,income_10000_14999,income_15000_19999,income_20000_24999,income_25000_29999,income_30000_34999,income_35000_39999,income_40000_44999,income_45000_49999,income_50000_59999,income_60000_74999,income_75000_99999,income_100000_124999,income_125000_149999,income_150000_199999,income_200000_or_more)
summary(temp_cases_TX_income)
sumlist <- c(4,7,10,12,14,15,16)
starter_index <- 1
income_after_sum <- data.frame()
for (i in 1:length(sumlist)){
  temp_sum <- 0
  for(j in starter_index:sumlist[i]){
    #print(j)
    temp_sum <- temp_sum + temp_cases_TX_income[j]
  }
  temp_cases_TX_income[,i+16] <- temp_sum
  starter_index <- sumlist[i] + 1
}
income_after_sum <- temp_cases_TX_income[17:23]
df <- summary(income_after_sum)
write.csv(df,file = "IncomeInTX.csv")
df <- var(income_after_sum)
write.csv(df,file = "IncomeInTXVar.csv")


cases_state <- cases%>%group_by(state)%>%summarise(deaths = sum(deaths),confirmed_cases = sum(confirmed_cases),total_pop = sum(total_pop),median_income = mean(median_income))
dim(cases_state)
summary(cases_state)
deathRate <- cases_TX %>% 
  #  arrange(desc(confirmed_cases)) %>%    
  select(deaths,confirmed_cases,total_pop,median_income)
summary(deathRate)
deathRate <- cases_TX %>% filter(confirmed_cases > 100) %>%
  #  arrange(desc(confirmed_cases)) %>%    
  select(deaths,confirmed_cases,total_pop,median_income)
a <- summary(deathRate)
a
write.csv(a,file = "DeathInUS.csv")
a <- var(deathRate)
write.csv(a,file = "DeathInUSVar.csv")






cases_TX_select <- cases_TX %>% filter(confirmed_cases > 100) %>% 
  arrange(desc(confirmed_cases)) %>%    
  select(county_name, confirmed_cases, deaths, total_pop, median_income)
cases_TX_select <- cases_TX_select %>% mutate(
  cases_per_1000 = confirmed_cases/total_pop*1000, 
  deaths_per_1000 = deaths/total_pop*1000, 
  death_per_case = deaths/confirmed_cases)
a <- summary(cases_TX_select)
a
write.csv(a,file = "DeathInTX.csv")
a <- var(cases_TX_select[2:8],na.rm = TRUE)
write.csv(a,file = "DeathInTXVar.csv")



cor_TX <- cor(cases_TX_select[,-1])
ggcorrplot(cor_TX, p.mat = cor_pmat(cases_TX_select[,-1]), insig = "blank", hc.order = TRUE)


cor_state <- cor(cases_state_select[,-1])
ggcorrplot(cor_state, p.mat = cor_pmat(cases_state_select[,-1]),insig = "blank",hc.order = TRUE)


#Female and male's age in TX
library("tidyverse")
library("ggplot2")
library("ggrepel")
library("ggcorrplot")
library("DT")
getwd()
setwd("/Users/shuangliang/Documents/SMU/CS7331 Datamining/project1")
cases <- read_csv("COVID-19_cases_plus_census.csv")
#cases
#summary(cases)

cases <- cases %>% mutate_if(is.character, factor)
#dim(cases)
cases_TX <- cases %>% filter(state == "TX")
#dim(cases_TX)
#summary(cases_TX[,1:10])

sum(cases_TX$male_pop)
sum(cases_TX$female_pop)

temp_cases_TX_male_age <- cases_TX %>% filter(confirmed_cases > 100) %>% 
  #  arrange(desc(confirmed_cases)) %>%    
  select(male_under_5, male_5_to_9, male_10_to_14, male_15_to_17, male_18_to_19, male_20,male_21,male_22_to_24,male_25_to_29,male_30_to_34,male_35_to_39, male_40_to_44,male_45_to_49,male_50_to_54,male_55_to_59,male_60_61,male_62_64,male_65_to_66,male_67_to_69,male_70_to_74,male_75_to_79,male_80_to_84,male_85_and_over)
temp_cases_TX_female_age <- cases_TX %>% filter(confirmed_cases > 100) %>% 
  select(female_under_5, female_5_to_9, female_10_to_14, female_15_to_17, female_18_to_19, female_20,female_21,female_22_to_24,female_25_to_29,female_30_to_34,female_35_to_39, female_40_to_44,female_45_to_49,female_50_to_54,female_55_to_59,female_60_to_61,female_62_to_64,female_65_to_66,female_67_to_69,female_70_to_74,female_75_to_79,female_80_to_84,female_85_and_over)
summary(temp_cases_TX_male_age)
temp <- colSums(temp_cases_TX_male_age)
temp
name <- c('male_under_5','male_5_to_9', 'male_10_to_14', 'male_15_to_17', 'male_18_to_19', 'male_20','male_21','male_22_to_24','male_25_to_29','male_30_to_34','male_35_to_39', 'male_40_to_44','male_45_to_49','male_50_to_54','male_55_to_59','male_60_61','male_62_64','male_65_to_66','male_67_to_69','male_70_to_74','male_75_to_79','male_80_to_84','male_85_and_over')
temp_dataframe <- data.frame(name = name, count = temp)
temp_dataframe
name <- temp_dataframe$name
print(temp_dataframe$count[1]+temp_dataframe$count[2])
sumlist <- c(2,4,9,11,13,15,19,21,23)
starter_index <- 1
age_after_sum <- list()
#age_after_sum

for (i in 1:length(sumlist)){
  temp_sum <- 0
  for(j in starter_index:sumlist[i]){
    #print(j)
    temp_sum <- temp_sum + temp_dataframe$count[j]
  }
  age_after_sum[i] <- temp_sum
  starter_index <- sumlist[i] + 1
}
print(age_after_sum)

age_after_sum
age <- c('0 ~ 9', '10 ~ 17', '18 ~ 29','30 ~ 39', '40 ~ 49','50 ~ 59','60 ~ 69','70 ~ 79','80 +')
temp_age_dataframe <- data.frame(Age = age, count = age_after_sum)
ggplot(data = temp_age_dataframe, mapping = aes(x= 'Content',y=age_after_sum,fill = Age)) + geom_bar(stat = 'identity', position = 'stack',width = 1)+ coord_polar(theta = 'y') +theme(text=element_text(size=20)) + labs(x = '', y = '', title = 'Male in TX') + theme(axis.text = element_blank()) + theme(axis.ticks = element_blank())

temp <- colSums(temp_cases_TX_female_age)
temp
name <- c('female_under_5','female_5_to_9', 'female_10_to_14', 'female_15_to_17', 'female_18_to_19', 'female_20','female_21','female_22_to_24','female_25_to_29','female_30_to_34','female_35_to_39', 'female_40_to_44','female_45_to_49','female_50_to_54','female_55_to_59','female_60_61','female_62_64','female_65_to_66','female_67_to_69','female_70_to_74','female_75_to_79','female_80_to_84','female_85_and_over')
temp_dataframe <- data.frame(name = name, count = temp)
temp_dataframe
name <- temp_dataframe$name
print(temp_dataframe$count[1]+temp_dataframe$count[2])
sumlist <- c(2,4,9,11,13,15,19,21,23)
starter_index <- 1
age_after_sum <- list()
#age_after_sum

for (i in 1:length(sumlist)){
  temp_sum <- 0
  for(j in starter_index:sumlist[i]){
    #print(j)
    temp_sum <- temp_sum + temp_dataframe$count[j]
  }
  age_after_sum[i] <- temp_sum
  starter_index <- sumlist[i] + 1
}
print(age_after_sum)

age_after_sum
age <- c('0 ~ 9', '10 ~ 17', '18 ~ 29','30 ~ 39', '40 ~ 49','50 ~ 59','60 ~ 69','70 ~ 79','80 +')
temp_age_dataframe <- data.frame(Age = age, count = age_after_sum)
ggplot(data = temp_age_dataframe, mapping = aes(x= 'Content',y=age_after_sum,fill = Age)) + geom_bar(stat = 'identity', position = 'stack',width = 1)+ coord_polar(theta = 'y') +theme(text=element_text(size=20)) + labs(x = '', y = '', title = 'Female in TX') + theme(axis.text = element_blank()) + theme(axis.ticks = element_blank())
# Income in TX
library("tidyverse")
library("ggplot2")
library("ggrepel")
library("ggcorrplot")
library("DT")
getwd()
setwd("/Users/shuangliang/Documents/SMU/CS7331 Datamining/project1")
cases <- read_csv("COVID-19_cases_plus_census.csv")
#cases
#summary(cases)

cases <- cases %>% mutate_if(is.character, factor)
#dim(cases)
cases_TX <- cases %>% filter(state == "TX")
#dim(cases_TX)
#summary(cases_TX[,1:10])


temp_cases_TX_income <- cases_TX %>% filter(confirmed_cases > 100) %>% 
  #  arrange(desc(confirmed_cases)) %>%    
  select(income_less_10000,income_10000_14999,income_15000_19999,income_20000_24999,income_25000_29999,income_30000_34999,income_35000_39999,income_40000_44999,income_45000_49999,income_50000_59999,income_60000_74999,income_75000_99999,income_100000_124999,income_125000_149999,income_150000_199999,income_200000_or_more)
summary(temp_cases_TX_income)
temp <- colSums(temp_cases_TX_income)
temp
name <- c('income_less_10000','income_10000_14999','income_15000_19999','income_20000_24999','income_25000_29999','income_30000_34999','income_35000_39999','income_40000_44999','income_45000_49999','income_50000_59999','income_60000_74999','income_75000_99999','income_100000_124999','income_125000_149999','income_150000_199999','income_200000_or_more')
temp_dataframe <- data.frame(name = name, count = temp)
temp_dataframe
name <- temp_dataframe$name
print(temp_dataframe$count[1]+temp_dataframe$count[2])
sumlist <- c(4,7,10,12,14,15,16)
starter_index <- 1
income_after_sum <- list()
#age_after_sum

for (i in 1:length(sumlist)){
  temp_sum <- 0
  for(j in starter_index:sumlist[i]){
    #print(j)
    temp_sum <- temp_sum + temp_dataframe$count[j]
  }
  income_after_sum[i] <- temp_sum
  starter_index <- sumlist[i] + 1
}
print(income_after_sum)

income_after_sum
income <- c('0 ~ 24999', '25000 ~ 39999', '40000 ~ 59999','60000 ~ 99999', 'from 100000 ~ 149999','from 150000 ~ 199999','over 200000 +')
temp_income_dataframe <- data.frame(Income = income, count = income_after_sum)
ggplot(data = temp_income_dataframe, mapping = aes(x= 'Content',y=income_after_sum,fill = Income)) + geom_bar(stat = 'identity', position = 'stack',width = 1)+ coord_polar(theta = 'y') +theme(text=element_text(size=20)) + labs(x = '', y = '', title = 'Income in TX') + theme(axis.text = element_blank()) + theme(axis.ticks = element_blank())


#death rate in TX

library("tidyverse")
library("ggplot2")
library("ggrepel")
library("ggcorrplot")
library("DT")
getwd()
setwd("/Users/shuangliang/Documents/SMU/CS7331 Datamining/project1")
cases <- read_csv("COVID-19_cases_plus_census.csv")
# #cases
# #summary(cases)
#
cases <- cases %>% mutate_if(is.character, factor)
#dim(cases)
cases_TX <- cases %>% filter(state == "TX")
#dim(cases_TX)
#summary(cases_TX[,1:10])

cases <- cases %>% mutate_if(is.character, factor)
dim(cases)
cases_TX <- cases %>% filter(state == "TX")
dim(cases_TX)
summary(cases_TX[,1:10])
ggplot(cases_TX, mapping = aes(confirmed_cases)) + geom_histogram(bins = 20)
ggplot(cases_TX, mapping = aes(x = confirmed_cases, y = deaths, size = total_pop)) + geom_point()
ggplot(cases_TX, mapping = aes(x = confirmed_cases, y = deaths, label = county_name)) +
  geom_smooth(method = lm) +
  geom_point(mapping = aes(size = total_pop), color = "grey") +
  geom_text_repel(data = subset(cases_TX, deaths >= 1000))
cases_TX_select <- cases_TX %>% filter(confirmed_cases > 100) %>%
  arrange(desc(confirmed_cases)) %>%
  select(county_name, confirmed_cases, deaths, total_pop, median_income)
cases_TX_select <- cases_TX_select %>% mutate(
  cases_per_1000 = confirmed_cases/total_pop*1000,
  deaths_per_1000 = deaths/total_pop*1000,
  death_per_case = deaths/confirmed_cases)

head(cases_TX_select)
datatable(cases_TX_select) %>% formatRound(6:7, 4) %>% formatPercentage(8, 2)
ggplot(cases_TX_select, mapping = aes(x = cases_per_1000, y = deaths_per_1000, label = county_name)) +
  labs(title = 'Relationship between cases and deaths in TX') +
  geom_smooth(method = lm) +
  theme(text=element_text(size=20)) +
  geom_point(mapping = aes(size = total_pop), shape = 21, color = "green",fill = "grey") +
  geom_text_repel(data = subset(cases_TX_select, deaths_per_1000 > quantile(deaths_per_1000, .95)),size = 10)

#death rate in US
library("tidyverse")
library("ggplot2")
library("ggrepel")
library("ggcorrplot")
library("DT")


library("dplyr")

getwd()
setwd("/Users/shuangliang/Documents/SMU/CS7331 Datamining/project1")
cases <- read_csv("COVID-19_cases_plus_census.csv")
#cases
#summary(cases)

cases <- cases %>% mutate_if(is.character, factor)
#dim(cases)

# data_frame(ID, YEAR, NUM) %>%
#   group_by(ID, YEAR) %>%
#   summarise(NUM = sum(NUM))


cases_state <- cases%>%group_by(state)%>%summarise(deaths = sum(deaths),confirmed_cases = sum(confirmed_cases),total_pop = sum(total_pop),median_income = mean(median_income))
dim(cases_state)
#summary(cases_state[,1:3])
summary(cases_state)


ggplot(cases_state, mapping = aes(confirmed_cases)) + geom_histogram(bins = 20)
ggplot(cases_state, mapping = aes(x = confirmed_cases, y = deaths, size = total_pop)) + geom_point()
ggplot(cases_state, mapping = aes(x = confirmed_cases, y = deaths, label = state)) +
  geom_smooth(method = lm) +
  geom_point(mapping = aes(size = total_pop), color = "grey") +
  geom_text_repel(data = subset(cases_state, deaths >= 1000))
cases_state_select <- cases_state %>% filter(confirmed_cases > 100) %>%
  arrange(desc(confirmed_cases)) %>%
  select(state, confirmed_cases, deaths, total_pop, median_income)
cases_state_select <- cases_state_select %>% mutate(
  cases_per_1000 = confirmed_cases/total_pop*1000,
  deaths_per_1000 = deaths/total_pop*1000,
  death_per_case = deaths/confirmed_cases)

head(cases_state_select)
datatable(cases_state_select) %>% formatRound(6:7, 4) %>% formatPercentage(8, 2)
ggplot(cases_state_select, mapping = aes(x = cases_per_1000, y = deaths_per_1000, label = state)) +
  labs(title = 'Relationship between cases and deaths in US') +
  geom_smooth(method = lm) +
  theme(text=element_text(size=20)) +
  geom_point(mapping = aes(size = total_pop), shape = 21,color = "#39c5bb",fill = "white",stroke = 3) +
  geom_text_repel(data = subset(cases_state_select, deaths_per_1000 > quantile(deaths_per_1000, .95)),size = 10)
#Relationship between cases and deaths


