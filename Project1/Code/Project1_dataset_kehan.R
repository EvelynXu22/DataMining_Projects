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

