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

