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

