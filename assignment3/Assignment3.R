library("readr")
library("dplyr")
library("data.table")
library("tidyverse")

base_url <- "https://static.usafacts.org/public/data/covid-19"
deaths_url <- paste(base_url, "covid_deaths_usafacts.csv", sep = "/")
cases_url <- paste(base_url, "covid_confirmed_usafacts.csv", sep = "/")
population_url <- paste(base_url, "covid_county_population_usafacts.csv",
                        sep = "/")

# options are "read.csv", "read_csv" and "fread"
csv_loader <- fread

deaths <- csv_loader(deaths_url)
cases <- csv_loader(cases_url)
population <- csv_loader(population_url)

#Show the glimpse of the data
rick_morty_glimpse <- function(input_data){
  glimpse(input_data)
}

rick_morty_glimpse(deaths)
rick_morty_glimpse(cases)
rick_morty_glimpse(population)

#Cleaning column names, I will create generic function to do that
#I thought it would be much cleaner to have different variables and functions for every task
#That way, I don't have rerun the code if the task required original data
rick_morty_to_lower <- function(input_data){
  input_data %>% 
    rename_all(., .funs = tolower) ->
    renamed_output_data
  return(renamed_output_data)
}

deaths_lowered <- rick_morty_to_lower(deaths)
cases_lowered <- rick_morty_to_lower(cases)
population_lowered <- rick_morty_to_lower(population)

rick_morty_glimpse(deaths_lowered)
rick_morty_glimpse(cases_lowered)
rick_morty_glimpse(population_lowered)

#Change county name to countyname
rick_morty_to_rename <- function(input_data){
  input_data %>% 
    rename('countyname' = 'county name'
    ) ->
    output_data
  return(output_data)
}

deaths_lowered_renamed <- rick_morty_to_rename(deaths_lowered)
cases_lowered_renamed <- rick_morty_to_rename(cases_lowered)
population_lowered_renamed <- rick_morty_to_rename(population_lowered)

rick_morty_glimpse(deaths_lowered_renamed)
rick_morty_glimpse(cases_lowered_renamed)
rick_morty_glimpse(population_lowered_renamed)

#Function to change to required format of date
#The dates are formatted correctly, but this function will correct it if it founds one.
rick_morty_to_date <- function(input_data,vect){
  input_data %>% 
    rename_with(~ vect, all_of(as.character.Date(vect,format="%y-%m-%d"))
    ) ->
    output_data
  return(output_data)
}
#I am changing the returned column names into vector as rename(~ vect, all_of(..)) code only understands vector :(
vector_1_deaths <- names(deaths_lowered_renamed)      
vector_1_deaths[5:length(vector_1_deaths)]

vector_1_cases <- names(cases_lowered_renamed)      
vector_1_cases[5:length(vector_1_cases)]
deaths_lowered_renamed_datechanged <- rick_morty_to_date(deaths_lowered_renamed,vector_1_deaths[5:length(vector_1_deaths)])
cases_lowered_renamed_datechanged <- rick_morty_to_date(cases_lowered_renamed,vector_1_cases[5:length(vector_1_cases)])


#This is not smart way of doing it, but all dates start with 2
death_dates <- names(select(deaths_lowered_renamed, starts_with("2")))
death_dates
case_dates <- names(select(cases_lowered_renamed, starts_with("2")))
case_dates

#Aggregating Cases, Deaths and population

#Weird way of replacing cases
cases_x <- group_by(cases_lowered_renamed_datechanged, state)
cases <- summarize_at(
  cases_x,
  case_dates,
  sum,
  na.rm = TRUE
)

#Weird way of replacing deaths
deaths_x <- group_by(deaths_lowered_renamed_datechanged, state)
deaths <- summarize_at(
  deaths_x,
  death_dates,
  sum,
  na.rm = TRUE
)

#Weird way of replacing population
population_x <- group_by(population_lowered_renamed, state)
population <- summarize_at(
  population_x,
  c("population"),
  sum,
  na.rm = TRUE
)

# Reshaping, not so wierd way of replacing the cases
cases %>% 
  pivot_longer(cols = -one_of('state')
               ,names_to = 'date'
               ,values_to = 'cases'
  ) ->
  cases

deaths %>% 
  pivot_longer(cols = -one_of('state')
               ,names_to = 'date'
               ,values_to = 'deaths'
  ) ->
  deaths

#Merging datasets, left join as it doesn't really matter, which join
counts_x <- left_join(
  cases,
  deaths,
  by=c("state","date")
)

#Merging, left join, because data on the left is larger
count <- left_join(
  counts_x,
  population,
  by=c("state")
)


################################################################
# Question  2,

#With pipeline - order
count <- count %>%
  arrange(state,date)

#New cases and deaths
count %>% 
  mutate(new_cases = cases - lag(cases),
         new_deaths = deaths - lag(deaths)) %>% 
  ungroup() ->
  count

#
count %>% 
  mutate(new_cases = pmax(new_cases,0),
         new_deaths = pmax(new_deaths,0)) %>% 
  ungroup() ->
  count
#Not fun way of doing it but, this replaces the NA with first element.
#Note here, the first element is 0
count$new_cases[is.na(count$new_cases)] <- 0
count$new_deaths[is.na(count$new_deaths)] <- 0
glimpse(count)

#Comulative
count %>% 
  mutate(comulative_cases = cumsum(as.numeric(new_cases)),
         comulative_deaths = cumsum(as.numeric(new_deaths))) %>% 
  ungroup() ->
  count

#Case fatality proportion (P.S: fatality is not a rate)
count %>% 
  mutate(ifr = (cases/deaths)) %>% 
  ungroup() ->
  count
#IFR = 0, shows three possible things
#1: No cases, no cases (NaN): shouldn't be measured
#2: No deaths, with some cases (true zero): Should be measured and it is zero
#3: Some deaths, but no cases (Infinity): Physically impossible
count$ifr[is.na(count$ifr)] <- 0
count$ifr[is.infinite(count$ifr)] <- 0

#I changed mr column to mr_per_100_000 to show the mortality is given per 100,000 population
#The rationale behind doing so is the mortality rate is very small
count %>% 
  mutate(mr_per_100_000 = (deaths/population) * 100000) %>% 
  ungroup() ->
  count

##########################################################
#Question 3
#Comparing by deaths and cases
count_compare <- group_by(count, state)
count_compare_per_state <- summarize_at(
  count_compare,
  c("new_cases","new_deaths"),
  sum,
  na.rm = TRUE
)

count_compare_per_state <- count_compare_per_state %>%
  arrange(-new_cases)
count_compare_per_state$state[1:5]
#The top five states with high cases are: "CA" "TX" "FL" "NY" "IL"

count_compare_per_state <- count_compare_per_state %>%
  arrange(-new_deaths)
count_compare_per_state$state[1:5]
#The top five states with high deaths are: "CA" "TX" "FL" "NY" "IL"

#Comparing by infection fatality rate and mortality rate
#Using mean is not probably a good idea, as it can easily impacted by smaller or bigger reports
count_compare_severity <- group_by(count, state)
count_compare_per_state_severity <- summarize_at(
  count_compare_severity,
  c("ifr","mr_per_100_000"),
  mean,
  na.rm = TRUE
)

count_compare_per_state_severity <- count_compare_per_state_severity %>%
  arrange(-ifr)
count_compare_per_state_severity$state[1:5]
#The top five states with fatality proportions are:"UT" "AK" "HI" "VT" "NE"
count_compare_per_state_severity <- count_compare_per_state_severity %>%
  arrange(-mr_per_100_000)
count_compare_per_state_severity$state[1:5]
#The top five states with fatality proportions are:"NJ" "NY" "MS" "LA" "AZ"