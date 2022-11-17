library("readr")
library("dplyr")
library("data.table")
library("lubridate")

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
rick_morty_change_date <- function(input_data){
  #Starts with 2
  deaths_lowered_renamed_filtered <- select(input_data, starts_with("2"))

  input_data %>% 
    rename(deaths_lowered_renamed_filtered = ymd(deaths_lowered_renamed_filtered)
    ) ->
    output_data
  return(output_data)
}

deaths_lowered_renamed_datechanged <- rick_morty_change_date(deaths_lowered_renamed)
cases_lowered_renamed_datechanged <- rick_morty_change_date(cases_lowered_renamed)


ymd("2/12/2")
