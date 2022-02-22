# Assignment Overview ----------------------------------------------------------
# Assignment 3: Data Visualization (incarceration) (INFO-201, Winter 2022)

# Set up working directory
setwd("C:/Users/Samira Shirazy/Desktop/Assessments/a3-ssshirazy")
getwd()

# Load the necessary packages 
library("dplyr")
library("stringr")
library("tidyverse")
library("ggplot2")

# Load the *incarceration trends* data into a variable. `incarceration_data`
file_name <- "https://raw.githubusercontent.com/vera-institute/incarceration-trends/master/incarceration_trends.csv"
incarceration_data <- read.csv(file_name, header = TRUE, 
                               stringsAsFactors = FALSE)

# View the data
View(incarceration_data)

# Print the column names of the dataset
print(colnames(incarceration_data))

# What year was jail_rated_capacity the highest and what was the value?
highest_jrc <- incarceration_data %>%
  filter(jail_rated_capacity == max(jail_rated_capacity, na.rm = TRUE)) %>%
  select(jail_rated_capacity, year)
print(highest_jrc)

# What is the difference between `total_jail_pop` in 1970 to 2018?
difference_1970_2018 <- incarceration_data %>%
  filter(year == 2018 | year == 1970) %>%
  group_by(year) %>%
  summarize(yearly_pop = sum(total_jail_pop, na.rm = TRUE)) %>%
  summarize(difference = yearly_pop[2] - yearly_pop[1])
print(difference_1970_2018)

# What is the average percent of Black people in jail?
percent_black_jail <- incarceration_data %>%
  group_by(year) %>%
  summarize(tot_jail_pop = sum(total_jail_pop, na.rm = TRUE), 
            tot_black_prison_pop = sum(black_jail_pop, na.rm = TRUE)) %>%
  summarize(ave_jail_pop = mean(tot_jail_pop, na.rm = TRUE), 
            ave_black_jail = mean(tot_black_prison_pop, na.rm = TRUE)) %>%
  mutate(percent = ave_black_jail * 100 / ave_jail_pop) %>%
  pull(percent)
print(percent_black_jail)

# What is the average percent of White people in jail? 
percent_white_jail <- incarceration_data %>%
  group_by(year) %>%
  summarize(tot_jail_pop = sum(total_jail_pop, na.rm = TRUE), 
            tot_white_prison_pop = sum(white_jail_pop, na.rm = TRUE)) %>%
  summarize(ave_jail_pop = mean(tot_jail_pop, na.rm = TRUE), 
            ave_white_jail = mean(tot_white_prison_pop, na.rm = TRUE)) %>%
  mutate(percent = ave_white_jail * 100 / ave_jail_pop) %>%
  pull(percent)
print(percent_white_jail)

# What

# Chart Number 1
get_percents <- incarceration_data %>%
  select(year, state, total_jail_pop, aapi_jail_pop, native_jail_pop, black_jail_pop, 
         white_jail_pop, other_race_jail_pop, latinx_jail_pop) %>%
  filter(state == "TX") %>%
  group_by(year) %>%
  summarize(
    all_tot = sum(total_jail_pop, na.rm = TRUE),
    native_tot = sum(native_jail_pop, na.rm = TRUE),
    aapi_tot = sum(aapi_jail_pop, na.rm = TRUE),
    black_tot = sum(black_jail_pop, na.rm = TRUE),
    white_tot = sum(white_jail_pop, na.rm = TRUE),
    latinx_tot = sum(latinx_jail_pop, na.rm = TRUE),
    other_tot = sum(other_race_jail_pop, na.rm = TRUE)
  ) %>%
  gather(key = race, value = population, -year, -all_tot) %>%
  filter(year >= 1985)
View(get_percents)


ggplot(data = get_percents) +
  geom_area(
    mapping = aes(x = year, y = population, fill = race)
) +
  labs (
    title = "Population of people in jail by race",
    subtitle = "from the years: 1985 to 2018",
    fill = "Race",
    x = "Year",
    y = "Population"
  )

# Chart Number two

# Map !

map_stuffs <- incarceration_data %>%
  select(total_jail_pop, black_jail_pop, year, state) %>%
  replace_na(list(total_jail_pop = 0)) %>%
  replace_na(list(black_jail_pop = 0)) %>%
  filter(year == 2012) %>%
  group_by(state) %>%
  mutate(
    percent_black_in_jail = black_jail_pop * 100 / total_jail_pop
    ) %>%
  summarize(ave_percent = mean(percent_black_in_jail, na.rm = TRUE))
View(map_stuffs)

# Load a shapefile of U.S. states using ggplot's `map_data()` function
state_shape <- map_data("state")

# Create a blank map of U.S. states
ggplot(state_shape) +
  geom_polygon(
    mapping = aes(x = long, y = lat, group = group),
    color = "white", # show state outlines
    size = .1        # thinly stroked
  ) +
  coord_map() # use a map-based coordinate system
  
state_shape <- map_data("state") %>%
  mutate(state = region) %>%
  left_join(map_stuffs, by = "state")
View(state_shape)
