# Assignment Overview ----------------------------------------------------------
# Assignment 3: Data Visualization (incarceration) (INFO-201, Winter 2022)

# Set up working directory
#setwd("C:/Users/Samira Shirazy/Desktop/Assessments/a3-ssshirazy")
#getwd()

# Load the necessary packages 
library("dplyr")
library("stringr")
library("tidyverse")
library("ggplot2")
library("maps")

# Load the *incarceration trends* data into a variable. `incarceration_data`
file_name <- "https://raw.githubusercontent.com/vera-institute/incarceration-trends/master/incarceration_trends.csv"
incarceration_data <- read.csv(file_name, header = TRUE, 
                               stringsAsFactors = FALSE)

# View the data
View(incarceration_data)

# Print the column names of the dataset
print(colnames(incarceration_data))

# 1. What year was jail_rated_capacity the highest and what was the value?
highest_jrc <- incarceration_data %>%
  filter(jail_rated_capacity == max(jail_rated_capacity, na.rm = TRUE)) %>%
  select(jail_rated_capacity, year)
print(highest_jrc)

# 2. What is the difference between `total_jail_pop` in 1970 to 2018?
difference_1970_2018 <- incarceration_data %>%
  filter(year == 2018 | year == 1970) %>%
  group_by(year) %>%
  summarize(yearly_pop = sum(total_jail_pop, na.rm = TRUE)) %>%
  summarize(difference = yearly_pop[2] - yearly_pop[1])
View(difference_1970_2018)

# 3a. What is the average percent of Black people in jail (from 1970-2018)?
percent_black_jail <- incarceration_data %>%
  group_by(year) %>%
  summarize(tot_jail_pop = sum(total_jail_pop, na.rm = TRUE), 
            tot_black_prison_pop = sum(black_jail_pop, na.rm = TRUE)) %>%
  summarize(ave_jail_pop = mean(tot_jail_pop, na.rm = TRUE), 
            ave_black_jail = mean(tot_black_prison_pop, na.rm = TRUE)) %>%
  mutate(percent = ave_black_jail * 100 / ave_jail_pop)
print(percent_black_jail)

# 3b. What is the average percent of White people in jail (from 1978-2018)? 
percent_white_jail <- incarceration_data %>%
  group_by(year) %>%
  summarize(tot_jail_pop = sum(total_jail_pop, na.rm = TRUE), 
            tot_white_prison_pop = sum(white_jail_pop, na.rm = TRUE)) %>%
  summarize(ave_jail_pop = mean(tot_jail_pop, na.rm = TRUE), 
            ave_white_jail = mean(tot_white_prison_pop, na.rm = TRUE)) %>%
  mutate(percent = ave_white_jail * 100 / ave_jail_pop) %>%
  pull(percent)
print(percent_white_jail)
  
# 4: What have been the highest incarceration rates for Black and White 
# people in 2016?
 
  # 4a. First step: Calculate Incarceration rates
  
  # Black people
black_incarceration <- incarceration_data %>%
  select(black_pop_15to64, black_jail_pop, black_prison_pop, year, fips) %>%
  mutate(black_prison_jail = black_jail_pop + black_prison_pop) %>%
  filter(black_pop_15to64 > 0) %>%
  filter(black_pop_15to64 >= black_prison_jail) %>%
  mutate(black_incar_rate = black_prison_jail * 100000 / black_pop_15to64)
View(black_incarceration)
         
  # White people
white_incarceration <- incarceration_data %>%
  select(white_pop_15to64, white_jail_pop, white_prison_pop, year, fips) %>%
  mutate(white_prison_jail = white_jail_pop + white_prison_pop) %>%
  filter(white_pop_15to64 > 0) %>%
  filter(white_pop_15to64 >= white_prison_jail) %>%
  mutate(white_incar_rate = white_prison_jail * 100000 / white_pop_15to64)
View(white_incarceration)

  # 4b. Second Step: Wrangle data to find the highest incarceration 
  
  # Black people
black_highest_2000 <- black_incarceration %>%
  filter(year == 2000) %>%
  filter(black_incar_rate == max(black_incar_rate, na.rm = TRUE)) %>%
  pull(black_incar_rate)
print(black_highest_2000)

  # White people
white_highest_2000 <- white_incarceration %>%
  filter(year == 2000) %>%
  filter(white_incar_rate == max(white_incar_rate, na.rm = TRUE)) %>%
  pull(white_incar_rate)
print(white_highest_2000)

# Chart Number 1

# Find the total population of each racial group in jail in Texas from 1985 
# to 2016
get_percents <- incarceration_data %>%
  select(year, state, total_jail_pop, aapi_jail_pop, native_jail_pop,
         black_jail_pop, white_jail_pop, other_race_jail_pop,
         latinx_jail_pop) %>%
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

# Draw the map setting the `fill` based off of the racial groups
time_trend_chart <- ggplot(data = get_percents) +
  geom_area(
    mapping = aes(x = year, y = population, fill = race)
    ) +
  labs (
    title = "Population of people in jail by race in Texas",
    subtitle = "from the years: 1985 to 2016",
    fill = "Race",
    x = "Year",
    y = "Population"
  )
plot(time_trend_chart)

# Chart Number Two

# Select the `jail_rated_capacity` and `total_jail_pop` from the 
# `incarceration_data` data frame to help create chart.
jail_capacity_and_pop <- incarceration_data %>%
  select(jail_rated_capacity, total_jail_pop)

ggplot(jail_capacity_and_pop) +
  geom_point(
    mapping = aes(y = jail_rated_capacity, x = total_jail_pop),
    color = "Blue",
  ) +
  labs(
    title = "Jail rated capacity compared to the population",
    x = "Total jail population",
    y = "Jail rated capacity"
  )

ggplot(jail_capacity_and_pop) +
  geom_point(
    mapping = aes(x = jail_rated_capacity, y = total_jail_pop),
    color = "Blue"
  ) +
  labs(
    title = "Jail rated capacity compared to the population",
    x = "Jail rated capacity",
    y = "Total jail population"
  )

# Map

# Calculate the percent of Black people in jail over the whole Black population
  # `percent_black_in_jail` for the year 2018.
# Filter out any values that are greater than 100% because it wouldn't make
# sense for there to be more Black people in jail than the whole population

black_jail_percent <- incarceration_data %>%
  select(total_jail_pop, black_jail_pop, year, fips) %>%
  filter(year == 2018) %>%
  group_by(fips) %>%
  summarize(percent_black_in_jail = black_jail_pop * 100 / total_jail_pop) %>%
  filter(percent_black_in_jail <= 100.000)
View(black_jail_percent)

# Load a shapefile of U.S. counties using ggplot's `map_data()` function
  # Add a column named `polyname` so that it can be joined with county.fips
county_shape <- map_data("county") %>%
  unite(polyname, region, subregion, sep = ",") %>%
  left_join(county.fips, by = "polyname")
View(county_shape)

# Join `black_jail_percent` data to the U.S. shapefile. `joined_df`
joined_df <- county_shape %>%
  left_join(black_jail_percent, by = "fips")

# Define a blank theme for map
blank_theme <- theme_bw() +
  theme(
    axis.line = element_blank(),        # remove axis lines
    axis.text = element_blank(),        # remove axis labels
    axis.ticks = element_blank(),       # remove axis ticks
    axis.title = element_blank(),       # remove axis titles
    plot.background = element_blank(),  # remove gray background
    panel.grid.major = element_blank(), # remove major grid lines
    panel.grid.minor = element_blank(), # remove minor grid lines
    panel.border = element_blank()      # remove border around plot
  )

# Draw the map setting the `fill` of each county using their Black percent
ggplot(joined_df) +
  geom_polygon(
    mapping = aes(x = long, y = lat, group = group,
                  fill = percent_black_in_jail),
    size = .1
  ) +
  labs(
    fill = "Percent (%)",
    title = "2018: Percent of Black people in jail
    versus the population in the U.S."
  ) +
  scale_fill_continuous(low = "#93E278", high = "#9955DD") +
  coord_map() + # use a map-based coordinate system
  blank_theme
