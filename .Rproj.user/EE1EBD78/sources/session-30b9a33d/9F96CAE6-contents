# data analysis

library(tidyverse)
getwd()
gapminder_data <- read_csv("data/gapminder_data.csv")

# get stats fast
# summarize gives us a small data frame first word in the parenthesis is the title for that column
summarise(gapminder_data, averageLifeExp=mean(lifeExp), medianLifeExp=median(lifeExp))

# Learning to pipe
# First argument is what you pipe into it
# You can pipe multiple functions together
# To save the output, we need to assign it to an object, right now its just printing it

gapminder_summary<-gapminder_data%>%
  summarize(averageLifeExp=mean(lifeExp))
gapminder_summary

# Filtering
# == logical operator
# != exception
gapminder_summary_2007<-gapminder_data%>%
  filter(year == 2007)%>%
  summarize(average = mean(lifeExp))

# Finding average gdpPercap for the first year of the dataset
gapminder_data %>%
  summarize(first_year=min(year))

gapminder_data%>%
  filter(year == 1952)%>%
  summarize(average = mean(gdpPercap))

gapminder_data%>%
  filter(year == min(year))%>%
  summarize(average = mean(gdpPercap))

# Using group_by()
# Find statistics behind specific groups

gapminder_data%>%
  group_by(year)%>%
  summarize(average=mean(lifeExp))

gapminder_data%>%
  group_by(continent)%>%
  summarize(average=mean(lifeExp))

gapminder_data%>%
  group_by(year,continent)%>%
  summarize(average=mean(lifeExp))

gapminder_data%>%
  group_by(year,continent)%>%
  summarize(average=mean(lifeExp), 
            error =sd(lifeExp))

# Mutate function
# Create a new column as a result of other columns

gapminder_data%>%
  mutate(gdp = pop * gdpPercap)

# Mutate a new column which is a population in millions 
# Mutate doesn't change an object, to save it, you need to assign it into a new object 

gapminder_data%>%
  mutate(popInMillion = pop/1000000)

# Select
# keep population and year
gapminder_data%>%
  select(pop, year)

# Remove a specific column
gapminder_data%>%
  select(-continent)

gapminder_data%>%
  select(-continent, -pop)

# We normally format data in a *wide format*, but R doesn't like that, it uses *long format*
# long format: every row is a single observation

# Pivot_wider
# Makes data easier for humans to read
gapminder_data%>%
  select(country, continent, year, lifeExp)%>%
  pivot_wider(names_from = year, values_from = lifeExp)%>%
  View()

# Messy Data :)
# You are responsible for the quality of data you provide R

read_csv("co2-un-data.csv")

#With messy data is better to assign to an object once its formatted
# in R fuctions separate arguments with commas, to give it a list, we need to use the command concatenate, to make it one piece of information that goes to the col_names argument.
read_csv("co2-un-data.csv", skip = 1)

read_csv("co2-un-data.csv", skip = 2,
         col_names = c("region", "country", "year", "series", "value", "footnotes", "source"))

co2_emissions_dirty <- read_csv("co2-un-data.csv", skip = 2,
                                col_names = c("region", "country", "year", "series", "value", "footnotes", "source"))
#Separate series into the 2 different units
#We used mutate to create a new column, but by using the same name as the old column it gets replaced

co2_emissions_dirty%>%
  select(country, year, series, value)%>%
  mutate(series = recode(series, "Emissions (thousand metric tons of carbon dioxide)" = "total_emissions", "Emissions per capita (metric tons of carbon dioxide)" = "per_capita_emissions"))%>%
  pivot_wider(names_from = series, values_from = value)

co2_emissions_dirty2<-co2_emissions_dirty%>%
  select(country, year, series, value)%>%
  mutate(series = recode(series, "Emissions (thousand metric tons of carbon dioxide)" = "total_emissions", "Emissions per capita (metric tons of carbon dioxide)" = "per_capita_emissions"))%>%
  pivot_wider(names_from = series, values_from = value)

co2_emissions_dirty2

# Filter for year 2005
# Select to remove the year column

co2_emissions_dirty2%>%
filter (year == 2005)%>%
  select (-year)

co2_emissions<-co2_emissions_dirty2%>%
  filter (year == 2005)%>%
  select (-year)  

co2_emissions

#Bringing in 2007 Population Data

gapminder_data_2007<-read_csv("data/gapminder_data.csv")%>%
  filter (year == 2007)%>%
  select (country, pop, lifeExp, gdpPercap)

gapminder_data_2007
# Join data frames

inner_join(co2_emissions, gapminder_data_2007)

anti_join(co2_emissions, gapminder_data_2007)
anti_join(gapminder_data_2007, co2_emissions)

full_join(co2_emissions, gapminder_data_2007)

anti_join(gapminder_data, co2_emissions, by="country")

joined_co2_pop<-inner_join(co2_emissions, gapminder_data_2007)

#Writing a CSV

write_csv(joined_co2_pop, file = "data/joined_co2_pop.csv")

#Find relationship between CO2 and GDP