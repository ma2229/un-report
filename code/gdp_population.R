# Analyze life expectancy and CO2 emissions versus population with gapminder
# Date: Jan 17th, 2023

# Load in packages necessary for analysis
install.packages("tidyverse")
library("tidyverse")
library("readr")
library("wesanderson")
library("ggprism")

# Read in data for analysis
gapminder_1997 <- read_csv("gapminder_1997.csv")
read_csv("gapminder_1997.csv")
read_csv(file = "gapminder_1997.csv")

# Plotting data for visualization
# 1. Make the skeleton
ggplot(data = gapminder_1997)
# 2. Give it instructions, aesthetics and labels
ggplot(data = gapminder_1997) +
  aes(x = gdpPercap) +
  labs(x = "GDP per Capita") +
  aes(y = lifeExp) +
  labs(y = "Life Expectancy (yrs)") 
# 3. Plot
ggplot(data = gapminder_1997) +
  aes(x = gdpPercap) +
  labs(x = "GDP per Capita") +
  aes(y = lifeExp) +
  labs(y = "Life Expectancy (yrs)") +
  geom_point() +
  labs(title = "Do people in wealthy countries live longer?") +
  aes(color = continent) + 
  scale_color_brewer(palette = "Set1")
# Using different color palettes
install.packages("wesanderson")
library("wesanderson")

names(wes_palettes)
ggplot(data = gapminder_1997) +
  aes(x = gdpPercap) +
  labs(x = "GDP per Capita") +
  aes(y = lifeExp) +
  labs(y = "Life Expectancy (yrs)") +
  geom_point() +
  labs(title = "Do people in wealthy countries live longer?") +
  aes(color = continent) + 
  scale_color_manual(values = wes_palette("Darjeeling1"))
#Change size/shape of the data plots
ggplot(data = gapminder_1997) +
  aes(x = gdpPercap) +
  labs(x = "GDP per Capita") +
  aes(y = lifeExp) +
  labs(y = "Life Expectancy (yrs)") +
  geom_point() +
  labs(title = "Do people in wealthy countries live longer?") +
  aes(color = continent) + 
  scale_color_manual(values = wes_palette("Darjeeling1")) +
  aes(size = pop/1000000) +
  labs(size = "Population (in millions)") +
  aes(shape = continent) 
#Group lines in a more concise way
ggplot(data = gapminder_1997) +
  aes(x = gdpPercap, y = lifeExp, color = continent, size = pop/1000000, shape = continent) +
  labs(x = "GDP per Capita", y = "Life Expectancy (yrs)", title = "Do people in wealthy countries live longer?", size = "Population (in millions)") +
  geom_point() +
  scale_color_manual(values = wes_palette("Darjeeling1"))

# Read in all the data from gapminder (more years than 1997!)
gapminder_data <- read_csv("gapminder_data.csv")
View(gapminder_data)
dim(gapminder_data)
head(gapminder_data)  

ggplot(data = gapminder_data) +
  aes(x=year, y=lifeExp, color=continent) +
  geom_point() +
  scale_color_manual(values = wes_palette("Darjeeling1"))

#learn about data
str(gapminder_data)

#different ways to view data
ggplot(data = gapminder_data) +
  aes(x = year, y = lifeExp, color = continent) +
  geom_line() +
  scale_color_manual(values = wes_palette("Darjeeling1"))

ggplot(data = gapminder_data) +
  aes(x = year, y = lifeExp, color = continent, group = country) +
  geom_line() +
  scale_color_manual(values = wes_palette("Darjeeling1"))

ggplot(data = gapminder_data) +
  geom_boxplot() +
  aes (x = continent, y = lifeExp)

ggplot(data = gapminder_data) +
  aes (x = continent, y = lifeExp) +
  geom_violin() +
  geom_jitter()
                     
ggplot(data = gapminder_1997) +
  aes (x = continent, y = lifeExp) +
  geom_jitter() +
  geom_violin()

#Functions within function
ggplot(data = gapminder_1997) +
  aes (x = continent, y = lifeExp) +
  geom_violin(color = "pink")+
  geom_jitter((aes(size = pop))) 

ggplot(data = gapminder_1997) +
  aes (x = continent, y = lifeExp) +
  geom_violin(fill = "pink")+
  geom_jitter((aes(size = pop))) 

ggplot(data = gapminder_1997) +
  aes (x = continent, y = lifeExp) +
  geom_violin(fill = "pink", color = "cornflowerblue") +
  geom_jitter((aes(size = pop))) 

ggplot(data = gapminder_1997) +
  aes (x = continent, y = lifeExp, color = continent) +
  geom_violin() +
  geom_jitter((aes(size = pop))) 

ggplot(data = gapminder_1997) +
  aes (x = continent, y = lifeExp) +
  geom_violin((aes(color = continent))) +
  geom_jitter((aes(size = pop))) 

#Univariate plots

ggplot(gapminder_1997) +
  aes(x = lifeExp) +
  geom_histogram(bins = 20)

#themes

ggplot(gapminder_1997) +
  aes(x = lifeExp) +
  geom_histogram(bins = 20) +
  theme_classic()

install.packages("ggprism")
library("ggprism")

ggplot(gapminder_1997) +
  aes(x = lifeExp) +
  geom_histogram(bins = 20) +
  theme_prism()

#Facets multiple plots in one go

ggplot(data = gapminder_1997) +
  aes(x = gdpPercap, y = lifeExp) +
  labs(x = "GDP per Capita", y = "Life Expectancy (yrs)") +
  geom_point() +
  facet_wrap(vars(continent))

ggplot(data = gapminder_1997) +
  aes(x = gdpPercap, y = lifeExp) +
  labs(x = "GDP per Capita", y = "Life Expectancy (yrs)") +
  geom_point() +
  facet_grid(rows = vars(continent))

#Save plots
ggsave("figureawesome_plot.jpg", width = 6, height = 4)
ggsave("awesome_plot.tiff", width = 6, height = 4)

#a previous plot
#First attempt worked because we specified
lifeExp_hist_prism <- ggplot(gapminder_1997) +
  aes(x = lifeExp) +
  geom_histogram(bins = 20) +
  theme_prism()
lifeExp_hist_prism
ggsave(plot = lifeExp_hist_prism,
       file = "cool_prism_plot.jpeg",
       device = "jpeg",
       width = 4, height = 4)
#If you use the right order, it should work without being specific
ggsave("cool_prism_plot2.jpeg", lifeExp_hist_prism)

