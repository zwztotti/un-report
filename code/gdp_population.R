# Analysis relationship between GDP and life expectancy
library(tidyverse)
library(readr)
gapminder_1997 <- read_csv("gapminder_1997.csv")
View(gapminder_1997)

  
test <- read_csv("gapminder_1997.csv")  # when to use double quotation mark?

Sys.Date() #output the current date, used for knowing when I last ran

getwd() #output current directory

sum(5,6) #adds numbers

round(3.1415, 3)
read_csv(file = 'gapminder_1997.csv') # when to use single quotation mark?


#Draw it step by step
ggplot(data = gapminder_1997) + 
  aes(x = gdpPercap) +
  labs(x = "GDP Per Capita")+
  aes(y = lifeExp)+
  labs(y = "Life Expectancy")+
  geom_point()+ #The form of drawing
  labs(title = "Do people in wealthy countries live longer?")+
  aes(color = continent)+
  scale_color_brewer(palette = "Set1")+
  aes(size = pop/1000000)+
  labs(size = "Population (in millions)")+ #change the Size legend of the figure 
  aes(shape = continent)

#Draw it all at once
ggplot(data = gapminder_1997)+
  aes(x = gdpPercap, y = lifeExp, color = continent, size = pop/1000000, shape = continent)+
  geom_point()+ 
  scale_color_brewer(palette = "Set1")+
  labs(x = "GDP Per Capita", y = "Life Expectancy",
       title = "Do people in wealthy countries live longer?", 
       size = "Population (in millions)")
  
gapminder_data <- read.csv("gapminder_data.csv")

dim(gapminder_data)  #Preview the overall size of the data
head(gapminder_data)  #Preview the first few rows of the data table

ggplot(data = gapminder_data)+
  aes(x = year, y = lifeExp, color = continent)+
  geom_point()
  
ggsave("figures/gdpPercap_lifeExp.png") #Save the figures 

