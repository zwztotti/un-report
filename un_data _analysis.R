library(tidyverse)

gapminder_data <- read_csv("gapminder_data.csv")

summarise(gapminder_data, averageLifeExp = mean(lifeExp))

gapminder_data %>% summarise(averageLifeExp = mean(lifeExp)) #  %>%是把上面的数据或者命令传递到下一个命令

gapminder_data %>%
  summarise(averageLifeExp = mean(lifeExp))

# %>%是把上面的数据或者命令传递到下一个命令
# %>% 快捷输入方式
# shift + command + m
# shift + control + m

# <- 是创建或者命名一个新的数据集
# 快捷输入方式 
# ALT + -

gapminder_data_summarized <- gapminder_data %>% summarise(averageLifeExp = mean(lifeExp))

gapminder_data %>% 
  summarize(recent_year = max(year))

gapminder_data %>% 
  filter(year == 2007) %>% 
  summarise(average = mean(lifeExp))

# 找到统计数据最早的一年，然后计算gdpPercap的mean值
gapminder_data %>% 
  summarise(first_year = min(year))

gapminder_data %>% 
  filter(year == 1952) %>% 
  summarise(average = mean(gdpPercap))

# 更简洁的code 找到统计数据最早的一年，然后计算gdpPercap的mean值
gapminder_data %>% 
  filter(year == min(year)) %>% 
  summarise(average = mean(gdpPercap))

# 找到统计数据最早的一年，创建一个my_object文件储存数据，然后计算gdpPercap的mean值，读取文件中储存的数据需要用到pull命令
my_object <- gapminder_data %>% 
  summarise(first_year = min(year))

gapminder_data %>% 
  filter(year == pull(my_object)) %>% 
  summarise(average = mean(gdpPercap))


# group_by command
gapminder_data %>% 
  group_by(year) %>% 
  summarise(average = mean(lifeExp))

gapminder_data %>% 
  group_by(continent) %>% 
  summarise(average = mean(lifeExp))

#计算一个数据的方差SE和标准差SD
gapminder_data %>% 
  group_by(continent) %>% 
  summarise(average = mean(lifeExp), min=min(lifeExp), max=max(lifeExp), variance=var(lifeExp), standard_deviation=sqrt(var(lifeExp)))

# mutate()

gapminder_data %>% 
  mutate(gdp = pop * gdpPercap)

gapminder_data %>% 
  mutate(gdp = pop * gdpPercap, popInMillions = pop/1000000)


# select() 选择某些数据，或者用减号（-）来删除某些数据
gapminder_data %>% 
  select(pop, year)

gapminder_data %>% 
  select(-continent)

gapminder_data %>% 
  select(country, continent, year, lifeExp)

gapminder_data %>% 
  select(year, starts_with('c'))

gapminder_data %>% 
  select(year, ends_with('p'))

# 把长数据转化格式横向展开
# 把country, continent, year,lifeExp这些数据，按照year横向展开lifeExp的数值
gapminder_data %>% 
  select(country, continent, year,lifeExp) %>% 
  pivot_wider(names_from = year, values_from = lifeExp)

# new dataset

getwd()

gapminder_data_2007 <- read.csv("gapminder_data.csv") %>% 
  filter(year == 2007 & continent == "Americas") %>% 
  select(-year, -continent)


temp <- read_csv("co2-un-data.csv")

read_csv('co2-un-data.csv', skip = 1)

co2_emissions_dirty <- read_csv("co2-un-data.csv", skip = 2, col_names = c("region", "country",
                                                                           "year", "series", "value", 
                                                                           "footnotes", "source") )

read_csv("co2-un-data.csv", skip = 1) %>% 
  rename(country = ...2)

co2_emissions_dirty %>% 
  select(country, year, series, value)

# 代码换行最好是在每个符号之后换行，不要改变命令中的空格位置
co2_emissions_dirty %>% 
  select(country, year, series, value) %>% 
  mutate(series = recode(series, 
                         "Emissions (thousand metric tons of carbon dioxide)" = "total_emissions", 
                         "Emissions per capita (metric tons of carbon dioxide)" = "per_capita_emissions")) %>% 
  pivot_wider(names_from = series, values_from = value) #把数据换成横向展示


co2_emissions_dirty %>% 
  select(country, year, series, value) %>% 
  mutate(series = recode(series, 
                         "Emissions (thousand metric tons of carbon dioxide)" = "total_emissions", 
                         "Emissions per capita (metric tons of carbon dioxide)" = "per_capita_emissions")) %>% 
  pivot_wider(names_from = series, values_from = value) %>% 
  count(year)

# 选择2005年的指定数据，然后可以删掉年份信息
co2_emissions_dirty %>% 
  select(country, year, series, value) %>% 
  mutate(series = recode(series, 
                         "Emissions (thousand metric tons of carbon dioxide)" = "total_emissions", 
                         "Emissions per capita (metric tons of carbon dioxide)" = "per_capita_emissions")) %>% 
  pivot_wider(names_from = series, values_from = value) %>% 
  filter(year == 2005) %>% 
  select(-year)

# 把选择出来的数据放到一个新的数据集中
co2_emissions <- co2_emissions_dirty %>% 
  select(country, year, series, value) %>% 
  mutate(series = recode(series, 
                         "Emissions (thousand metric tons of carbon dioxide)" = "total_emissions", 
                         "Emissions per capita (metric tons of carbon dioxide)" = "per_capita_emissions")) %>% 
  pivot_wider(names_from = series, values_from = value) %>% 
  filter(year == 2005) %>% 
  select(-year)

#如何合并两个数据集，他们有相同的分类情况下？inner_join 
inner_join(gapminder_data, co2_emissions) # 默认情况下按照找到的第一行一样的数据的顺序合并Joining with `by = join_by(country)

inner_join(gapminder_data, co2_emissions, join_by(country))


