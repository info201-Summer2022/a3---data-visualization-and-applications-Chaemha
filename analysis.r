library(tidyverse) 
library(dplyr)
library(ggplot2)
library(plotly)
library(maps)
library(usdata)

incarceration <- read.csv("https://raw.githubusercontent.com/vera-institute/incarceration-trends/master/incarceration_trends.csv")

country_jail_rate <- incarceration %>% mutate(latinx_jail_rate = (latinx_jail_pop /latinx_pop_15to64)*100) %>% 
  mutate(black_jail_rate = (black_jail_pop / black_pop_15to64)*100) 
country_jail_rate <-  country_jail_rate %>% select (year, county_name, latinx_jail_rate, black_jail_rate, state, fips)

LA_county <- country_jail_rate %>% filter(county_name == "Los Angeles County") %>% slice(31:49)
mean_la_latinx <- mean(LA_county$latinx_jail_rate)
mean_la_black <- mean(LA_county$black_jail_rate)
max_la_latinx <- max(LA_county$latinx_jail_rate)
max_la_black <- max(LA_county$black_jail_rate)
min_la_latinx <- min(LA_county$latinx_jail_rate)
min_la_black <- min(LA_county$black_jail_rate)
Cook_county <- country_jail_rate %>% filter(county_name == "Cook County") %>% slice(31:49)
mean_cook_latinx <- mean(Cook_county$latinx_jail_rate)
mean_cook_black <- mean(Cook_county$black_jail_rate)
max_cook_latinx <- max(Cook_county$latinx_jail_rate)
max_cook_black <- max(Cook_county$black_jail_rate)
min_cook_latinx <- min(Cook_county$latinx_jail_rate)
min_cook_black <- min(Cook_county$black_jail_rate)
Harris_county <- country_jail_rate %>% filter(county_name == "Harris County") %>% slice(31:49)
mean_harris_latinx <- mean(Harris_county$latinx_jail_rate)
mean_harris_black <- mean(Harris_county$black_jail_rate)
max_harris_latinx <- max(Harris_county$latinx_jail_rate)
max_harris_black <- max(Harris_county$black_jail_rate)
min_harris_latinx <- min(Harris_county$latinx_jail_rate)
min_harris_black <- min(Harris_county$black_jail_rate)

diff_la <- LA_county %>% mutate(black_latinx_diff = (black_jail_rate / latinx_jail_rate)) %>% 
  select(year, county_name, black_latinx_diff)
diff_cook <- Cook_county %>% mutate(black_latinx_diff = (black_jail_rate / latinx_jail_rate)) %>% 
  select(year, county_name, black_latinx_diff)
diff_harris <- Harris_county %>% mutate(black_latinx_diff = (black_jail_rate / latinx_jail_rate)) %>% 
  select(year, county_name, black_latinx_diff)
combined <- rbind(diff_la, diff_cook, diff_harris)
ggp_combined <- ggplot(combined, aes(x = year, y = black_latinx_diff, color = county_name)) + geom_line()
print(ggp_combined + labs(title = "Black Over Lanix Jail Rate By Top 3 County in 2000-2018", y = "Black over Latinx Jail Rate", x= "Year", color = "County"))

black_la <- LA_county %>% select(year, county_name, black_jail_rate)
black_harris <- Harris_county %>% select(year, county_name, black_jail_rate)
black_cook <- Cook_county %>% select(year, county_name, black_jail_rate)
black <- rbind(black_la, black_cook, black_harris)

bar_black <- ggplot(black, aes(x = year, y = black_jail_rate, fill = county_name)) + geom_col(position = "dodge")
print(bar_black + labs(title = "Change in Jail Rate of Black in Different Regions (2000-2018)", y = "Black Jail Rate (%)", x = "Year", fill = "County"))

filtered <- country_jail_rate %>% filter(year == 2018)

filtered <- filtered %>% mutate(black_over_latinx = (black_jail_rate / latinx_jail_rate)*100) 

map_shapes <- map_data("county") %>% 
  unite(polyname, region, subregion, sep = ",") %>% left_join(county.fips, by="polyname")

map_data <- map_shapes %>% left_join(filtered, by="fips")

blank_theme <- theme_bw() +
  theme(
    axis.line = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    plot.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank()
  )

map <- ggplot(map_data) +
  geom_polygon(
    mapping = aes(x = long, y =lat, group = group, fill = black_over_latinx),
    color = "gray", size = 0.3
  ) + 
  coord_map() + 
  scale_fill_continuous(limits = c(0, max(map_data$black_over_latinx)), na.value = "white", low = "yellow", high = "red") + blank_theme +
  ggtitle("Black Jail Rate over Latinx Jail Rate in the United States") +
  guides(fill=guide_legend(title="Black over Latinx jail rate"))

map
