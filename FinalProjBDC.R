### Packages ####

library(dplyr) #Data wrangling tools
library(ggplot2) # Used for graphing
library(RColorBrewer) # Color Palettes for Graphs 
library(plotly) #Interactive plots
library(patchwork) #Show charts together 
library(vcd) #Cramer V's table
### Reading Data #### 
vehicles2022 <- read.csv("~/Downloads/CRSS2022CSV/vehicle.csv")
vehicles2021 <- read.csv("~/Downloads/CRSS2021CSV/vehicle.csv")
vehicles2020 <- read.csv("~/Downloads/CRSS2020CSV/vehicle.csv")
weather2022 <- read.csv("~/Downloads/CRSS2022CSV/weather.csv")
weather2021 <- read.csv("~/Downloads/CRSS2021CSV/weather.csv")
weather2020 <- read.csv("~/Downloads/CRSS2020CSV/weather.csv")
accident2020 <- read.csv("~/Downloads/CRSS2020CSV/accident.csv")
accident2021 <- read.csv("~/Downloads/CRSS2021CSV/accident.csv")
accident2022 <- read.csv("~/Downloads/CRSS2022CSV/accident.csv")


# 2020 data cleaning ####

cleanweather2020 <- weather2020 %>% 
  select(CASENUM, WEATHERNAME) %>% #Selecting columns I need
  rename(casenum = CASENUM, weather = WEATHERNAME) %>% #Simplifying column names 
  mutate(weather = recode(weather, "Not Reported" = "NR", "Freezing Rain or Drizzle" = "Rain", "Reported as Unknown" = "NR", 
                          "Fog, Smog, Smoke" = "Smoke", "Severe Crosswinds" = "Windy", 
                          "Blowing Snow" = "Snow")) #Making names smaller for cleaner graph 

 

# 2021 data cleaning ####
cleanweather2021 <- weather2021 %>% 
  select(CASENUM, WEATHERNAME) %>% #Selecting columns I need
  rename(casenum = CASENUM, weather = WEATHERNAME) %>% #Simplifying column names 
 mutate(weather = recode(weather, "Not Reported" = "NR", "Freezing Rain or Drizzle" = "Rain", "Reported as Unknown" = "NR", 
                          "Fog, Smog, Smoke" = "Smoke", "Severe Crosswinds" = "Windy", 
                          "Blowing Snow" = "Snow", "Blowing Sand, Soil, Dirt" = "Windy"))

# 2022 data cleaning ####
cleanweather2022 <- weather2022 %>% 
  select(CASENUM, WEATHERNAME) %>% #Selecting columns I need
  rename(casenum = CASENUM, weather = WEATHERNAME) %>% #Simplifying column names 
  mutate(weather = recode(weather, "Not Reported" = "NR", "Freezing Rain or Drizzle" = "Rain", "Reported as Unknown" = "NR", 
                          "Fog, Smog, Smoke" = "Smoke", "Severe Crosswinds" = "Windy", 
                          "Blowing Snow" = "Snow", "Blowing Sand, Soil, Dirt" = "Windy"))


#  vehicles data cleaning #### 

unique(vehicles2020$MAKENAME)

vhmake2020 <- vehicles2020 %>% mutate(Year = 2020) %>% select(Year, CASENUM, MAKENAME) %>% rename(casenum = CASENUM, make = MAKENAME) %>% filter(make %in% c("Toyota", "Hyundai", "Nissan/Datsun", "BMW", "Audi", "Mercedes-Benz"))
vhmakewthr2020 <- left_join(vhmake2020, cleanweather2020, by = "casenum") 

vhmake2021 <- vehicles2021 %>% mutate(Year = 2021) %>% select(Year, CASENUM, MAKENAME) %>% rename(casenum = CASENUM, make = MAKENAME) %>% filter(make %in% c("Toyota", "Hyundai", "Nissan/Datsun", "BMW", "Audi", "Mercedes-Benz"))
vhmakewthr2021 <- left_join(vhmake2021, cleanweather2021, by = "casenum") 

vhmake2022 <- vehicles2022 %>% mutate(Year = 2022) %>% select(Year, CASENUM, MAKENAME) %>% rename(casenum = CASENUM, make = MAKENAME) %>% filter(make %in% c("Toyota", "Hyundai", "Nissan/Datsun", "BMW", "Audi", "Mercedes-Benz"))
vhmakewthr2022 <- left_join(vhmake2022, cleanweather2022, by = "casenum") 

vhmakewthr <- rbind(vhmakewthr2020, vhmakewthr2021, vhmakewthr2022) 

#Creating count dataset for overall data
weather_countspop1 <- vhmakewthr %>%
  count(make, weather) %>% rename("count" = "n") %>% filter(weather %in% c("Rain", "Clear", "Cloudy", "Snow"))


# Coding for plot to show overall count of accidents per vehicle make 

countvhm <- ggplot(weather_countspop1, aes(x = make, y = count, text = paste("Make: ", make, "<br>Weather: ", weather, "<br>Count: ", count))) +
  geom_bar(stat = "identity", fill = "pink", color = "black") +
  labs(
    title = "Number of Accidents per Vehicle Make Total",
    x = "Vehicle Make",
    y = "Number of Accidents"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  

#Making it interactive with hover information 
plot1 <- ggplotly(countvhm, tooltip = "text")
plot1


# Creating my sample ####
sampled_data <- vhmakewthr %>%
  group_by(make) %>%                    # Group by the 'make' column
  sample_n(1600, replace = FALSE) %>%     #only 1666 Audis in the data, so must be less than 
  ungroup()


# Contigency Table ####

contingency_weather <- sampled_data %>% select(weather, make) 

contingency_weather$weather <- ifelse(contingency_weather$weather == "Rain", 1, 0)

contingency_table <- table(contingency_weather$make, contingency_weather$make)

print(contingency_table)

chisq.test(contingency_table) #There is a correlation 

assocstats(contingency_table) #Cramer's V 

model <- glm(weather ~ make, family = binomial(link = "logit"), data = contingency_weather) #Logistics 
summary(model) 

# Creating count for each make in weather with sample data
weather_countspop2 <- sampled_data %>%
  count(make, weather) %>% rename("count" = "n") %>% filter(weather %in% c("Rain", "Clear", "Cloudy", "Snow")) %>%
  mutate(percent = count / 1600) 

rainy_count <- weather_countspop2 %>% filter(weather == "Rain") 
rainy_count 
# Stacked Bar Graph sample data
stacked <- ggplot(weather_countspop2, aes(x = make, y = count, fill = weather, text = paste("Make: ", make, "<br>Weather: ", weather, "<br>Count: ", count))) +
  geom_bar(stat = "identity", position = "stack") + 
  scale_fill_brewer(palette = "Pastel1") +# Use 'stat = "identity"' to specify that y is pre-calculated
  labs(title = "Rain vs Non-Rain Crashes by Car Make",
       x = "Car Make",
       y = "Count of Crashes") +
  theme_minimal()

plot2 <- ggplotly(stacked, tooltip = "text")
plot2 

#Pie Chart sample data 
rain_crashes <- sampled_data %>%
  filter(weather == "Rain") %>%
  group_by(make) %>%
  summarise(count = n())  

clear_crashes <- sampled_data %>% 
  filter(weather == "Clear") %>% 
  group_by(make) %>% 
  summarise(count = n()) 
# Create a pie chart sample data
pie_chart1 <- ggplot(rain_crashes, aes(x = "", y = count, fill = make, text = paste("Make: ", make, "<br>Count: ", count))) +
  geom_bar(stat = "identity", width = 1) +
  scale_fill_brewer(palette = "Pastel1") +
  coord_polar(theta = "y") +  # Convert bar chart to pie chart
  labs(title = "Proportion of Rain Crashes by Car Make") +
  theme_minimal() +
  theme(axis.text.x = element_blank())  # Remove x-axis labels (they are not needed for a pie chart)


pie_chart2 <- ggplot(clear_crashes, aes(x = "", y = count, fill = make)) +
  geom_bar(stat = "identity", width = 1) +
  scale_fill_brewer(palette = "Pastel1") +
  coord_polar(theta = "y") +  # Convert bar chart to pie chart
  labs(title = "Proportion of Clear Crashes by Car Make") +
  theme_minimal() +
  theme(axis.text.x = element_blank())  # Remove x-axis labels (they are not needed for a pie chart)

# Creating sample data with 4 main weather conditions

vhmakewthr2 <- sampled_data %>% filter(weather %in% c("Rain", "Clear", "Cloudy", "Snow"))


piecharts <- pie_chart1 + pie_chart2

# Distribution of Vehicle Makes by Rainy Condition Sample data
ggplot(vhmakewthr2, aes(x = make, fill = make)) +
  geom_bar() +
  facet_wrap(~ weather) + 
  labs(title = "Distribution of Vehicle Makes by Rainy Condition",
       x = "Vehicle Make", y = "Number of Crashes") +  
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_brewer(palette = "Pastel1")




 