library(tidyverse)
library(ggplot2)
library(formattable)
library(stringr)
h <- head
g <- glimpse
s <- summary

# master data frames 
# ho: hospital number 
# st: state name
# pop: population
ho <- read.csv("Structural_Measures_-_Hospital.csv", stringsAsFactors = FALSE)
st <- read.csv("state.csv", stringsAsFactors = FALSE)
pop <- read.csv("nst-est2019-popchg2010_2019.csv", stringsAsFactors = FALSE)
        
# data cleaning        
ho1 <- ho %>% 
        count(State) %>% 
        rename(Hospital_Number = n) %>% 
        inner_join(st, by = c("State" = "Code")) %>% 
        select(-Abbrev) %>% 
        rename(State_abbv = State, State = State.y)

pop1 <- pop %>%
        select(NAME, POPESTIMATE2010:POPESTIMATE2019) %>%
        gather(key = "Year", value = "Population", -NAME) %>%
        filter(NAME %in% ho1$State)

year <- sort(rep(2010:2019, 51))

pop1$Year <- year

pop2 <- pop1 %>%
        filter(Year == 2018) 

ho2 <- ho1 %>%
        inner_join(pop2, by = c("State" = "NAME")) %>%
        mutate(Hospitals_per_million = Hospital_Number / Population * 1000000) 

rank_absolute_hospitals <- ho2 %>%
        arrange(desc(Hospital_Number))

rank_relative_hospitals <- ho2 %>%
        arrange(desc(Hospitals_per_million)) %>% 
        mutate(Hospitals_per_million = round(Hospitals_per_million))

rsquare <- round(cor(ho2$Hospital_Number, ho2$Population), digits = 2)

# plot I 
hospital_number_vs_population <- ggplot(ho2, aes(x = Population, 
                                                 y = Hospital_Number, 
                                                 col = Hospitals_per_million)) + 
        geom_point(alpha = 0.7, size = 2) + 
        ggtitle("Hospital Number vs Population") + 
        ylab("Hospital Number") + 
        geom_smooth(method = "lm", col = "red", se = FALSE) 

# tables
absolute_five_table <- data.frame(Top_States = rank_absolute_hospitals$State[1:5],
                                  Top_Number = rank_absolute_hospitals$Hospital_Number[1:5], 
                                  Bottom_States = rank_absolute_hospitals$State[47:51],
                                  Bottom_Number = rank_absolute_hospitals$Hospital_Number[47:51])
relative_five_table <- data.frame(Top_States = rank_relative_hospitals$State[1:5], 
                                  Top_Number_per_million = rank_relative_hospitals$Hospitals_per_million[1:5], 
                                  Bottom_State = rank_relative_hospitals$State[47:51],
                                  Bottom_Number_per_million = rank_relative_hospitals$Hospitals_per_million[47:51])
absolute_five <- formattable(absolute_five_table, 
                                           list(Top_Number = color_tile("transparent", "lightpink"),
                                                Bottom_Number = color_tile("transparent", "lightblue")))
relative_five <- formattable(relative_five_table, 
                             list(Top_Number_per_million = color_tile("transparent", "#FF66FF"),
                                  Bottom_Number_per_million = color_tile("transparent", "#0099FF")))

relative_num_histo <- ggplot(ho2, aes(x = Hospitals_per_million)) + 
        geom_histogram(binwidth = 2, fill = "#0066CC", col = "black") + 
        ggtitle("Distribution of Relative Hospital Number") + 
        ylab("Count") + 
        xlab("Hospitals per Million People") + 
        geom_vline(xintercept = mean(ho2$Hospitals_per_million), col = "red", size = 1.2) + 
        theme(panel.background = element_rect(fill = "white"))
        
