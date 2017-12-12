#Set the working directory
setwd("/users/pranavdar/Desktop/Analytics/Datasets")

#Load the required libraries
library(dplyr)
library(ggplot2)

#Import the dataset
romans <- read.csv("roman-emperors.csv", stringsAsFactors = F)

glimpse(romans)

#How did the emperors gain ascension?
romans %>% 
  group_by(Succession) %>% 
  summarise(count_of_succession = n()) %>% 
  arrange(desc(count_of_succession)) %>% 
  ggplot(aes(reorder(Succession, count_of_succession), count_of_succession)) + 
  geom_bar(stat = "identity", fill = "green") + 
  geom_text(aes(label = count_of_succession)) + coord_flip() + 
  labs(x = "Succession Type", y = "Count", title = "Types of Succession")

#Cause of death
romans %>% 
  group_by(Cause) %>% 
  summarise(death_count = n()) %>% 
  arrange(desc(death_count)) %>% 
  ggplot(aes(reorder(Cause, death_count), death_count)) + 
  geom_bar(stat = "identity", fill = "green") + 
  geom_text(aes(label = death_count)) + coord_flip() + 
  labs(x = "Cause of Death", y = "Death Count", title = "Causes of Death")

#A look at the killers
romans %>% 
  group_by(Killer) %>% 
  summarise(killer_count = n()) %>% 
  arrange(desc(killer_count)) %>% 
  ggplot(aes(reorder(Killer, killer_count), killer_count)) + 
  geom_bar(stat = "identity", fill = "green") + geom_text(aes(label = killer_count)) + 
  coord_flip() + labs(x = "Killer Type", y = "Count", title = "How were the emperors killed?")

#A look at the reign duration
romans$Reign.Start <- as.POSIXct(romans$Reign.Start)
romans$Reign.End <- as.POSIXct(romans$Reign.End)

romans$reign_duration <- as.numeric(difftime(romans$Reign.End, romans$Reign.Start, units = "weeks"))/52.25
romans$reign_duration <- round(romans$reign_duration, digits = 0)
romans$reign_duration <- abs(romans$reign_duration)

romans %>% 
  select(Name, reign_duration) %>% 
  arrange(desc(reign_duration)) %>% 
  top_n(25, wt = reign_duration) %>% 
  ggplot(aes(reorder(Name, reign_duration), reign_duration)) + 
  geom_bar(stat = "identity", fill = "green") + coord_flip() + 
  geom_text(aes(label = reign_duration)) + 
  labs(x = "Emperor name", y = "Reign Duration", title = "Duration of each Emperor's reign (top 25)")

#Average reign duration
mean(romans$reign_duration)

#Lifespan
romans$Birth <- as.POSIXct(romans$Birth, format = "%Y-%m-%d")
romans$Death <- as.POSIXct(romans$Death)

romans$lifespan <- as.numeric(difftime(romans$Death, romans$Birth, units = "weeks"))/52.25
romans$lifespan <- round(romans$lifespan, digits = 0)
romans$lifespan <- abs(romans$lifespan)

romans %>% 
  select(Name, lifespan) %>% 
  arrange(desc(lifespan)) %>% 
  top_n(25, wt = lifespan) %>% 
  ggplot(aes(reorder(Name, lifespan), lifespan)) + 
  geom_bar(stat = "identity", fill = "green") + 
  coord_flip() + geom_text(aes(label = lifespan)) + 
  labs(x = "Emperor name", y ="Lifespan", title = "Lifespan of Emperors")

#Average lifespan
mean(romans$lifespan, na.rm = TRUE)

#Lifespan v Reign Duration
ggplot(romans, aes(lifespan, reign_duration)) + geom_point()

cor(romans$lifespan, romans$reign_duration, use = "complete.obs")
