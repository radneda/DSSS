
### ----------------------------------------------------------------------------
### --- DataKolektiv DATA SCIENCE SUMMER SCHOOL 2022
### --- MACHINE LEARNING IN R
### --- Week 1. Introduction to R for ML
### --- author: Goran S. Milovanović, Phd
### --- DataKolektiv, Chief Scientist/Owner
### --- DataKolektiv, 2022.
### --- script: 05_tidyverse.R
### --- description: dplyr and tidyr, mainly
### --- license: GPL-3.0 license
### --- https://www.gnu.org/licenses/gpl-3.0.html

### ----------------------------------------------------------------------------

# install.packages("tidyverse")
library(tidyverse)

# - read listings.csv.gz for AirBnB
# - from: http://insideairbnb.com/get-the-data/
data_url <- 
  "http://data.insideairbnb.com/the-netherlands/north-holland/amsterdam/2022-03-08/data/listings.csv.gz"
con <- gzcon(url(data_url))
txt <- readLines(con)
data_set <- read.csv(textConnection(txt))

str(data_set)

glimpse(data_set)

# - select
data_subset <- dplyr::select(data_set,
                             id,
                             name,
                             host_response_time)
table(data_subset$host_response_time)

data_set$host_response_time[data_set$host_response_time=="N/A"] <- NA

data_subset <- select(data_set, 
                      id, 
                      name, 
                      host_response_time)
table(data_subset$host_response_time)

hrt_table <- as.data.frame(
  table(data_subset$host_response_time)
)

hrt_table <- as.data.frame(
  table(data_set$host_response_time,
        data_set$host_is_superhost)
)

data_subset <- dplyr::select(data_set,
                             id,
                             name,
                             host_response_time)
data_subset <- dplyr::filter(data_subset,
                             host_response_time == "within an hour")
data_subset$host_response_time <- NULL

# pipe operator: %>% 
data_subset <- select(data_set, 
                      id, 
                      name, 
                      host_response_time) %>%
  filter(host_response_time == "within an hour")

data_subset <- data_set %>% 
  select(id,
         name,
         host_response_time) %>%
  filter(host_response_time == "within an hour")

# group_by and summarise
data_subset <- data_set %>% 
  select(host_response_time) %>%
  group_by(host_response_time) %>% 
  summarise(count = n())

data_subset <- data_set %>% 
  select(host_response_time, host_is_superhost) %>%
  group_by(host_response_time, host_is_superhost) %>% 
  summarise(count = n())

data_subset <- data_set %>% 
  select(host_response_time, host_is_superhost) %>%
  filter(!is.na(host_response_time) & !is.na(host_is_superhost)) %>% 
  group_by(host_response_time, host_is_superhost) %>% 
  summarise(count = n()) %>% 
  arrange(host_is_superhost, host_response_time)

ggplot(data_subset, 
       aes(x = host_response_time, 
           y = count, 
           fill = host_is_superhost,
           color = host_is_superhost, 
           group = host_is_superhost)) +
  geom_point() + geom_path() + 
  ggtitle("AirBnB Hosts") + 
  theme_bw() + 
  theme(axis.text.x = element_text(angle = 90)) + 
  theme(panel.border = element_blank()) + 
  theme(plot.title = element_text(hjust = .5))


data_subset <- data_set %>% 
  select(host_response_time, host_is_superhost) %>%
  filter(!is.na(host_response_time) & !is.na(host_is_superhost)) %>% 
  group_by(host_response_time, host_is_superhost) %>% 
  summarise(count = n()) %>% 
  group_by(host_is_superhost) %>% 
  mutate(percent = count/sum(count)*100) %>% 
  arrange(host_is_superhost, host_response_time)

ggplot(data_subset, 
       aes(x = host_response_time, 
           y = percent, 
           fill = host_is_superhost,
           color = host_is_superhost, 
           group = host_is_superhost)) +
  geom_point() + geom_path() + 
  ggtitle("AirBnB Hosts") + 
  theme_bw() + 
  theme(axis.text.x = element_text(angle = 90)) + 
  theme(panel.border = element_blank()) + 
  theme(plot.title = element_text(hjust = .5))

