#install.packages('tidyverse')
#install.packages('visdat')
library(tidyverse)
library(visdat)


### Movie Datasets ###
links <- read_csv('links.csv')
movies <-read_csv('movies.csv')
ratings <- read_csv('ratings.csv')
tags <- read_csv('tags.csv')

# Glimpse
glimpse(links)
glimpse(movies)
glimpse(ratings)
glimpse(tags)

# Missing Values
vis_miss(links)
vis_miss(movies) # no missing data
vis_miss(ratings) # no missing data
vis_miss(tags) # no missing data

### Data Cleaning & Preprocessing ###
combined.df <- ratings %>%
                inner_join(tags, by = c('userId', 'movieId')) %>%
                inner_join(movies, by = c('movieId'))
