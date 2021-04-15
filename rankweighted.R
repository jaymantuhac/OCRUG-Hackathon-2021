movies<- read.csv(file = 'movies.csv')
ratings<- read.csv(file = 'ratings.csv')
tags<- read.csv(file = 'tags.csv')

year=sub("\\).*", "", sub(".*\\(", "", movies$title)) 
movies$year=year

by_movie <- ratings %>% group_by(movieId) 

MovieAvergaeRatings=by_movie%>% summarise(
  MeanuserRatings = mean(rating)
)

Count <- aggregate(userId~ movieId, data=ratings,FUN=length)

moviecount=merge(Count,movies, by='movieId')
moviec=merge(moviecount,MovieAvergaeRatings, by='movieId')

colnames(moviec)[2] <-"NumberCount"

# Get variable m at 95th
m <- quantile(moviec$NumberCount, .95)

# Get variable C
c <- mean(moviec$MeanuserRatings)


weighted_rating1 <- moviec %>%
  group_by(movieId) %>% filter(year >= 2010) %>% 
  mutate(weighted_rating = ((NumberCount/ (NumberCount + m)) *MeanuserRatings ) +
           ((m / (NumberCount + m)) * c)
  )

w1=arrange(weighted_rating1 ,desc(weighted_rating1$weighted_rating))

hist(weighted_rating1$weighted_rating)
mean(weighted_rating1$weighted_rating)
median(weighted_rating1$weighted_rating)

quantile(w1$weighted_rating, c(.75)) # returns 3.29, use 3.30 as cutoff point for a "popular" movie

weighted_rating1$popular <- ifelse(weighted_rating1$weighted_rating >= 3.30, 1, 0)

# Subset Popular movies
popular_movies <- weighted_rating1 %>% filter(popular == 1)

# Adds binary genre categories to original movies dataset
library(splitstackshape)
genres_ref <- cSplit_e(movies, "genres", "|", type = "character", fill = 0)

#Join genres_ref with popular_movies
popular_movies <- merge(popular_movies, genres_ref, by = 'movieId')
popular_movies_edited <- subset(popular_movies, select = -c(title.y, genres.y, year.y))
# popular_movies_edited <- popular_movies_edited %>% rename(`genres_Film-Noir` = genres_Film_Noir)

# Linear Regression Modeling
popular_movies_full_mod <- lm(weighted_rating ~ genres_Action
           + genres_Adventure
           + genres_Animation
           + genres_Children
           + genres_Comedy
           + genres_Crime
           + genres_Documentary
           + genres_Drama
           + genres_Fantasy
           + `genres_Film-Noir`
           + genres_Horror
           + genres_IMAX
           + genres_Musical
           + genres_Mystery
           + genres_Romance
           + `genres_Sci-Fi`
           + genres_Thriller
           + genres_War
           + genres_Western, data = popular_movies_edited)
summary(popular_movies_full_mod)

# Foward/Backward/Stepwise Selection
step(lm(weighted_rating ~ 1, data = popular_movies_edited), scope = list(upper=popular_movies_full_mod), 
    direction = "forward")

step(lm(weighted_rating ~ 1, data = popular_movies_edited), scope = list(upper=popular_movies_full_mod), 
     direction = "backward")

step(lm(weighted_rating ~ 1, data = popular_movies_edited), scope = list(upper=popular_movies_full_mod), 
     direction = "both")


# Analysis on all 2010 movies - from the past decade
all_2010_movies <- merge(weighted_rating1, genres_ref, by = 'movieId')
all_2010_movies <- subset(all_2010_movies, select = -c(title.y, genres.y, year.y))

all_2010_movies_full_mod <- lm(weighted_rating ~ genres_Action
                              + genres_Adventure
                              + genres_Animation
                              + genres_Children
                              + genres_Comedy
                              + genres_Crime
                              + genres_Documentary
                              + genres_Drama
                              + genres_Fantasy
                              + `genres_Film-Noir`
                              + genres_Horror
                              + genres_IMAX
                              + genres_Musical
                              + genres_Mystery
                              + genres_Romance
                              + `genres_Sci-Fi`
                              + genres_Thriller
                              + genres_War
                              + genres_Western, data = popular_movies_edited)
summary(all_2010_movies_full_mod)

step(lm(weighted_rating ~ 1, data = all_2010_movies), scope = list(upper=all_2010_movies_full_mod), 
     direction = "forward")

step(lm(weighted_rating ~ 1, data = all_2010_movies), scope = list(upper=all_2010_movies_full_mod), 
     direction = "backward")

step(lm(weighted_rating ~ 1, data = all_2010_movies), scope = list(upper=all_2010_movies_full_mod), 
     direction = "both")
