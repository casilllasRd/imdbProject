imdb_data = read_csv('data/imdb-data-raw.csv')

imdb = imdb_data %>%
  mutate(year=str_replace_all(year, '\\D*', ''), 
         runtime=str_replace_all(runtime, '\\D*', ''), 
         primary_genre=str_replace(genre, ',.*', ''), 
         secondary_genres=str_extract(genre, ',+.*'),
         secondary_genres=str_replace(secondary_genres, ',', '')) %>%
  
  mutate(year=as.integer(year), 
         runtime=as.integer(runtime)) %>%
  
  mutate(primary_genre=str_trim(primary_genre, side = 'both'), 
         secondary_genres=str_trim(secondary_genres, side='both')) %>%
  
  group_by(primary_genre) %>%
  mutate(genre_count = n()) %>%
  mutate(primary_genre = if_else(genre_count <= 50, 'Other', primary_genre)) %>%
  
  ungroup()%>%
  group_by(rating) %>%
  mutate(rating_count = n()) %>%
  mutate(rating = if_else(rating_count <= 50, 'Other', rating)) %>%
  
  select(title:runtime, stars:secondary_genres)



write_csv(imdb, 'data/clean-imdb.csv')
