library(rvest)
library(tidyverse)

imdb_data = tibble()

for (i in seq(from = 1, to = 2500, by = 50)){
  url = paste0("https://www.imdb.com/search/title/?title_type=feature&release_date=1980-01-01,2020-12-31&user_rating=1.0,10.0&num_votes=0,&certificates=US%3AG,US%3APG,US%3APG-13,US%3AR,US%3ANC-17&moviemeter=0,&runtime=0,&adult=include&start=",i,"&ref_=adv_nxt")
  
  title = read_html(url) %>%
    html_nodes('.lister-item-header a') %>%
    html_text()
  
  year = read_html(url) %>%
    html_nodes('.text-muted.unbold') %>%
    html_text()
  
  rating = read_html(url) %>%
    html_nodes('.certificate') %>%
    html_text()
  
  runtime = read_html(url) %>%
    html_nodes('.runtime') %>%
    html_text()
  
  genre = read_html(url) %>%
    html_nodes('.genre') %>%
    html_text()
  
  stars = read_html(url) %>%
    html_nodes('.ratings-imdb-rating strong') %>%
    html_text()
  
  director = read_html(url) %>%
    html_nodes('.text-muted+ p a:nth-child(1)') %>%
    html_text()
  
  description = read_html(url) %>%
    html_nodes('.ratings-bar+ .text-muted') %>%
    html_text()
  
  imdb_data = bind_rows(imdb_data, tibble(title=title, year=year, rating=rating, runtime=runtime,
                                          genre=genre, stars=stars, director=director, description=description))
}

write_csv(imdb_data, 'data/imdb-data-raw.csv')




