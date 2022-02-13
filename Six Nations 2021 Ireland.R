library(package = "scales")
library(package = "ggthemes")
library(package = "tidyjson")
library(package = "tidyverse")

library(forcats)
#source("v_theme.R")

filename = 'Irelandpoints.json'
Irelandpoints <- paste(readLines(filename), collapse="")

films <- Irelandpoints %>% as.tbl_json %>% gather_array %>%
  spread_values(
    title = jstring("title"),
    director = jstring('director'),
    year = jstring('year'),
    avg_rating = jnumber('avg_rating'),
    watches = jnumber("watches"),
    likes = jnumber("likes"),
    time = jnumber("time")
  )
films %>% head(n = 5) %>% select(title, director, year)

cast <- Irelandpoints  %>% as.tbl_json  %>% gather_array %>%
  spread_values(
    title = jstring("title"),
    director = jstring('director'),
    year = jstring('year'),
    avg_rating = jnumber('avg_rating'),
    watches = jnumber("watches"),
    likes = jnumber("likes"),
    time = jnumber("time")
  ) %>% enter_object("cast") %>% gather_array() %>%
  spread_values(
    name = jstring("name")
  )
cast %>% head(n = 8) %>% select(title, year, name)

#characters <- FilmDetails  %>% as.tbl_json  %>% gather_array %>%
#  spread_values(
#    title = jstring("title"),
#    director = jstring('director'),
#   year = jstring('year'),
#   avg_rating = jnumber('avg_rating'),
#   watches = jnumber("watches"),
#  likes = jnumber("likes")
#  ) %>% enter_object("characters") %>% gather_array() %>%
#  spread_values(
#    name = jstring("name")
#  )

nrow(films)

films %>% ggplot(aes(x = year)) +
  geom_bar() +
  labs(title = 'Ireland points by time') + 
  theme_fivethirtyeight()
#ggsave("imgs/films_by_year.png", width = 8, height = 5)

by_director <- films %>% group_by(director) %>% summarise(n = n()) %>% arrange(-n)
by_director %>% filter(n > 0) %>%
  ggplot(aes(x = fct_reorder(director, n), y = n)) + 
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(title = 'Ireland points scorers (times scored not points scored)') +
  theme_fivethirtyeight()
#ggsave("imgs/director_count.png", width = 8, height = 6)

# pull out just the top 5 directors
top_directors <- by_director %>% head(n = 6)
# filter films to those directed by these titans of Kung fu
films_top_director <- films %>% filter(director %in% top_directors$director)
#plot bar chart
films_top_director %>%
  ggplot(aes(x = year)) +
  geom_bar(aes(fill = director)) +
  labs(title = 'Point Scorers by Minute') + 
  theme_fivethirtyeight()

# Try Fill Position
films_top_director %>%
  ggplot(aes(x = year)) +
  geom_bar(aes(fill = director), position = "fill") +
  labs(title = 'Top Director Count by Year') + 
  theme_fivethirtyeight()

films_top_director_all <- films %>% mutate(director_label = ifelse(director %in% top_directors$director, director, 'Other'))
films_top_director_all %>%
  ggplot(aes(x = year)) +
  geom_bar(aes(fill = director_label)) +
  labs(title = 'Ireland Scorers by Minute', fill = '') + 
  theme_fivethirtyeight()
#ggsave("imgs/top_directors_by_year.png", width = 8, height = 5)

films_top_director_all %>%
  ggplot(aes(x = year)) +
  geom_bar(aes(fill = director_label), position = 'fill') +
  labs(title = 'Ireland Scores by Minute %', fill = '') + 
  theme_fivethirtyeight() +
  scale_y_continuous(labels = percent)
#ggsave("imgs/top_directors_by_year_fill.png", width = 8, height = 5)

summary(films)

by_actor <- cast %>% group_by(name) %>% summarise(n = n()) %>% arrange(-n)
by_actor %>% filter(n > 1) %>%
  ggplot(aes(x = fct_reorder(name, n), y = n)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(title = 'Ireland players on the field at more than 1 scoring moment') +
  theme_fivethirtyeight()

top_actor <- by_actor %>% head(n = 16)
films_top_actor <- cast %>% filter(name %in% top_actor$name)
films_top_actor %>% 
  ggplot(aes(x = year)) +
  geom_bar() +
  labs(title = 'Top Actors Film Count by Year') + 
  facet_wrap( ~ fct_relevel(name, top_actor$name)) +
  # only label half of the years to make things a bit look cleaner
  scale_x_discrete(labels = function(x) { return(ifelse(as.numeric(x) %% 2, x, '')) }) +
  theme_fivethirtyeight() + 
  # angle label text
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position="none")

year_count <- films %>% select(year) %>% unique() %>% nrow()
actor_active_years <- films_top_actor %>% group_by(name) %>% 
  summarise(active_years = length(unique(year)), 
            percent_active = active_years / year_count, 
            start = min(as.numeric(year)), 
            end = max(as.numeric(year))) %>% 
  arrange(percent_active)
actor_active_years %>% 
  ggplot(aes(x = fct_inorder(name), y = percent_active)) + 
  geom_bar(stat = "identity") +
  coord_flip() + 
  scale_y_continuous(labels = percent) + 
  labs(title = "Percentage of points scoring present for") +
  theme_fivethirtyeight()
#ggsave("imgs/actor_percent_active.png", width = 8, height = 5)

actor_active_years %>% gather(caps, cap_year, start, end) %>%
  ggplot(aes(x = cap_year, y = fct_inorder(name))) +
  geom_point(size = 3) + 
  geom_path(aes(group = name)) +
  labs(title = "Ireland - When Players Were On The Pitch For Points") +
  theme_fivethirtyeight()
#ggsave("imgs/actor_career_span.png", width = 8, height = 5)   

summary(by_actor$n)

library(package = "reshape2")

# filter actors not in many movies
min_movie_actors <- by_actor %>% filter(n > 0)
popular_cast <- cast %>% filter(name %in% min_movie_actors$name) 
cast_movie_matrix <- popular_cast %>%
  acast(name ~ title,  fun.aggregate = length)

dim(cast_movie_matrix)

cast_movie_df_filtered <- cast_movie_matrix %>% colSums(.)

norm <- cast_movie_matrix / rowSums(cast_movie_matrix)
hc_norm_cast <- hclust(dist(norm, method = "manhattan"))

library(ggdendro)
ggdendrogram(hc_norm_cast, rotate = TRUE)

ordering <-hc_norm_cast$labels[hc_norm_cast$order]
ordering

# http://stackoverflow.com/questions/13281303/creating-co-occurrence-matrix
cooccur <- cast_movie_matrix %*% t(cast_movie_matrix)
diag(cooccur) <- 0
heatmap(cooccur, symm = TRUE )

summary(rowSums(cooccur))

summary(colSums(cooccur))

summary(colSums(cooccur != 0))

collab_counts <- as.data.frame(colSums(cooccur != 0))

library(igraph)

cooccur <- cast_movie_matrix %*% t(cast_movie_matrix)
#cooccur <- ifelse(cooccur < 4, 0, cooccur)
g <- graph.adjacency(cooccur, weighted = TRUE, mode = "undirected", diag = FALSE)
summary(E(g)$weight)

summary(degree(g))

summary(strength(g))

library(igraph)
cooccur <- cast_movie_matrix %*% t(cast_movie_matrix)
#cooccur <- ifelse(cooccur < 4, 0, cooccur)
g <- graph.adjacency(cooccur, weighted = TRUE, mode = "undirected", diag = FALSE)
low_degree_v <- V(g)[degree(g) < 10] #identify those vertices part of less than three edges
g <- delete_vertices(g, low_degree_v) #exclude them from the graph
low_weight_e <- E(g)[E(g)$weight < 3]
g <- delete_edges(g, low_weight_e)
low_strength_v <- V(g)[strength(g) < 90]
g <- delete_vertices(g, low_strength_v) #exclude them from the graph
V(g)$betweenness <- strength(g)
plot(g, edge.width = E(g)$weight, 
     #layout=layout.fruchterman.reingold,
     layout=layout_with_fr,
     vertex.label.dist=0.5,
     #vertex.size = V(g)$betweenness,
     vertex.size = 3,
     vertex.color='steelblue',
     vertex.frame.color='white',        #the color of the border of the dots 
     vertex.label.color='black',        #the color of the name labels
     vertex.label.font=2,           #the font of the name labels
     vertex.label.cex=1,            #specifies the size of the font of the labels. can also be made to vary
     edge.color = hsv(0,0.2,0.5,alpha=0.2)
)
