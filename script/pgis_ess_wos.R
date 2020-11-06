library(here)
library(tidyverse)
library(gt)

here()
pgis <- read_csv(here("pgis_ess_wos_2020-11-03.csv"))
pgis <- pgis %>%
  mutate(publication_type = as_factor(publication_type),
         publication_name = as_factor(publication_name),
         published_year = as_factor(published_year),
         total_times_cited_count = as.integer(total_times_cited_count),
         digital_object_identifier = as.character(digital_object_identifier))

pgis %>% 
  filter(publication_type=="J") %>%
  mutate(publication_name = str_to_title(publication_name, locale = "en")) %>%
  count(publication_name, sort = TRUE) %>%
  mutate(publication_name = fct_reorder(publication_name, n)) %>%
  ggplot(aes(publication_name, n)) +
  geom_col() +
  coord_flip() +
  labs(title = "PGIS + ESS Most Common Journals",
       y = "publication",
       x = "count")

pgis %>% 
  filter(publication_type=="J") %>%
  count(published_year, sort = TRUE) %>%
  ggplot(aes(published_year, n)) +
  geom_col() +
  labs(title = "PGIS + ESS Publications by Year",
       y = "count",
       x = "year")

pgis %>%
  filter(publication_type=="J") %>%
  mutate(publication_name = str_to_title(publication_name, locale = "en")) %>%
  count(title, publication_name, author, published_year, 
        digital_object_identifier,
        wt = total_times_cited_count, 
        name = "cited", sort = TRUE) %>%
  rename(Title = title,
         Publication = publication_name,
         Authors = author,
         Year = published_year,
         DOI = digital_object_identifier) %>%
  gt() %>%
  tab_header(title = "PGIS + ESS Most Cited Publications")


