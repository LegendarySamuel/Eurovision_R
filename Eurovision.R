#### Libraries
library(dplyr)
library(ggplot2)
library(tidyr)
library(pheatmap)

##### Laden der Daten ----
eurovision <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2022/2022-05-17/eurovision.csv')
eurovision_v2 <- read.csv("./contestants.csv")
eurovision_votes <- read.csv("./votes.csv")
life_expectancy <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2023/2023-12-05/life_expectancy.csv')
life_expectancy_different_ages <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2023/2023-12-05/life_expectancy_different_ages.csv')
life_expectancy_female_male <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2023/2023-12-05/life_expectancy_female_male.csv')

#### Data Wrangling
eurovision_finals <- eurovision %>%
  filter(section == "final" | section == "grand-final")

##### Data Overview ----
## Austragungsorte
eurovision %>%
  distinct(event, host_city) %>%
  count(host_city, sort = TRUE)
eurovision %>%
  distinct(event, host_country) %>%
  count(host_country, sort = TRUE)

## Gewinner
eurovision_finals %>%
  filter(rank == 1) %>%
  select(year, host_city, artist_country, artist, song, rank) %>%
  print(n=69)

eurovision_finals %>%
  filter(rank == 1) %>%
  count(artist_country, sort = TRUE) %>%
  print(n=69)

eurovision_finals %>%
  filter(artist_country == 'Germany') %>%
  distinct(year, artist_country, rank) %>%
  ggplot(aes(x = year, y = rank)) +
  geom_line(color = "red", size = 1) +
  geom_point(color = "red", size = 3, alpha = 0.7) +
  scale_y_reverse(breaks = 1:26) +
  labs(title = "Platzierungen Deutschlands im Eurovision-Finale",
       x = "Jahr", y = "Platzierung") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

eurovision_finals %>%
  filter(artist_country %in% c('Germany', 'Sweden')) %>%
  distinct(year, artist_country, rank) %>%
  ggplot(aes(x = year, y = rank, color = artist_country)) +
  geom_line(size = 1) +
  geom_point(size = 2, alpha = 0.7) +
  scale_y_reverse(breaks = 1:26) +
  labs(title = "Platzierungen ausgewählter Länder im Eurovision-Finale",
       x = "Jahr", y = "Platzierung",
       color = "Land") +
  theme_minimal()

## Punktevergabe
agg_votes <- eurovision_votes %>%
  group_by(from_country, to_country) %>%
  summarise(total_points = sum(total_points, na.rm = TRUE), .groups = "drop")

# Gesamtpunkte pro Geberland berechnen
total_by_giver <- agg_votes %>%
  group_by(from_country) %>%
  summarise(total_given = sum(total_points), .groups = "drop")

# Join und Normalisierung
normalized_votes <- agg_votes %>%
  left_join(total_by_giver, by = "from_country") %>%
  mutate(relative_points = total_points / total_given)

# Gesamtpunkte pro Empfängerland berechnen
#total_by_receiver <- agg_votes %>%
#  group_by(to_country) %>%
#  summarise(total_received = sum(total_points), .groups = "drop")

# Join und Normalisierung
#normalized_votes <- agg_votes %>%
#  left_join(total_by_receiver, by = "to_country") %>%
#  mutate(relative_points = total_points / total_received)

# Eindeutige Kombinationen von Ländercode und -name extrahieren
country_map <- eurovision_v2 %>%
  select(to_country_id, to_country) %>%
  distinct(to_country_id, .keep_all = TRUE)

# Alle Ländercodes aus dem Votes-Datensatz
all_codes <- unique(c(normalized_votes$from_country, normalized_votes$to_country))

# Alle Codes aus der Mapping-Tabelle
mapped_codes <- unique(country_map$to_country_id)

# Fehlende Codes finden
missing_codes <- setdiff(all_codes, mapped_codes)
print(missing_codes)

# Fehlende Länder ergänzen
manual_map <- tibble(
  to_country_id = c("ad", "wld"),
  to_country = c("Andorra", "Rest of the world")
)

country_map <- bind_rows(country_map, manual_map)
# Mapping-Tabelle mit eindeutigen Namen
country_map_from <- country_map %>%
  rename(from_country_name = to_country)

country_map_to <- country_map %>%
  rename(to_country_name = to_country)

# Join für from_country
normalized_votes_named <- normalized_votes %>%
  left_join(country_map_from, by = c("from_country" = "to_country_id"))

# Join für to_country
normalized_votes_named <- normalized_votes_named %>%
  left_join(country_map_to, by = c("to_country" = "to_country_id"))

normalized_votes_named <- normalized_votes_named %>%
  mutate(relative_points = as.numeric(relative_points))

# Matrix erstellen
vote_matrix_norm <- normalized_votes_named %>%
  select(from_country_name, to_country_name, relative_points) %>%
  pivot_wider(
    names_from = to_country_name,
    values_from = relative_points,
    values_fill = list(relative_points = 0)
  )
str(normalized_votes_named)
# Zeilen als rownames setzen
vote_matrix_df <- as.data.frame(vote_matrix_norm)
rownames(vote_matrix_df) <- vote_matrix_df$from_country_name
vote_matrix_df$from_country_name <- NULL

# Sortieren
vote_matrix_df <- vote_matrix_df[order(rownames(vote_matrix_df)), ]
vote_matrix_df <- vote_matrix_df[, order(colnames(vote_matrix_df))]

# Heatmap anzeigen
pheatmap(as.matrix(vote_matrix_df),
         cluster_rows = FALSE,
         cluster_cols = FALSE,
         main = "Heatmap: Punkte-Geber (Zeilen) vs Punkte-Empfänger (Spalten)\nAnteil aller von 1957 bis 2023 vergebenen Punkte pro Land beim ESC",
         color = colorRampPalette(c("white", "blue"))(100))

# Heatmap geclustert (Geber und Empfänger)
pheatmap(as.matrix(vote_matrix_df),
         cluster_rows = TRUE,
         cluster_cols = TRUE,
         main = "Heatmap: Punkte-Geber (Zeilen) vs Punkte-Empfänger (Spalten)\nAnteil aller von 1957 bis 2023 vergebenen Punkte pro Land beim ESC",
         color = colorRampPalette(c("white", "blue"))(100))

# Heatmap geclustert (Geber)
pheatmap(as.matrix(vote_matrix_df),
         cluster_rows = TRUE,
         cluster_cols = FALSE,
         main = "Heatmap: Punkte-Geber (Zeilen) vs Punkte-Empfänger (Spalten)\nAnteil aller von 1957 bis 2023 vergebenen Punkte pro Land beim ESC",
         color = colorRampPalette(c("white", "blue"))(100))

# Heatmap geclustert (Empfänger)
pheatmap(as.matrix(vote_matrix_df),
         cluster_rows = FALSE,
         cluster_cols = TRUE,
         main = "Heatmap: Punkte-Geber (Zeilen) vs Punkte-Empfänger (Spalten)\nAnteil aller von 1957 bis 2023 vergebenen Punkte pro Land beim ESC",
         color = colorRampPalette(c("white", "blue"))(100))

# Top 3 pro Land
top3_table <- normalized_votes_named %>%
  group_by(from_country_name) %>%
  slice_max(order_by = relative_points, n = 3, with_ties = FALSE) %>%
  arrange(from_country_name, desc(relative_points)) %>%
  select(from_country_name, to_country_name, relative_points)

top3_list <- top3_table %>%
  group_by(from_country_name) %>%
  summarise(top_3 = paste(to_country_name, collapse = ", "))
