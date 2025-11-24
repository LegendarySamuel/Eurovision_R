#### Laden der Daten
eurovision <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2022/2022-05-17/eurovision.csv')
life_expectancy <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2023/2023-12-05/life_expectancy.csv')
life_expectancy_different_ages <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2023/2023-12-05/life_expectancy_different_ages.csv')
life_expectancy_female_male <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2023/2023-12-05/life_expectancy_female_male.csv')
votes <- read.csv("./votes.csv")
betting <- read.csv("./betting_offices.csv")
contestants <-read.csv("./contestants.csv")

install.packages("tidyverse")
library(tidyverse)

life_agg <- life_expectancy %>%
  # Umbenennen von Entity zu 'country', für Join operator
  rename(country = Entity) %>%
  # Filtern auf den Zeitraum des ESC
  filter(Year >= 1956 & Year <= 2022) %>%
  group_by(country) %>%
  summarise(avg_life_exp = mean(LifeExpectancy, na.rm = TRUE))

euro_agg <- eurovision %>%
  # Filtere nur Finale 
  filter(str_detect(section, "grand-final")) %>% 
  group_by(artist_country) %>% # Gruppieren nach dem land, das auftritt
  summarise(avg_rank = mean(rank, na.rm = TRUE)) %>%
  rename(country = artist_country) # damit es für join  passt

# datensätze joinen/verbinden
plot_data <- inner_join(euro_agg, life_agg, by = "country")


ggplot(plot_data, aes(x = avg_life_exp, y = avg_rank)) +
  geom_point(color = "steelblue", size = 3, alpha = 0.7) + # Punkte
  geom_smooth(method = "lm", color = "red", se = FALSE) +  # lineare regression
  geom_text(aes(label = country), vjust = 1.5, size = 3, check_overlap = TRUE) +
  labs(
    title = "Zusammenhang: Lebenserwartung und ESC-Platzierung",
    subtitle = "Durchschnittliche Platzierung vs. Durchschnittliche Lebenserwartung (1956-2022)",
    x = "Durchschnittliche Lebenserwartung (Jahre)",
    y = "Durchschnittliche Platzierung"
  ) +
  theme_minimal()

test_ergebnis <- cor.test(plot_data$avg_life_exp, plot_data$avg_rank)

print(test_ergebnis) #p wert ist 0.02 also statistisch relevant da kleiner als 0.05 es gibt also eine negative korrelation
