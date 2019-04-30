library(tidyverse)
library(rvest)
library(purrr)
library(tidyr)
library(ggthemes)
library(readr)
library(janitor)
library(forcats)

bklyn_deaths <- read_csv("brooklyn_deaths.csv")

original_columns <- colnames(bklyn_deaths)

new_columns <- str_remove_all(original_columns, " - 2011-13 Avg") %>%
  str_replace_all(" age at death ", "_") %>%
  #str_replace_all("-", "_") %>%
  str_replace_all("\\+", "plus") %>%
  str_remove_all("Number of ") %>%
  str_replace_all(" ", "_") %>%
  str_remove_all("_years")

colnames(bklyn_deaths) <- new_columns

bklyn_deaths_long <- bklyn_deaths %>%
  gather(sex_age, value, -Area_Name, -males, -females) %>%
  mutate(sex_age = str_replace(sex_age, "ale_", "ale ")) %>%
  separate(sex_age, c("sex", "age"), " ") %>%
  mutate(age = as.factor(age))

# order age factors
bklyn_deaths_long$age <- fct_relevel(bklyn_deaths_long$age, "under_28_days", "1-12_months",
            "1-4", "5-9", "10-14", "15-19", "20-24", "25-29",
            "30-34", "35-39", "40-44", "45-49", "50-54", "55-59",
            "60-64", "65-69", "70-74", "75-79", "80-84", "85plus")

bg_color <- "gray95"
a_theme <- theme_wsj() +
  theme(
    panel.background = element_rect(fill = bg_color),
    plot.background = element_rect(fill = bg_color),
    legend.position = "none",
    axis.line.x.bottom = element_blank(),
    axis.ticks.x.bottom = element_blank(),
    axis.text.x = element_text(face = "plain"),
    panel.grid.major.y = element_line( colour = "darkgray"),
    plot.subtitle = element_text(size = 15),
    plot.title = element_text(size = 21)
  )

b_theme <- theme_minimal() +
  theme(
    legend.position = "bottom"
  )

bklyn_deaths_long %>%
  filter(Area_Name == "New York City") %>%
  ggplot(aes(x = reorder(age, -age), y = value, fill = sex)) +
  geom_col(position = "dodge") +
  coord_flip() +
  #facet_wrap(~sex) +
  b_theme 

nyc_deaths <- read_csv("nyc_deaths.csv")
nyc_pop <- read_csv("nyc_pop.csv")

nyc_deaths <- nyc_deaths[1:184,]
nyc_pop <- nyc_pop[1:184,]

nyc_data <- nyc_pop %>%
  left_join(nyc_deaths, by = "Area Name")


