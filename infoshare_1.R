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
nyc_buildings <- read_csv("nyc_buildings.csv")

nyc_deaths <- nyc_deaths[1:184,]
nyc_pop <- nyc_pop[1:184,]
nyc_buildings <- nyc_buildings[1:184,]

nyc_data <- nyc_pop %>%
  left_join(nyc_deaths, by = "Area Name") %>%
  left_join(nyc_buildings, by = "Area Name")


male_deaths_11209 <- read_csv("male_deaths_11209.csv")
female_deaths_11209 <- read_csv("female_deaths_11209.csv")

pop_by_sex <- read_csv("population_11209.csv")
pop_by_race <- read_csv("pop_race_11209.csv")
pop_by_hh_size <- read_csv("household_size_11209.csv")

# load zip code data I hope?
library(sp)
library(rgdal)
library(leaflet)
library(leaflet.extras)
library(htmltools)

zip_codes <- readOGR("zip_codes", "ZIP_CODE_040114")

zipz <- spTransform(zip_codes, CRS("+init=epsg:4326"))

zipz %>%
  leaflet() %>%
  #addProviderTiles("CartoDB") %>%
  addPolygons(weight = 1,
            label = ~paste(COUNTY, ": ", ZIPCODE),
            highlight = highlightOptions(weight = 3,
                                         color = "yellow",
                                         bringToFront = TRUE)) 

# add columns to get population total for each zip
nyc_data$total_pop <- 0
for(i in 1:nrow(nyc_data)) {
  nyc_data$total_pop[i] = sum(nyc_data[i, 63:85])
}
sum(nyc_data[9, 74:96])
# Left join nyc_data onto zipz@data and store in nyc_data_zip
library(rebus)
# patterns for borough detection
bk_pattern <- START %R% "112"
bx_pattern <- START %R% "104"
si_pattern <- START %R% "103"
m_pattern <- START %R% "10" %R% char_class("012")
q_pattern <- START %R% "11" %R% negated_char_class("2")

nyc_data <- nyc_data %>%
  mutate(borough = case_when(str_detect(`Area Name`, m_pattern) ~ "Manhattan",
                             str_detect(`Area Name`, bk_pattern) ~ "Brooklyn",
                             str_detect(`Area Name`, bx_pattern) ~ "Bronx",
                             str_detect(`Area Name`, si_pattern) ~ "Staten Island",
                             str_detect(`Area Name`, q_pattern) ~ "Queens",
                             TRUE ~ `Area Name`))


nyc_data2 <- nyc_data2 %>%
  mutate(borough = case_when(str_detect(`Area Name`, m_pattern) ~ "Manhattan",
                             str_detect(`Area Name`, bk_pattern) ~ "Brooklyn",
                             str_detect(`Area Name`, bx_pattern) ~ "Bronx",
                             str_detect(`Area Name`, si_pattern) ~ "Staten Island",
                             str_detect(`Area Name`, q_pattern) ~ "Queens",
                             TRUE ~ `Area Name`))



#save csv of nyc_data
write_csv(nyc_data, "nyc_data_sample.csv")


nyc_data_2 <- nyc_data %>%
  filter(!is.na(MapID.x) & !is.na(`White alone persons`)) 

nyc_data_2$total_pop <- 0
for(i in 1:nrow(nyc_data_2)) {
  nyc_data_2$total_pop[i] = sum(nyc_data_2[i, 63:85])
}

zipz@data$ZIPCODE <- as.character(zipz@data$ZIPCODE)

zipz@data <- zipz@data %>% 
  left_join(nyc_data_2, by = c("ZIPCODE" = "MapID.x"))

#map by total Asian population

# create color palette with colorNumeric()
nyc_pal_asian <- colorBin("Blues", domain = (zipz@data$`Asian alone persons`) / (zipz@data$total_pop))

zipz %>%
  leaflet() %>%
  addProviderTiles("CartoDB.PositronNoLabels") %>%
  # set boundary thickness to 1 and color polygons
  addPolygons(weight = 1, opacity = 1, fillOpacity = 0.7, color = "white",
              fillColor = ~nyc_pal_asian(`Asian alone persons` / total_pop ),
              # add labels that display mean income
              label = ~paste(`Area Name`, ": ", round((`Asian alone persons` / total_pop) * 100, 2), "%" ),
              # highlight polygons on hover
              highlight = highlightOptions(weight = 5, color = "orange",
                                           bringToFront = TRUE))  %>% 
  setView(lng = -74.0, lat = 40.7, zoom = 11) %>%
  addLegend("topleft", pal = nyc_pal_asian, values = ~((`Asian alone persons`) / (total_pop ) ),
            title = "Percentage Asian Alone",
            #labFormat = labelFormat(suffix = "%"),
            opacity = 1)

#map by total white population

# create color palette with colorNumeric()
nyc_pal_white <- colorQuantile("viridis", reverse = TRUE,
                          domain = (zipz@data$`White alone persons`) / (zipz@data$total_pop))
zipz %>%
  leaflet() %>%
  addProviderTiles("CartoDB.PositronNoLabels") %>%
  # set boundary thickness to 1 and color polygons
  addPolygons(weight = 1, opacity = 1, fillOpacity = 0.7, color = "white",
              fillColor = ~nyc_pal_white(`White alone persons` / total_pop ),
              # add labels that display mean income
              label = ~paste(`Area Name`, ": ", round((`White alone persons` / total_pop) * 100, 2), "%" ),
              # highlight polygons on hover
              highlight = highlightOptions(weight = 5, color = "orange",
                                           bringToFront = TRUE))  %>% 
  setView(lng = -73.98, lat = 40.7, zoom = 11) %>%
  addLegend("topleft", pal = nyc_pal_white, values = ~((`White alone persons` * 100) / (total_pop * 100)),
            title = "Percentage White Alone",
            opacity = 1)


## write a function to make a map by race
pct_map <- function(race) {
  map_pal <- colorQuantile("viridis", reverse = TRUE,
                           domain = (zipz@data$race))
}

zipz@data %>% clean_names()


#save csv of nyc_data_2
write_csv(nyc_data_2, "nyc_data_sample_2.csv")
