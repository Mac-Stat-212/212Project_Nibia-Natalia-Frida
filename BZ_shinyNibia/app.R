library(shiny)
library(tidyverse)
library(sf)
library(plotly)
library(geobr)
library(zoo)
library(scales)
library(bslib)

tree_cover_loss <- read.csv("~/Desktop/2024-2025/Spring 2025/STAT_212/Project_name/stat212-final-project/data/raw/Brazil_TreeCoverLoss.csv")

brazil_outline <- geobr::read_country(year = 2010)
states <- read_state(year = 2010, showProgress = FALSE)
muni_sf <- read_municipality(year = 2010, showProgress = FALSE)
state_nameBR <- unique(states$name_state)

tree_cover_loss_brazil <- tree_cover_loss %>%
  filter(country == "Brazil", threshold == 30) %>%
  select(-country, -threshold) %>%
  mutate(
    loss_2000_2020_ha = rowSums(across(starts_with("tc_loss_ha_20"), .names = "x")),
    extent_2020_ha = extent_2000_ha + (gain_2000.2020_ha - loss_2000_2020_ha),
    name_muni = subnational2
  )

mapping_BR <- tree_cover_loss_brazil %>%
  left_join(muni_sf, by = "name_muni") %>%
  rename(name_state = subnational1) %>%
  mutate(
    relative_extent_per_size2000 = extent_2000_ha / area_ha * 100,
    relative_extent_per_size2010 = extent_2010_ha / area_ha * 100,
    relative_extent_per_size2020 = extent_2020_ha / area_ha * 100,
    relative_change_2000_2010 = relative_extent_per_size2010 - relative_extent_per_size2000,
    relative_change_2010_2020 = relative_extent_per_size2020 - relative_extent_per_size2010,
    relative_change_2000_2020 = relative_extent_per_size2020 - relative_extent_per_size2000
  )

classify_change <- function(change, cover2000) {
  case_when(
    is.na(change) ~ "No data available",
    cover2000 < 20 ~ "Less than 20% tree cover in 2000",
    change >= 11 ~ "Large proportional gain",
    change >= 1 ~ "Slight proportional gain",
    abs(change) < 1 ~ "No proportional gain or loss",
    change <= -1 & change > -11 ~ "Slight proportional loss",
    change <= -11 & change > -20 ~ "Large proportional loss",
    change <= -20 ~ "Extremely large proportional loss",
    TRUE ~ "Other"
  )
}

mapping_BR <- mapping_BR %>%
  mutate(actualChange = classify_change(relative_change_2000_2020, relative_extent_per_size2000)) %>%
  mutate(actualChange = factor(actualChange, levels = c(
    "Large proportional gain", "Slight proportional gain", 
    "No proportional gain or loss", "Slight proportional loss",
    "Large proportional loss", "Extremely large proportional loss",
    "Less than 20% tree cover in 2000", "No data available", "Other"
  )))

custom_colors <- c(
  "Large proportional gain" = "#138030", 
  "Slight proportional gain" = "#19a93f", 
  "No proportional gain or loss" = "#ddb588", 
  "Slight proportional loss" = "#d37d4f", 
  "Large proportional loss" = "#985125", 
  "Extremely large proportional loss" = "#623518",
  "Less than 20% tree cover in 2000" = "#9e9e9e",  
  "No data available"= "#000000"
)

mapping_BR_leanings <- mapping_BR %>%
  pivot_longer(cols = starts_with("tc_loss_ha_"), names_to = "year_tc", values_to = "loss_ha") %>%
  mutate(year_tc = parse_number(year_tc), loss_k = loss_ha / 1000)

presidents <- tibble(
  name = c("Fernando Henrique Cardoso", "Luiz Inácio Lula da Silva", "Dilma Rousseff", 
           "Michel Temer", "Jair Bolsonaro", "Luiz Inácio Lula da Silva"),
  start = c(2000, 2003, 2011, 2016, 2019, 2023),
  end = c(2002, 2010, 2015, 2018, 2022, 2023),
  leaning = c("Center/Right", "Left", "Left", "Center/Right", "Right", "Left")
)

pres_years <- presidents %>%
  rowwise() %>%
  mutate(year_tc = list(start:end)) %>%
  unnest(year_tc) %>%
  select(name, year_tc, leaning)

annual_loss <- mapping_BR_leanings %>%
  left_join(pres_years, by = "year_tc") %>%
  group_by(year_tc, name, leaning) %>%
  summarise(total_loss_k = sum(loss_k, na.rm = TRUE), .groups = "drop")

ribbons <- pres_years %>%
  group_by(name, leaning) %>%
  summarise(xmin = min(year_tc) - 0.5, xmax = max(year_tc) + 0.5, .groups = "drop")

data_long <- mapping_BR %>%
  select(name_state, name_muni, area_ha, starts_with("tc_loss_ha_"), geom) %>%
  pivot_longer(cols = starts_with("tc_loss_ha_"), names_to = "year_tc", values_to = "loss_ha") %>%
  mutate(year_tc = as.numeric(gsub("tc_loss_ha_", "", year_tc)),
         loss_pct = (loss_ha / area_ha) * 100)

state_loss <- data_long %>%
  group_by(year_tc, name_state) %>%
  summarise(total_loss = sum(loss_ha, na.rm = TRUE), .groups = "drop") %>%
  arrange(year_tc) %>%
  mutate(roll_avg_3yr = rollmean(total_loss, k = 3, fill = NA, align = "right"))


mapping_BR <- mapping_BR %>% st_as_sf()

ui <- page_navbar(
  title = "Presidential Political Leaning and Tree Cover Loss in Brazil 2000-",
  inverse = TRUE,
  
  nav_panel("Tree Cover Loss in Brazil",
            layout_sidebar(
              sidebar = sidebar(
                sliderInput("year", "Select year:", min = 2000, max = 2020, value = 2000, step = 10, sep = "")
              ),
              layout_column_wrap(width = 1,
                                 plotOutput("tree_cover_map"), plotOutput("map_State_tcl_by_municipality"))
            )
  ),
  
  nav_panel("Compare over time",
            layout_sidebar(
              sidebar = sidebar(
                selectInput("state", "Select state:", state_nameBR)
              ),
              layout_column_wrap(width = 1,
                                 plotOutput("line_STATE_tree_cover_loss"))
            )
  ),
  
  nav_panel("About",
            layout_sidebar(
              sidebar = sidebar(
                sliderInput("year", "Select year:", min = 2000, max = 2020, value = 2000, step = 10)
              ),
              layout_column_wrap(width = 1,
                                 plotOutput("line_COUNTRY_tree_cover_by_political_leaning_plot"))
            )
  )
)

server <- function(input, output) {
  output$line_COUNTRY_tree_cover_by_political_leaning_plot <- renderPlot({
    ggplot() +
      geom_rect(data = ribbons, aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = +Inf, fill = leaning), alpha = 0.2) +
      geom_line(data = annual_loss, aes(x = year_tc, y = total_loss_k), size = 0.8, color = "black") +
      geom_point(data = annual_loss, aes(x = year_tc, y = total_loss_k), color = "black", size = 2) +
      scale_fill_brewer(palette = "Set1", name = "Political Leaning") +
      labs(title = "Annual tree cover loss, with political leaning", x = "Year", y = "Loss (×1000 ha)") +
      theme_minimal(base_size = 14) +
      theme(legend.position = "bottom")
  })
  
  output$map_State_tcl_by_municipality <- renderPlot({
    ggplot() +
      geom_sf(data = brazil_outline, fill = 'black', color = "black", lwd = 0.3) +
      geom_sf(data = mapping_BR, aes(fill = actualChange, geometry = geom), lwd = 0.02,na.rm=FALSE) + 
      scale_fill_manual(values = custom_colors, name = "Gain or loss (%)",na.value = 'lightgrey') + 
      labs(
        title = "Brazil Tree Cover Extent Change from 2000–2020 by Municipality",
        subtitle = "Change only for municipalities with >20% tree cover in 2000.\nEmpty areas represent municipalities missing polygon data.",
        caption = 'Source: ; Missing values in light yellow.') +
      theme_void()
  })
  
  output$tree_cover_map <- renderPlot({
    col_name <- str_c("relative_extent_per_size", input$year)
    
    ggplot() +
      geom_sf(data = brazil_outline, fill = 'black', color = "black", lwd = 0.3) +
      geom_sf(data = mapping_BR, aes(fill = !!sym(col_name)), color = NA) +
      scale_fill_gradient(low = "white", high = "#138030") +
      labs(
        title = paste("Relative Tree Cover in", input$year),
        subtitle = "Tree cover relative to municipality area"
      ) +
      theme_void()
  })
  
  output$line_STATE_tree_cover_loss <- renderPlot({
    state_loss %>%
      filter(name_state == input$state) %>%
      ggplot(aes(x = year_tc, y = total_loss)) +
      geom_line(color = "gray70") +
      geom_line(aes(y = roll_avg_3yr), color = "darkgreen", size = 1) +
      geom_smooth(color = "red", method = "lm", se = FALSE, linetype = "dashed", linewidth = 0.5) +
      scale_y_continuous(labels = label_number(scale = 1/1000, suffix = "k", accuracy = 1)) +
      labs(title = "Annual Tree Cover Loss in Select Brazilian States",
           subtitle = "With 3-year rolling average and linear trend line (2001–2023)",
           y = "Tree Cover Loss (thousands of ha)",
           x = "Year") +
      theme_minimal()
  })
}

shinyApp(ui = ui, server = server)

