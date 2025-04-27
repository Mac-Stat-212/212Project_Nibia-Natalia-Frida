#
# Shiny app for Brazil Tree Cover Loss 2000-2023
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(tidyverse)
library(sf)
library(plotly)
library(geobr)

# loading data

tree_cover_loss <- read.csv("../../data/raw/Brazil_TreeCoverLoss.csv")

## Mapping data
### State data
states <- read_state(
  year = 2010, 
  showProgress = FALSE
)

no_axis <- theme(axis.title=element_blank(),
                 axis.text=element_blank(),
                 axis.ticks=element_blank())

### Municipality geometries
muni_sf <- geobr::read_municipality(year = 2010,
                                    showProgress = FALSE)


state_nameBR <- states %>% pull(name_state) %>% unique()


# Making loss over time

tree_cover_loss_brazil <- tree_cover_loss_brazil %>% 
  mutate(
    loss_2000.2020_ha = tc_loss_ha_2001+tc_loss_ha_2002+tc_loss_ha_2003+tc_loss_ha_2004+tc_loss_ha_2005+tc_loss_ha_2006+tc_loss_ha_2007+tc_loss_ha_2008+tc_loss_ha_2009+tc_loss_ha_2010+tc_loss_ha_2011+tc_loss_ha_2012+tc_loss_ha_2013+tc_loss_ha_2014+tc_loss_ha_2015+tc_loss_ha_2016+tc_loss_ha_2017+tc_loss_ha_2018+tc_loss_ha_2019+tc_loss_ha_2020
  ) %>% 
  mutate(
    extent_2020_ha = extent_2000_ha + (gain_2000.2020_ha- loss_2000.2020_ha) ## If the forest was 10 in 2000, and then it gained 3 and then lost 5. It would be 10 + (3-5) = 10-2 = 8
  )

head(tree_cover_loss_brazil)



## Combining Datasets

tree_cover_loss_brazil <- tree_cover_loss_brazil %>% 
  mutate(name_muni = subnational2)

mapping_BR <- tree_cover_loss_brazil %>%
  left_join(muni_sf, by = "name_muni")

head(mapping_BR)


# Adjusting Extend ha 2000 and 2010 to be proportional to the sizes of the municipalities


mapping_BR <- mapping_BR %>% 
  mutate(
    relative_extent_per_size2000 =  (extent_2000_ha/area_ha)*100,
    relative_extent_per_size2010 =  (extent_2010_ha/area_ha)*100,
    relative_extent_per_size2020 =  (extent_2020_ha/area_ha)*100
  )


mapping_BR <- mapping_BR %>% 
  mutate(relative_change_2000_2010 = relative_extent_per_size2010 - relative_extent_per_size2000) %>%  #If it was 10 in 2000, and 12 in 2010, then it would be 12-10 = 2 (positive number), which means that now there is more tree cover loss  
  mutate(relative_change_2010_2020 = relative_extent_per_size2020 - relative_extent_per_size2010) %>%  
  mutate(relative_change_2000_2020 = relative_extent_per_size2020 - relative_extent_per_size2000) 


mapping_BR  <-  mapping_BR  %>% 
  mutate(actualChange = case_when(
    relative_change_2000_2020 >= 11 ~ "Large proportional gain",
    relative_change_2000_2020 >= 1 & relative_change_2000_2020 <= 10 ~ "Slight proportional gain",
    relative_change_2000_2020 == 0 ~ "No proportional gain or loss",
    relative_change_2000_2020 <= -1 & relative_change_2000_2020 >= -10 ~ "Slight proportional loss",
    relative_change_2000_2020 <= -11 & relative_change_2000_2020 >=-20 ~ "Large proportional loss",
    relative_change_2000_2020 < -20 ~ "Extremely large proportional loss", # Adjusted condition
    is.na(relative_change_2000_2020) ~ "No data available",  
    TRUE ~ "Other" 
  ))

head(mapping_BR)

mapping_BR  <-  mapping_BR  %>% 
  mutate(actualChange = case_when(
    relative_change_2000_2020 >= 11 & relative_extent_per_size2000 > 20 ~ "Large proportional gain",
    relative_change_2000_2020 >= 1 & relative_change_2000_2020 <= 10 & relative_extent_per_size2000 > 20 ~ "Slight proportional gain",
    relative_change_2000_2020 >= -1 & relative_change_2000_2020 <= 1 & relative_extent_per_size2000 > 20 ~ "No proportional gain or loss",
    relative_change_2000_2020 <= -1 & relative_change_2000_2020 >= -10 & relative_extent_per_size2000 > 20 ~ "Slight proportional loss",
    relative_change_2000_2020 <= -11 & relative_change_2000_2020 >=-20 & relative_extent_per_size2000 > 20 ~ "Large proportional loss",
    relative_change_2000_2020 < -20 & relative_extent_per_size2000 > 20 ~ "Extremely large proportional loss", 
    relative_extent_per_size2000 < 20 ~ "Less than 30% tree cover in 2000",
    is.na(relative_change_2000_2020) ~ "No data available", 
    TRUE ~ "Other" 
  ))%>% 
  mutate(actualChange = factor(actualChange, 
                               levels = c(
                                 "Large proportional gain", 
                                 "Slight proportional gain", 
                                 "No proportional gain or loss", 
                                 "Slight proportional loss", 
                                 "Large proportional loss", 
                                 "Extremely large proportional loss",
                                 "Less than 30% tree cover in 2000",
                                 "No data available", 
                                 "Other" 
                               )))

mapping_BR <- mapping_BR %>% 
  mutate(factor(actualChange))

custom_colors <- c("Large proportional gain" = "#138030", "Slight proportional gain" = "#19a93f", "No proportional gain or loss" ="#ddb588" , "Slight proportional loss" =  "#d37d4f", "Large proportional loss" = "#985125", "Extremely large proportional loss" =  "#623518","Less than 20% tree cover in 2000" = "#9e9e9e",  "No data available"= "#000000")



# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("Presidential Political Leaning and Tree Cover Loss in Brazil 2000-"),
  
  # Dropdown 
  tabPanel("Explore",
           sidebarLayout(
             sidebarPanel(
               selectInput(inputId = "state" , label = "Select state:", choices = state_nameBR ),
               sliderInput("year",
                           "Select year:",
                           min = 2000,
                           max = 2023,
                           value = 2012)
               
             ),
             
             # Show a plot of the generated distribution
             mainPanel(
               fluidRow(
                 column(12, plotlyOutput("line_COUNTRY_tree_cover_by_political_leaning_plot"))
               ),
               fluidRow(
                 column(6, plotOutput("line_STATE_tree_cover_loss")),
                 column(6, plotOutput("map_State_tcl_by_municipality"))
               )
             )
           )
  ),
  tabPanel("Compare over time"),
  tabPanel("About")
)


# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$line_COUNTRY_tree_cover_by_political_leaning_plot <- renderPlotly({
    p <- data_by_dist %>% 
      filter(metro_name == input$city) %>% 
      ggplot(aes(x= distmiles, y = entropy))+
      geom_point(aes(key = tract_id))+
      geom_smooth(se = FALSE, method = 'loess', span = input$span)+
      labs(title = "Diversity Gradient (2020 Census)", x = "Distance from city hall (miles)", y = "Diversity score" )
    
    ggplotly(p, source = "plotly_scatterplot") %>%
      event_register("plotly_selected")
  })
  
  output$map_plot <- renderPlot({
    
    metro_data <- data_by_dist %>% 
      filter(metro_name == input$city)
    ed <- event_data("plotly_selected", source = "plotly_scatterplot")
    
    if (is.null(ed)) {
      p <- ggplot(metro_data) +
        geom_sf(aes(fill = entropy)) +
        labs(title = "Map of diversity scores (2020 US census)", fill = "Diversity Score") +
        theme_minimal()
    } else {
      selected_tracts <-  metro_data %>% filter(tract_id %in% ed$key)
      bbox_selected <- st_bbox(selected_tracts)
      
      p <- data_by_dist %>% 
        filter(metro_name == input$city) %>% 
        #sf::st_cast("MULTIPOLYGON") %>% 
        ggplot(aes(fill = entropy)) +
        geom_sf() +
        geom_sf(data = selected_tracts, color = "red") +
        theme_classic()+
        coord_sf(xlim = c(bbox_selected["xmin"], bbox_selected["xmax"]),
                 ylim = c(bbox_selected["ymin"], bbox_selected["ymax"])
        ) 
    }
    p
  })
  
  output$line_STATE_tree_cover_loss <- renderPlot({
    p <- race_dist %>%  
      filter(metro_name == input$city) %>% 
      ggplot(aes(x=race, y = total)) +
      geom_col()+
      labs(title = "Race/ethnicity, selected city", x = "Race/ethnicity", y = "Population")
    p
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

