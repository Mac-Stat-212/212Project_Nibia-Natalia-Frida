library(shiny)
library(tidyverse)
library(sf)
library(plotly)
library(geobr)
library(zoo)
library(scales)
library(bslib)

# Loading data
tree_cover_loss <- read.csv("data/raw/Brazil_TreeCoverLoss.csv")

# Loading shapefiles for Brazil at the national, state, and municipality levels
brazil_outline <- geobr::read_country(year = 2010)
states <- read_state(year = 2010, showProgress = FALSE)
muni_sf <- read_municipality(year = 2010, showProgress = FALSE)

# List of Brazilian states for dropdown input
state_nameBR <- states %>% pull(name_state) %>% unique() %>% 
  setdiff(c("Rio De Janeiro", "Rio Grande Do Sul", "Rio Grande Do Norte", "Mato Grosso Do Sul", "Espirito Santo"))

# Load and clean tree cover loss data for Brazil 
tree_cover_loss_brazil <- tree_cover_loss %>%
  filter(country == "Brazil", threshold == 30) %>%
  select(-country, -threshold) %>%
  mutate(
    loss_2000_2020_ha = rowSums(across(starts_with("tc_loss_ha_20"), .names = "x")),
    extent_2020_ha = extent_2000_ha + (gain_2000.2020_ha - loss_2000_2020_ha),
    name_muni = subnational2
  )

# Merge cleaned tree cover data with shapefiles and perform calculations on tree cover change
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

# Classify proportional tree cover change from each municipality
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

# Organize the order of the factor levels for the map legend
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

# Pivot longer for plotting
mapping_BR_leanings <- mapping_BR %>%
  pivot_longer(cols = starts_with("tc_loss_ha_"), names_to = "year_tc", values_to = "loss_ha") %>%
  mutate(year_tc = parse_number(year_tc), loss_k = loss_ha / 1000)

# Create dataset of Brazilian presidents, their political leanings and information for each
presidents <- tibble(
  name    = c("Fernando Henrique","Lula da Silva", "Dilma Rousseff","Michel Temer", "Jair Bolsonaro","Lula"),
  start = c(2000, 2003, 2011, 2016, 2019, 2023),
  end = c(2002, 2010, 2015, 2018, 2022, 2023),
  leaning = c("Center/Right", "Left", "Left", "Center/Right", "Right", "Left"),
  details = c("During Fernando Henrique Cardoso's double term (1995–2002), his administration, though center-right leaning, implemented several impactful environmental policies. Cardoso's government tripled the amount of protected Amazon rainforest areas, increased the legal reserve area for rural properties in the Amazon from 50% to 80%—a measure aimed at curbing deforestation—and positioned Brazil as an active participant in the creation of the Kyoto Protocol. However, during the last two years of his second term (2001–2002), rising commodity prices triggered an increase in deforestation. In 2001, deforestation reached 18,165 km², and in 2002, an electoral year, it surged to 21,651 km²—the highest rate recorded since measurements began. These developments highlight the complex interplay between environmental policy achievements and economic pressures during Cardoso's administration.", 
              "During Luiz Inácio Lula Da Silva’s administration (two terms from 2003 to 2011), which was left leaning, Brazil achieved a remarkable 83% reduction in deforestation rates in the Amazon between 2004 and 2011. This success was primarily driven by the implementation of the Action Plan for Prevention and Control of Deforestation in the Legal Amazon (PPCDAm), a comprehensive initiative involving multiple ministries. The plan focused on expanding protected areas, enhancing remote monitoring with near-real-time deforestation tracking, and strengthening law enforcement to combat illegal logging. Under the leadership of Marina Silva, Minsiter of Environment until 2008, the government also introduced a blacklist of municipalities with the highest deforestation rates, targeting them with increased enforcement. The administration further restricted agricultural credit, linking access to compliance with environmental regulations, effectively deterring illegal land clearing. Lula’s government also recognized deforestation as environmental and socio-economic issue, gradually addressing its root causes, including rural credit policies and economic incentives. The political commitment from Lula and his cabinet, coupled with Marina Silva’s leadership, established a model of coordinated environmental governance that significantly curbed deforestation.", 
              "During Dilma Rousseff’s administration (2011–2016), which was a continuation of the left-leaning government initiated by Luiz Inácio Lula da Silva, efforts to curb deforestation in the Amazon initially persisted. The Action Plan for Prevention and Control of Deforestation in the Legal Amazon (PPCDAm) remained in place, focusing on expanding protected areas, improving remote monitoring, and strengthening the enforcement of forestry laws. However, Rousseff’s administration was marked by a shift towards economic growth at the expense of environmental protection. Deforestation rates, which initially declined, began to rise again, a trend driven by a relaxation of environmental regulations, the consolidation of power by agribusiness interests, and large-scale infrastructure projects like the Belo Monte mega-dam. The administration also faced financial constraints, with significantly lower funding allocated to anti-deforestation measures compared to Lula’s term. Rousseff’s term was further overshadowed by the Lava Jato scandal, which exposed widespread corruption, including in the environmental and infrastructure sectors. This scandal not only damaged the credibility of the government but also facilitated the rise of pro-business, deregulation-oriented leaders, setting the stage for Jair Bolsonaro’s election in 2019.", 
              "During Michel Temer’s presidency (2016–2018), Brazil experienced a significant rollback of environmental protections, driven by his administration's strong ties to ruralist agribusiness interests. As an unpopular president with approval ratings consistently in the single digits, Temer relied on ruralist support to maintain political stability amid corruption allegations, including his involvement in the Lava Jato scandal. This alliance empowered ruralists to weaken indigenous land rights, reduce environmental oversight, and promote agribusiness expansion. One of his most controversial actions was an amnesty erasing up to $2.1 billion in unpaid environmental fines, primarily benefiting those responsible for illegal deforestation. He also attempted to open the 4.6 million-hectare RENCA reserve in the Amazon to mining interests, a move blocked only after widespread national and international outcry. Temer further supported the “land grabbers” law, which allowed illegal occupants of public land in the Amazon to obtain titles, effectively legalizing land grabbing and accelerating deforestation. Under his leadership, 2017 became the deadliest year for social and environmental activists in Brazil, with 57 murders linked to land conflicts. Temer's prioritization of agribusiness as an economic engine came at the cost of environmental conservation, leaving a legacy of weakened environmental policies and heightened conflict over land and resources.", 
              "During Jair Bolsonaro’s administration (2019–2022), Brazil witnessed a dramatic acceleration of deforestation in the Amazon, reaching a fifteen-year high in 2021. Bolsonaro’s right-leaning government systematically weakened environmental protections, scaled back the enforcement of environmental laws, and promoted the exploitation of Indigenous lands for mining and logging—despite constitutional prohibitions. His administration reduced the budget for environmental agencies, replaced experienced environmental policymakers with military officials, and dismantled mechanisms for citizen participation in environmental policy. Bolsonaro further encouraged agribusiness expansion and sought to legitimize illegal squatting on public lands through legislative proposals. In 2019, when widespread fires broke out in the Amazon, Bolsonaro rejected millions of dollars in aid from the Group of Seven (G7), framing international assistance as an attack on Brazilian sovereignty. While he established an Amazon Council and deployed the armed forces to combat environmental crimes in 2020, these measures were widely criticized as ineffective, with military interventions often undermining the efforts of federal environmental agencies. During his term, deforestation across the Brazilian Amazon totaled 35,193 km², highlighting the severe impact of his policies on the world’s largest rainforest.", 
              "Since returning to office in 2023, Luiz Inácio Lula da Silva’s administration has made significant strides in reducing deforestation in the Amazon. Satellite images from INPE show that deforestation rates in the Amazon fell by 62% in 2023 and by an additional 32% in the first half of 2024, reaching their lowest levels since 2019. Lula immediately reinstated the Action Plan for the Prevention and Control of Deforestation in the Legal Amazon (PPCDAm), emphasizing a commitment to end deforestation by 2030. His administration has also targeted illegal mining, with alerts for illegal mining in the Yanomami Indigenous Territory dropping to zero for the first time since 2020. Notably, Lula appointed a native Brazilian to his cabinet for the first time, signaling a renewed focus on Indigenous rights. However, despite these successes, his government faces significant challenges. The current Brazilian Congress is the most pro-agribusiness in history, making it difficult for Lula to enforce stronger environmental protections without facing resistance. Additionally, although deforestation in the Amazon has declined, forest loss in the Cerrado has increased, and severe drought conditions—exacerbated by climate change—continue to threaten the Amazon’s ecological balance. These challenges raise concerns about the long-term sustainability of Lula’s environmental gains."
  ))

pres_years <- presidents %>%
  rowwise() %>%
  mutate(year_tc = list(start:end)) %>%
  unnest(year_tc) %>%
  select(name, year_tc, leaning)

# Link tree cover loss with political leaning data
annual_loss <- mapping_BR_leanings %>%
  left_join(pres_years, by = "year_tc") %>%
  group_by(year_tc, name, leaning) %>%
  summarise(total_loss_k = sum(loss_k, na.rm = TRUE), .groups = "drop")

y_max <- max(annual_loss$total_loss_k, na.rm = TRUE)

ribbons <- presidents %>%
  mutate(xmin    = start - 0.5 , xmax    = end + 0.5, xmid = (start + end) / 2) %>%
  select(xmin, xmax, xmid, name, leaning)

# Reshape and summarize data to calculate the 3-year rolling average of tree cover loss per state
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

# Define the Shiny app UI. Includes 4 tabs: About the project, National Tree Cover, State Trends and Political Leanings. 

ui <- page_navbar(
  title = "Deforestation and political shifts in Brazil (2000–2023)",
  inverse = TRUE,
  
  nav_panel("About This Project",
                               h3("About This Project"),
                               h4("Importance of the Amazon"),
                               p("The Amazon is one of the planet's most vital ecosystems, spanning over 8 million square kilometers and housing around 30% of the world’s biodiversity. Brazil holds the largest share of this rainforest, with 60% of the Amazon falling within its borders. As a tropical forest, the Amazon plays a crucial role in regulating Earth’s climate, storing about one-quarter of all land-based carbon (Pan et al., 2011; Griscom, B., as cited by Kreir, F., 2022). It accomplishes this by cycling greenhouse gases between vegetation, soil, and the atmosphere, helping to maintain stable atmospheric gas concentrations (Jackson et al., 2017). This carbon storage capacity is a key factor in climate change models, which rely heavily on forests acting as carbon sinks. However, with one-third of tropical forests lost to deforestation in recent centuries (Kreier, F., 2022), the rapid decline in forest carbon storage is alarming. Given the Amazon's critical role as a carbon sink, understanding the political and policy factors driving deforestation is essential."),
                               h4("Research questions"),
                               p("Our project focuses on two main questions. First, how has tree cover in Brazil changed from 2000 to 2023? Second, does the political leaning of an administration help explain some of these patterns? By analyzing deforestation trends under different political administrations, we aim to determine whether governments of varying political leanings are associated with higher or lower rates of deforestation."),
                               h4("Motivation"),
                               p("On an individual level, we all found meaningful connections with this topic."),
                               p("Nibia’s research work in a tropical forest for the last two summers allowed her to engage with both the environmental and social relevance of preserving these ecosystems. She is very passionate about climate mitigation action and democratizing information in the Global South."),
                               p("Natalia is a Political Science major. She grew up in a very biodiverse country, which encouraged her to be interested in understanding what protects and harms these spaces. She has taken courses and done research on Latin American politics and wanted to expand the areas she looked at by starting with environmental policies."),
                               p("Frida was drawn to this topic due to her interest in sustainable finance and environmental policy, especially in Latin America. During her summer internship, she worked with clients across Latin America, many of them from Brazil, which sparked an interest around the country’s environmental challenges and its crucial role in South American climate efforts. She has also been taking Brazilian Portuguese classes in order to better engage with a wide variety of stakeholders in Brazil and be able to read academic literature in the language. In the future, she hopes to work at the intersection of economics and environmental policy, and this project offered a unique opportunity to explore those dynamics."),
                               
                               h4("Data"),
                               p("For this project, we used tree cover and deforestation data from Global Forest Watch (GFW), an open-access platform developed by the World Resources Institute in partnership with the University of Maryland’s GLAD lab and Google. GFW provides near real-time data on forest change, including tree cover extent, deforestation rates, and associated CO₂ emissions. We focused on data for Brazil from 2000 to 2023 to examine spatial forest loss at the municipality level."),
                               
                               h4("Key findings"),
                               p("The analysis demonstrates a clear link between the political leaning of Brazilian administrations and deforestation rates in the Amazon. Left-leaning governments (Lula and Rousseff) generally exhibited stronger environmental policies and lower deforestation rates, while right-leaning administrations (Temer and Bolsonaro) were associated with higher deforestation due to deregulation and favoring agribusiness interests. Key policies, such as the Action Plan for Prevention and Control of Deforestation in the Legal Amazon (PPCDAm), played a critical role in reducing deforestation during the Lula and early Rousseff administrations. Conversely, the weakening of environmental agencies under Temer and Bolsonaro led to a sharp increase in deforestation."),
                               
                               h4(" Limitations"),
                               p("This analysis has several limitations that should be considered when interpreting the findings. First, the analysis focuses primarily on political leadership, potentially overlooking other influential factors such as global commodity prices, international pressure, and the effectiveness of local governance. Data limitations also exist, as the study relies on deforestation data that may contain measurement gaps or fail to capture smaller-scale forest clearings. Finally, there is a temporal mismatch between policy changes and their impact on deforestation, as deforestation often responds to policies with a delay."),
                               
                               h4("Future directions"),
                               p("Future research could address these limitations by incorporating a broader range of socioeconomic factors to better understand their interaction with political ideologies. Further analysis should also evaluate the enforcement of environmental policies, and a regional analysis could provide insights into how state and municipal governments, with varying political dynamics, influence deforestation rates. Additionally, a focused study on the protection of Indigenous lands would help clarify the role of legal protections and enforcement in preserving forest cover. Finally, future research could explore the interactions between deforestation, rising temperatures, drought conditions, and their combined impact on Amazon forest resilience, helping to anticipate potential tipping points."),
                               
                               h5("Authors"),
                               p("Nibia Becerra Santillan, Natalia Morales Flores and Frida Morado Salazar"),
                               p("Professor Brianna Heggeseth")
  ),
  
  nav_panel("National Tree Cover",
            layout_sidebar(
              sidebar = sidebar(
                sliderInput("year", "Select year for Relative Tree Cover (ha):", min = 2000, max = 2020, value = 2000, step = 10, sep = ""),
                p("Tropical forests like the Amazon are critical regulators of Earth’s climate, storing one-quarter of all terrestrial carbon (Pan et al., 2011; Griscom, B cited by Kreir, F 2022)). Forests cycle large quantities of greenhouse gases through vegetation and soils from and into the atmosphere, stabilizing the atmospheric concentration of these gasses (Jackson et al., 2017). A particularly important feature of these forests is their role in climate mitigation due to their capacity to store more carbon dioxide than that they release, making up for some of the human-made CO2 emissions. This capacity diminishes as the tree density decreases both due to the loss of trees and the alteration of the soils when trees are removed. 
In this project, we focus on Brazil, the country that contains the majority of the Amazon Basin ecosystem. We examine tree cover as the proportion of a municipality’s area (the smallest geographic division in Brazil) that is covered by trees. Our dataset begins in the year 2000, using data from Global Forest Watch. Big differences in tree cover across regions and municipalities may reflect natural ecosystem variability.
")
              ),
              layout_column_wrap(width = 1,
                                 plotOutput("tree_cover_map"))
            )
  ),
  
  nav_panel("Tree Cover Extent Change",
            layout_sidebar(
              sidebar = sidebar(
                p("This graph shows changes in tree cover from 2000 to 2020, categorized by the degree of gain or loss relative to each municipality’s area. A large proportional gain or loss indicates a change of around 20% of the municipality’s total area, while extreme proportional loss refers to losses greater than 20%.

	The visible decrease in tree cover in areas surrounding protected zones in the Amazon Basin reflects broader failures in the enforcement of environmental laws in Brazil. Research shows that these adjacent areas are particularly vulnerable, as they offer easier access to natural resources and face higher risk when environmental policies are weakly enforced or poorly implemented (Almeida-Rocha & Peres, 2021). For instance, illegal deforestation for timber often occurs along the edges of the Amazon, where access is easier and infrastructure allows for more consistent activity. When government funding and enforcement are lacking, these border areas become the easiest targets. This pattern of degradation underscores how political priorities can directly shape the effectiveness of conservation efforts.
")
              ),
              layout_column_wrap(width = 1,
                                 plotOutput("map_tree_cover_loss_map"))  
            )
  ),
  
  nav_panel("State Trends",
            layout_sidebar(
              sidebar = sidebar(
                selectInput("state", "Select state:", state_nameBR),
                p("Some states are more significant than others when talking about tree cover loss and the Amazon tropical rainforest. States like Distrito Federal are very small and urban, so the trend line appears to be almost a straight horizontal line, showing no change in tree cover loss. Other states such as Sao Paulo and Mina Gerais even show a decrease in tree cover loss, but it is important to note that these include the Triângulo Mineiro (Minas Triangle) in the west is characterized by cerrado, a type of savanna.
However, states that contain a significant portion of the rainforest such as Amazonas, Roraima, Pará, and others, do see a constant increase in tree cover loss. This emphasizes the direct impact of the national tree cover loss in Brazil on the Amazon tropical rainforest.
")
              ),
              layout_column_wrap(width = 1,
                                 plotOutput("line_STATE_tree_cover_loss"))
            )
  ),
  
  nav_panel("Political Leanings",
            layout_column_wrap(width = 1,
                               plotlyOutput("line_COUNTRY_tree_cover_by_political_leaning_plot"),
                               textOutput("point_info"))
  )
)


# Creates annual tree cover loss with political leaning plot 
server <- function(input, output) {
  output$line_COUNTRY_tree_cover_by_political_leaning_plot <- renderPlotly({
    p <- ggplot() +
      geom_rect(data = ribbons, aes(xmin = xmin, xmax = xmax, ymin = 0, ymax = y_max, fill = factor(ribbons$leaning)), alpha = 0.2, inherit.aes = FALSE) +
      geom_vline(data = ribbons, aes(xintercept = xmin), linetype = "dashed", color = "grey28") +
      geom_line(data = annual_loss, aes(x = year_tc, y = total_loss_k), size = 0.8, color = "black") +
      geom_point(data = annual_loss, aes(x = year_tc, y = total_loss_k), color = "black", size = 2) +
      geom_text(data = ribbons, aes(x = xmid, label = name), y = y_max - 200, vjust = 0, size = 3.5, inherit.aes = FALSE) +
      scale_fill_manual(name = "Political Leaning", values = c("Left" = "#377eb8", "Right" = "#e41a1c", "Center/Right" = "#ffae42")) +
      scale_x_continuous(breaks = presidents$start, labels = presidents$start) +
      labs(title = "Annual tree cover loss, with political leaning", x = "Year", y = "Loss (×1000 ha)") +
      theme_minimal(base_size = 14) +
      theme(legend.position = "bottom", plot.title.position  = "plot", plot.title = element_text(hjust = 0.5, vjust = 1.2, margin = margin(t = 15))
        ) 
    
    p
    
    ggplotly(p, source = "tree_plot") %>% config(displayModeBar = FALSE)
    
    })
  
# Creates tree cover map, in which the user selects a year between 2000, 2010 and 2020
  output$tree_cover_map <- renderPlot({
    col_name <- str_c("relative_extent_per_size", input$year)
    
      ggplot() +
      geom_sf(data = brazil_outline, fill = 'black', color = "black", lwd = 0.3) +
      geom_sf(data = mapping_BR, aes(fill = !!sym(col_name)), color = NA) +
      scale_fill_gradient(low = "white", high = "#138030") +
      labs(
        title = paste("Percentage Tree Cover per municipality in", input$year),
        fill = "Percentage Tree Cover",
        subtitle = "Tree cover relative to municipality area", caption = 'Source: Global Forest Watch ; Missing values in black.'
      ) +
      theme_void()
  
  })
  
# Creates tree cover extent change plot from 2000 to 2020 
  output$map_tree_cover_loss_map <- renderPlot({
    ggplot() +
      geom_sf(data = brazil_outline, fill = 'black', color = "black", lwd = 0.3) +
      geom_sf(data = mapping_BR, aes(fill = actualChange, geometry = geom), lwd = 0.02,na.rm=FALSE) + 
      scale_fill_manual(values = custom_colors, name = "Gain or loss (%)",na.value = 'lightgrey') + 
      labs(
        title = "Brazil Tree Cover Extent Change from 2000–2020 by Municipality",
        subtitle = "Change only for municipalities with >20% tree cover in 2000.\nEmpty areas represent municipalities missing polygon data.",
        caption = 'Source: Global Forest Watch; Missing values in light yellow.') +
      theme_void()
  })
  
# Creates annual tree cover loss plot with 3-year rolling average 
  output$line_STATE_tree_cover_loss <- renderPlot({
    line_colors <- c("Loss in ha" = "darkgreen", "LM" = "red")
    
    p <- state_loss %>% 
      filter(name_state == input$state) %>%
      ggplot(aes(x = year_tc, y = total_loss)) +
      geom_line(aes(color = "Loss in ha")) +
      geom_smooth(aes(color = "LM"), method = "lm", se = FALSE, linetype = "dashed", linewidth = 0.5) +
      scale_y_continuous(labels = label_number(scale = 1/1000, suffix = "k", accuracy = 1)) +
      labs(title = "Annual Tree Cover Loss in Select Brazilian States",
           subtitle = "With 3-year rolling average and linear trend line (2001–2023)",
           y = "Tree Cover Loss (thousands of ha)",
           x = "Year") +
      scale_color_manual(values = line_colors) +
      theme_minimal()
    
    p
    
  })
  
# Makes political leaning plot interactive
  output$point_info <- renderText({
    click_data <- event_data("plotly_click", source = "tree_plot")
    if (is.null(click_data)) {
      return("Click on a point to see detailed information.")
    }
    year_selected <- click_data$x
    president_row <- presidents[year_selected >= presidents$start & year_selected <= presidents$end, ]
    
    if (nrow(president_row) == 0) {
      return("No information available for the selected year.")
    }
    
    paste0(president_row$details)
  })
  
}



shinyApp(ui = ui, server = server)

