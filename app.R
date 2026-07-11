library(tidyverse)
library(shiny)
library(leaflet)
library(tidyverse)
library(rnaturalearth)
library(rnaturalearthdata)
library(sf)
library(ggrepel)
library(treemapify)
library(colorspace)
library(plotly)


# set theme
theme_set(theme_bw())


# this is a shiny app which allows us to visualize the twitter
# and diversity data derived from the UNT geotweet archive


# first tab: map of languages showing the most twitter languages
# allow the selection of year, measure -> impact the map
# clicking a country shows the distribution of languages in that country
# selecting a list number of countries draws a line plot showing their diversity across time according to the selected measure. 
# select a language and gets its distribution


# read data ######
df_tweets = read_csv("data/final_dataset/n_tweets_country_year_language.csv")
df_language = read_csv("data/final_dataset/language_data.csv")
df_country = read_csv("data/final_dataset/country_data.csv")
df_diversity = read_csv("data/final_dataset/diversity_measures.csv")
df_geo = ne_countries(scale = "large")
sim_m = read_rds("data/final_dataset/lexical_similarity_matrix.rds")


# set up geo
df_geo = df_geo %>%
  select(iso_a2_eh,
         geometry) %>%
  group_by(iso_a2_eh) %>%
  summarize(geometry = first(geometry))

# join datasets to get meta data
df_tweets = df_tweets %>%
  left_join(df_language, join_by(language_code)) %>%
  left_join(df_country, join_by(country_code))

df_diversity = df_diversity %>%
  left_join(df_country, join_by(country_code))


# fix namibia
df_tweets = df_tweets %>%
  mutate(country_code = case_when(is.na(country_code) ~ "NA",
                                  TRUE ~ country_code))
df_diversity = df_diversity %>%
  mutate(country_code = case_when(is.na(country_code) ~ "NA",
                                  TRUE ~ country_code))


# add most dominant language and its percent as a diversity variable that can be visualized on the map
# select the most dominant language of each country and year
df_dominant = df_tweets %>%
  group_by(country_code, year) %>%
  mutate(percent = n_tweets / sum(n_tweets) * 100) %>%
  slice_max(order_by = percent, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  rename("dominant_language" = language_name,
         "dominant_language_percent" = percent) %>%
  select(country_code, year, dominant_language, dominant_language_percent)


df_diversity = df_dominant %>%
  right_join(df_diversity, join_by(country_code, year))


# set up language data
df_lang = df_tweets %>%
  group_by(country_code, year) %>%
  mutate(percent = n_tweets / sum(n_tweets) * 100) %>%
  select(country_code, country_name, year, language_name, n_tweets, percent)


# add geodata
df_diversity = df_diversity %>%
  right_join(df_geo, join_by("country_code" == "iso_a2_eh")) %>%
  st_as_sf()

df_lang = df_lang %>%
  right_join(df_geo, join_by("country_code" == "iso_a2_eh")) %>%
  st_as_sf()

mst_spkn_langs = df_dominant %>%
  distinct(dominant_language) %>% pull(dominant_language)

# colors for languages
custom_palette = c(
  "French" = "#1f78b4",  # blue
  "English"  = "#e31a1c",  # red
  "Arabic" = "#33a02c",  # green
  "Chinese"  = "#ff7f00",   # orange
  "Spanish" = "#ffDA03", # yellow
  "Russian" = "#6a3d9a", # purple
  "German" = "#606060" # Grey
)

rest_palette = qualitative_hcl(40, "Dark 3")

missing_langs = setdiff(mst_spkn_langs, names(custom_palette))
rest_palette = setNames(rest_palette[seq_along(missing_langs)], missing_langs)
full_palette = c(custom_palette, rest_palette)

###### functions

get_languages = function(data = df_tweets, country, y){
  langs = data %>%
    filter(country_name == country) %>%
    filter(year == y) %>%
    mutate(tweets_perc = round(n_tweets / sum(n_tweets) * 100, 2)) %>%
    select(language_name, family, n_tweets, tweets_perc) %>%
    arrange(desc(tweets_perc)) %>%
    rename("Language" = "language_name",
           "Family" = "family",
           "Tweets (%)" = "tweets_perc",
           "Tweets" = "n_tweets")
  return(langs)
}



get_languages_global = function(data = df_tweets, y = 2021) {
  # summarizes languages on a global level according to tweets
  langs = data %>%  
    filter(year == y) %>%
    group_by(language_name) %>%
    summarise(
      family = unique(family),
      n_tweets = sum(n_tweets, na.rm = TRUE)) %>%
    mutate(percent = round(100 * n_tweets / sum(n_tweets), 2)) %>%
    rename("Language" = "language_name",
           "Family" = "family",
           "Tweets (%)" = "percent",
           "Tweets" = "n_tweets") %>%
    arrange(desc(Tweets))
  return(langs)
}
get_languages_global()

get_languages_by_continent = function(data = df_tweets, continents, y = 2021) {
  # summarizes languages on a continent according to speakers
  langs = data %>%
    filter(year == y) %>%
    filter(continent %in% continents) %>%
    group_by(language_name) %>%
    summarise(
      family = unique(family),
      n_tweets = sum(n_tweets, na.rm = TRUE)) %>%
    mutate(percent = round(100 * n_tweets / sum(n_tweets), 2)) %>%
    rename("Language" = "language_name",
           "Family" = "family",
           "Tweets (%)" = "percent",
           "Tweets" = "n_tweets") %>%
    arrange(desc(Tweets))
  return(langs)
}


get_diversity_ts = function(data = df_diversity, country = "Austria", measure = "richness"){
  # this function takes two or more countries, a diversity measure and plots a timeseries
  
  diversity_measure = sym(measure)
  
  # filter to countries of interest
  data = data %>%
    filter(country_name == country)
  
  #plot
  p = data %>%
    ggplot(aes(y = !!diversity_measure, fill = country_name, color = country_name, x = as.numeric(year))) +
    geom_point() + 
    geom_line() + 
    labs(title = paste0("Diveristy time series, ",country, ", ", rlang::as_name(diversity_measure))) + 
    scale_x_continuous(
      breaks = seq(min(data$year), max(data$year), by = 1)
    ) +
    labs(fill = "Country",
         color = "Country")
  return(p)
  
}

get_diversity_ts()


get_treemap = function(data = df_tweets, country = c("Austria"), y = "2021") {
  
  #filter and get percent
  data = data %>%
    filter(country_name %in% country) %>%
    filter(year == y) %>%
    mutate(percent = n_tweets / sum(n_tweets) * 100)
  
  p = ggplot(data, aes(
    area = percent,
    fill = family,
    label = paste0(language_name, "\n", round(percent,2), "%")
  )) +
    geom_treemap(color = "black") +
    geom_treemap_text(
      colour = "white",
      place = "centre",
      grow = TRUE,
      reflow = TRUE
    ) + 
    labs(title = paste0("Language distribution in ", country, ", ", y)) +
    theme(legend.position = "none")
    
  return(p)
  
}  
get_treemap()

get_top_10_langs_ts = function(data = df_tweets, country = "Austria") {
  
  data_filtered = data %>%
    filter(country_name == country) %>%
    group_by(year) %>%
    mutate(percent = n_tweets / sum(n_tweets) * 100) %>%
    ungroup()
  
  
  # make complete
  data_filtered = data_filtered %>%
    select(year, ISO6393, language_name, n_tweets, percent) %>%
    complete(year, ISO6393, language_name,
             fill = list(n_tweets = 0, percent = 0))
  
  # Top 10 languages overall
  top_langs = data_filtered %>%
    group_by(language_name) %>%
    summarise(tot_tweets = sum(n_tweets)) %>%
    slice_max(tot_tweets, n = 10) %>%
    pull(language_name)
  
  data_plot = data_filtered %>%
    mutate(language_name = ifelse(language_name %in% top_langs, language_name, "*Other")) %>%
    group_by(year, language_name) %>%
    summarize(percent = sum(percent)) %>% ungroup()
  
  ggplot(data_plot, aes(x = year, y = percent, fill = language_name)) +
    geom_area(alpha = 0.8, linewidth = 0.1, color = "white") +
    labs(
      title = paste("Top 10 over time in", country),
      x = "year",
      y = "Percent"
    ) +
    scale_y_continuous(labels = scales::percent_format(scale = 1)) +
    theme_minimal() +
    theme(legend.position = "right") + 
    labs(fill = "Language")
}

get_top_10_langs_ts(data = df_tweets)

# add percentage and most spoken language

# adjust naming of variables
df_diversity = df_diversity %>% 
  rename("Richness" = "richness",
         "Exponent Shannon" = "exp_shannon",
         "Inverse Simpson" = "inv_simpson",
         "Lexical Diversity (q=0)" = "div_q_0",
         "Lexical Diversity (q=1)" = "div_q_1",
         "Lexical Diversity (q=2)" = "div_q_2",
         "Number of Tweets" = "tot_tweets",
         "Dominant Language" = "dominant_language")

# define continent choices
continent_choices = sort(unique(df_diversity$continent))
year_choices = sort(unique(df_diversity$year))

ui = fluidPage(
  titlePanel("Global Digital Linguistic Diversity (twitter)"),
  tabsetPanel(
    id = "main_tabs",
    
    # --- Tab 1: Diversity Dashboard ---
    tabPanel("Diversity",
             sidebarLayout(
               sidebarPanel(
                 selectInput(
                   inputId = "selected_measure",
                   label = "Select a measure to visualize:",
                   choices = c(
                     "Richness", "Exponent Shannon", "Inverse Simpson",
                     "Lexical Diversity (q=0)", "Lexical Diversity (q=1)",
                     "Lexical Diversity (q=2)", "Number of Tweets", "Dominant Language"
                   ),
                   selected = "Dominant Language"
                 ),
                 selectInput(
                   inputId = "selected_year",
                   label = "Select a year:",
                   choices = sort(unique(df_diversity$year)),
                   selected = 2021,
                   multiple = FALSE
                 ),
                 radioButtons(
                   inputId = "selected_vis",
                   label = "Select Visualization:",
                   choices = c("Time Series" = "ts", "Language Treemap" = "treemap"),
                   selected = "treemap",
                   inline = TRUE
                 ),
                 
                 checkboxGroupInput(
                   inputId = "selected_continents",
                   label = "Select continent(s):",
                   choices = continent_choices,
                   selected = continent_choices
                 ),
                 actionButton("select_all", "Select All"),
                 actionButton("deselect_all", "Deselect All"),
                 br(), br(),
                 hr(),
                 h4("Most Widely Tweeted Languages"),
                 radioButtons(
                   "table_view_mode", 
                   "Select Table View:", 
                   choices = c("Country" = "country", "Continent" = "continent", "Global" = "global"),
                   selected = "country",
                   inline = TRUE
                 ),
                 DT::dataTableOutput("language_table")
               ),
               mainPanel(
                 leafletOutput("map", height = "700px"),
                 conditionalPanel(
                   condition = "input.selected_vis == 'ts'",
                   plotOutput("ts", height = "300px")
                 ),
                 conditionalPanel(
                   condition = "input.selected_vis == 'treemap'",
                   plotOutput("language_treemap", height = "450px")
                 )
                 
               )
             )
    ),
    tabPanel("About",
              h3("About this dashboard"),
              p("This dashboard was created to visualize the TweetLingDiv dataset and showcase how the linguistic diversity on Twitter has developed from 2012 and 2021. The underlying data is a derivative on the UNT geotweet Archive (Feng et al., 2023) which collects geotagged tweet, and we counted the number of tweets
              in each language across country and year to derive measures of diversity. The data was processed by Hannes Essfors who is the owner and maintainer of the dashboard and reserves all rights. Linguistic Diversity was calculated according to the Leinster-cobbold framework (2012). For more information, please see
               Essfors (2025). The dataset can be accessed through Zenodo: https://zenodo.org/records/18257720. The dataset should be cited as: Essfors, H. (2026). TweetLingDiv [Dataset]. Zenodo. https://doi.org/10.5281/zenodo.19332457"),
              p("If you want to cite the dashboard directly, please use: Essfors, H (2026). The Global Digital Linguistic Diversity (twitter) Dashboard [Shiny web application]"),
              p("1. Yunhe Feng, Zexuan Meng, Colton Clemmer, Heng Fan, and Yan Huang. 2023. Data and Resources Paper: A Multi-granularity Decade-Long Geo-Tagged Twitter Dataset for Spatial Computing. In Proceedings of the 31st ACM International Conference on Advances in Geographic Information Systems (SIGSPATIAL '23). Association for Computing Machinery, New York, NY, USA, Article 104, 1–4. https://doi.org/10.1145/3589132.3625654"),
              p("2. Essfors, H. (2025). Global linguistic diversity – Adapting the Leinster-Cobbold framework from ecology for humanities research. In T. Arnold, M. Fantoli, & R. Ros (Eds.), Anthology of Computers and the Humanities (Vol. 3, pp. 653–669). Association for Computers and the Humanities. https://doi.org/10.63744/srhQaCwGo5mj"),

    )
    ))


# Server
server = function(input, output, session) {
  # --- Diversity data ---
  color_data = reactive({
    req(input$selected_measure, input$selected_continents, input$selected_year)
    df_diversity %>%
      filter(continent %in% input$selected_continents) %>%
      filter(year == input$selected_year) %>%
      mutate(selected_value = .data[[input$selected_measure]])
  })
  
  # for numeric color values
  measures_pal = reactive({
    req(input$selected_measure)
    
    if (input$selected_measure != "Dominant Language") {
      colorNumeric(
        palette = "YlGnBu",
        domain = color_data()$selected_value,
        na.color = "grey"
      )
    } else {
      NULL
    }
  })
  
  # for categoric language colors
  language_colors = colorFactor(
    palette = full_palette, 
    domain = mst_spkn_langs, 
    na.color = "grey"
  )
  
  # for the language map
  lang_data_filtered = reactive({
    req(input$selected_lang, input$selected_lang_year)
    df_lang %>%
      filter(language_name == input$selected_lang,
             year == input$selected_lang_year)
  })
  
  # color for the language map
  lang_pal = reactive({
    colorNumeric(
      palette = "YlGnBu",
      domain = lang_data_filtered()$percent,
      na.color = "grey"
    )
  })
  
  # Diversity map
  output$map = renderLeaflet({
    leaflet(df_diversity) %>%
      addTiles() %>%
      setView(lng = 0, lat = 20, zoom = 2)
  })
  
  # langauge map
  output$lang_map = renderLeaflet({
    leaflet(df_lang) %>%
      addTiles() %>%
      setView(lng = 0, lat = 20, zoom = 2)
  })
  
  # observe for diversity map
  observe({
    data = color_data()
    
    # maps
    # case if dominant is selected
    if (input$selected_measure == "Dominant Language") {
      leafletProxy("map", data = data) %>%
        clearShapes() %>%
        clearControls() %>%
        addPolygons(
          layerId = ~country_name,
          fillColor = ~language_colors(`Dominant Language`),
          fillOpacity = ~dominant_language_percent/100,   
          color = "#BDBDC3",
          weight = 1,
          highlight = highlightOptions(weight = 2, color = "#666", bringToFront = TRUE),
          label = ~paste0(country_name, ": ", `Dominant Language`, " (", round(dominant_language_percent, 2), "%)")
        ) %>%
        addLegend(
          pal = language_colors,
          values = data$`Dominant Language`,
          opacity = 1,
          title = "Dominant Language",
          position = "bottomright"
        )
    
    
    # creates the map for numeric measures
    } else {
      pal_func = measures_pal()
      
      leafletProxy("map", data = data) %>%
        clearShapes() %>%
        clearControls() %>%
        addPolygons(
          layerId = ~country_name, 
          fillColor = ~pal_func(selected_value),
          fillOpacity = 0.7,
          color = "#BDBDC3",
          weight = 1,
          highlight = highlightOptions(weight = 2, color = "#666", fillOpacity = 0.9, bringToFront = TRUE),
          label = ~paste0(country_name, ": ", round(selected_value, 2))
        ) %>%
        addLegend(
          pal = pal_func,
          values = data$selected_value,
          opacity = 0.7,
          title = input$selected_measure,
          position = "bottomright",
          labFormat = labelFormat(digits = 3)
        )
    }
  })
  
  # observe for language map
  observe({
  data = lang_data_filtered()
  pal = lang_pal()
  
  leafletProxy("lang_map", data = data) %>%
    clearShapes() %>%
    clearControls() %>%
    addPolygons(
      layerId = ~country_code,
      fillColor = ~pal(percent),
      fillOpacity = 0.7,
      color = "#BDBDC3",
      weight = 1,
      highlight = highlightOptions(weight = 2, color = "#666", bringToFront = TRUE),
      label = ~paste0(country_name, ": ", round(percent, 2), "%"),
      labelOptions = labelOptions(direction = "auto")
    ) %>%
    addLegend(
      pal = pal,
      values = data$percent,
      title = paste("Percent of", input$selected_lang, "tweets"),
      position = "bottomright"
    )
})
  
  
  # time series
  output$ts = renderPlot({
    req(clicked_country(), input$selected_measure, input$selected_vis)
    if(input$selected_vis == "ts" && input$selected_measure != "Dominant Language"){
      p = get_diversity_ts(data = df_diversity, country = clicked_country(), measure = input$selected_measure)
    } else if(input$selected_vis == "ts" && input$selected_measure == "Dominant Language"){
      p = get_top_10_langs_ts(data = df_tweets, country = clicked_country())
    }
    plot(p)
  })

  #treemap
  output$language_treemap = renderPlot({
    req(clicked_country(), input$selected_year)
    get_treemap(country = clicked_country(), y = input$selected_year)
   
  })
  
  # Continent select/deselect
  observeEvent(input$select_all, {
    updateCheckboxGroupInput(session, "selected_continents", selected = continent_choices)
  })
  observeEvent(input$deselect_all, {
    updateCheckboxGroupInput(session, "selected_continents", selected = character(0))
  })
  
  clicked_country = reactiveVal(NULL)
  observeEvent(input$map_shape_click, {
    clicked_country(input$map_shape_click$id)
    # Reset table mode back to "country" when user clicks a map
    updateRadioButtons(session, "table_view_mode", selected = "country")
  })
  
  # Use radioButtons input directly
  table_view_mode = reactive({ input$table_view_mode })
  
  # Language table
  output$language_table = DT::renderDataTable({
    mode = table_view_mode()
    
    if (mode == "global") {
      lang_data = get_languages_global(y = input$selected_year) 
      
    } else if (mode == "continent") {
      if (!is.null(input$selected_continents)) {
        lang_data = get_languages_by_continent(
          continents = input$selected_continents, 
          y = input$selected_year
        )
      } else {
        lang_data = data.frame()
      }
      
    } else if (mode == "country" && !is.null(clicked_country())) {
      lang_data = get_languages(country = clicked_country(), y = input$selected_year)
      
    } else {
      lang_data = data.frame()
    }
    
    DT::datatable(lang_data, options = list(pageLength = 10), rownames = FALSE)
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
