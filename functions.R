library(tidyverse)
library(rnaturalearth)
library(rnaturalearthdata)
library(sf)
library(ggrepel)
library(treemapify)

# this script aims to give a first description of the data collected
# read data
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


mid_cords = st_coordinates(st_point_on_surface(df_geo$geometry))
df_geo$mid_long = mid_cords[,1]
df_geo$mid_lat = mid_cords[,2]

# theme
theme_set(theme_bw())


# join datasets
df = df_tweets %>%
  left_join(df_language, join_by(language_code)) %>%
  left_join(df_country, join_by(country_code))

df_diversity = df_diversity %>%
  left_join(df_country, join_by(country_code))


# fix namibia
df = df %>%
  mutate(country_code = case_when(is.na(country_code) ~ "NA",
                                  TRUE ~ country_code))
df_diversity = df_diversity %>%
  mutate(country_code = case_when(is.na(country_code) ~ "NA",
                                  TRUE ~ country_code))


# generate a similarity matrix based on the languages

##### functions #####

##### Visualization ########
n_tweets_map = function(data = df, years = min(data$year):max(data$year), continents = unique(df$continent), aggregate = "sum", labels = FALSE){
  # this function takes a vector of years, continents and an aggregate function and plots the aggregated number 
  # of tweets in the year range on a map.
  
  # filter to year of interest
  data = data %>%
    filter(year %in% years)
  
  # filter to continents of interest
  data = data %>%
    filter(continent %in% continents)
  
  # aggregate funciton
  agg_fun = get(aggregate)
  
  # aggregate
  data_agg = data %>%
    group_by(country_code) %>%
    summarize(agg = agg_fun(n_tweets))
  
  # join with geo and plot map
  data_plot = data_agg %>%
    left_join(df_geo, join_by("country_code" == "iso_a2_eh")) %>%
    st_as_sf()
  
  if (labels == TRUE){
    data_plot %>%
      ggplot(aes(geometry = geometry, fill = agg)) +
      geom_sf(color = "black") + 
      scale_fill_viridis_c() + 
      labs(title = paste0(aggregate, " tweets, ", min(years), "-", max(years)),
           fill = aggregate) +
      geom_label_repel(data = data_plot, aes(label = as.character(agg), x = mid_long, y = mid_lat),
                       inherit.aes = FALSE)
  } else{
    data_plot %>%
      ggplot(aes(geometry = geometry, fill = agg)) +
      geom_sf(color = "black") + 
      scale_fill_viridis_c() + 
      labs(title = paste0(aggregate, " tweets, ", min(years), "-", max(years)),
           fill = aggregate)
  }
  
}

most_common_tweets_map = function(data = df, years = min(data$year):max(data$year), continents = unique(df$continent), labels = FALSE){
  # this function takes a vector of years, continents and plots a map with the most commonly occuring language
  
  # filter to year of interest
  data = data %>%
    filter(year %in% years)
  
  # filter to continents of interest
  data = data %>%
    filter(continent %in% continents)
  
  
  # aggregate
  data_agg = data %>%
    group_by(country_code, language_code) %>%
    summarize(n_tweets = sum(n_tweets),
              language_name = unique(language_name)) %>%
    mutate(percent = n_tweets / sum(n_tweets) * 100) %>%
    group_by(country_code) %>%
    mutate(rank_percent = rank(-percent)) %>%
    filter(rank_percent < 2)
  
  
  # join with geo and plot map
  data_plot = data_agg %>%
    left_join(df_geo, join_by("country_code" == "iso_a2_eh")) %>%
    st_as_sf()
  if(labels == TRUE){
    data_plot %>%
      ggplot(aes(geometry = geometry, fill = language_name, alpha = percent)) +
      geom_sf(color = "black") + 
      labs(title = paste("Most common tweet language,", min(years), "-", max(years))) + 
      geom_label_repel(data = data_plot, aes(x = mid_long, y = mid_lat, label = language_name, fill = language_name),
                       inherit.aes = FALSE)
  } else{
    data_plot %>%
      ggplot(aes(geometry = geometry, fill = language_name, alpha = percent)) +
      geom_sf(color = "black") + 
      labs(title = paste("Most common tweet language,", min(years), "-", max(years)))
  }
  
  
}


language_distribution_bar = function(data = df, country = "world", years = min(data$year):max(data$year)){
  # this function takes a country or vector of countries and range of years and displays the distribution of languages according
  # to tweets for each year in the range as a stacked barchart
  
  # check if world is supplied
  if (length(country) == 1 && country == "world"){
    # group by year and sum tweets across langauges + add percent
    data = data %>% filter(year %in% years) %>%
      group_by(year, language_name) %>%
      summarize(n_tweets = sum(n_tweets)) %>%
      mutate(percent = n_tweets / sum(n_tweets) * 100)
    
    # assign labs for each year to indicate the number of tweets
    labs = data %>%
      group_by(year) %>%
      summarize(tot_tweets = sum(n_tweets))
    
    
    data %>%
      ggplot(aes(y = percent, x = year, fill = language_name)) +
      geom_bar(stat = "identity",
               position = "stack",
               color = "black") +
      geom_text(data = labs, aes(x = year, y = 102, label = paste("n:", tot_tweets)),
                inherit.aes = FALSE) + 
      theme(title = paste0("distribution of twitter languages in the world"))
    
    
  }
  else{# one or more country was supplied, applying logic to plot countries
    
    data = data %>%
      filter(year %in% years) %>%
      filter(country_name %in% country) %>%
      group_by(country_code, year) %>%
      mutate(percent = n_tweets / sum(n_tweets) * 100)
    
    labs = data %>%
      group_by(country_name, year) %>%
      summarize(tot_tweets = sum(n_tweets))
    
    data %>%
      ggplot(aes(y = percent, x = year, fill = language_name)) +
      geom_bar(stat = "identity",
               position = "stack",
               color = "black") +
      geom_text(data = labs, aes(x = year, y = 102, label = paste("n:", tot_tweets)),
                inherit.aes = FALSE) + 
      facet_wrap(~country_name)
    
  }
}

language_distribution_line = function(data = df, country = "world", langs = "all",  years = min(data$year):max(data$year)){
  # this function takes a country or vector of countries and range of years and plots the percentage of tweet
  # for each language as a time series
  
  
  if(length(country) == 1 && country == "world"){
    data = data %>%
      filter(year %in% years) %>%
      group_by(year, language_name) %>%
      summarize(n_tweets = sum(n_tweets)) %>%
      mutate(percent = n_tweets / sum(n_tweets) * 100)
    
    
    labs_1 = data %>%
      group_by(year) %>%
      summarize(tot_tweets = sum(n_tweets))
    
    
    # check if langs have been provided: if yes -> filter
    if(length(langs) == 1 && langs == "all"){
      # do nothing
    }
    else{
      # filter
      data = data %>%
        filter(language_name %in% langs)
    }
    
    labs_2 = data %>%
      group_by(language_name) %>%
      filter(percent == max(percent)) %>%
      summarize(max_percent = max(percent),
                year_max = first(year))
    
    data %>%
      ggplot(aes(y = percent, x = year, fill = language_name, color = language_name)) +
      geom_point() + 
      geom_line() +
      geom_text(data = labs_1, aes(x = year, y = max(data$percent) + 2, label = paste("n:", tot_tweets)),
                inherit.aes = FALSE) + 
      geom_label_repel(data = labs_2, aes(x = year_max, y = max_percent, label = language_name, color = language_name),
                       inherit.aes = FALSE) +
      theme(legend.position = "none") +
      theme("distribution of the twitter languages of the world")
    
  }
  
  else{
    data = data %>%
      filter(year %in% years) %>%
      filter(country_name %in% country) %>%
      group_by(country_code, year) %>%
      mutate(percent = n_tweets / sum(n_tweets) * 100)
    
    
    labs_1 = data %>%
      group_by(country_name, year) %>%
      summarize(tot_tweets = sum(n_tweets))
    
    # check if langs have been provided: if yes -> filter
    if(length(langs) == 1 && langs == "all"){
      # do nothing
    }
    else{
      # filter
      data = data %>%
        filter(language_name %in% langs)
    }
    
    labs_2 = data %>%
      group_by(country_name, language_name) %>%
      filter(percent == max(percent)) %>%
      summarize(max_percent = max(percent),
                year_max = first(year))
    
    
    
    data %>%
      ggplot(aes(y = percent, x = year, fill = language_name, color = language_name)) +
      geom_point() + 
      geom_line() +
      geom_text(data = labs_1, aes(x = year, y = 102, label = paste("n:", tot_tweets)),
                inherit.aes = FALSE) + 
      geom_label_repel(data = labs_2, aes(x = year_max, y = max_percent, label = language_name, color = language_name),
                       inherit.aes = FALSE) + 
      facet_wrap(~country_name) + 
      theme(legend.position = "none")
  }
}

#### a function that takes a country and returns a pie chart indicating the top most tweeted languages

top_langs_pie = function(data = df, country = "world", years = min(data$year):max(data$year)){
  
  if(length(country) == 1 && country == "world"){
    # only filter years
    data = data %>%
      filter(year %in% years)
  } else{
    data = data %>%
      filter(country_name %in% country) %>%
      filter(year %in% years)
  }
  
  top_10 = data %>%
    group_by(language_name) %>%
    summarize(tweets = sum(n_tweets)) %>%
    mutate(percent = tweets/sum(tweets) * 100) %>%
    mutate(rank_perc = rank(-percent)) %>%
    mutate(language_name = case_when(rank_perc > 10 ~ "other",
                                     TRUE ~ language_name)) %>%
    group_by(language_name) %>%
    summarize(percent = sum(percent)) %>%
    arrange(desc(percent))
  
  top_10 = top_10 %>%
    rowwise() %>%
    mutate(labels = paste0(language_name, "\n", as.character(round(percent,2 )), " %"))
  
  pie(x = top_10$percent, labels = top_10$labels,
      main = paste0("Top 10 most tweeted languages in ", country, " ", min(years), "-", max(years)))
  
}


top_langs_tree = function(data = df, country = "world", years = min(data$year):max(data$year)){
  
  # filter data
  if(length(country) == 1 && country == "world"){
    data = data %>%
      filter(year %in% years)
  } else {
    data = data %>%
      filter(country_name %in% country, year %in% years)
  }
  
  # aggregate
  top_10 = data %>%
    group_by(language_name) %>%
    summarize(tweets = sum(n_tweets), .groups = "drop") %>%
    mutate(percent = tweets / sum(tweets) * 100) %>%
    mutate(rank_perc = rank(-percent)) %>%
    mutate(language_name = case_when(rank_perc > 10 ~ "other",
                                     TRUE ~ language_name)) %>%
    group_by(language_name) %>%
    summarize(percent = sum(percent), .groups = "drop") %>%
    arrange(desc(percent)) %>%
    rowwise() %>%
    mutate(labels = paste0(language_name, "\n", round(percent, 2), "%"))
  
  # plot
  p = ggplot(top_10, aes(
    area = percent,
    fill = language_name,
    label = labels
  )) +
    geom_treemap(color = "black") +
    geom_treemap_text(
      colour = "white",
      place = "centre",
      grow = TRUE,
      reflow = TRUE
    ) +
    labs(
      title = paste0(
        "Top 10 most tweeted languages in ",
        " ",
        min(years), "-", max(years)
      ),
      fill = "Language"
    ) 
  plot(p)
  return(p)
}

lang_distribution_map = function(data = df, lang, continents = unique(data$continent),  years = min(data$year):max(data$year)){
  # this function takes a language, a range of years and plots the percentage of tweets in that language in each country
  
  # filter to year of interest
  data = data %>%
    filter(year %in% years) %>%
    filter(continent %in% continents)
  
  # calculate percentage per country
  data_agg = data %>%
    group_by(country_code, language_name) %>%
    summarize(n_tweets = sum(n_tweets)) %>%
    mutate(percent = n_tweets / sum(n_tweets) * 100)
  
  # filter to only the language of interest
  data_agg = data_agg %>%
    filter(language_name == lang)
  
  # join with geo and plot map
  data_plot = data_agg %>%
    right_join(df_geo, join_by("country_code" == "iso_a2_eh")) %>%
    st_as_sf()
  
  data_plot %>%
    ggplot(aes(geometry = geometry, fill = percent)) +
    geom_sf(color = "black") + 
    scale_fill_gradient2(low = muted("white"),
                         high = muted("blue")) + 
    labs(title = paste0("Distribution of ", lang, " as percent of tweets ", min(years), "-", max(years)))
  
}


##### diversity computation ######

get_prop_vec = function(tweets){
  #takes a vector with tweets and turns it into a proportion vector
  prop_vec = tweets / sum(tweets)
  return(prop_vec)
}

subset_and_reorder = function(matrix, labels){
  # this function takes a matrix and labels and input and subsets the matrix to match the values in the label
  
  matrix = matrix[labels, labels]
  
  return(matrix)
}


get_shannon_diversity = function(langs, tweets, sim_m){
  # calculates diversity for q = 1 ergo shannon given a vector with proportions
  
  # subset sim matrix to the languages
  sim_m = subset_and_reorder(sim_m, langs)
  
  
  prop_vec = get_prop_vec(tweets)
  
  # for each proportion, get the expected similarity to all other proportions
  expected = log(sim_m %*% prop_vec)
  
  # for each proportion, multiply by expected similarity to all other proportions
  # and derive entropy
  E = -1 * sum(prop_vec * expected)
  
  # exponentiate entropy to get diversity
  D = exp(E)
  
  return(D)
}

get_diversity_q = function(langs, tweets, sim_m, q = 0){
  # a general function to implement diversity for any q
  
  # to avoid division with zero, implement shannon diversity as a special case
  if (q == 1){
    return(get_shannon_diversity(langs, tweets, sim_m))
  }
  
  # subset sim matrix to the languages
  sim_m = subset_and_reorder(sim_m, langs)
  
  # 
  prop_vec = get_prop_vec(tweets)
  
  
  
  
  # get expected similarity to all other prop for each proportion
  expected = sim_m %*% prop_vec
  
  # raise the expected similarity to the power of q-1
  expected_order = expected^(q-1)
  
  # multiply the expected similarity with each proportion and take the reciprocal
  D = (sum(prop_vec * expected_order))^(1/(1-q))
  
  return(D)
}


get_naive_diversity_q = function(langs, tweets, q = 0){
  # a general function to implement naive diversity for any q
  
  tweets = get_prop_vec(tweets)
  
  I = diag(length(prop_vec))
  
  # get diversity
  D = get_diversity_q(langs, prop_vec, I, q)
  
  return(D)
}


##### diversity visualizations ######

get_diversity_map = function(data = df_diversity, diversity_measure = richness, y = 2021, continents = unique(df$continent)){
  # this function takes a year, a diversity measure and optionaly a continent(s) and plots the diversity on a map
  
  # filter to year of interest
  data = data %>%
    filter(year == y)
  
  # filter to continents of interest
  data = data %>%
    filter(continent %in% continents)
  
  # filter to selected diversity measure
  diversity_measure = enquo(diversity_measure)
  
  data = data %>%
    select(country_code, !!diversity_measure)
  
  # join with geo and plot map
  data_plot = data %>%
    right_join(df_geo, join_by("country_code" == "iso_a2_eh")) %>%
    st_as_sf()
  
  #plot
  data_plot %>%
    ggplot(aes(geometry = geometry, fill = !!diversity_measure)) +
    geom_sf(color = "black") + 
    scale_fill_gradient2(low = muted("white"),
                         high = muted("blue")) + 
    labs(title = paste0(rlang::as_name(diversity_measure), ", ", y))
  
}


get_all_diversity_country_ts = function(data = df_diversity, country = "Austria"){
  # this function takes a country and plots its diversity as timelines
  
  # filter to country of interest
  data = data %>%
    filter(country_name == country)
  
  # change to long format
  data_plot = data %>%
    pivot_longer(cols = c("richness", "exp_shannon", "inv_simpson", "div_q_0", "div_q_1", "div_q_2"),
                 names_to = "measure")
  
  
  
  #plot
  data_plot %>%
    ggplot(aes(y = value, fill = measure, color = measure, x = year)) +
    geom_point() + 
    geom_line() + 
    labs(title = paste0("Diveristy, ", country))
  
}

get_country_comparison_country_ts = function(data = df_diversity, country = c("Austria", "Germany"), measure = richness){
  # this function takes two or more countries, a diversity measure and plots a timeseries
  
  diversity_measure = enquo(measure)
  
  # filter to countries of intereset
  data = data %>%
    filter(country_name %in% country)
  
  #plot
  data %>%
    ggplot(aes(y = !!diversity_measure, fill = country_name, color = country_name, x = year)) +
    geom_point() + 
    geom_line() + 
    labs(title = paste0("Diveristy time series, ", rlang::as_name(diversity_measure)))
  
}

#### for the app #####



