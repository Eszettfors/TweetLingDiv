library(tidyverse)
library(rnaturalearth)
library(rnaturalearthdata)
library(sf)
library(ggrepel)
library(xtable())

# this script aims to give a first description of the data collected

# read data
df_tweets = read_csv("data/final_dataset/n_tweets_country_year_language.csv")
df_language = read_csv("data/final_dataset/language_data.csv")
df_country = read_csv("data/final_dataset/country_data.csv")
df_geo = ne_countries(scale = "large")


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

# fix namibia
df = df %>%
  mutate(country_code = case_when(is.na(country_code) ~ "NA",
                   TRUE ~ country_code))

## what is the total number of tweets?
sum(df$n_tweets) %>% print()
# 8 118 398

## what years are covered?
min(df$year) # 2012
max(df$year) # 2021

## how many countries and languages are included?
df %>% distinct(country_code) %>% nrow() #236 countries
df %>% distinct(language_code) %>% nrow() # 70 languages

### how does the total number of tweets vary by year?
df %>%
  group_by(year) %>%
  summarise(n_tweets = sum(n_tweets)) %>%
  ggplot(aes(y = n_tweets,
             x = year)) + 
  geom_bar(stat = "identity",
           fill = "yellow",
           color = "black") +
  labs(title = "Temporal distribution of tweets") +
  geom_text(aes(label = as.character(n_tweets)),
            vjust = -0.2) +
  labs(y = "Number of Tweets")

# there is a peak at 2016 and then it falls. There is an abnormality at 2019 but it is coherent with the finding in release paper: https://yunhefeng.me/material/SIGSPATIAL-2023.pdf 
# (the feature of tagging tweets exactly were discontinued in 2019)3

# in 2012, there were only 22390 tweets -> unreliable

## how does the number of tweets vary by continent and year?

df %>%
  group_by(continent, year) %>%
  summarise(n_tweets = sum(n_tweets)) %>%
  ggplot(aes(y = n_tweets,
             x = year, 
             fill = continent)) + 
  geom_bar(stat = "identity",
           position = "stack",
           color = "black") + 
  labs(x = "Number of tweets",
         title = "Temporal distribution of tweets")
           

df %>%
  group_by(continent, year) %>%
  summarise(n_tweets = sum(n_tweets)) %>%
  ggplot(aes(y = n_tweets,
             x = year,
             fill = continent,
             color = continent)) + 
  geom_point() + 
  geom_line() + 
  labs(title = "temporal and continental distribution of tweets") 

# lots of data from South America at the start, oceania and Africa are observing some data from 2015 and onwards; least data for oceania

# continent as share of tweets for each year
# table
df %>%
  group_by(year, continent) %>%
  summarise(n_tweets = sum(n_tweets)) %>%
  mutate(percent = round(n_tweets / sum(n_tweets),4) * 100) %>%
  select(!n_tweets) %>%
  group_by(continent) %>%
  pivot_wider(names_from = year,
              values_from = percent) %>%
  xtable()

# bar
df %>%
  group_by(year, continent) %>%
  summarise(n_tweets = sum(n_tweets)) %>%
  mutate(percent = round(n_tweets / sum(n_tweets),4) * 100) %>%
  ggplot(aes(y = percent, x = year, fill = continent, color = continent)) +
  geom_point() + 
  geom_line() + 
  labs(title = "percent tweets from each continent every year")


# how does the number of countries vary by year?
df %>%
  group_by(year) %>%
  summarise(n_countries = n_distinct(country_code)) %>%
  ggplot(aes(y = n_countries,
             x = year)) + 
  geom_bar(stat = "identity",
           fill = "yellow",
           color = "black") +
  geom_point() + 
  geom_line(linetype = 1, colour = "navy", size = 1) + 
  labs(title = "Number of countries across time")

# large increase in country coverage from 2013 to 2014; small dip in 1019

## how does the number of years covered vary per country?

df %>%
  group_by(country_name) %>%
  summarize(continent = unique(continent),
            n_years = n_distinct(year)) %>%
  ggplot(aes(y = reorder(country_name, n_years), 
         x = n_years,
         fill = continent)) + 
  geom_bar(stat = 'identity')


## how are the number of tweets per country distributed on a map?

# total number of tweets
df %>%
  group_by(country_code) %>%
  summarize(country_code = unique(country_code),
            country_name = unique(country_name),
            tweets = sum(n_tweets)) %>%
  mutate(percent = tweets / sum(tweets) * 100) %>%
  left_join(df_geo, join_by("country_code" == "iso_a2_eh")) %>%
  st_as_sf() %>%
  ggplot(aes(geometry = geometry, fill = percent)) +
  geom_sf(color = "black") + 
  scale_fill_viridis_c() + 
  labs(title = "Distribution of total number of tweets (%)")
  

# mean number of tweets
df %>%
  group_by(country_code) %>%
  summarize(country_code = unique(country_code),
            country_name = unique(country_name),
            continent = unique(continent),
            median_n_tweets = median(n_tweets)) %>%
  left_join(df_geo, join_by("country_code" == "iso_a2_eh")) %>%
  st_as_sf() %>%
  ggplot(aes(geometry = geometry, fill = median_n_tweets)) +
  geom_sf(color = "black") + 
  scale_fill_viridis_c() + 
  labs(title = "Distribution of median number of tweets across years")


# brazil, USA and Argentine has lots of data; western europe + japan + south east Asia has good coverage
# South sudan is missing from the dataset

#### distribution of tweets

# ecdf
df %>%
  group_by(country_code) %>%
  summarize(tweets = sum(n_tweets)) %>%
  mutate(percent = tweets/sum(tweets) * 100) %>%
  ggplot(aes(y = tweets)) + 
  stat_ecdf()

# top tweeters
top_tweets = df %>%
  group_by(country_name) %>%
  summarize(tweets = sum(n_tweets),
            median_tweets = median(n_tweets)) %>%
  mutate(percent = tweets/sum(tweets) * 100) %>%
  arrange(desc(percent))

top_tweets = top_tweets %>%
  mutate(rank_perc = rank(percent)) %>%
  mutate(country_name = case_when(rank_perc < max(rank_perc) - 9 ~ "other",
                                  TRUE ~ country_name)) %>%
  group_by(country_name) %>%
  summarize(percent = sum(percent)) %>%
  arrange(desc(percent))

top_tweets = top_tweets %>%
  rowwise() %>%
  mutate(labels = paste0(country_name, "\n", as.character(round(percent,2 )), " %"))

pie(x = top_tweets$percent, labels = top_tweets$labels,
    main = "top 10 countries according to number of tweets")

# USA constitutes 28% of all data points followed by brazil, japan, UK, Argentina


# histogram
df %>%
  group_by(country_code) %>%
  summarize(tweets = sum(n_tweets)) %>%
  ggplot(aes(x = tweets)) + 
  geom_histogram()

# differentiate on continent
df %>%
  group_by(country_code) %>%
  summarize(tweets = sum(n_tweets),
            continent = unique(continent)) %>%
  ggplot(aes(x = tweets)) +
  geom_histogram() + 
  facet_wrap(~continent,
             scale = "free")

# heavy right tail distribution of tweets everywhere



#### languages ######

# distribution of languages across macroarea
df_language %>%
  group_by(area) %>%
  summarize(langs = n()) %>%
  ggplot(aes(y = langs, x = area, fill = area)) + 
  geom_bar(stat = "identity") + 
  geom_label(aes(label = as.character(langs))) + 
  labs(title = "Distribution of languages across macroarea")

# 2 african languages, 63 Eurasian languages, 3 North American languages, 2 Papunesian languages
# no Australian and South American languages

# distribution of tweets across macroarea
df %>%
  group_by(area) %>%
  summarize(tweets = sum(n_tweets)) %>%
  ggplot(aes(y = tweets, x = area, fill = area)) + 
  geom_bar(stat = "identity") + 
  geom_label(aes(label = as.character(tweets))) + 
  labs(title = "Distribution of tweets across macroarea")

# distribution of langs across threat level
df_language %>%
  group_by(aes) %>%
  summarize(langs = n()) %>%
  ggplot(aes(y = langs, x = as.factor(aes), fill = as.factor(aes))) +
  geom_bar(stat = "identity") + 
  geom_label(aes(label = as.character(langs))) + 
  labs(x = "threat") +
  theme(legend.position = "none") + 
  labs(title = "Languages present according to threat level")


# distribution of tweets across threat level
df %>%
  group_by(aes) %>%
  summarize(tweets = sum(n_tweets)) %>%
  ggplot(aes(y = tweets, x = as.factor(aes), fill = as.factor(aes))) + 
  geom_bar(stat = "identity") + 
  geom_label(aes(label = as.character(tweets))) + 
  labs(title = "Distribution of tweets across threat level",
       x = "threat")

# almost all tweets in non threatened languages

# distribution of languages across language family
df_language %>%
  distinct(family) %>%
  nrow() # 15 different language families

df_language %>%
  group_by(family) %>%
  summarize(languages = n()) %>%
  mutate(percent = languages / sum(languages) * 100) %>%
  ggplot(aes(y = percent, x = family, fill = family)) + 
  geom_bar(stat = "identity") + 
  geom_label(aes(label = as.character(languages))) + 
  theme(legend.position = "none") + 
  labs(title = "Distribution of languages across language families")

# mostly indo european langauges, but quite a lot of language families represented

df %>%
  group_by(family) %>%
  summarize(tweets = sum(n_tweets)) %>%
  mutate(percent = tweets / sum(tweets) * 100) %>%
  ggplot(aes(y = percent, x = family, fill = family)) + 
  geom_bar(stat = "identity") + 
  geom_label(aes(label = as.character(tweets))) + 
  theme(legend.position = "none") + 
  labs(title = "Distribution of tweets across language families")

# 80 percent of tweets are indo european

##### distribution across languages

df %>%
  group_by(language_name) %>%
  summarize(tweets = sum(n_tweets)) %>%
  mutate(percent = tweets / sum(tweets) * 100) %>%
  ggplot(aes(x = percent,
             y = reorder(language_name, percent))) + 
  geom_bar(stat = 'identity') +
  geom_label_repel(aes(label = as.character(tweets)))

df %>%
  group_by(language_name) %>%
  summarize(tweets = sum(n_tweets)) %>%
  mutate(percent = tweets / sum(tweets) * 100) %>%
  arrange(desc(tweets))

# 41.1 percent of tweets are in english

# pie chart
top_tweets = df %>%
  group_by(language_name) %>%
  summarize(tweets = sum(n_tweets)) %>%
  mutate(percent = tweets/sum(tweets) * 100) %>%
  mutate(rank_perc = rank(percent)) %>%
  mutate(language_name = case_when(rank_perc < max(rank_perc) - 9 ~ "other",
                                  TRUE ~ language_name)) %>%
  group_by(language_name) %>%
  summarize(percent = sum(percent)) %>%
  arrange(desc(percent))

top_tweets = top_tweets %>%
  rowwise() %>%
  mutate(labels = paste0(language_name, "\n", as.character(round(percent,2 )), " %"))

pie(x = top_tweets$percent, labels = top_tweets$labels,
    main = "Top 10 most tweeted languages across the world")

top_langs_pie(country = "world", years = 2016)

### how does the distribution vary per year?

# bar plot
df %>%
  group_by(year, language_name) %>%
  summarize(tweets = sum(n_tweets)) %>%
  mutate(percent = tweets / sum(tweets) * 100) %>%
  group_by(year) %>%
  mutate(ranking = rank(-percent)) %>%
  mutate(top_10 = case_when(ranking < 11 ~ language_name,
                            TRUE ~ "other")) %>%
  group_by(top_10, year) %>%
  summarize(percent = sum(percent)) %>%
  ggplot(aes(y = percent, x = year,  fill = top_10)) + 
  geom_bar(stat = 'identity',
           position = 'stack',
           color = "black") + 
  labs(title = "distribution of languages on twitter across years")

# line plot
df_line = df %>%
  group_by(year, language_name) %>%
  summarize(n_tweets = sum(n_tweets)) %>%
  mutate(percent = n_tweets / sum(n_tweets) * 100)

labs_df = df_line %>%
  group_by(language_name) %>%
  filter(percent == max(percent)) %>%
  summarize(max_percent = max(percent),
            year_max = first(year))

df_line %>%
  ggplot(aes(y = percent, x = year, fill = language_name, color = language_name)) +
  geom_point() + 
  geom_line() +
  geom_label_repel(data = labs_df, aes(x = year_max, y = max_percent, label = language_name, color = language_name),
                   inherit.aes = FALSE) + 
  theme(legend.position = "none") + 
  labs(title = "timeseries of the distribution of tweets globally")

### map of language distribution
data = df %>%
  group_by(language_code) %>%
  summarize(language_code = unique(language_code),
            language_name = unique(language_name),
            latitude = unique(latitude),
            longitude = unique(longitude),
            tweets = sum(n_tweets)) %>%
  mutate(percent = tweets / sum(tweets) * 100) %>%
  mutate(longitude = ifelse(longitude > 180, longitude - 360, longitude))

df_geo %>%
  ggplot() +
  geom_sf(color = "black") + 
  geom_point(data = data,
              aes(y = latitude, x = longitude,
                  size = tweets, color = language_name))


data = df %>%
  group_by(language_code) %>%
  summarize(language_code = unique(language_code),
            language_name = unique(language_name),
            latitude = unique(latitude),
            longitude = unique(longitude),
            tweets = sum(n_tweets)) %>%
  mutate(percent = tweets / sum(tweets) * 100) %>%
  mutate(longitude = ifelse(longitude > 180, longitude - 360, longitude))


df_geo %>%
  ggplot() +
  geom_sf(color = "black") + 
  geom_point(data = data,
             aes(y = latitude, x = longitude,
                 size = tweets, color = language_name))



#### colour country according to the most dominant twitter language
df %>%
  group_by(country_code, country_name, language_name) %>%
  summarize(tweets = sum(n_tweets)) %>%
  mutate(percent = tweets / sum(tweets) * 100) %>%
  group_by(country_code) %>%
  mutate(ranking = rank(-percent)) %>% 
  filter(ranking < 2) %>%
  left_join(df_geo, join_by("country_code" == "iso_a2_eh")) %>%
  ggplot(aes(geometry = geometry, fill = language_name, alpha = percent)) + 
  geom_sf(color = "black") + 
  labs(title = "most common twitter language per country, percent") +
  geom_label_repel(aes(x = mid_long, y = mid_lat, label = language_name))


# bar plot showing the number of countries where the language is dominant
df %>%
  group_by(country_code, language_name) %>%
  summarize(tweets = sum(n_tweets),
            family = unique(family)) %>%
  mutate(percent = tweets / sum(tweets) * 100) %>%
  group_by(country_code) %>%
  mutate(ranking = rank(-percent)) %>% 
  filter(ranking < 2) %>%
  group_by(language_name) %>%
  summarize(n_countries = n(),
            family = unique(family)) %>%
  ggplot(aes(x = n_countries, y = reorder(language_name, n_countries), fill = family)) + 
  geom_bar(stat = 'identity') +
  labs(x = "# countries, most common",
       y = "language",
       title = "most commonly occuring dominant language") + 
  geom_label(aes(label = n_countries))

### second most dominant twitter language
df %>%
  group_by(country_code, language_name) %>%
  summarize(tweets = sum(n_tweets)) %>%
  mutate(percent = tweets / sum(tweets) * 100) %>%
  group_by(country_code) %>%
  mutate(ranking = rank(-percent)) %>%
  filter(ranking > 1 & ranking < 3) %>%
  left_join(df_geo, join_by("country_code" == "iso_a2_eh")) %>%
  ggplot(aes(geometry = geometry, fill = language_name, alpha = percent)) + 
  geom_sf(color = "black") + 
  labs(title = "Second most common twitter language per country")

df %>%
  group_by(country_code, language_name) %>%
  summarize(tweets = sum(n_tweets),
            family = unique(family)) %>%
  mutate(percent = tweets / sum(tweets) * 100) %>%
  group_by(country_code) %>%
  mutate(ranking = rank(-percent)) %>% 
  filter(ranking > 1 & ranking < 3) %>%
  group_by(language_name) %>%
  summarize(n_countries = n(),
            family = unique(family)) %>%
  ggplot(aes(x = n_countries, y = reorder(language_name, n_countries), fill = family)) + 
  geom_bar(stat = 'identity') +
  labs(x = "# countries, second most common",
       y = "language") + 
  geom_label(aes(label = n_countries))




###### functions ########

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
           fill = aggregate) + 
      scale_fill_gradient2(low = "white",
                          high = muted("blue"))
  }
  
}

n_tweets_map(df, aggregate = "sum", continent = "Asia", year = 2021, labels = FALSE)

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

most_common_tweets_map(years = 2012:2021, continent = "Africa", labels = TRUE)
top_langs_pie("Angola")


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
                inherit.aes = FALSE) 
      
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

language_distribution_bar(country = "world")
language_distribution_bar(country = c("Russia", "Ukraine"))
language_distribution_bar(country = c("Morocco", "Finland"))


language_distribution_line = function(data = df, country = "world", langs = "all",  years = min(data$year):max(data$year)){
  # this function takes a country or vector of countries and range of years and plots the percentage of tweet
  # for each language as a time series
  
  
  # add possibility to specify languages
  
  
  
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

language_distribution_line(country = "world")
language_distribution_line(country = c("Angola"))



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

top_langs_pie(country = "world", years = c(2012,2021))
top_langs_pie(country = "China")

language_distribution_line(country = "China")
# create a function that allows us to specify a range of years and a language and get
# a map coloured according to its presence

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
    scale_fill_viridis_c() + 
    labs(title = paste0("Distribution of ", lang, " as percent of tweets ", min(years), "-", max(years))) + 
    scale_fill_gradient2(low = "white",
                         high = muted("blue"))

}

lang_distribution_map(lang = "Japanese")

# to do: create a Dashboard to allow for interaction with the data

# to do: generate diversity measures.

