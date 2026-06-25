library(tidyverse)
library(rnaturalearth)
library(rnaturalearthdata)

# this script adds meta data to the countries

# read data
df_country = read_csv("data/processed/country_data.csv")
df_geo = ne_countries(scale = "large")

# fix namibia
df_country = df_country %>%
  mutate(country_code = case_when(is.na(country_code) ~ "NA",
                                  TRUE ~ country_code))


# extract relevant data
df_geo = df_geo %>%
  select(iso_a2_eh, iso_a3_eh, name, continent, region_wb, income_grp) %>%
  as_tibble() %>%
  select(!geometry) %>%
  rename("country_code" = iso_a2_eh,
         "country_name" = name)


# any countries not in df_geo?
df_geo %>%
  filter(country_code == "FR")
df_country %>%
  filter(!country_code %in% df_geo$country_code)


# fix manually:
# BQ -> belongs to the netherlands
# CX -> Belongs to australia
# CC -> belongs to Australia
# RE -> belongs to France
# YT -> belongs to France
# GP -> belongs to France
# BV -> belongs to Norway
# GF -> belongs to France
# MQ -> belongs to France

# solution: remove from country dataframe and merge the respective data in the UNT dataframe
df_country = df_country %>%
  filter(!country_code %in% c("RE", "YT", "GP", "MQ", "BV", "GF", "BQ", "CX", "CC"))

# read UNT dataset
df_UNT = read_csv("data/processed/n_tweets_year_country.csv")

# change language codes for territory
df_UNT = df_UNT %>%
  mutate(country = case_when(country %in% c("RE",
                                            "YT",
                                            "GP",
                                            "GF",
                                            "MQ") ~ "FR",
                                  country %in% c("CX",
                                                 "CC") ~ "AU",
                                  country == "BQ" ~ "NL",
                                  country == "BV" ~ "NO",
                                  TRUE ~ country))


# merge data
df_UNT = df_UNT %>%
  group_by(country,
           year,
           language) %>%
  summarize(n_tweets = sum(n_tweets))  %>%
  ungroup()

# multiple entries for country code in df geo -> fix
df_geo %>%
  count(country_code) %>%
  filter(n > 1)

# grab the first one -> corresponds to correct entry
df_geo = df_geo %>%
  group_by(country_code, iso_a3_eh) %>%
  summarize(country_name = first(country_name),
            continent = first(continent),
            region_wb = first(region_wb),
            income_grp = first(income_grp))

# add data from df_geo
df_country = df_country %>%
  left_join(df_geo, join_by(country_code))


# fix continents (seven seas)
df_country %>%
  filter(continent %in% c("Seven seas (open ocean)", "Antarctica"))

df_country = df_country %>%
  mutate(continent = case_when(country_code == "SH" ~ "Africa",
                   country_code == "MV" ~ "Asia",
                   country_code == "TF" ~ "Antarctica",
                   country_code == "HM" ~ "Antarctica",
                   country_code == "MU" ~ "Africa",
                   country_code == "GS" ~ "South America",
                   country_code == "IO" ~ "Asia",
                   country_code == "SC" ~ "Africa",
                   TRUE ~ continent))

df_country = df_country %>%
  rename("country_code_a3" = iso_a3_eh)

# adjust column naming
df_UNT = df_UNT %>%
  rename("country_code" = country,
         "language_code" = language)


# add ISOcodes
ISO = read_csv("data/final_dataset/language_data.csv")

df_UNT = df_UNT %>%
  left_join(ISO %>% select(language_code, ISO6393), join_by(language_code))

# export
write_csv(df_country, "data/final_dataset/country_data.csv")
write_csv(df_UNT, "data/final_dataset/n_tweets_country_year_language.csv")
