library(tidyverse)


# this script processes the country data files and joins them into a single dataframe which is then exported as a csv


# a data frame to hold the data
df = data.frame(country = as.character(), year = as.double(), language = as.character(), n_tweets = as.double())


# access files
path = "data/country_data"
files = list.files(path)


# read and merge files
for (file in files){
  country_data = read_csv(paste(path, file, sep = "/"))
  df = rbind(df, country_data)
}

# filter tweets with 0 entries
df = df %>%
  filter(n_tweets != 0)


# merge id and in (indonesian) and he and iw (hebrew)
df = df %>%
  mutate(language = case_when(language == "in" ~ "id",
                   language == "he" ~ "iw",
                   TRUE ~ language))

# merge the number of tweets in cases of cooccurance
df = df %>%
  group_by(country, year, language) %>%
  summarize(n_tweets = sum(n_tweets))


# export csv
write_csv(df, "data/processed/n_tweets_year_country.csv")
