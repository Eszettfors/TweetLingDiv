library(tidyverse)
library(lingtypology)

# this script adds language meta data from glottolog

# read UNT data
df_lang = read_delim("data/processed/language_data.csv", 
                            delim = ";", escape_double = FALSE, trim_ws = TRUE)

# read glottolog data
df_glotto = read_csv("data/raw/glottolog.csv")


# extract iso, area, classification, family, lat, long
df_glotto = df_glotto %>%
  select(!c(med, medovertime, bib, category, level, ID, subclassification))


# langs not in glottolog?
df_lang %>%
  filter(!ISO6393 %in% df_glotto$ISO6393)


# join glotto with UNT data
df_full = df_glotto %>%
  right_join(df_lang, join_by(ISO6393)) %>%
  select(!language)

# adjust column name
df_full = df_full %>%
  rename("language_code" = ISO6391)

# two cases of duplicates -> he and in; these should be hebrew and ID
# -> remove
df_full = df_full %>% 
  filter(!language_code %in% c("he", "in"))

# check NA
colSums(is.na(df_full))

# add basque as family
df_full = df_full %>%
  mutate(family = case_when(ISO6393 == "eus" ~ "Basque",
                            TRUE ~ family))

# fix missing aes for pushto; adjust aes of serbo-croatian languages and polish
df_full %>%
  filter(is.na(aes))
df_full = df_full %>%
  mutate(aes = case_when(ISO6393 == "pus" ~ 1,
                         ISO6393 %in% c("hrv", "bos", "srp", "pol") ~ 1,
                         TRUE ~ aes))

# check NA
colSums(is.na(df_full))

# adjust column placement
df_full = df_full %>%
  rename("latitude" = Latitude,
         "longitude" = Longitude)

df_full = df_full %>%
  relocate(language_code, language_name, ISO6393, macroarea, glottocode, latitude, longitude, aes, classification, family)

df_full %>%
  filter(ISO6393 == "lav")
# export data
write_csv(df_full, "data/final_dataset/language_data.csv")

