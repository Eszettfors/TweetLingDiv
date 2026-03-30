library(tidyverse)
source("dataset_creation/diversity_measures/asjp_get_similarity.R")
##### this script generates a similarity matrix with lexical similarity between all languages in the twitter data

# read data
df_language = read_csv("data/final_dataset/language_data.csv")

# extract ISO codes
langs = df_language %>%
  distinct(ISO6393) %>%
  pull()

# generate matrix
sim_m = get_ldn_sim_matrix(langs)

# write matrix as rds
write_rds(sim_m, "data/final_dataset/lexical_similarity_matrix.rds")
