library(tidyverse)
source("dataset_creation/diversity_measures/asjp_get_similarity.R")

# this script takes the twitter data and generates diversity measures for each country and year

#read twitter data
df_tweets = read_csv("data/final_dataset/n_tweets_country_year_language.csv")
df_langs = read_csv("data/final_dataset/language_data.csv")

# add ISO6393 codes to twitter data
df_tweets = df_langs %>%
  select(language_code, ISO6393, language_name) %>%
  right_join(df_tweets, join_by(language_code))

# fix namibia
df_tweets %>%
  mutate(ISO6393 = case_when(is.na(country_code) ~ "ISO6393",
                             TRUE ~ country_code))


# generate similarity measures ----------------
# pull langs from data
langs = df_tweets %>%
  distinct(ISO6393) %>%
  pull()

# generate a similarity matrix based on the languages
sim_m = get_ldn_sim_matrix(langs)

# generate diversity
# for each year, generate q = 0, q = 1 and q = 2 for each muncipality both naive and non naive

get_prop_vec = function(tweets){
  #takes a vector with tweets and turns it into a proportion vector
  prop_vec = tweets / sum(tweets)
  return(prop_vec)
}


get_richness = function(tweets){
  # takes a vector with tweets per language and calculates the richness
  tweets = tweets[tweets != 0]
  richness = length(tweets)
  return(richness)
}

get_exp_shannon = function(tweets){
  # takes a vector of proportions and calculates the exponent shannon entropy
  
  prop_vec = get_prop_vec(tweets)
  prop_vec = prop_vec[prop_vec != 0]
  log_vec = log(prop_vec)
  entropy = -sum(prop_vec*log_vec)
  return(exp(entropy))
}


get_inv_simp = function(prop_vec){
  # takes a vector of proportions and calculates the inverse simpson
  
  prop_vec = get_prop_vec(prop_vec)
  prop_vec = prop_vec[prop_vec != 0]
  squared_prop = prop_vec*prop_vec
  inv_simp = 1/sum(squared_prop)
  
  return(inv_simp)
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
  
  # proportion vector
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

div_measures = df_tweets %>%
  group_by(country_code, year) %>%
  summarize(tot_tweets = sum(n_tweets),
            richness = get_richness(n_tweets),
            exp_shannon = get_exp_shannon(n_tweets),
            inv_simpson = get_inv_simp(n_tweets),
            div_q_0 = get_diversity_q(ISO6393, n_tweets, sim_m = sim_m, q = 0),
            div_q_1 = get_diversity_q(ISO6393, n_tweets, sim_m = sim_m, q = 1),
            div_q_2 = get_diversity_q(ISO6393, n_tweets, sim_m = sim_m, q = 2))


# export
write_csv(div_measures, file = "data/final_dataset/diversity_measures.csv")


