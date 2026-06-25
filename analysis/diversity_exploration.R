library(tidyverse)
source("functions.R")
library(GGally)
library(DescTools)

# read data
df_diversity = read_csv("data/final_dataset/diversity_measures.csv")
df_diversity = df_diversity %>%
  left_join(df_country, join_by(country_code))

# fix namibia
df_diversity = df_diversity %>%
  mutate(country_code = case_when(is.na(country_code) ~ "NA",
                                  TRUE ~ country_code))

div_measures = c("richness", "exp_shannon", "inv_simpson", "div_q_0", "div_q_1", "div_q_2")

get_diversity_map(diversity_measure = richness, y = 2021)
get_diversity_map(diversity_measure = exp_shannon, y = 2021)
get_diversity_map(diversity_measure = inv_simpson, y = 2021)
get_diversity_map(diversity_measure = div_q_0, y = 2021)
get_diversity_map(diversity_measure = div_q_1, y = 2021)
get_diversity_map(diversity_measure = div_q_2, y = 2021)


top_langs_pie(country = "Morocco", year = 2021)
top_langs_tree(country = "Pakistan", year = 2021)
language_distribution_bar(country = "Senegal")
# indonesian and Tagalog are occuring everywhere; are these indication of miss classification by the Twitter algorithm or 
# perhaps the result of bots? If it is the result of misclassification; then it biases richness, and similarity calculations will be unreliable
# see e.g. Somalia and 


# apply a filter of more than 200 tweets
df_200 = df_diversity %>% filter(tot_tweets > 500)
get_diversity_map(df_200, diversity_measure = richness, y = 2021)
get_diversity_map(df_200, diversity_measure = exp_shannon, y = 2021)
get_diversity_map(df_200, diversity_measure = inv_simpson, y = 2021)
get_diversity_map(df_200, diversity_measure = div_q_0, y = 2021)
get_diversity_map(df_200, diversity_measure = div_q_1, y = 2021)
get_diversity_map(df_200, diversity_measure = div_q_2, y = 2021)

# can we somehow incorporate a confidence score more sofisticated than the number of tweets?

get_all_diversity_country_ts(country = "Finland")


get_country_comparison_country_ts(measure = div_q_2, country = c("Denmark", "Norway", "Sweden", "Finland"))

##### what are the most diverse countries? ######

# correlations
df_diversity %>%
  select(!c(country_code, country_name, continent, year)) %>%
  ggpairs()

# filter to (countries,years) with more than 200 tweets
df_diversity %>%
  filter(tot_tweets > 100) %>%
  select(!c(country_code, country_name, continent, year)) %>%
  ggpairs()
# with the filter, richness lacks tails

# more tweets are positively associated with richness
# more tweets are negatively associated with abundance weighted measures


# 
get_rank_plot = function(data = df_diversity, measure_x = richness, measure_y = exp_shannon, y = 2021, k = 20){
  # function that takes a year and two diversity measures and plots the rank changes, labeling the top k rank changers
  
  # filter year
  data = data %>%
    filter(year == y)
  
  # set up measures
  x = enquo(measure_x)
  y = enquo(measure_y)
  
  # calculate the ranks
  data = data %>%
    mutate(measure_x = rank(!!x),
           measure_y = rank(!!y)) %>%
    mutate(shifts = abs(measure_y - measure_x))
  
  # label top k rank shifts
  top_k = data %>%
    arrange(desc(shifts)) %>%
    slice_head(n = k)
  
  data %>%
    ggplot(aes(y = measure_y, x = measure_x, color = continent)) +
    geom_point() + 
    geom_abline() + 
    geom_text_repel(data = top_k, aes(label = country_name))
}

get_rank_plot()

#### deeper look at 2021:

# filter year
df_diversity_21 = df_diversity %>%
  filter(year == 2021)

# add ranks
df_diversity_21_rank = df_diversity_21 %>%
  mutate(across(div_measures, rank))


# richness
df_diversity_21_rank %>%
  arrange(-richness)

# exp shannon
df_diversity_21_rank %>%
  arrange(-exp_shannon)
# european countries and Uzbekistan as a runnerup
top_langs_tree(country = "Uzbekistan", year = 2021) # again, Indonesian occuring; Uzbek is probably turkmen classified as Turkish


get_rank_plot(measure_x = richness, measure_y = exp_shannon)


# inv_simpson
df_diversity_21_rank %>%
  arrange(-inv_simpson)
# similar; but armenia and Macao present
top_langs_tree(country = "Armenia", year = 2021)

get_rank_plot(measure_x = exp_shannon, measure_y = inv_simpson) 

# div_q_1
get_rank_plot(measure_x = exp_shannon, measure_y = div_q_1)
# european countries are perceived as less diverse: e.g. because indo european languages are somewhat similar to english
# African, North American and Oceanian countries are perceived as more diverse
top_langs_tree(country = "Poland", year = 2021)


# are countries getting more or less diverse with time?

# filter more than 200
df_diversity_ts = df_diversity %>%
  filter(tot_tweets > 100)

Desc(df_diversity_ts$richness)
df_diversity_ts %>%
  ggplot(aes(y = richness, x = as.factor(year))) + 
  geom_boxplot()
# drop in median richness for 2019; makes sense since the number of tweets are lower for 2010, and there is an association
# between number of tweets and richness

Desc(df_diversity_ts$exp_shannon)







