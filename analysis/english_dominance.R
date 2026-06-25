library(DescTools)
library(scales)
source("functions.R")
library(lme4)

# this script seeks to assess if the dominance of english on twitter has been going down.

language_distribution_line()


df_eng = df %>%
  group_by(year, country_code, country_name, language_name) %>%
  summarize(n_tweets = sum(n_tweets)) %>%
  mutate(percent = n_tweets / sum(n_tweets) * 100) %>%
  mutate(n_tweets_tot = sum(n_tweets)) %>%
  filter(language_name == "English")

# look at change since 2016
df_eng = df_eng %>%
  filter(year > 2015)

# filter countries and years with very few tweets
df_eng = df_eng %>%
  filter(n_tweets_tot > 100)

Desc(df_eng$n_tweets_tot)
Desc(df_eng$percent)

#
df_eng %>%
  group_by(country_name) %>%
  summarize(n = n()) %>%
  ggplot(aes(x = n)) +
  geom_histogram()



# if only one data point, nothing can be done
filtered_countries = df_eng %>%
  group_by(country_name) %>%
  summarize(n = n()) %>%
  filter(n %in% c(1,2)) %>% pull(country_name)

df_eng = df_eng %>%
  filter(!country_name %in% filtered_countries)

# metric: delta_eng = difference in percent between 2016 and 2021

df_eng_change = df_eng %>%
  filter(year %in% c(2016, 2021)) %>%
  group_by(country_code, country_name) %>%
  arrange(year) %>%
  summarize(change_percent_units = percent - lag(percent),
            change_percent_raito = percent / lag(percent)) %>%
  filter(!is.na(change_percent_units))

# map showing change in percentage of English Tweets from 2016 to 2021
df_eng_change %>%
  right_join(df_geo, join_by("country_code" == "iso_a2_eh")) %>%
  st_as_sf() %>%
  ggplot(aes(geometry = geometry, fill = change_percent_units)) + 
  geom_sf(color = "black") + 
  scale_fill_gradient2(low = muted("red"),
                       mid = "white",
                       high = muted("blue")) + 
  labs(title = "change in percentage points of english between 2016 and 2021")

language_distribution_line(country = "India")
# df_eng_change

Desc(df_eng_change$change_percent_units) # left tail
Desc(df_eng_change$change_percent_raito)

t.test(df_eng_change$change_percent_units)
wilcox.test(df_eng_change$change_percent_units)


df_eng %>%
  ggplot(aes(y = percent, x = factor(year))) + 
  geom_boxplot() + 
  geom_point(aes(color = country_name)) + 
  theme(legend.position = "none")

df_eng %>%
  filter(country_name %in% c("Sweden", "Morocco", "Austria", "India", "Japan")) %>%
  ggplot(aes(y = percent, x = year)) + 
  geom_point() + 
  geom_smooth(method = "lm", aes(fill = country_name))

df_eng %>%
  ggplot(aes(y = percent, x = year)) + 
  geom_point() +
  geom_smooth(method = "lm")

# global model
mdl_global = lm(data = df_eng, log(percent) ~ year)
summary(mdl_global)
res = resid(mdl_global)
qqnorm(res)
qqline(res)

# estimate -0.076; the log percentage points of english goes down with 0.076 per year
mdl_global_z = lm(data = df_eng, scale(log(percent)) ~ scale(year))
summary(mdl_global_z) # 0.12 -> weak effect

# one model for each country
df_mdl = df_eng %>%
  group_by(country_name) %>%
  summarize(estimate = lm(percent ~ year)$coefficients[2],
            std_error = summary(lm(percent ~ year))$coefficients[2,2],
            n = n())

# mean estimate

Desc(df_mdl$estimate)

MeanCI(df_mdl$estimate) # clearly a negative effect between -0.86 and -1.61 percent points per year

# weighted mean by inverse standard error. If standard error is large, the inverse
# is small -> less impact on the mean

weighted_mean = function(est, se){
  return(sum(est * (1/se^2)) / sum((1/se^2)))
}

weighted_mean(df_mdl$estimate, df_mdl$std_error) # -0.28 is the weighted mean, still a negative effect. 
Desc(df_mdl$estimate)
