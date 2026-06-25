library(tidyverse)
source("functions.R")
library(dendextend)
library(MASS)
library(plotly)


# read data
sim_m = read_rds("data/final_dataset/lexical_similarity_matrix.rds")

langs = df_language$language_name
colnames(sim_m) = langs
rownames(sim_m) = langs

# clustering
d = as.dist(1-sim_m)
hc = hclust(d, method = "complete")

plot(hc)


### MDS

get_elbow = function(d){
  stress_values = c()
  for (k in 1:10) {
    mds_result = isoMDS(d, k = k)
    stress_values = c(stress_values, mds_result$stress)
  }
  # Plot stress vs dimensions
  plot(1:10, stress_values, type = "b",
       xlab = "Number of Dimensions (k)",
       ylab = "Stress",
       main = "Elbow Method for isoMDS")
}


# k = 2
get_elbow(d)

mds = isoMDS(d, k = 2)
mds_points = mds$points %>%
  as.data.frame()

mds_stress = mds$stress
print(mds_stress)

colnames(mds_points) = c("x", "y")
mds_points$ISO6393 = rownames(mds_points)
mds_points = mds_points %>%
  left_join(df_language, join_by(ISO6393))
  

mds_points %>%
  ggplot(aes(y = y, x = x, fill = family, color = family)) + 
  geom_point() + 
  geom_text_repel(aes(label = language_name))

# k = 3
mds_3 = isoMDS(d, k = 3)
mds_points_3 = mds_3$points %>%
  as.data.frame()

mds_3$stress

colnames(mds_points_3) = c("x", "y", "z")
mds_points_3$ISO6393 = rownames(mds_points_3)
mds_points_3 = mds_points_3 %>%
  left_join(df_language, join_by(ISO6393))


plot_ly(
  data = mds_points_3,
  x = ~x, y = ~y, z = ~z,
  type = "scatter3d",
  mode = "markers+text",
  color = ~family,                     # color by family
  colors = "Set1",                     # or any palette
  text = ~language_name,               # hover label
  textposition = "top center",         # similar to geom_text_repel
  marker = list(size = 8, opacity = 0.8)
) %>%
  layout(
    title = "MDS Plot (Plotly)",
    xaxis = list(title = "Dimension 1"),
    yaxis = list(title = "Dimension 2"),
    zaxis = list(title = "Dimension 3")
  )
