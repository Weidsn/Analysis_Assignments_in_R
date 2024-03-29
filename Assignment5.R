# Assignment 5

## 



df_mammalSleep <- read.csv("mammal_Sleep.csv") |> 
  select(sleep_total, sleep_rem) |> 
  drop_na() |> 
  as.data.frame()

# Scale the data
mammalSleep <- scale(df_mammalSleep)

# mammalSleep2 <- read.csv("mammal_Sleep.csv") |> 
#   select(sleep_total, sleep_rem) |> 
#   scale() |> 
#   as.data.frame() |> 
#   mutate(sleep_rem = replace_na(sleep_rem, 0))

fviz_nbclust(mammalSleep,kmeans,method="wss")
fviz_nbclust(mammalSleep,kmeans,method="silhouette")
fviz_nbclust(mammalSleep,kmeans,method="gap_stat")

km_mammalSleep <- kmeans(mammalSleep, centers=4, nstart=20)
autoplot(km_mammalSleep, mammalSleep)


fviz_cluster(list(data = mammalSleep, 
                  cluster = km_mammalSleep$cluster))

df_mammalSleep$cluster <- km_mammalSleep$cluster

fviz_cluster(list(data = mammalSleep, 
                  cluster = km_mammalSleep$cluster),
             geom = c("point")) +
  geom_point(size=1, alpha = 0.4)

view(mammalSleep)

center_mammal <- km_mammalSleep$centers

km_mammalSleep |> 
  glimpse()

df_mammalSleep |> 
  ggplot(aes(sleep_total, sleep_rem, color=factor(cluster)))+
  geom_point()+
  labs(color="Cluster")

# 1. Load the dataset mammal_Sleep.csv into a data frame.
# 2. Create a new data frame mammalSleep with sleep_total and sleep_rem.
# 3. Fit a k-means clustering model with 4 clusters to the data subset mammalSleep.
# 4. Find the centroids of the clusters in the model on the chart 
# (If you can’t figure how to it using fviz_cluster function, 
# you can use plot() or geom_point().

#    Instructions
#    •	Create a markdown file and rename it as Assignment5.rmd
#    •	Load the required packages and clean the data (if required)
#    •	Split the dataset and train the model
#    •	Test the model and predict the values required in each question
#    •	Attach the knit file (the one with .html extension) on Brightspace (d2l)
#    