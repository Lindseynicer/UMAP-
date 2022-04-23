setwd("R_UMAP")

# description
# https://towardsdatascience.com/umap-dimensionality-reduction-an-incredibly-robust-machine-learning-algorithm-b5acb01de568

################################################################################
# Plot umap (easy way)
###############################################################################
head(iris, 3)

iris.data = iris[, grep("Sepal|Petal", colnames(iris))]
iris.labels = iris[, "Species"]

# https://rdrr.io/github/ropenscilabs/umapr/f/README.md
library(umapr)
library(tidyverse)

iris.umap = umap(iris.data)
head(iris.umap) 

# plot the result
iris.umap %>% 
  mutate(Species = iris$Species) %>%
  ggplot(aes(UMAP1, UMAP2, color = Species)) + geom_point()

##############################################################################
# Explore the hyperparameters of umap
##############################################################################
# Function parameters
# 1. n_neighbor = Find the nearest neighbors
# low = local structure, high = global structure

neighbors <- c(4, 8, 16, 32, 64, 128)

neighbors %>% 
  map_df(~umap(as.matrix(iris[,1:4]), n_neighbors = .x) %>% 
           mutate(Species = iris$Species, Neighbor = .x)) %>% 
  mutate(Neighbor = as.integer(Neighbor)) %>% 
  ggplot(aes(UMAP1, UMAP2, color = Species)) + 
  geom_point() + 
  facet_wrap(~ Neighbor, scales = "free")

# 2. Finding a low-dimensional representation
# min_dist = min distance btw embedded points
# low = overlapping points, higher = distance btw points
dists <- c(0.001, 0.01, 0.05, 0.1, 0.5, 0.99)

dists %>% 
  map_df(~umap(as.matrix(iris[,1:4]), min_dist = .x) %>% 
           mutate(Species = iris$Species, Distance = .x)) %>% 
  ggplot(aes(UMAP1, UMAP2, color = Species)) + 
  geom_point() + 
  facet_wrap(~ Distance, scales = "free")

# distance
dists <- c("euclidean", "manhattan", "canberra", "cosine", "hamming", "dice")

dists %>% 
  map_df(~umap(as.matrix(iris[,1:4]), metric = .x) %>% 
           mutate(Species = iris$Species, Metric = .x)) %>% 
  ggplot(aes(UMAP1, UMAP2, color = Species)) + 
  geom_point() + 
  facet_wrap(~ Metric, scales = "free")

# n_epochs
n_epochs <- as.integer(c(20, 50, 100, 200, 500, 800, 1000))

n_epochs %>% 
  map_df(~umap(as.matrix(iris[,1:4]), n_epochs = .x) %>% 
           mutate(Species = iris$Species, n_epochs = .x)) %>% 
  ggplot(aes(UMAP1, UMAP2, color = Species)) + 
  geom_point() + 
  facet_wrap(~ n_epochs, scales = "free")

################################################################################
# Interactive UMAP plot
###############################################################################
# https://plotly.com/r/t-sne-and-umap-projections/
library(plotly) 
library(umap) 
iris.data = iris[, grep("Sepal|Petal", colnames(iris))] 
iris.labels = iris[, "Species"] 
iris.umap = umap(iris.data, n_components = 2, random_state = 15) 
layout <- iris.umap[["layout"]] 
layout <- data.frame(layout) 
final <- cbind(layout, iris$Species) 

fig <- plot_ly(final, x = ~X1, y = ~X2, color = ~iris$Species, colors = c('#636EFA','#EF553B','#00CC96'), type = 'scatter', mode = 'markers')%>%  
  layout(
    plot_bgcolor = "#e5ecf6",
    legend=list(title=list(text='species')), 
    xaxis = list( 
      title = "0"),  
    yaxis = list( 
      title = "1")) 

iris.umap = umap(iris.data, n_components = 3, random_state = 15) 
layout <- iris.umap[["layout"]] 
layout <- data.frame(layout) 
final <- cbind(layout, iris$Species) 

fig2 <- plot_ly(final, x = ~X1, y = ~X2, z = ~X3, color = ~iris$Species, colors = c('#636EFA','#EF553B','#00CC96')) 
fig2 <- fig2 %>% add_markers() 
fig2 <- fig2 %>% layout(scene = list(xaxis = list(title = '0'), 
                                     yaxis = list(title = '1'), 
                                     zaxis = list(title = '2'))) 

fig 
fig2





##############################################################################
# Animation UMAP
##############################################################################
# https://gist.github.com/JEFworks/343d704e26da1d1138be6bb8eb400c6c 