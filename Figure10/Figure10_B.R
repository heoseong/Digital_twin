##Fig.5B
rm(list=ls())

library(dplyr)
library(tidyverse)
library(lubridate)
library(ggplot2)
library(gridExtra)
library(hrbrthemes)
library(GGally)
library(viridis)
library(nord)

fruit <- read.csv("citrus_fruit_data_3_replicate.csv", encoding = "UTF-8")
fruit$monthweek <- paste(fruit$researched_month, fruit$researched_week, sep = "-")
fruit <- fruit %>% rename(tag =  tag_no)

fruit_iab <- fruit %>% filter(farm_id == "iab")
fruit_iab <- fruit_iab %>% group_by(monthweek, tag) %>% 
  summarise(brix = mean(brix),
            size = mean(size))
fruit_iab$monthweek <- factor(fruit_iab$monthweek)


iab_brix <- fruit_iab %>%
  select(tag, monthweek, brix) %>%
  drop_na() %>% 
  glimpse()
head(iab_brix)

spread_iab_brix <- iab_brix %>%
  spread(tag, brix)  %>%
  glimpse()


iab_size <- fruit_iab %>%
  select(tag, monthweek, size) %>%
  drop_na() %>% 
  glimpse()

spread_iab_size <- iab_size%>%
  spread(tag, size)  %>%
  glimpse()


# Run the hierarchical cluster analysis
cluster_iab_brix <- t(spread_iab_brix[-1])
iab_brix_dist <- dist(cluster_iab_brix, method="euclidean")  
fit <- hclust(iab_brix_dist, method="ward.D")   

cluster_iab_size <- t(spread_iab_size[-1])
iab_size_dist <- dist(cluster_iab_size, method="euclidean")  
fit2 <- hclust(iab_size_dist, method="ward.D") 


# Plot the cluster analysis
plot(fit, family="Arial")
rect.hclust(fit, k=4, border="cadetblue")

plot(fit2, family="Arial")
rect.hclust(fit2, k=4, border="cadetblue")


clustered_data <- cutree(fit, k=4)
clustered_data_tidy <- as.data.frame(as.table(clustered_data)) %>% glimpse()
colnames(clustered_data_tidy) <- c("tag","cluster")
clustered_data_tidy$tag <- as.numeric(clustered_data_tidy$tag)
glimpse(clustered_data_tidy)
table(clustered_data_tidy$cluster)

clustered_data2 <- cutree(fit2, k=4)
clustered_data_tidy2 <- as.data.frame(as.table(clustered_data2)) %>% glimpse()
colnames(clustered_data_tidy2) <- c("tag","cluster")
clustered_data_tidy2$tag <- as.numeric(clustered_data_tidy2$tag)
glimpse(clustered_data_tidy2)
table(clustered_data_tidy2$cluster)


joined_clusters <- fruit_iab %>%
  inner_join(clustered_data_tidy, by = "tag") %>%
  glimpse()

joined_clusters2 <- fruit_iab %>%
  inner_join(clustered_data_tidy2, by = "tag") %>%
  glimpse()

View(joined_clusters)
glimpse(joined_clusters)
iab_brix_joined_clusters <- joined_clusters %>%
  select(monthweek, brix, cluster) %>%
  drop_na() %>%
  glimpse()

View(joined_clusters2)
glimpse(joined_clusters2)
iab_size_joined_clusters <- joined_clusters2 %>%
  select(monthweek, size, cluster) %>%
  drop_na() %>% 
  glimpse()


aa1 <- joined_clusters |> tidyr::pivot_wider(id_cols=tag, names_from=monthweek, values_from=brix)
aa1
aaa1 <- left_join(aa1, joined_clusters, by="tag")
aaa1$cluster <- as.factor(aaa1$cluster)
head(aaa1)
names(aaa1)[9] <- c("Cluster")



bb1 <- joined_clusters2 |> tidyr::pivot_wider(id_cols=tag, names_from=monthweek, values_from=size)
bb1
bbb1 <- left_join(bb1, joined_clusters2, by="tag")
bbb1$cluster <- factor(bbb1$cluster)
head(bbb1)
names(bbb1)[9] <- c("Cluster")



isu <- ggparcoord(aaa1,
                  scale="globalminmax",
                  columns = 2:5, groupColumn = 9,
                  showPoints = TRUE, 
                  title = "") +
  geom_line(size=1) +
  geom_point(size=3) +
  theme_ipsum() +
  theme_bw() +
  theme(legend.position = c(0.9, 0.17),
        legend.key = element_rect(colour="transparent", fill="transparent"),
        legend.background=element_rect(fill= alpha("white", 0.5)),
        legend.key.size = unit(0.5, 'cm'),
        legend.key.height = unit(0.5, 'cm'),
        legend.key.width = unit(0.5, 'cm')) +
  labs(x = 'Month-week', y= 'Sugar content (°Bx)') +
  scale_y_continuous(limits = c(7, 17), breaks=seq(7, 17, 2))+
  scale_x_discrete(labels = c('10-3', '10-4', '11-1', '11-2')) +
  scale_color_brewer(palette = "Paired")
#  scale_color_manual(values = c("steelblue1", "steelblue2", "steelblue3", "steelblue4"))


isi <- ggparcoord(bbb1,
                  scale="globalminmax",
                  columns = 2:5, groupColumn = 9,
                  showPoints = TRUE, 
                  title = "") +
  geom_line(size=1) +
  geom_point(size=3) +
  theme_ipsum() +
  theme_bw() +
  theme(plot.title = element_text(size=10), legend.position = c(0.9, 0.8),
        legend.key = element_rect(colour="transparent", fill="transparent"),
        legend.background=element_rect(fill= alpha("white", 0.5)),
        legend.key.size = unit(0.5, 'cm'),
        legend.key.height = unit(0.5, 'cm'),
        legend.key.width = unit(0.5, 'cm')) +
  labs(x = 'Month-week', y= 'Fruit size (mm)', fill = "Cluster") +
  scale_y_continuous(limits = c(40, 80), breaks=seq(40, 80, 10)) +
  scale_x_discrete(labels = c('10-3', '10-4', '11-1', '11-2')) +
  scale_color_brewer(palette = "Dark2") 
#  scale_color_manual(values = c("slateblue1", "slateblue2", "slateblue3", "slateblue4"))


grid.arrange(isu, isi, nrow=1)
