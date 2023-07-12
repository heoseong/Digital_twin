##Fig.4A

library(dplyr)
library(tidyverse)
library(ggplot2)
library(gridExtra)

soil <- read.csv("soil_selected_farm.csv", header = T, fileEncoding = "UTF-8")
substr(soil$farm_id, 1, 1) <- toupper(substr(soil$farm_id, 1, 1))
soil <- soil |> filter(farm_id %in% c("Hab", "Iab"))  


ac <- ggplot(soil, aes(x=farm_id, y=acid, color=farm_id, fill=farm_id)) +
  geom_bar(stat="identity", alpha=0.9) +
  theme_bw() + ylim(0, 8) +
  xlab("") + ylab("pH (1:5)") +
  annotate(geom = "rect",
           xmin = 0.0,
           xmax = 3,
           ymin = 6,
           ymax = 7,
           fill = "grey",
           alpha = 0.5,
           colour = "grey",
           lty = 2) +
  theme(legend.position = "none", plot.title = element_text(size=8),
        legend.key = element_rect(colour="transparent", fill="transparent"),
        legend.background=element_rect(fill= alpha("white", 0.5)))

ph <- ggplot(soil, aes(x=farm_id, y=phosphate, color=farm_id, fill=farm_id)) +
  geom_bar(stat="identity", alpha=0.9) +
  theme_bw() + ylim(0, 300) +
  xlab("") + ylab("Available phosphate (mg/kg)") +
  annotate(geom = "rect",
           xmin = 0.0,
           xmax = 3.0,
           ymin = 200,
           ymax = 300,
           fill = "grey",
           alpha = 0.5,
           colour = "grey",
           lty = 2) +
  theme(legend.position = "none", legend.title = element_blank(), plot.title = element_text(size=8),
        legend.key = element_rect(colour="transparent", fill="transparent"),
        legend.background=element_rect(fill= alpha("white", 0.5)))

om <- ggplot(soil, aes(x=farm_id, y=OM, color=farm_id, fill=farm_id)) +
  geom_bar(stat="identity", alpha=0.9) +
  theme_bw() + ylim(0, 200) +
  xlab("") + ylab("Organic matter (g/kg)") +
  annotate(geom = "rect",
           xmin = 0.0,
           xmax = 3.0,
           ymin = 20,
           ymax = 30,
           fill = "grey",
           alpha = 0.5,
           colour = "grey",
           lty = 2) +
  theme(legend.position = "none", plot.title = element_text(size=8),
        legend.key = element_rect(colour="transparent", fill="transparent"),
        legend.background=element_rect(fill= alpha("white", 0.5)))

kk <- ggplot(soil, aes(x=farm_id, y=K, color=farm_id, fill=farm_id)) +
  geom_bar(stat="identity", alpha=0.9) +
  theme_bw() + ylim(0, 1) +
  xlab("") + ylab(expression(Exchangeable~potassium~(cmol[c]/kg)))+
  annotate(geom = "rect",
           xmin = 0.0,
           xmax = 3.0,
           ymin = 0.5,
           ymax = 0.8,
           fill = "grey",
           alpha = 0.5,
           colour = "grey",
           lty = 2) +
  theme(legend.position = "none", plot.title = element_text(size=8),
        legend.key = element_rect(colour="transparent", fill="transparent"),
        legend.background=element_rect(fill= alpha("white", 0.5)))

ca <- ggplot(soil, aes(x=farm_id, y=Ca, color=farm_id, fill=farm_id)) +
  geom_bar(stat="identity", alpha=0.9) +
  theme_bw() + ylim(0, 11) +
  xlab("") + ylab(expression(Exchangeable~calsium~(cmol[c]/kg))) +
  annotate(geom = "rect",
           xmin = 0.0,
           xmax = 3.0,
           ymin = 5,
           ymax = 6,
           fill = "grey",
           alpha = 0.5,
           colour = "grey",
           lty = 2) +
  theme(legend.position = "none", plot.title = element_text(size=8),
        legend.key = element_rect(colour="transparent", fill="transparent"),
        legend.background=element_rect(fill= alpha("white", 0.5)))

mg <- ggplot(soil, aes(x=farm_id, y=Mg, color=farm_id, fill=farm_id)) +
  geom_bar(stat="identity", alpha=0.9) +
  theme_bw() +
  xlab("Farm") + ylab(expression(Exchangeable~magnesium~(cmol[c]/kg))) +
  annotate(geom = "rect",
           xmin = 0.0,
           xmax = 3.0,
           ymin = 1.5,
           ymax = 2,
           fill = "grey",
           alpha = 0.5,
           colour = "grey",
           lty = 2) +
  theme(legend.position = "none", plot.title = element_text(size=8),
        legend.key = element_rect(colour="transparent", fill="transparent"),
        legend.background=element_rect(fill= alpha("white", 0.5)))

ec <- ggplot(soil, aes(x=farm_id, y=EC, color=farm_id, fill=farm_id)) +
  geom_bar(stat="identity", alpha=0.9) +
  theme_bw() +
  xlab("") + ylab("Electrical conductivity (dS/m)") +
  annotate(geom = "rect",
           xmin = 0.0,
           xmax = 3.0,
           ymin = 1.5,
           ymax = 2,
           fill = "grey",
           alpha = 0.5,
           colour = "grey",
           lty = 2) +
  theme(legend.position = "none", plot.title = element_text(size=8),
        legend.key = element_rect(colour="transparent", fill="transparent"),
        legend.background=element_rect(fill= alpha("white", 0.5)))


grid.arrange(ph, kk, ca, mg, ac, om, ec, nrow=1)
