##Fig.4C

library(dplyr)
library(tidyverse)
library(ggplot2)
library(gridExtra)

env <- read.csv("env_raw_merge.csv", encoding = "UTF-8")
substr(env$farm_id, 1, 1) <- toupper(substr(env$farm_id, 1, 1))

environ <- env |> 
  filter(farm_id %in% c("Hab", "Iab")) |> 
  filter(monthweek %in% c("11-2", "11-3", "11-4", "11-5"))
head(environ)

environ$monthweek <- str_replace(environ$monthweek, "11-2", "2-Nov")
environ$monthweek <- str_replace(environ$monthweek, "11-3", "3-Nov")
environ$monthweek <- str_replace(environ$monthweek, "11-4", "4-Nov")
environ$monthweek <- str_replace(environ$monthweek, "11-5", "5-Nov")

sunshine <- environ |> 
  group_by(farm_id, monthweek) |> 
  summarise(temp_mean=mean(temp),
            humi_mean=mean(humi),
            press_mean=mean(press),
            temp_sd = sd(temp),
            humi_sd = sd(humi),
            press_sd=sd(press))
sunshine[is.na(sunshine)] <- 0

tm <- ggplot(sunshine, aes(x=monthweek, y=temp_mean, group=farm_id, color=farm_id)) +
  geom_line(aes(color=farm_id, linetype=farm_id), linewidth=0.8) +
  geom_point(size=2) +
  geom_errorbar(aes(ymin=temp_mean-temp_sd, ymax=temp_mean + temp_sd), width=0.1, alpha=0.5) +
  theme_bw() +
  theme(legend.position = "none", plot.title = element_text(size=10),
        legend.key = element_rect(colour="transparent", fill="transparent"),
        legend.background=element_rect(fill= alpha("white", 0.5))) +
  labs(x="", y = "Daily average temperature (°C)")

hu <- ggplot(sunshine, aes(x=monthweek, y=humi_mean, group=farm_id, color=farm_id)) +
  geom_line(aes(color=farm_id, linetype=farm_id), linewidth=0.8) +
  geom_point(size=2) +
  geom_errorbar(aes(ymin=humi_mean-humi_sd, ymax=humi_mean + humi_sd), width=0.1, alpha=0.5) +
  theme_bw() +
  theme(legend.position = "none", plot.title = element_text(size=10),
        legend.key = element_rect(colour="transparent", fill="transparent"),
        legend.background=element_rect(fill= alpha("white", 0.5))) +
  labs(x="Week-month", y = "Daily average humidity (%)")

pr <- ggplot(sunshine, aes(x=monthweek, y=press_mean, group=farm_id, color=farm_id)) +
  geom_line(aes(color=farm_id, linetype=farm_id), linewidth=0.8) +
  geom_point(size=2) +
  geom_errorbar(aes(ymin=press_mean-press_sd, ymax=press_mean + press_sd), width=0.1, alpha=0.5) +
  theme_bw() +
  theme(legend.position = "none", plot.title = element_text(size=10),
        legend.key = element_rect(colour="transparent", fill="transparent"),
        legend.background=element_rect(fill= alpha("white", 0.5))) +
  labs(x="", y = "Daily average air pressure (atm)")

grid.arrange(tm, hu, pr, nrow=1)
