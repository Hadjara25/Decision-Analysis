library(ggplot2)
library(tidyverse)
library(dplyr)
library(ggthemes)
library(chillR)


? ggthemes

stakeholder<-read.csv("stakeholder.csv")

stakeholder


## stakeholder experieince, availability and expertise in decision analysis

ggplot(data = stakeholder, aes(x = Experience,
                               y = Availability,
                               label = stakeholders,
                               color = Expertise)) +
  geom_point(aes(shape=Gender)) +
  xlab("Relevant Experience") +





## label names of stakeholders and expand space to show full names

 ggplot(data = stakeholder,

  scale_x_continuous(labels = paste (seq (0, 5, by = 1)),
                   breaks = seq (0, 5, by = 1),
                   limits = c(0, 5),
                   expand = c(0, 1)) +
  scale_y_continuous(labels = paste (seq(0,5, by = 1)),
                     breaks = seq (0, 5, by = 1),
                     limits = c(0, 5),
                     expand = c(0, 1))) +
  theme(plot.margin = unit(c(1,1,1,1), "cm"))+
  theme(legend.position = "none")+


  ## create line to categorize stakeholders



 geom_hline(yintercept = 2.5, color = "white", size =2)+
  geom_vline(xintercept = 2.5, color = "white", size =2)+

  ## show all names of overlapped values

  geom_text_repel(box.padding = 0.3, max.overlaps = Inf, size = 3)+
  annotate("test", label = "potential core experts",
           x = 4.5, y = 3.2, size = 5, color = "grey48")+
  annotate("text", label = "Resource persons",
           x = 4.5, y = 0.25, size = 5, color = "grey48"))














