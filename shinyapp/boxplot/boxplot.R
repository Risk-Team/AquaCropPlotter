library(tidyverse)

#modify currect plotting to boxplot
#y axis select variable with values to plot
#x axis now select grouping variable
#allow color and facet grouping
#select plot absolute vs relative change values
#add line of reference baseline of average historical value
#select reference year range
#select split by time horizon

data = read.delim("combined_seasonal_data.tsv")

prm = read.delim("combined_parameter_data (1).tsv")

ggplot(data, aes(y=BioMass, x=name.variable3, fill=name.variable7))+
  stat_boxplot(geom="errorbar")+#add horizontal line to end of whiskers
  geom_boxplot()+
  #geom_point(position = position_jitterdodge(jitter.width = 0.2), size=0.1)
  facet_grid(.~name.variable4)

#select reference year range
ref.year = c(2020,2020)
xaxis.var = "name.variable3"
grouping.var = "name.variable7"

data.ref = data %>%
  filter(Year1 %in% c(ref.year[1]:ref.year[2])) %>%
  select(!starts_with(c("Day","Month","Year"))) %>%
  group_by(across(all_of(c(xaxis.var, grouping.var)))) %>%
  summarise(across(where(is.numeric), ~mean(.x, na.rm = TRUE))) 

data.norm = data %>%
  group_by(across(all_of(c(xaxis.var, grouping.var)))) %>%
  mutate(across(where(is.numeric) &!starts_with(c("Day","Month","Year")), ~ 100*(.x - mean(.x[which(Year1 %in% c(ref.year[1]:ref.year[2]))], na.rm = TRUE))/mean(.x[which(Year1 %in% c(ref.year[1]:ref.year[2]))], na.rm = TRUE) )) 

ggplot(data.norm, aes(y=BioMass, x=name.variable3, fill=name.variable7))+
  stat_boxplot(geom="errorbar")+#add horizontal line to end of whiskers
  geom_boxplot()+
  #geom_point(position = position_jitterdodge(jitter.width = 0.2), size=0.1)
  facet_grid(.~name.variable4)

ggplot(data.norm, aes(y=BioMass, x=name.variable3))+
  stat_boxplot(geom="errorbar")+#add horizontal line to end of whiskers
  geom_boxplot()+
  #geom_point(position = position_jitterdodge(jitter.width = 0.2), size=0.1)
  facet_grid(.~name.variable4)


ggplot(data, aes(y=BioMass, x=Year1))+
  geom_point()+
  geom_line()+
  #geom_point(position = position_jitterdodge(jitter.width = 0.2), size=0.1)
  facet_grid(name.variable3~name.variable7)
ggplot(data.norm, aes(y=BioMass, x=Year1))+
  geom_point()+
  geom_line()+
  facet_grid(name.variable3~name.variable7)
  