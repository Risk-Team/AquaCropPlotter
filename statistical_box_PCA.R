#library(vegan)
library(tidyverse)
library(plotly)


#### read in the combined dataset ####

# sowing date should be a column
# climate.model should contain the climate models being used. This can be more than one. 

data <- read_tsv("./Aquacrop_combined_data.tsv") %>% 
  mutate(irrigation=ifelse(is.na(irrigation), "rainfed", irrigation), sowing_date= paste0(Day1, "_", Month1))

#### quality check. The file has to contain col name location ####

# location has to be present
ifelse(sum(str_detect(colnames(data), "location")), "you can proceed", "check your data, col location is missing")
# number of stations have to be hiher than 4
#ifelse(sum(str_detect(colnames(data), "location")), "you can proceed", "check your data, col location is missing")


#### automatic variable selection and scaling ####
# since variables will be present at the end of the df, we select those plus Year1 and the stresses

data.filt <- data %>% 
  select(40:ncol(.), Year1, contains("Str")) %>% 
  mutate_if(str_detect(colnames(.),"Str"), scale) %>% 
  mutate(location=as.character(location))

#### grouping by ####
# flexible grouping by 
p <- data.filt %>% 
  group_by(soil, rcp, location, Year1) %>% # the options should be based on the colnames as always
  summarise(ExpStr=mean(ExpStr), StoStr=mean(StoStr), TempStr=mean(TempStr)) %>% 
  ggplot(aes(ExpStr, StoStr, frame=Year1, color=TempStr, shape=location))+
  geom_jitter(alpha=0.6, width = 0.5 )+
  theme_bw()+
  facet_grid(soil~ rcp)+ # decide which variable in facet
  scale_color_gradient(low = "red", high = "black")
  
ggplotly(p) # I cannot remove the double legend for some reasons. The problem is the frame argument

#### PCa by time frame ####
# PRELIMINaRTY
# pca.df <- data %>% 
#   group_by(soil,location) %>%
#   summarise_if(is.numeric, median) %>% 
#   mutate(col.comb=paste0(soil, "_", location)) %>% 
#   column_to_rownames( var = "col.comb") %>% 
#   select(-soil, -location)
# 
# # PCA
# stress.pca <- rda(pca.df, scale=TRUE) 
# 
# # manual plotting of PCA
# smry <- summary(stress.pca)
# 
# df1  <- data.frame(smry$sites[,1:2])# PC1 and PC2 for sites
# df1$ID <- rownames(df1)
# 
# # extracting descriptive variables
# var <- map.table.s %>% 
#   select(Sample_ID,Status, Subpopulation) %>% 
#   dplyr::rename(ID=Sample_ID)
# 
# 
# df1.1 <- left_join(df1, var, by="ID")
# 
# rownames(df1.1) <- df1.1$ID
# 
# df1.1 <- df1.1 %>% 
#   select(PC1,PC2,Subpopulation, ID)
# 
# (plot2 <- ggplot(df1.1, aes(x=PC1*2, y=PC2*2)) + 
#     geom_point(aes(color=Subpopulation),size=1.5,alpha=0.4)+
#     geom_hline(yintercept=0, linetype="dotted") +
#     geom_vline(xintercept=0, linetype="dotted") +
#     coord_fixed()+
#     ylim(-4,5)+
#     xlim(-3, 5)+theme_Publication(base_size = 10, base_family = "Times")+
#     ylab("PC2 (15%)")+
#     xlab("PC1 (46%)")+
#     scale_color_manual(values=c("Red", "orange", "Blue", "darkgreen")))
# 
# 
# 
# df2  <- data.frame(smry$species[,1:2]) %>% 
#   arrange(desc(PC1))
# 
# df3  <- data.frame(smry$species[,1:2]) %>% 
#   arrange(PC1)
# 
# df4  <- data.frame(smry$species[,1:2]) %>% 
#   arrange(desc(PC2))
# 
# df5  <- data.frame(smry$species[,1:2]) %>% 
#   arrange(PC2)
# 
# (plot3 <- plot2+ geom_segment(data=df2[1,], aes(x=0, xend=PC1*2, y=0, yend=PC2*2), 
#                               color="red", arrow=arrow(length=unit(0.01,"npc")))+
#     geom_text(data=df2[1,], 
#               aes(x=PC1*2,y=PC2*2,label=rownames(df2[1,]),
#                   hjust=0.5*(1-sign(PC1)),vjust=0.5*(1-sign(PC2))), 
#               color="red", size=3)+
#     geom_segment(data=df3[1,], aes(x=0, xend=PC1*2, y=0, yend=PC2*2), 
#                  color="red", arrow=arrow(length=unit(0.01,"npc")))+
#     geom_text(data=df3[1,], 
#               aes(x=PC1*2,y=PC2*2,label=rownames(df3[1,]),
#                   hjust=0.5*(1-sign(PC1)),vjust=0.5*(1-sign(PC2))), 
#               color="red", size=3)+
#     geom_segment(data=df4[1,], aes(x=0, xend=PC1*2, y=0, yend=PC2*2), 
#                  color="red", arrow=arrow(length=unit(0.01,"npc")))+
#     geom_text(data=df4[1,], 
#               aes(x=PC1*2,y=PC2*2,label=rownames(df4[1,]),
#                   hjust=0.5*(1-sign(PC1)),vjust=0.5*(1-sign(PC2))), 
#               color="red", size=3)+
#     geom_segment(data=df5[1,], aes(x=0, xend=PC1*2, y=0, yend=PC2*2), 
#                  color="red", arrow=arrow(length=unit(0.01,"npc")))+
#     geom_text(data=df5[1,], 
#               aes(x=PC1*2,y=PC2*2,label=rownames(df5[1,]),
#                   hjust=0.5*(1-sign(PC1)),vjust=0.5*(1-sign(PC2))), 
#               color="red", size=3))
