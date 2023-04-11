library(readxl)
library(tidyverse)
library(formattable)
library(ggplot2)

NICEI_Tables_Q4_2022 <- read_excel("NICEI-Tables-Q4-2022.xlsx", 
                                   sheet = "Table 6", 
                                   range = "A2:D8")
df<- NICEI_Tables_Q4_2022
df1<-df[, 1:2, drop=FALSE] #If the drop argument is provided with the value FALSE the columns are not converted to vector objects.
df1[5,1]<-""

formattable(df1)

ggplot(df1, aes(Sector, `Quarterly contribution (pps)*`))  + geom_col()  +coord_flip()

df2<-map_df(df1, rev)
df2$category<- c("A", "B", "B", "B", "B", "C")
formattable(df2)

hbplt2<-ggplot(df2, aes(Sector, `Quarterly contribution (pps)*`, fill = category)) + geom_col() + coord_flip()
hbplt2

df3<-df2
df3$order<-c(1:nrow(df3))
formattable(df3)

final_plot<-df3 %>% 
  ggplot(df3, mapping=aes(fct_reorder(Sector, desc(order)), `Quarterly contribution (pps)*`, fill = category)) + 
  geom_col(width = 0.25) +
  theme_classic() +
  geom_text(aes(label = `Quarterly contribution (pps)*`), hjust = -1.25)

final_plot <- df3 %>% 
  ggplot(df3, mapping=aes(fct_reorder(Sector, desc(order)), `Quarterly contribution (pps)*`, fill = category)) + 
  geom_col(width = 0.25) +
  theme_classic() +
  geom_text(aes(label=ifelse( `Quarterly contribution (pps)*` < 0, `Quarterly contribution (pps)*`, "")), hjust = 1.1) +
  geom_text(aes(label=ifelse( `Quarterly contribution (pps)*` > 0, `Quarterly contribution (pps)*`, "")), hjust = -0.2) +
  scale_fill_manual(values= c("#000066", "blue", "#cccc00")) +
  theme(aspect.ratio = 1/2,
        plot.margin = unit(c(2,2,2,2), "cm"),
        panel.background = element_blank(),
        panel.grid.major.x = element_line(color = "grey"),
        plot.title = element_text(hjust = 0.5, face="bold"),
        axis.text = element_text(size=10, face = "bold"),
        axis.line = element_line(colour = "grey"),
        axis.ticks = element_blank(),
        legend.position="none") +
  coord_flip()+
  scale_y_continuous(limits=c(-0.5,1.5), expand = c(0, 0)) +
  labs(x = "", y="", title = "Contributions of component indices to quarterly change in \nNICEI** (pps)") 

final_plot
