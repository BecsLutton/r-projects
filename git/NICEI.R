library(readxl)
library(janitor)
library(ggplot2)
library(dplyr)
library(reshape2)
library(tidyverse)


#==========cleaning===============
df<- read_excel("~/Documents/IDES/r/r-projects-main/NICEI-Tables-Q4-2022.xlsx",sheet = "Table 1")

df1 <- df %>% 
  row_to_names(row_number = 1) %>% 
  select(c(0:5))

df1$date <- paste(df1$Year, df1$Quarter, sep="\nQ")
df1<-df1[, c(6, 1, 2, 3, 4, 5)]
df2<-df1[,-c(1:3)]
df2 <- as.data.frame(apply(df2, 2, as.numeric))
df2$date<-(df1$date)
df2<-df2 %>% relocate(date)
sapply(df2, class)


#============data plotting===============
ggplot(df2, aes(date, NICEI, group=1)) + geom_line(color="black")

df3<-melt(df2, id.vars="date")
ggnicei<-ggplot() + geom_line(data = df3, aes(x=date, y=value, group = variable, color = variable))



every_nth = function(n) {
  return(function(x) {x[c(TRUE, rep(FALSE, n - 1))]})
}

final_plot<-ggnicei + 
  scale_color_manual(values= c("black", "blue", "#cccc00"), 
                     labels=c("NICEI", "Private Sector Component Index", "Public Sector Component Index")) +
  geom_hline(yintercept=100, linetype = 'dashed') + 
  annotate("text", x="2014\nQ1", y= 99, label = "Baseline 2019 = 100", size =3, fontface = "bold") + 
  theme(line = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        plot.title = element_text(colour = "chartreuse3", face = "bold"),
        axis.text = element_text(face = "bold"),
        axis.line = element_line(colour = "NA"),
        legend.title=element_blank(),
        legend.key=element_blank(),
        legend.position = c(0.15,0.15),
        plot.margin = unit(c(2,2,2,2), "cm")) +
  scale_x_discrete(breaks = every_nth(n = 8)) + 
  scale_y_continuous(breaks = seq(75, 110, by=5), limits=c(75,110))+
  annotate(geom = "point", x = "2022\nQ4", y = 105.7, colour = "red", size = 2) +
  annotate(geom = "label", x = "2021\nQ4", y = 108, label = "105.7", hjust = "left", fill="blue", color = "white") +
  annotate("segment", x = "2022\nQ1", xend = "2022\nQ4", y = 107.8, yend = 105.7, linewidth = 0.2) +
  labs(x = "", y="", title = "NICEI Trend to Q4 2022") 

#The plot in the original pdf seems to have a thicker NICEI line; if this is desired, all that is required is to overlay the
#final_plot with 

df4<-filter(df3, variable == "NICEI")
final_plot + geom_line(data=df4, aes(x=date, y=value, group = 1), linewidth = 1)

