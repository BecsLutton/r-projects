# installing relevant libraries
library(readxl)
library(janitor)
library(ggplot2)
library(dplyr)
library(reshape2)
library(tidyverse)


#==========cleaning===============
df <- read_excel("NICEI-Tables-Q4-2022.xlsx", sheet = "Table 1", skip = 1) #read in excel data from local directory

# dropping unused columns and merging the Year and Quarter columns
df1 <- df %>% 
  select(c(0:5)) %>%
  mutate(date = paste(Year, Quarter, sep="\nQ"), .before = 1, .keep = "unused")


#============data plotting===============
ggplot(df1, aes(date, NICEI, group=1)) + geom_line(color="black") #testing NICEI data

# restructuring the data from the 4 columns to 3 that contain the date, variable (previous column headers) and their values
# useful for colouring plots based on category as is needed in this case
df2<-melt(df1, id.vars="date") 

#plot of data
ggnicei<-ggplot() + 
  geom_line(data = df2, 
            aes(x=date, y=value, 
                group = variable, 
                color = variable)
            )
ggnicei #sanity check

#Create function to return a function as the breaks argument in order to return appropriate x-axis spacing
every_nth = function(n) {
  return(
    function(x){
      x[c(TRUE, rep(FALSE, n - 1))]
      }
    )
}

# create the final plot
final_plot<-ggnicei + 
  scale_color_manual(values= c("black", "blue", "#cccc00"),             # selecting colours for the lines
                     labels=c("NICEI",                                  # creating the line labels for the legend
                              "Private Sector Component Index",         
                              "Public Sector Component Index")) +
  geom_hline(yintercept=100, linetype = 'dashed') +                    # generating the dashed horizontal line (baseline)
  annotate("text",                                                     # Forming the text for the baseline
           x="2014\nQ1", 
           y= 99, 
           label = "Baseline 2019 = 100", 
           size =3, 
           fontface = "bold") + 
  theme(line = element_blank(),                                 # plot theme
        panel.grid.major = element_blank(),                     # no major gridlines
        panel.grid.minor = element_blank(),                     # no minor gridlines
        panel.background = element_blank(),                     # blank background
        plot.title = element_text(colour = "chartreuse3",       # title formatting: colour and bold
                                  face = "bold"),
        axis.text = element_text(face = "bold"),                # formatting axis text: bold
        axis.line = element_line(colour = "NA"),                # no axis lines
        legend.title=element_blank(),                           # remove legend title 
        legend.key=element_blank(),                             # remove background colour from legend key
        legend.position = c(0.15,0.15),                         # legend position
        plot.margin = unit(c(2,2,2,2), "cm")) +                 # adjusting plot padding for exported image
  scale_x_discrete(breaks = every_nth(n = 8)) +                 # implementing the function for axis labels every 8th (Q1 every other year)   
  scale_y_continuous(breaks = seq(75, 110, by=5),               # formatting Y-axis: 75-115 in increments of 5
                     limits=c(75,110))+
  annotate(geom = "point",      # inserting red point at Y-value 105.7
           x = "2022\nQ4",      
           y = 105.7, 
           colour = "red", 
           size = 2) +
  annotate(geom = "label",      # inserting blue label with white font stating 105.7
           x = "2021\nQ4", 
           y = 108, 
           label = "105.7", 
           hjust = "left", 
           fill="blue", 
           color = "white") +
  annotate("segment",           # connecting line between point and label
           x = "2022\nQ1", 
           xend = "2022\nQ4", 
           y = 107.8, yend = 105.7,
           linewidth = 0.2) +
  labs(x = "",                                  # no X-axis title
       y="",                                    # no Y-axis title
       title = "NICEI Trend to Q4 2022")        # plot title

final_plot

#The plot in the original pdf seems to have a thicker NICEI line; if this is desired, all that is required is to overlay the
#final_plot with 

df4<-filter(df2, variable == "NICEI")
final_plot2<- final_plot + 
  geom_line(data=df4, 
            aes(x=date, 
                y=value, 
                group = 1), 
            linewidth = 1)
final_plot2

# Instead of exporting manually, scripted export of plot image can be done
# Note the decrease in text readability using this method. Text sizes need increased if using this method
# Manual export if this plot is in the local directory named, "NICEI_r_plot.png"

png("NICEI-plot.png", width = 950, height = 750)
final_plot2
dev.off()
