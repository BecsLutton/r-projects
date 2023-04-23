# installing relevant libraries
library(readxl)
library(tidyverse)
library(formattable)
library(ggplot2)

#---------------- Data prep--------------------
# read in relevant data range from Excel table in local directory
df <- read_excel("NICEI-Tables-Q4-2022.xlsx", 
                                   sheet = "Table 6", 
                                   range = "A2:B8")

# Replace row 5 column 1 NA with empty space.
# The plot in the publication to be replicated has an evident gap between NICEI bar and rest
df[5,1]<-""

formattable(df)     #table check

#plot histogram to check layout and comparison to original
ggplot(df,                                            # data
       aes(Sector,                                    # X values
           `Quarterly contribution (pps)*`))  +       # Y values
  geom_col() +                                        # Plot type
  coord_flip()                                        # Horizontal layout

#--------------- Plot prep---------------------
df1<-map_df(df, rev)      # reverse the order of the columns in df
df1$category<- c("A", "B", "B", "B", "B", "C")      # creating a new column to assist with colour scheme in original
formattable(df1)    # table check

# Creating similar plot as above but with 3 colours
hbplt<-ggplot(df1, 
              aes(Sector, 
                  `Quarterly contribution (pps)*`, 
                  fill = category)) + 
  geom_col() + 
  coord_flip()

hbplt
#note the plot doesn't seem to plot bars in order of table. 

df2<-df1        # creating new data frame based on previous

df2$order<-c(1:nrow(df2))     # adding row attributing an order for the plot     


#----------------Plotting-------------------

final_plot <- ggplot(df2, 
                     aes(fct_reorder(Sector, desc(order)),   # attributes a sequence of plotting Sector based on 'order' 
                         `Quarterly contribution (pps)*`,
                         fill = category)) + 
  geom_col(width = 0.25) +          # adjusts the column widths so they are thinner
  theme_classic() +           # blank background as per original image
  geom_text(aes(label =         # adding data labels to the plot
                  ifelse(`Quarterly contribution (pps)*` < 0, `Quarterly contribution (pps)*`, "")),   # if value is negative 
            hjust = 1.1) +     # position label on left
  geom_text(aes(label =
                  ifelse( `Quarterly contribution (pps)*` > 0, `Quarterly contribution (pps)*`, "")),  # if value is negative 
            hjust = -0.2) +    # position label on left
  scale_fill_manual(values= c("#000066",         # colour first category 
                              "blue",            # colour second category 
                              "#cccc00")) +      # colour third category 
  theme(aspect.ratio = 1/2,                                         # change aspect ratio of plot
        plot.margin = unit(c(2,2,2,2), "cm"),                       # pad area around plot for image capture
        panel.grid.major.x = element_line(color = "grey"),          # colour major gridlines grey
        plot.title = element_text(hjust = 0.5,                      # centre the plot title and bolden
                                  face="bold"),
        axis.text = element_text(size=10, face =                    # make axis text bold and size 10
                                   "bold"),
        axis.line = element_line(colour = "grey"),                  # colour axis lines grey 
        axis.ticks = element_blank(),                               # remove axis ticks
        legend.position="none") +                                   # no legend
  coord_flip()+                                  # flip plot to horizontal         
  scale_y_continuous(limits=c(-0.5,1.5),         # alter y axis to start at -0.5 and end at 1.5
                     expand = c(0, 0)) +         # y axis limits to start at plot origin
  labs(x = "",                                   # plot to have no X- or Y- titles but a main plot title
       y="", 
       title = "Contributions of component indices to quarterly change in \nNICEI** (pps)"
       ) 

final_plot    #sanity check

# save plot as image (text sizes need amended)
# Note this is a repeat of plot script above except for text sizes
final_plot_sized <- ggplot(df2, 
                     aes(fct_reorder(Sector, desc(order)),   # attributes a sequence of plotting Sector based on 'order' 
                         `Quarterly contribution (pps)*`,
                         fill = category)) + 
  geom_col(width = 0.25) +          # adjusts the column widths so they are thinner
  theme_classic() +           # blank background as per original image
  geom_text(aes(label =         # adding data labels to the plot
                  ifelse(`Quarterly contribution (pps)*` < 0, `Quarterly contribution (pps)*`, "")),   # if value is negative 
            hjust = 1.1) +     # position label on left
  geom_text(aes(label =
                  ifelse( `Quarterly contribution (pps)*` > 0, `Quarterly contribution (pps)*`, "")),  # if value is negative 
            hjust = -0.2) +    # position label on left
  scale_fill_manual(values= c("#000066",         # colour first category 
                              "blue",            # colour second category 
                              "#cccc00")) +      # colour third category 
  theme(aspect.ratio = 1/2,                                         # change aspect ratio of plot
        plot.margin = unit(c(2,2,2,2), "cm"),                       # pad area around plot for image capture
        panel.grid.major.x = element_line(color = "grey"),          # colour major gridlines grey
        plot.title = element_text(size = 17,                        # centre the plot title, size = 17 and bolden
                                  hjust = 0.5,                      
                                  face="bold"),
        axis.text = element_text(size=13, face =                    # make axis text bold and size 13
                                   "bold"),
        axis.line = element_line(colour = "grey"),                  # colour axis lines grey 
        axis.ticks = element_blank(),                               # remove axis ticks
        legend.position="none") +                                   # no legend
  coord_flip()+                                  # flip plot to horizontal         
  scale_y_continuous(limits=c(-0.5,1.5),         # alter y axis to start at -0.5 and end at 1.5
                     expand = c(0, 0)) +         # y axis limits to start at plot origin
  labs(x = "",                                   # plot to have no X- or Y- titles but a main plot title
       y="", 
       title = "Contributions of component indices to quarterly change in \nNICEI** (pps)"
  ) 
png("contributions_resized.png", width = 950, height = 750)
final_plot_sized
dev.off()
