# installing relevant libraries
library(readxl)
library(xts)
library(formattable)
library(flextable)
library(tidyverse)
library(gridExtra)
library(grid)
library(officer)

#read in data (excel format) from local directory
df <- read_excel("NICEI-Tables-Q4-2022.xlsx", sheet = "Table 1", skip = 1)

#calculate the data required for the Growth table from the raw data (fractional differences)
Q_Q <- (df[nrow(df),] - df[nrow(df)-1,])/df[nrow(df)-1,]    # Calculate Q/Q row
Y_Y <- (df[nrow(df),] - df[nrow(df)-4,])/df[nrow(df)-4,]    # Calculate Y/Y row
Tri <- (df[nrow(df),] - df[nrow(df)-12,])/df[nrow(df)-12,]  # Calculate Triennial row

# Create new column with year and Quarter combined, separated with a space, and delete the two individual columns 
df1<- df %>% mutate(date = paste(Year, Quarter, sep="\nQ"), .before = 1, .keep = "unused")

# Convert from data frame to a time series object to enable use of moving/rolling average calculations
TS <- df1 %>% ts(frequency = 4, start = c(2006,1), end = c(2022, 4))    # frequency = 4 means data consists of quarterly samples.

# Calculate the 4Q rolling average of dataset
r4Q <-TS %>%                   # all data (window() would be used if calculations were to be on data subsets)
  as.xts() %>%                 # coerces the timeseries object to an eXtensible format required for time-based analysis
  apply.yearly(mean) %>%       # apply mean function to each distinct year of data
  as.data.frame()              # pipe data to data frame for additional work

# calculation of fractional difference for rolling 4Q annual average
r4q<-(r4Q[nrow(r4Q),] - r4Q[nrow(r4Q)-1,])/r4Q[nrow(r4Q)-1,]

# create the base table of Growth rates
dt<-rbind(Q_Q[,-c(1:2)],
          Y_Y[,-c(1:2)], 
          Tri[,-c(1:2)], 
          r4q[,-1]) %>%   
  round(digits = 3)

# create the column of names
growth <- data.frame("Growth Rates" = c("Q/Q",
                                        "Y/Y", 
                                        "Triennial \n(3Yr) Change", 
                                        "Rolling 4Q \nAnnual Ave")
                     )

DT<-cbind(growth, dt)   # combine base table with names
colnames(DT)<-c("growth", "NICEI", "private", "public", "services", "production", "construction")  #convert column names to single value names (needed for the compose later)
DT


pDT<- DT %>% mutate(across(-c(1),percent))    #convert values to percentage
dft<-flextable(pDT) %>% theme_box()   # pretty print of table using flextable (easier to customise than formattable)

#prettify table
dft<-bold(dft, part = "header")                   # bold header
dft<-bg(dft, bg = "olivedrab3", part = "header")  # fill the header with colour
dfxt <- dft                                       # rename for next step of adding arrows  
dfxt


# Complex process of adding coloured arrows corresponding to values.
# Each column must be addressed first adding the green arrows, as this is the predominant colour
# Then each column with red arrows must be edited to reflect this. 
# This is a manual process because there isn't a consistent rule upon which to allocate the arrows.
# Each value corresponds to its previous value in a different year/quarter dependent on the category it's describing.
# There are R emoticon/graphics packages available but it was easier to find .png files and save them to the local directory and upload

dfxt2 <- flextable::compose(dfxt,   #data
                 j = 2,   #2nd column
                 value = as_paragraph(     # allows concatenation of text chunks and images etc
                   as_image(src = "arrow_up.png", width = .15, height = .15),   # arrow first as per the publication being replicated
                   " ",    # space follows the arrow
                   as_chunk(NICEI, props = fp_text(color = "black")    # The text to be added, which is the value in the column NICEI. 
                            )                                           # This line is why the column names needed changed earlier (line 49)
                   ),
                 part = "body")   # noting that the compose function is applied to the table body
dfxt2    #table check
dfxt2 <- flextable::compose(dfxt2,
                 j = 3,
                 value = as_paragraph(
                   as_image(src = "arrow_up.png", width = .15, height = .15), " ",
                   as_chunk(private, props = fp_text(color = "black"))
                 ),
                 part = "body")
dfxt2
dfxt2 <- flextable::compose(dfxt2,
                 j = 4,
                 value = as_paragraph(
                   as_image(src = "arrow_up.png", width = .15, height = .15), " ",
                   as_chunk(public, props = fp_text(color = "black"))
                 ),
                 part = "body")
dfxt2
dfxt2 <- flextable::compose(dfxt2,
                 j = 5,
                 value = as_paragraph(
                   as_image(src = "arrow_up.png", width = .15, height = .15), " ",
                   as_chunk(services, props = fp_text(color = "black"))
                 ),
                 part = "body")
dfxt2
dfxt2 <- flextable::compose(dfxt2,
                 j = 6,
                 value = as_paragraph(
                   as_image(src = "arrow_up.png", width = .15, height = .15), " ",
                   as_chunk(production, props = fp_text(color = "black"))
                 ),
                 part = "body")
dfxt2
dfxt2 <- flextable::compose(dfxt2,
                 j = 7,
                 value = as_paragraph(
                   as_image(src = "arrow_up.png", width = .15, height = .15), " ",
                   as_chunk(construction, props = fp_text(color = "black"))
                 ),
                 part = "body")
dfxt2
dfxt2<-  flextable::compose(dfxt2,
                 j = 6, i = ~production < 0.011, 
                 value = as_paragraph(
                   as_image(src = "arrow_down.png", width = .15, height = .15), " ",
                   as_chunk(production, props = fp_text(color = "black"))
                 ),
                 part = "body")
dfxt2
dfxt2 <- flextable::compose(dfxt2,
                 j = 7, i = ~construction < 0.04,
                 value = as_paragraph(
                   as_image(src = "arrow_down.png", width = .15, height = .15), " ",
                   as_chunk(construction, props = fp_text(color = "black"))
                 ),
                 part = "body")

dfxt2   #this should be the completed body 

#Need to change the header names back to the proper format
dfxt3 <- set_header_labels(dfxt2,
                           values = list(
                             growth = "Growth Rates",
                             NICEI = "NICEI",
                             private = "Private Sector",
                             public = "Public Sector",
                             services = "Services",
                             production = "Production",
                             construction = "Construction"
                           )
)

dfxt3

#prettifying the table via formatting to reflect the publication be replicated
dfxt4 <- width(dfxt3, j = 1, width = 1.3086) %>%    # Adjusting the width of column 1 
  align(align = "center", part = "all") %>%         # Center aligning all of table contents
  align(j=1, align = "left", part = "body")         # Left aligning column 1

dfxt4
growth_table <-dfxt4    # Final table


