library(readxl)
library(xts)
library(formattable)
library(flextable)
library(tidyverse)
library(gridExtra)
library(grid)
library(officer)

df <- read_excel("NICEI-Tables-Q4-2022.xlsx", sheet = "Table 1", skip = 1)

# Calculate Q/Q row
Q_Q <- (df[nrow(df),] - df[nrow(df)-1,])/df[nrow(df)-1,]
Y_Y <- (df[nrow(df),] - df[nrow(df)-4,])/df[nrow(df)-4,]
Tri <- (df[nrow(df),] - df[nrow(df)-12,])/df[nrow(df)-12,]


df1<- df %>% mutate(date = paste(Year, Quarter, sep="\nQ"), .before = 1, .keep = "unused")

TS <- df1 %>% ts(frequency = 4, start = c(2006,1), end = c(2022, 4))
r4Q <-window(TS, 1, c(2022,4)) %>%  #selects a window starting at row 1, ending at period 4 of 2022
  as.xts() %>%
  apply.yearly(mean) %>%
  as.data.frame()

r4q<-(r4Q[nrow(r4Q),] - r4Q[nrow(r4Q)-1,])/r4Q[nrow(r4Q)-1,]
dt<-rbind(Q_Q[,-c(1:2)], Y_Y[,-c(1:2)], Tri[,-c(1:2)], r4q[,-1]) %>% round(digits = 3)

growth <- data.frame("Growth Rates" = c("Q/Q", "Y/Y", "Triennial \n(3Yr) Change", "Rolling 4Q \nAnnual Ave"))

DT<-cbind(growth, dt)
colnames(DT)<-c("growth", "NICEI", "private", "public", "services", "production", "construction")
DT


pDT<- DT %>% mutate(across(-c(1),percent))
formattable(pDT)
dft<-flextable(pDT) %>% theme_box() 

dft<-bold(dft, part = "header") # bold header
dft<-bg(dft, bg = "olivedrab3", part = "header")
dfxt <- dft
dfxt

dfxt2 <- flextable::compose(dfxt,
                 j = 2,
                 value = as_paragraph(
                   as_image(src = "arrow_up.png", width = .15, height = .15), " ",
                   as_chunk(NICEI, props = fp_text(color = "black"))
                 ),
                 part = "body")
dfxt2
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

dfxt2

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

dfxt4 <- width(dfxt3, j = 1, width = 1.3086) %>%
  align(align = "center", part = "all") %>%
  align(j=1, align = "left", part = "body")


growth_table <-dfxt4


