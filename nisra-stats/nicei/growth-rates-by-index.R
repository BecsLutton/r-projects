library(readxl)
library(formattable)
library(flextable)
library(tidyverse)
library(gridExtra)
library(grid)
library(officer)


growth <- c("Q/Q", "Y/Y", "Triennial \n(3Yr) Change", "Rolling 4Q \nAnnual Ave")
NICEI <- c(0.014, 0.013, 0.056, 0.03)
private <- c(0.016, 0.01,0.057,0.031)
public <- c(0.006,0.023,0.054,0.026)
services <- c(0.01,0.018,0.062,0.044)
production <- c(0.006,0.01,0.045,0.026)
construction <- c(0.088,0.007,0.045,0.036)

df<- data.frame(growth, NICEI, private, public, services, production, construction) 

df1<- df %>% mutate(across(-c(1),percent))
formattable(df1)
dft<-flextable(df1) %>% theme_box() 

dft<-bold(dft, part = "header") # bold header
dft<-bg(dft, bg = "olivedrab3", part = "header")
dft


dfxt <- dft
dfxt

dfxt2 <- compose(dfxt,
                j = 2,
                value = as_paragraph(
                  as_image(src = "~/Downloads/arrow_up.png", width = .15, height = .15), " ",
                  as_chunk(NICEI, props = fp_text(color = "black"))
                ),
                part = "body")
dfxt2 <- compose(dfxt2,
                 j = 3,
                 value = as_paragraph(
                   as_image(src = "~/Downloads/arrow_up.png", width = .15, height = .15), " ",
                   as_chunk(private, props = fp_text(color = "black"))
                 ),
                 part = "body")
dfxt2 <- compose(dfxt2,
                 j = 4,
                 value = as_paragraph(
                   as_image(src = "~/Downloads/arrow_up.png", width = .15, height = .15), " ",
                   as_chunk(public, props = fp_text(color = "black"))
                 ),
                 part = "body")
dfxt2 <- compose(dfxt2,
                 j = 5,
                 value = as_paragraph(
                   as_image(src = "~/Downloads/arrow_up.png", width = .15, height = .15), " ",
                   as_chunk(services, props = fp_text(color = "black"))
                 ),
                 part = "body")
dfxt2 <- compose(dfxt2,
                 j = 6,
                 value = as_paragraph(
                   as_image(src = "~/Downloads/arrow_up.png", width = .15, height = .15), " ",
                   as_chunk(production, props = fp_text(color = "black"))
                 ),
                 part = "body")
dfxt2 <- compose(dfxt2,
                 j = 7,
                 value = as_paragraph(
                   as_image(src = "~/Downloads/arrow_up.png", width = .15, height = .15), " ",
                   as_chunk(construction, props = fp_text(color = "black"))
                 ),
                 part = "body")
dfxt2<-  compose(dfxt2,
          j = 6, i = ~production < 0.011, 
          value = as_paragraph(
            as_image(src = "~/Downloads/arrow_down.png", width = .15, height = .15), " ",
            as_chunk(production, props = fp_text(color = "black"))
            ),
          part = "body")
dfxt2 <- compose(dfxt2,
                 j = 7, i = ~construction < 0.037,
                 value = as_paragraph(
                   as_image(src = "~/Downloads/arrow_down.png", width = .15, height = .15), " ",
                   as_chunk(production, props = fp_text(color = "black"))
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

