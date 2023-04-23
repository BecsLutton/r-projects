#load("~/onedrive/Scenic/data/r/initial.RData")

#rm(list = ls(all.names = TRUE)) #will clear all objects includes hidden objects.

library(tidyverse)
library(dplyr)
library(DT)
library(knitr)
library(stringr)
library(ggplot2)
library(plotly)
library(reshape2)
library(gridExtra)
library(cowplot)
library(ggpubr)

#---------------------------------------------------
# Getting data and initial clean/organising of data
#---------------------------------------------------

#raw31 <- read.csv("2022-01-31.csv", comment.char="#", stringsAsFactors=FALSE)
raw09 <- read.csv("220209.csv", comment.char="#", stringsAsFactors=FALSE)
raw10 <- read.csv("220210.csv", comment.char="#", stringsAsFactors=FALSE)
raw11 <- read.csv("220211.csv", comment.char="#", stringsAsFactors=FALSE)

raw<-raw09
raw_cat<-raw[c(4:nrow(raw)),c(6, 7, 8)] #removing csv header info and dropping unnecessary columns
colnames(raw_cat)<-c("time", "value", "metric") #renaming columns
row.names(raw_cat)<-1:nrow(raw_cat) #changing indexes from 4:end back to 1:end-3
kable(head(raw_cat)) #Checking outcome is as desired

length(unique(raw_cat$metric)) #checking no. of metics, should be 134
length(unique(grep(glob2rx("fields_*"), raw_cat$metric, value=TRUE))) #checking that there are 134 metrics available before cleaning
df_cat <- with(raw_cat, subset(raw_cat, grepl(glob2rx("fields_*"), raw_cat$metric))) #dropping metrics that don't begin with fields_
length(unique(df_cat$metric)) #checking removal of anomalous metrics

df<-df_cat %>% pivot_wider(names_from = metric, values_from = value) #pivoting the table to get metrics from single column list to column headers

date_time<-as.matrix(str_split_fixed(df$time, "T", 2)) #splitting time column into date and time
timecat<-substr(date_time[,2], 1, 8) #tidying the time by rounding up to seconds


df1<-na.omit(as.data.frame(cbind(date_time[,1], timecat, df[-1])))#dropping the columns with _NA_ in rows and replacing time with new date and time columns
names(df1)[1]<-"Date"
names(df1)[2]<-"Time"


if (as.numeric(as.character(df1$`fields_General_0_Part Count`[1])) > 15){
  x<-as.numeric(as.character(df1$`fields_General_0_Part Count`[1]))
  df1$`fields_General_0_Part Count`<-(as.numeric(as.character(df1$`fields_General_0_Part Count`))-x)
}


#--------------------------------------------------
# creating the factors for potential data analysis
#--------------------------------------------------
df1$`fields_General_0_Part Count` <- as.factor(df1$`fields_General_0_Part Count`)
df1$fields_Tool_0_tool_name <- as.factor(df1$fields_Tool_0_tool_name)
df1$fields_Channels_0_Msg <- as.factor(df1$fields_Channels_0_Msg)
df1$`fields_Channels_0_Program Status` <- as.factor(df1$`fields_Channels_0_Program Status`)
df1$`fields_Channels_1_Program Status` <- as.factor(df1$`fields_Channels_1_Program Status`)
#-----------------------------------------------


#-----------------------------------------------
# Getting tool names and filtering for chosen 2
#-----------------------------------------------

ch1ToolNames <-unique(df1$fields_Tool_0_tool_name)
ch1ToolNames

#2 tools of interest:
fullRad <- filter(df1, fields_Tool_0_tool_name == "FULL_RAD_118_TC")
finishSplines <- filter(df1, fields_Tool_0_tool_name == "FINISH_SPLINES_TC")

#-------------------------------------------------------------------
#checking what RPM bands the tools operate in and no of occurrences
#-------------------------------------------------------------------

fullrad2<-filter(fullRad,`fields_General_0_Part Count` == 20) #filtering for one part (number 2)
fullrad_rpm_0 <- fullrad2[ , grepl("RPM", names(fullrad2) ) ] #sort for columns with RPM in column title for partcount = 2
f1 <- apply(fullrad_rpm_0 , 1 , function(x) any( x > 0 ) ) #test for zeroes
fullrad_rpm <- fullrad_rpm_0[f1, ] #dataset after test function is applied
unique(fullrad_rpm$`fields_S1_0_Desired RPM`)


#------- INSERT FULL RAD INFO--------------


finishsplines2<-filter(finishSplines, `fields_General_0_Part Count` == 20) #filtering for one part (number 2)
finishsplines2_rpm <- finishsplines2[ , grepl("RPM", names( finishsplines2 ) ) ] #sort for columns with RPM in column title for partcount = 2
f2 <- apply( finishsplines2_rpm , 1 , function(x) any( x > 0 ) ) #test for zeroes
finishsplines2_rpm <- finishsplines2_rpm[ f2 , ] #dataset after test function is applied
unique(finishsplines2_rpm$`fields_S1_0_Desired RPM`)

finishsplines3000<-filter(finishSplines, `fields_S1_0_Desired RPM`== 3000) #clean whole tool data set for single RPM band of 3000
length(unique(finishsplines3000$`fields_General_0_Part Count`)) # checking that all parts have experienced this RPM

#-------------------------
#plotting (and) stats
#-------------------------

colnames(finishsplines3000)
class(finishsplines3000$`fields_General_0_Part Count`)

finishsplines3000$fields_S1_0_RPM<-as.numeric(finishsplines3000$fields_S1_0_RPM)
finishsplines3000$fields_S1_0_Load<-as.numeric(finishsplines3000$fields_S1_0_Load)
finishsplines3000$fields_S1_0_Power<-as.numeric(finishsplines3000$fields_S1_0_Power)
finishsplines3000$fields_S1_0_Torque<-as.numeric(finishsplines3000$fields_S1_0_Torque)
finishsplines3000$fields_S1_0_Current<-as.numeric(finishsplines3000$fields_S1_0_Current)
finishsplines3000$`fields_S1_0_Feed Rate`<-as.numeric(finishsplines3000$`fields_S1_0_Feed Rate`)

finishsplines3000$fields_S4_0_RPM<-as.numeric(finishsplines3000$fields_S4_0_RPM)
finishsplines3000$fields_S4_0_Load<-as.numeric(finishsplines3000$fields_S4_0_Load)
finishsplines3000$fields_S4_0_Power<-as.numeric(finishsplines3000$fields_S4_0_Power)
finishsplines3000$fields_S4_0_Torque<-as.numeric(finishsplines3000$fields_S4_0_Torque)
finishsplines3000$fields_S4_0_Current<-as.numeric(finishsplines3000$fields_S4_0_Current)
finishsplines3000$`fields_S4_0_Feed Rate`<-as.numeric(finishsplines3000$`fields_S4_0_Feed Rate`)

class(finishsplines3000$fields_S1_0_Current)

Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

finishsplines3000$`fields_General_0_Part Count`<-as.numeric(as.character(finishsplines3000$`fields_General_0_Part Count`))

fspow<-finishsplines3000 %>%
 group_by(`fields_General_0_Part Count`) %>%
 summarize(
      Mean = mean(fields_S1_0_Current),
      Median  = median(fields_S1_0_Current),
      Mode = Mode(fields_S1_0_Current),
      Q1 = quantile(fields_S1_0_Current, 0.25),
      Q3 = quantile(fields_S1_0_Current, 0.75))

names(fspow)[1]<-"PartCount"
kable(head(fspow))

fspowrange<-setNames(cbind(fspow, (fspow$Q3-fspow$Q1)), c("PartCount", "Mean", "Median", "Mode", "Q1", "Q3", "IQR"))
kable(head(fspowrange))


par(mfrow=c(3,2))
p1<-plot(fspowrange$Mean, ylim=c(-0.35,0.3)) + lines(predict(lm(fspowrange$Mean~fspowrange$`PartCount`)),col='green')
p2<-plot(fspowrange$Median, ylim=c(-0.35,0.3)) + lines(predict(lm(fspowrange$Median~fspowrange$`PartCount`)),col='black')
p3<-plot(fspowrange$Mode, ylim=c(-0.35,0.3)) + lines(predict(lm(fspowrange$Mode~fspowrange$`PartCount`)),col='blue')
p6<-plot(fspowrange$IQR, ylim=c(-0.35,0.3)) + lines(predict(lm(fspowrange$IQR ~fspowrange$`PartCount`)),col='magenta')
p4<-plot(fspowrange$Q1, ylim=c(-0.35,0.3)) + lines(predict(lm(fspowrange$Q1~fspowrange$`PartCount`)),col='red')
p5<-plot(fspowrange$Q3, ylim=c(-0.35,0.3)) + lines(predict(lm(fspowrange$Q3~fspowrange$`PartCount`)),col='red')
#
# par(mfrow=c(3,2))
# p1<-plot(fspowrange$Mean, ylim=c(0,140)) + lines(predict(lm(fspowrange$Mean~fspowrange$`PartCount`)),col='green')
# p2<-plot(fspowrange$Median, ylim=c(0,140)) + lines(predict(lm(fspowrange$Median~fspowrange$`PartCount`)),col='black')
# p3<-plot(fspowrange$Mode, ylim=c(0,140)) + lines(predict(lm(fspowrange$Mode~fspowrange$`PartCount`)),col='blue')
# p6<-plot(fspowrange$IQR, ylim=c(0,140)) + lines(predict(lm(fspowrange$IQR ~fspowrange$`PartCount`)),col='magenta')
# p4<-plot(fspowrange$Q1, ylim=c(0,140)) + lines(predict(lm(fspowrange$Q1~fspowrange$`PartCount`)),col='red')
# p5<-plot(fspowrange$Q3, ylim=c(0,140)) + lines(predict(lm(fspowrange$Q3~fspowrange$`PartCount`)),col='red')


p4<-plot(fspow$Q1, ylim=c(0,140)) + lines(predict(lm(fspow$Q1~fspow$PartCount)),col='red')
par(new=TRUE)
p5<-plot(fspow$Q3, ylim=c(0,140)) + lines(predict(lm(fspow$Q3~fspow$PartCount)),col='blue')


pmed <- plot_ly(fspow, x = ~PartCount, y = ~Median, type = 'scatter', mode = 'lines+markers')
fspmed = add_lines(pmed, x=~PartCount, y=predict(lm(fspow$Median~fspow$PartCount)), name="Linear Trendline")
fspmed = layout(fspmed, title = "Median values for Finish Splines ")
print(fspmed)

count <-c()
for (val in finishsplines3000$`fields_General_0_Part Count`){
  if(val<10){
    count<-append(count,1)
  }else if(val>=10 & val<20){
    count<-append(count,2)
  }else if(val>=20 & val<30){
    count<-append(count,3)
  }else if(val>=30 & val <40){
    count<-append(count,4)
  }else if(val>=40 & val <50){
    count<-append(count,5)
  }else if(val>=50 & val<60){
    count<-append(count,6)
  }else if(val>=60 & val<70){
    count<-append(count,7)
  }else if(val>=70 & val<80){
    count<-append(count,8)
  }
}

length(finishsplines3000)

fs10<-finishsplines3000
fs10$`fields_General_0_Part Count`<-count
unique(fs10$`fields_General_0_Part Count`)
length(finishsplines3000$`fields_General_0_Part Count`)

fs10$S1_chip_load<-fs10$fields_S1_0_RPM/fs10$`fields_S1_0_Feed Rate`
fs10$S1_desired_chip_load<-as.numeric(fs10$`fields_S1_0_Desired RPM`)/as.numeric(fs10$`fields_S1_0_Desired Feed Rate`)

fs10$S4_chip_load<-fs10$fields_S4_0_RPM/fs10$`fields_S4_0_Feed Rate`
fs10$S4_desired_chip_load<-as.numeric(fs10$`fields_S4_0_Desired RPM`)/as.numeric(fs10$`fields_S4_0_Desired Feed Rate`)


fscurr10<-fs10 %>%
  summarize(
    Mean = mean(fields_S1_0_Current),
    Median  = median(fields_S1_0_Current),
    Mode = Mode(fields_S1_0_Current),
    Q1 = quantile(fields_S1_0_Current, 0.25),
    Q3 = quantile(fields_S1_0_Current, 0.75))

fspow10<-fs10 %>%
  summarize(
    Mean = mean(fields_S1_0_Power),
    Median  = median(fields_S1_0_Power),
    Mode = Mode(fields_S1_0_Power),
    Q1 = quantile(fields_S1_0_Power, 0.25),
    Q3 = quantile(fields_S1_0_Power, 0.75))

kable(fspow10)
kable(fscurr10)


ggplot(fs10,
       aes(`fields_General_0_Part Count`, fields_S1_0_Power, color=as.factor(`fields_General_0_Part Count`))) +
  geom_jitter()+
  theme_minimal()+
  #theme(legend.position = "none")+
  scale_fill_brewer(palette="Dark2")+
  labs(x=expression(bold("Part Count in groups of 10")), y=expression(bold("S1 Power (W)")), title=expression(bold("S1 Power for Finish Splines Tool over 1 shift of 65 parts"))) +
  theme(plot.title=element_text(hjust=0.5, size=14))

dftime<-which(fs10$`fields_General_0_Part Count` != dplyr::lag(fs10$`fields_General_0_Part Count`))
timez<-fs10[dftime,]$Time
time<-append(fs10$Time[1], timez)

#-----------getting rid of outliers for plot visibility-----------
#--------
# S1
#--------

#Power
s1_power_plot <-
  ggplot(fs10, aes(`fields_General_0_Part Count`, fields_S1_0_Power, group = `fields_General_0_Part Count`)) +
  geom_boxplot(colour="black")+
  geom_jitter(aes(color=as.factor(`fields_General_0_Part Count`)), outlier.shape=NA, position=position_jitter(0.2), size = 2)+
  stat_summary(fun = "median", geom = "point", shape = 23, size = 3, fill="blue") +
  stat_summary(fun = "mean", geom = "point", shape = 21, size = 3, fill="red",  colour="black" )+
  scale_y_continuous(limits = quantile(fs10$fields_S1_0_Power, c(0.1, 0.9)))+
  theme_minimal()+
  theme(legend.position = "none")+
  scale_fill_brewer(palette="Dark2")+
  scale_x_discrete(name ="Time", limits=c(time))+theme(legend.position = "none")
  # +labs(x=expression(bold("Part Count in groups of 10")),
  #      y=expression(bold("S1 Power (W)")),
  #      title=expression(bold("S1 Power for Finish Splines Tool over 1 shift on 11/02/22")),
  #      subtitle=expression("Mean = red circle; Median = blue diamond")) +
  # theme(plot.title=element_text(size=18.), plot.subtitle = element_text(size=14), legend.text = element_text(size=15), legend.key.size = unit(2,"line"), axis.text = element_text(size=15)) +
  # scale_color_discrete(name =expression(underline("Group of 10 Parts"))) +theme(legend.title.align = 1, legend.title = element_text(size=15)) +
  # guides(color = guide_legend(override.aes = list(size = 3)))


# Torque
s1_torque_plot <-
  ggplot(fs10, aes(`fields_General_0_Part Count`, fields_S1_0_Torque, group = `fields_General_0_Part Count`)) +
  geom_boxplot(colour="black")+
  geom_jitter(aes(color=as.factor(`fields_General_0_Part Count`)), outlier.shape=NA, position=position_jitter(0.2), size = 2)+
  stat_summary(fun = "median", geom = "point", shape = 23, size = 3, fill="blue") +
  stat_summary(fun = "mean", geom = "point", shape = 21, size = 3, fill="red",  colour="black" )+
  scale_y_continuous(limits = quantile(fs10$fields_S1_0_Torque, c(0.1, 0.9)))+
  theme_minimal()+
  theme(legend.position = "none")+
  scale_fill_brewer(palette="Dark2")+
  scale_x_discrete(name ="Time", limits=c(time))+theme(legend.position = "none")
  # +labs(x=expression(bold("Part Count in groups of 10")),
  #      y=expression(bold("S1 Torque (Nm)")),
  #      title=expression(bold("S1 Torque for Finish Splines Tool over 1 shift on 11/02/22")),
  #      subtitle=expression("Mean = red circle; Median = blue diamond")) +
  # theme(plot.title=element_text(size=18.), plot.subtitle = element_text(size=14), legend.text = element_text(size=15), legend.key.size = unit(2,"line"), axis.text = element_text(size=15)) +
  # scale_color_discrete(name =expression(underline("Group of 10 Parts"))) +theme(legend.title.align = 1, legend.title = element_text(size=15)) +
  # guides(color = guide_legend(override.aes = list(size = 3)))


# Load
s1_load_plot <-
  ggplot(fs10, aes(`fields_General_0_Part Count`, fields_S1_0_Load, group = `fields_General_0_Part Count`)) +
  geom_boxplot(colour="black")+
  geom_jitter(aes(color=as.factor(`fields_General_0_Part Count`)), outlier.shape=NA, position=position_jitter(0.2), size = 2)+
  stat_summary(fun = "median", geom = "point", shape = 23, size = 3, fill="blue") +
  stat_summary(fun = "mean", geom = "point", shape = 21, size = 3, fill="red",  colour="black" )+
  scale_y_continuous(limits = quantile(fs10$fields_S1_0_Load, c(0.1, 0.9)))+
  theme_minimal()+
  theme(legend.position = "none")+
  scale_fill_brewer(palette="Dark2")+
  scale_x_discrete(name ="Time", limits=c(time)) +theme(legend.position = "none")
  #+ labs(x=expression(bold("Part Count in groups of 10")),
  #      y=expression(bold("S1 Load (%)")),
  #      title=expression(bold("S1 Load for Finish Splines Tool over 1 shift on 11/02/22")),
  #      subtitle=expression("Mean = red circle; Median = blue diamond")) +
  # theme(plot.title=element_text(size=18.), plot.subtitle = element_text(size=14), legend.text = element_text(size=15), legend.key.size = unit(2,"line"), axis.text = element_text(size=15)) +
  # scale_color_discrete(name =expression(underline("Group of 10 Parts"))) +theme(legend.title.align = 1, legend.title = element_text(size=15)) +
  # guides(color = guide_legend(override.aes = list(size = 3)))


# RPM
s1_rpm_plot <-
  ggplot(fs10, aes(`fields_General_0_Part Count`, fields_S1_0_RPM, group = `fields_General_0_Part Count`)) +
  geom_boxplot(colour="black")+
  geom_jitter(aes(color=as.factor(`fields_General_0_Part Count`)), outlier.shape=NA, position=position_jitter(0.2), size = 2)+
  stat_summary(fun = "median", geom = "point", shape = 23, size = 3, fill="blue") +
  stat_summary(fun = "mean", geom = "point", shape = 21, size = 3, fill="red",  colour="black" )+
  scale_y_continuous(limits = quantile(fs10$fields_S1_0_RPM, c(0.1, 0.9)))+
  theme_minimal()+
  theme(legend.position = "none")+
  scale_fill_brewer(palette="Dark2")+
  scale_x_discrete(name ="Time", limits=c(time))+theme(legend.position = "none")
  #+labs(x=expression(bold("Part Count in groups of 10")),
  #      y=expression(bold("S1 RPM")),
  #      title=expression(bold("S1 RPM for Finish Splines Tool over 1 shift on 11/02/22")),
  #      subtitle=expression("Mean = red circle; Median = blue diamond")) +
  # theme(plot.title=element_text(size=18.), plot.subtitle = element_text(size=14), legend.text = element_text(size=15), legend.key.size = unit(2,"line"), axis.text = element_text(size=15)) +
  # scale_color_discrete(name =expression(underline("Group of 10 Parts"))) +theme(legend.title.align = 1, legend.title = element_text(size=15)) +
  # guides(color = guide_legend(override.aes = list(size = 3)))

# Feed Rate
s1_fr_plot <-
  ggplot(fs10, aes(`fields_General_0_Part Count`, `fields_S1_0_Feed Rate`, group = `fields_General_0_Part Count`)) +
  geom_boxplot(colour="black")+
  geom_jitter(aes(color=as.factor(`fields_General_0_Part Count`)), outlier.shape=NA, position=position_jitter(0.2), size = 2)+
  stat_summary(fun = "median", geom = "point", shape = 23, size = 3, fill="blue") +
  stat_summary(fun = "mean", geom = "point", shape = 21, size = 3, fill="red",  colour="black" )+
  scale_y_continuous(limits = quantile(fs10$`fields_S1_0_Feed Rate`, c(0.1, 0.9)))+
  theme_minimal()+
  theme(legend.position = "none")+
  scale_fill_brewer(palette="Dark2")+
  scale_x_discrete(name ="Time", limits=c(time)) +theme(legend.position = "none")
  # +labs(x=expression(bold("Part Count in groups of 10")),
  #      y=expression(bold("S1 Feed Rate (inch/min)")),
  #      title=expression(bold("S1 Power for Finish Splines Tool over 1 shift on 11/02/22")),
  #      subtitle=expression("Mean = red circle; Median = blue diamond")) +
  # theme(plot.title=element_text(size=18.), plot.subtitle = element_text(size=14), legend.text = element_text(size=15), legend.key.size = unit(2,"line"), axis.text = element_text(size=15)) +
  # scale_color_discrete(name =expression(underline("Group of 10 Parts"))) +theme(legend.title.align = 1, legend.title = element_text(size=15)) +
  # guides(color = guide_legend(override.aes = list(size = 3)))

# Chip Load
s1_cl_spread <-
  fs10 %>%
  ggplot(aes(x =fs10$S1_chip_load, fill= factor(`fields_General_0_Part Count`))) +
  geom_density(aes(y = ..density..), adjust = 1, alpha = 0.5) +
  theme_bw() +
  facet_wrap(~`fields_General_0_Part Count`, ncol=1) +
  theme(legend.position = "none") +
  scale_fill_brewer(palette = "Dark2") + scale_x_continuous(limits = c(0.002776, 0.00278))

#--------
# S4
#--------

#Power
s4_power_plot <-
  ggplot(fs10, aes(`fields_General_0_Part Count`, fields_S4_0_Power, group = `fields_General_0_Part Count`)) +
  geom_boxplot(colour="black")+
  geom_jitter(aes(color=as.factor(`fields_General_0_Part Count`)), outlier.shape=NA, position=position_jitter(0.2), size = 2)+
  stat_summary(fun = "median", geom = "point", shape = 23, size = 3, fill="blue") +
  stat_summary(fun = "mean", geom = "point", shape = 21, size = 3, fill="red",  colour="black" )+
  scale_y_continuous(limits = quantile(fs10$fields_S4_0_Power, c(0.1, 0.9)))+
  theme_minimal()+
  theme(legend.position = "none")+
  scale_fill_brewer(palette="Dark2")+
  scale_x_discrete(name ="Time", limits=c(time))
  #+ labs(x=expression(bold("Part Count in groups of 10")),
  #      y=expression(bold("S4 Power (W)")),
  #      title=expression(bold("S4 Power for Finish Splines Tool over 1 shift on 11/02/22")),
  #      subtitle=expression("Mean = red circle; Median = blue diamond")) +
  # theme(plot.title=element_text(size=18.), plot.subtitle = element_text(size=14), legend.text = element_text(size=15), legend.key.size = unit(2,"line"), axis.text = element_text(size=15)) +
  # scale_color_discrete(name =expression(underline("Group of 10 Parts"))) +theme(legend.title.align = 1, legend.title = element_text(size=15)) +
  # guides(color = guide_legend(override.aes = list(size = 3)))


# Torque
s4_torque_plot <-
  ggplot(fs10, aes(`fields_General_0_Part Count`, fields_S4_0_Torque, group = `fields_General_0_Part Count`)) +
  geom_boxplot(colour="black")+
  geom_jitter(aes(color=as.factor(`fields_General_0_Part Count`)), outlier.shape=NA, position=position_jitter(0.2), size = 2)+
  stat_summary(fun = "median", geom = "point", shape = 23, size = 3, fill="blue") +
  stat_summary(fun = "mean", geom = "point", shape = 21, size = 3, fill="red",  colour="black" )+
  scale_y_continuous(limits = quantile(fs10$fields_S4_0_Torque, c(0.1, 0.9)))+
  theme_minimal()+
  theme(legend.position = "none")+
  scale_fill_brewer(palette="Dark2")+
  scale_x_discrete(name ="Time", limits=c(time))
#+  labs(x=expression(bold("Part Count in groups of 10")),
  #      y=expression(bold("S4 Torque (Nm)")),
  #      title=expression(bold("S4 Torque for Finish Splines Tool over 1 shift on 11/02/22")),
  #      subtitle=expression("Mean = red circle; Median = blue diamond")) +
  # theme(plot.title=element_text(size=18.), plot.subtitle = element_text(size=14), legend.text = element_text(size=15), legend.key.size = unit(2,"line"), axis.text = element_text(size=15)) +
  # scale_color_discrete(name =expression(underline("Group of 10 Parts"))) +theme(legend.title.align = 1, legend.title = element_text(size=15)) +
  # guides(color = guide_legend(override.aes = list(size = 3)))


# Load
s4_load_plot <-
  ggplot(fs10, aes(`fields_General_0_Part Count`, fields_S4_0_Load, group = `fields_General_0_Part Count`)) +
  geom_boxplot(colour="black")+
  geom_jitter(aes(color=as.factor(`fields_General_0_Part Count`)), outlier.shape=NA, position=position_jitter(0.2), size = 2)+
  stat_summary(fun = "median", geom = "point", shape = 23, size = 3, fill="blue") +
  stat_summary(fun = "mean", geom = "point", shape = 21, size = 3, fill="red",  colour="black" )+
  scale_y_continuous(limits = quantile(fs10$fields_S4_0_Load, c(0.1, 0.9)))+
  theme_minimal()+
  theme(legend.position = "none")+
  scale_fill_brewer(palette="Dark2")+
  scale_x_discrete(name ="Time", limits=c(time))
  # labs(x=expression(bold("Part Count in groups of 10")),
  #      y=expression(bold("S4 Load (%)")),
  #      title=expression(bold("S4 Load for Finish Splines Tool over 1 shift on 11/02/22")),
  #      subtitle=expression("Mean = red circle; Median = blue diamond")) +
  # theme(plot.title=element_text(size=18.), plot.subtitle = element_text(size=14), legend.text = element_text(size=15), legend.key.size = unit(2,"line"), axis.text = element_text(size=15)) +
  # scale_color_discrete(name =expression(underline("Group of 10 Parts"))) +theme(legend.title.align = 1, legend.title = element_text(size=15)) +
  # guides(color = guide_legend(override.aes = list(size = 3)))


# RPM
s4_rpm_plot <-
  ggplot(fs10, aes(`fields_General_0_Part Count`, fields_S4_0_RPM, group = `fields_General_0_Part Count`)) +
  geom_boxplot(colour="black")+
  geom_jitter(aes(color=as.factor(`fields_General_0_Part Count`)), outlier.shape=NA, position=position_jitter(0.2), size = 2)+
  stat_summary(fun = "median", geom = "point", shape = 23, size = 3, fill="blue") +
  stat_summary(fun = "mean", geom = "point", shape = 21, size = 3, fill="red",  colour="black" )+
  scale_y_continuous(limits = quantile(fs10$fields_S4_0_RPM, c(0.1, 0.9)))+
  theme_minimal()+
  theme(legend.position = "none")+
  scale_fill_brewer(palette="Dark2")+
  scale_x_discrete(name ="Time", limits=c(time))
  # labs(x=expression(bold("Part Count in groups of 10")),
  #      y=expression(bold("S4 RPM")),
  #      title=expression(bold("S4 PRM for Finish Splines Tool over 1 shift on 11/02/22")),
  #      subtitle=expression("Mean = red circle; Median = blue diamond")) +
  # theme(plot.title=element_text(size=18.), plot.subtitle = element_text(size=14), legend.text = element_text(size=15), legend.key.size = unit(2,"line"), axis.text = element_text(size=15)) +
  # scale_color_discrete(name =expression(underline("Group of 10 Parts"))) +theme(legend.title.align = 1, legend.title = element_text(size=15)) +
  # guides(color = guide_legend(override.aes = list(size = 3)))

# Feed Rate
s4_fr_plot <-
  ggplot(fs10, aes(`fields_General_0_Part Count`, `fields_S4_0_Feed Rate`, group = `fields_General_0_Part Count`)) +
  geom_boxplot(colour="black")+
  geom_jitter(aes(color=as.factor(`fields_General_0_Part Count`)), outlier.shape=NA, position=position_jitter(0.2), size = 2)+
  stat_summary(fun = "median", geom = "point", shape = 23, size = 3, fill="blue") +
  stat_summary(fun = "mean", geom = "point", shape = 21, size = 3, fill="red",  colour="black" )+
  scale_y_continuous(limits = quantile(fs10$`fields_S4_0_Feed Rate`, c(0.1, 0.9)))+
  theme_minimal()+
  theme(legend.position = "none")+
  scale_fill_brewer(palette="Dark2")+
  scale_x_discrete(name ="Time", limits=c(time))
  # labs(x=expression(bold("Part Count in groups of 10")),
  #      y=expression(bold("S4 Feed Rate (inch/min)")),
  #      title=expression(bold("S4 Feed Rate for Finish Splines Tool over 1 shift on 11/02/22")),
  #      subtitle=expression("Mean = red circle; Median = blue diamond")) +
  # theme(plot.title=element_text(size=18.), plot.subtitle = element_text(size=14), legend.text = element_text(size=15), legend.key.size = unit(2,"line"), axis.text = element_text(size=15)) +
  # scale_color_discrete(name =expression(underline("Group of 10 Parts"))) +theme(legend.title.align = 1, legend.title = element_text(size=15)) +
  # guides(color = guide_legend(override.aes = list(size = 3)))

# Chip Load
s4_cl_spread <-
  fs10 %>%
  ggplot(aes(x =fs10$S4_chip_load, fill= factor(`fields_General_0_Part Count`))) +
  geom_density(aes(y = ..density..), adjust = 1, alpha = 0.5) +
  theme_bw() +
  facet_wrap(~`fields_General_0_Part Count`, ncol=1) +
  theme(legend.position = "none") +
  scale_fill_brewer(palette = "Dark2") + scale_x_continuous(limits = c(0.002776, 0.00278))


ggarrange(s4_rpm_plot, s1_rpm_plot, ncol = 2)
ggarrange(s4_power_plot, s4_torque_plot, s4_load_plot, s4_rpm_plot, ncol = 2, nrow = 2)
ggarrange(s4_fr_plot, s4_cl_spread, ncol = 2)

