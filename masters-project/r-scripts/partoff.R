#-------------------------------------------------------------------
#checking what RPM bands the tools operate in and no of occurrences
#-------------------------------------------------------------------
partoff2<-filter(partoff, `fields_General_0_Part Count` == 2) #filtering for one part (number 2)
partoff2_coord <- partoff2[ , grepl("Coord", names( partoff2 ) ) ] #sort for columns with RPM in column title for partcount = 2
unique(partoff2_coord$fields_Channels_0_Y_Coord)
table(partoff2_coord$fields_Channels_0_Y_Coord)#load("~/onedrive/Scenic/data/r/initial.RData")

partoffY0<-filter(partoff,
                  fields_Channels_0_Y_Coord == 0,
                  `fields_Channels_0_Program Status`==3,
                  `fields_General_0_Axes Status`==0,
                  between(partoff$`fields_General_0_Part Count`, 16,69))

#-------------------------
#plotting (and) stats
#-------------------------
#df_po_spindle1 <- partoffY0[ , grepl("fields_S1", names( partoffY0) ) ] #for future reference if needed
#df_po_spindle4 <- partoffY0[ , grepl("fields_S4", names( partoffY0) ) ] #for future reference if needed

#-----------------------------
#  DRIVE VARIABLES
#-----------------------------
po_Y0<-setNames(data.frame(partoffY0$Date), "Date")
po_Y0$Time<-partoffY0$Time
po_Y0$PartCount<-as.numeric(partoffY0$`fields_General_0_Part Count`)
po_Y0$DrivePower <- as.numeric(partoffY0$fields_Channels_0_DrivePower)
po_Y0$DriveCurrent <- as.numeric(partoffY0$fields_Channels_0_DriveCurrent)
po_Y0$DriveTorque <- as.numeric(partoffY0$fields_Channels_0_DriveTorque)
po_Y0$DriveLoad <- as.numeric(partoffY0$fields_Channels_0_DriveLoad)
po_Y0$DriveTemp<-as.numeric(partoffY0$`fields_Channels_0_Drive Temperature`)
po_Y0$Name<-as.factor(partoffY0$fields_Tool_0_tool_name)
po_Y0$ToolNo<-as.numeric(partoffY0$fields_Tool_0_tool_no)
po_Y0$Type<-as.factor("Mill")
po_Y0$Life<-as.numeric(partoffY0$fields_Tool_0_tool_life_min)
po_Y0$X<-as.numeric(partoffY0$fields_Channels_0_X_Coord)
po_Y0$Y<-as.numeric(partoffY0$fields_Channels_0_Y_Coord)
po_Y0$Z<-as.numeric(partoffY0$fields_Channels_0_Z_Coord)

#-----------------------------
#  S1 VARIABLES
#-----------------------------
po_Y0_S1<-setNames(data.frame(as.factor(matrix("S1", nrow = nrow(po_Y0)))), "Spindle")
po_Y0_S1$DRPM<-as.numeric(partoffY0$`fields_S1_0_Desired RPM`)
po_Y0_S1$RPM<-as.numeric(partoffY0$fields_S1_0_RPM)
po_Y0_S1$Load<-as.numeric(partoffY0$fields_S1_0_Load)
po_Y0_S1$Power<-as.numeric(partoffY0$fields_S1_0_Power)
po_Y0_S1$Torque<-as.numeric(partoffY0$fields_S1_0_Torque)
po_Y0_S1$Current<-as.numeric(partoffY0$fields_S1_0_Current)
po_Y0_S1$FR<-as.numeric(partoffY0$`fields_S1_0_Feed Rate`)
po_Y0_S1$DFR<-as.numeric(partoffY0$`fields_S1_0_Desired Feed Rate`)
po_Y0_S1$DCL<-as.numeric(partoffY0$`fields_S1_0_Desired RPM`)/as.numeric(partoffY0$`fields_S1_0_Desired Feed Rate`)
po_Y0_S1$CL<-as.numeric(partoffY0$`fields_S1_0_RPM`)/as.numeric(partoffY0$`fields_S1_0_Feed Rate`)

po_Y0_S4$DCL[is.nan(po_Y0_S4$DCL)] <-0
po_Y0_S4$CL[is.nan(po_Y0_S4$CL)] <-0

#-----------------------------
#  S4 VARIABLES
#-----------------------------
po_Y0_S4<-setNames(data.frame(as.factor(matrix("S4", nrow = nrow(po_Y0)))), "Spindle")
po_Y0_S4$DRPM<-as.numeric(partoffY0$`fields_S4_0_Desired RPM`)
po_Y0_S4$RPM<-as.numeric(partoffY0$fields_S4_0_RPM)
po_Y0_S4$Load<-as.numeric(partoffY0$fields_S4_0_Load)
po_Y0_S4$Power<-as.numeric(partoffY0$fields_S4_0_Power)
po_Y0_S4$Torque<-as.numeric(partoffY0$fields_S4_0_Torque)
po_Y0_S4$Current<-as.numeric(partoffY0$fields_S4_0_Current)
po_Y0_S4$DFR<-as.numeric(partoffY0$`fields_S4_0_Desired Feed Rate`)
po_Y0_S4$FR<-as.numeric(partoffY0$`fields_S4_0_Feed Rate`)
po_Y0_S4$DCL<-as.numeric(partoffY0$`fields_S4_0_Desired RPM`)/as.numeric(partoffY0$`fields_S4_0_Desired Feed Rate`)
po_Y0_S4$CL<-as.numeric(partoffY0$`fields_S4_0_RPM`)/as.numeric(partoffY0$`fields_S4_0_Feed Rate`)

po_Y0_S4$DCL[is.nan(po_Y0_S4$DCL)] <-0
po_Y0_S4$CL[is.nan(po_Y0_S4$CL)] <-0

po_Y0_S1<-cbind(po_Y0, po_Y0_S1)
po_Y0_S4<-cbind(po_Y0, po_Y0_S4)

plot_ly(po_Y0_S4, x = ~PartCount, y = ~abs(Torque), type = 'scatter',  mode = 'markers')
#---------------------------------------------------------------------------------------------------------------

po_Scurr<-po_Y0_S1%>%
  group_by(PartCount) %>%
  summarise(
    Mean = mean(abs(Current), trim=0.1),
    Median  = median(abs(Current)),
    Mode = Mode(abs(Current)),
    Q1 = quantile(Current, 0.25),
    Q3 = quantile(Current, 0.75),
    IQR = Q3-Q1,
    SD = sd(Current),
    SD_trim = sd_trim(Current, trim=0.1),
    mad=mad(Current, constant = 1.4826),
    size = length(Current))

po_Sload<-po_Y0_S1%>%
  group_by(PartCount) %>%
  summarise(
    Mean = mean(Load, trim=0.1),
    Median  = median(Load),
    Mode = Mode(Load),
    Q1 = quantile(Load, 0.25),
    Q3 = quantile(Load, 0.75),
    IQR = Q3-Q1,
    SD = sd(Load),
    SD_trim = sd_trim(Load, trim=0.1),
    mad=mad(Load, constant = 1.4826),
    size = length(Load))

po_STorque<-po_Y0_S1%>%
  group_by(PartCount) %>%
  summarise(
    Mean = mean(abs(Torque), trim=0.1),
    Median  = median(abs(Torque)),
    Mode = Mode(Torque),
    Q1 = quantile(Torque, 0.25),
    Q3 = quantile(Torque, 0.75),
    IQR = Q3-Q1,
    SD = sd(Torque),
    SD_trim = sd_trim(Torque, trim=0.1),
    mad=mad(Torque, constant = 1.4826),
    size = length(Torque))


#----------- DRIVE
po_DPower<-po_Y0%>%
  group_by(PartCount) %>%
  summarise(
    Mean = mean(DrivePower, trim=0.1),
    Median  = median(DrivePower),
    Mode = Mode(DrivePower),
    Q1 = quantile(DrivePower, 0.25),
    Q3 = quantile(DrivePower, 0.75),
    IQR = Q3-Q1,
    SD = sd(DrivePower),
    SD_trim = sd_trim(DrivePower, trim=0.1),
    mad=mad(DrivePower, constant = 1.4826),
    size = length(DrivePower))

po_DCurrent<-po_Y0%>%
  group_by(PartCount) %>%
  summarise(
    Mean = mean(DriveCurrent, trim=0.1),
    Median  = median(DriveCurrent),
    Mode = Mode(DriveCurrent),
    Q1 = quantile(DriveCurrent, 0.25),
    Q3 = quantile(DriveCurrent, 0.75),
    IQR = Q3-Q1,
    SD = sd(DriveCurrent),
    SD_trim = sd_trim(DriveCurrent, trim=0.1),
    mad=mad(DriveCurrent, constant = 1.4826),
    size = length(DriveCurrent))

po_DLoad<-po_Y0%>%
  group_by(PartCount) %>%
  summarise(
    Mean = mean(DriveLoad, trim=0.1),
    Median  = median(DriveLoad),
    Mode = Mode(DriveLoad),
    Q1 = quantile(DriveLoad, 0.25),
    Q3 = quantile(DriveLoad, 0.75),
    IQR = Q3-Q1,
    SD = sd(DriveLoad),
    SD_trim = sd_trim(DriveLoad, trim=0.1),
    mad=mad(DriveLoad, constant = 1.4826),
    size = length(DriveLoad))

po_DTorque<-po_Y0%>%
  group_by(PartCount) %>%
  summarise(
    Mean = mean(DriveTorque, trim=0.1),
    Median  = median(DriveTorque),
    Mode = Mode(DriveTorque),
    Q1 = quantile(DriveTorque, 0.25),
    Q3 = quantile(DriveTorque, 0.75),
    IQR = Q3-Q1,
    SD = sd(DriveTorque),
    SD_trim = sd_trim(DriveTorque, trim=0.1),
    mad=mad(DriveTorque, constant = 1.4826),
    size = length(DriveTorque))

po_DTemp<-po_Y0%>%
  group_by(PartCount) %>%
  summarise(
    Mean = mean(DriveTemp, trim=0.1),
    Median  = median(DriveTemp),
    Mode = Mode(DriveTemp),
    Q1 = quantile(DriveTemp, 0.25),
    Q3 = quantile(DriveTemp, 0.75),
    IQR = Q3-Q1,
    SD = sd(DriveTemp),
    SD_trim = sd_trim(DriveTemp, trim=0.1),
    mad=mad(DriveTemp, constant = 1.4826),
    size = length(DriveTemp))

#----------- S4


po_S4_curr<-po_Y0_S4%>%
  group_by(PartCount) %>%
  summarise(
    Mean = mean(abs(Current), trim=0.1),
    Median  = median(abs(Current)),
    Mode = Mode(Current),
    Q1 = quantile(Current, 0.25),
    Q3 = quantile(Current, 0.75),
    IQR = Q3-Q1,
    SD = sd(Current),
    SD_trim = sd_trim(Current, trim=0.1),
    mad=mad(Current, constant = 1.4826),
    size = length(Current))

po_S4_load<-po_Y0_S4%>%
  group_by(PartCount) %>%
  summarise(
    Mean = mean(Load, trim=0.1),
    Median  = median(Load),
    Mode = Mode(Load),
    Q1 = quantile(Load, 0.25),
    Q3 = quantile(Load, 0.75),
    IQR = Q3-Q1,
    SD = sd(Load),
    SD_trim = sd_trim(Load, trim=0.1),
    mad=mad(Load, constant = 1.4826),
    size = length(Load))

po_S4_Torque<-po_Y0_S4%>%
  group_by(PartCount) %>%
  summarise(
    Mean = mean(abs(Torque), trim=0.1),
    Median  = median(abs(Torque)),
    Mode = Mode(Torque),
    Q1 = quantile(Torque, 0.25),
    Q3 = quantile(Torque, 0.75),
    IQR = Q3-Q1,
    SD = sd(Torque),
    SD_trim = sd_trim(Torque, trim=0.1),
    mad=mad(Torque, constant = 1.4826),
    size = length(Torque))

#--------------------------------

par(mfrow=c(2,1))
plot(po_S4_curr$Mean) + lines(predict(lm(po_S4_curr$Median~po_Scurr$PartCount)),col='green')
plot(po_S4_curr$Median) + lines(predict(lm(po_S4_curr$Median~po_Scurr$PartCount)),col='blue')

a<-filter(po_Y0_S1, po_Y0_S1$PartCount=='10')
b<-filter(po_Y0_S1, po_Y0_S1$PartCount=='20')
c<-filter(po_Y0_S1, po_Y0_S1$PartCount=='30')
d<-filter(po_Y0_S1, po_Y0_S1$PartCount=='40')
e<-filter(po_Y0_S1, po_Y0_S1$PartCount=='50')
f<-filter(po_Y0_S1, po_Y0_S1$PartCount=='60')
g<-filter(po_Y0_S1, po_Y0_S1$PartCount=='70')

# par(mfrow=c(7,1))
# plot(density(a$DriveTorque), xlim=c(0, 5))
# plot(density(b$DriveTorque), xlim=c(0, 5))
# plot(density(c$DriveTorque), xlim=c(0, 5))
# plot(density(d$DriveTorque), xlim=c(0, 5))
# plot(density(e$DriveTorque), xlim=c(0, 5))
# plot(density(f$DriveTorque), xlim=c(0, 5))
# plot(density(g$DriveTorque), xlim=c(0, 5))

aa<-cbind(as.numeric(a$Load), a$PartCount)
dd<-cbind(as.numeric(d$Load), d$PartCount)
gg<-cbind(as.numeric(g$Load), g$PartCount)
h<-data.frame(rbind(aa, dd,gg))

kable(head(h))
h$X2<-as.factor(h$X2)
names(h)[2]<-"PartCount"

ggplot(h, aes(x = X1, fill = PartCount)) +
  geom_density(alpha = 0.5) +
    xlim(-0.1, 0.1)+
  labs(x=expression(bold("S1 Torque")), y=expression(bold("Density")),
       title=expression(bold("Spread of Torque values for Chamdrill 8D Tool: 10th & 70th"))) +
  theme_minimal() +
  theme(plot.title=element_text(hjust=0.5, size=14))



s1cu <- list(
  text = "<b>S1 Current</b>",
  xref = "paper",
  yref = "paper",
  yanchor = "top",
  xanchor = "center",
  align = "center",
  x = 0.5,
  y = 1,
  showarrow = FALSE
)

s1lo <- list(
  text = "<b>S1 Load</b>",
  xref = "paper",
  yref = "paper",
  yanchor = "bottom",
  xanchor = "center",
  align = "center",
  x = 0.5,
  y = 1,
  showarrow = FALSE
)

s1to <- list(
  text = "<b>S1 Torque</b>",
  xref = "paper",
  yref = "paper",
  yanchor = "bottom",
  xanchor = "center",
  align = "center",
  x = 0.5,
  y = 1,
  showarrow = FALSE
)

plotly_curr_S1<-plot_ly(po_Scurr, x = ~PartCount, y = ~Median, type = 'scatter')
s1curr_plot<-add_lines(plotly_curr_S1, x=~PartCount, y=predict(lm(po_Scurr$Median~poly(po_Scurr$PartCount, 3, raw=TRUE))), name="Linear Trendline", showlegend = F)
s1curr_plot = layout(s1curr_plot,  annotations = s1cu, title = "Median Values of Current Draw in Spindle over 74 parts using Chamdrill 8D Drill Tool")
print(s1curr_plot)

plotly_load_S1<-plot_ly(po_Sload, x = ~PartCount, y = ~Median, type = 'scatter')
s1load_plot<-add_lines(plotly_load_S1, x=~PartCount, y=predict(lm(po_Sload$Median~poly(po_Sload$PartCount, 3, raw=TRUE))), name="Linear Trendline", showlegend = F)
s1load_plot = layout(s1load_plot,  annotations = s1lo, title = "Median Values of Load on Spindle over 74 parts using Chamdrill 8D Drill Tool")
print(s1load_plot)

plotly_torque_S1<-plot_ly(po_STorque, x = ~PartCount, y = ~Median, type = 'scatter')
s1torque_plot<-add_lines(plotly_torque_S1, x=~PartCount, y=predict(lm(po_STorque$Median~poly(po_STorque$PartCount,3,raw=TRUE))), name="Linear Trendline", showlegend = F)
#s1torque_plot<-add_lines(plotly_torque_S1, x=~PartCount, y=predict(lm(po_STorque$Median~po_STorque$PartCount)), name="Linear Trendline", showlegend = F)
s1torque_plot = layout(s1torque_plot,  annotations = s1to, title = "Median Values of Torque on Spindle over 74 parts using Chamdrill 8D Drill Tool")
print(s1torque_plot)

plot_ly(po_DTorque, x = ~PartCount, y = ~Mean, type = 'scatter',  mode = 'lines+markers')


s4cu <- list(
  text = "<b>S4 Current</b>",
  xref = "paper",
  yref = "paper",
  yanchor = "top",
  xanchor = "center",
  align = "center",
  x = 0.5,
  y = 1,
  showarrow = FALSE
)

s4lo <- list(
  text = "<b>S4 Load</b>",
  xref = "paper",
  yref = "paper",
  yanchor = "bottom",
  xanchor = "center",
  align = "center",
  x = 0.5,
  y = 1,
  showarrow = FALSE
)

s4to <- list(
  text = "<b>S4 Torque</b>",
  xref = "paper",
  yref = "paper",
  yanchor = "bottom",
  xanchor = "center",
  align = "center",
  x = 0.5,
  y = 1,
  showarrow = FALSE
)


plotly_curr_S4<-plot_ly(po_S4_curr, x = ~PartCount, y = ~Median, type = 'scatter')
s4curr_plot<-add_lines(plotly_curr_S4, x=~PartCount, y=predict(lm(po_S4_curr$Median~poly(po_S4_curr$PartCount,3,raw=TRUE))), name="Linear Trendline", showlegend = F)
s4curr_plot = layout(s4curr_plot, annotations = s4cu, title = "Median Values of Current Draw in Chuck over 74 parts using Chamdrill 8D Drill Tool")
print(s4curr_plot)

plotly_load_S4<-plot_ly(po_S4_load, x = ~PartCount, y = ~Median, type = 'scatter')
s4load_plot<-add_lines(plotly_load_S4, x=~PartCount, y=predict(lm(po_S4_load$Median~poly(po_S4_load$PartCount,3,raw=TRUE))), name="Linear Trendline", showlegend = F)
s4load_plot = layout(s4load_plot, annotations = s4lo, title = "Median Values of Load on Chuck over 74 parts using Chamdrill 8D Drill Tool")
print(s4load_plot)

plotly_torque_S4<-plot_ly(po_S4_Torque, x = ~PartCount, y = ~Median, type = 'scatter')
s4torque_plot<-add_lines(plotly_torque_S4, x=~PartCount, y=predict(lm(po_S4_Torque$Median~poly(po_S4_Torque$PartCount,3,raw=TRUE))), name="Linear Trendline", showlegend = F)
s4torque_plot = layout(s4torque_plot, annotations = s4to, title = "Median Values of Torque on Chuck over 74 parts using Chamdrill 8D Drill Tool")
print(s4torque_plot)

t <- list(
  family = "Calibri",
  size = 25,
  color = 'Black')


fig <- subplot(s1curr_plot,
               s4curr_plot,
               s1torque_plot,
               s4torque_plot,
               s1load_plot,
               s4load_plot,
               nrows = 3,
               shareX = TRUE) %>%
  layout(title = list(text="<b>Median Values for Chamdrill 8D Tool over 'Life'</b>",
                      font = t,
                      y = 1, x = 0.5,
                      xanchor = 'center',
                      yanchor =  'top'),
         showlegend =FALSE,
         text=sprintf("<b>%s</b>", 1:10),
         plot_bgcolor='#e5ecf6')

fig

#-----------------------------------------
count <-c()
for (val in po_Y0_S1$PartCount){
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

nrow(po_Y0_S1)

c10<-po_Y0_S1
c10$PartCount<-count
unique(c10$PartCount)
nrow(c10)


fscurr10<-c10 %>%
  summarize(
    Mean = mean(Current),
    Median  = median(Current),
    Mode = Mode(Current),
    Q1 = quantile(Current, 0.25),
    Q3 = quantile(Current, 0.75))

kable(fscurr10)


dftime<-which(c10$PartCount != dplyr::lag(c10$PartCount))
timez<-c10[dftime,]$Time
time<-append(c10$Time[1], timez)
time

#----------------------------------------------

#--------
# S1
#--------
count2 <-c()
for (val in po_Y0_S1$PartCount){
  if(val<10){
    count2<-append(count2,1)
  }else if(val>=10 & val<20){
    count2<-append(count2,2)
  }else if(val>=20 & val<30){
    count2<-append(count2,3)
  }else if(val>=30 & val <40){
    count2<-append(count2,4)
  }else if(val>=40 & val <50){
    count2<-append(count2,5)
  }else if(val>=50 & val<60){
    count2<-append(count2,6)
  }else if(val>=60 & val<70){
    count2<-append(count2,7)
  }else if(val>=70 & val<80){
    count2<-append(count2,8)
  }
}

length(po_Y0_S1)
nrow(po_Y0_S1)

df_po10_s1<-po_Y0_S1
df_po10_s1$PartCount <- count2
unique(df_po10_s1$PartCount)
length(df_po10_s1$PartCount)


#Power
po_s1_power_plot <-
  ggplot(df_po10_s1, aes(PartCount, Power, group = PartCount)) +
  stat_boxplot(geom ='errorbar')+
  geom_boxplot(alpha = 0.5)+
  geom_jitter(aes(color=as.factor(PartCount)), outlier.shape=NA, position=position_jitter(0.25), size=0.5)+
  stat_summary(fun = "median", geom = "point", shape = 23, size = 3, fill="blue") +
  stat_summary(fun = "mean", geom = "point", shape = 21, size = 3, fill="red",  colour="black" )+
  scale_y_continuous(limits = quantile(df_po10_s1$Power, c(0.1, 0.9)))+
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

po_c1_power_plot <-
  ggplot(df_po10_s1, aes(PartCount, DrivePower, group = PartCount)) +
  stat_boxplot(geom ='errorbar')+
  geom_boxplot(alpha = 0.5)+
  geom_jitter(aes(color=as.factor(PartCount)), outlier.shape=NA, position=position_jitter(0.25), size=0.5)+
  stat_summary(fun = "median", geom = "point", shape = 23, size = 3, fill="blue") +
  stat_summary(fun = "mean", geom = "point", shape = 21, size = 3, fill="red",  colour="black" )+
  scale_y_continuous(limits = quantile(df_po10_s1$DrivePower, c(0.1, 0.9)))+
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

po_s1_current_plot <-
  ggplot(df_po10_s1, aes(PartCount, Current, group = PartCount)) +
  stat_boxplot(geom ='errorbar')+
  geom_boxplot(alpha = 0.25, outlier.colour = "red")+
  geom_jitter(aes(color=as.factor(PartCount)), outlier.shape=NA, position=position_jitter(0.25), size=0.5)+
  stat_summary(fun = "median", geom = "point", shape = 23, size = 3, fill="blue") +
  stat_summary(fun = "mean", geom = "point", shape = 21, size = 3, fill="red",  colour="black" )+
  scale_y_continuous(limits = quantile(df_po10_s1$Current, c(0.1, 0.9)))+
  theme_minimal()+
  theme(legend.position = "none")+
  #scale_fill_brewer(palette="Dark2")+
  scale_x_discrete(name ="Time", limits=c(time))+theme(legend.position = "none")
# +labs(x=expression(bold("Part Count in groups of 10")),
#      y=expression(bold("S1 Current (A)")),
#      title=expression(bold("S1 Current for Finish Splines Tool over 1 shift on 11/02/22")),
#      subtitle=expression("Mean = red circle; Median = blue diamond")) +
# theme(plot.title=element_text(size=18.), plot.subtitle = element_text(size=14), legend.text = element_text(size=15), legend.key.size = unit(2,"line"), axis.text = element_text(size=15)) +
# scale_color_discrete(name =expression(underline("Group of 10 Parts"))) +theme(legend.title.align = 1, legend.title = element_text(size=15)) +
# guides(color = guide_legend(override.aes = list(size = 3)))

po_c1_current_plot <-
  ggplot(df_po10_s1, aes(PartCount, DriveCurrent, group = PartCount)) +
  stat_boxplot(geom ='errorbar')+
  geom_boxplot(alpha = 0.25, outlier.colour = "red")+
  geom_jitter(aes(color=as.factor(PartCount)), outlier.shape=NA, position=position_jitter(0.25), size=0.5)+
  stat_summary(fun = "median", geom = "point", shape = 23, size = 3, fill="blue") +
  stat_summary(fun = "mean", geom = "point", shape = 21, size = 3, fill="red",  colour="black" )+
  scale_y_continuous(limits = quantile(df_po10_s1$DriveCurrent, c(0.1, 0.9)))+
  theme_minimal()+
  theme(legend.position = "none")+
  #scale_fill_brewer(palette="Dark2")+
  scale_x_discrete(name ="Time", limits=c(time))+theme(legend.position = "none")
# +labs(x=expression(bold("Part Count in groups of 10")),
#      y=expression(bold("S1 Current (A)")),
#      title=expression(bold("S1 Current for Finish Splines Tool over 1 shift on 11/02/22")),
#      subtitle=expression("Mean = red circle; Median = blue diamond")) +
# theme(plot.title=element_text(size=18.), plot.subtitle = element_text(size=14), legend.text = element_text(size=15), legend.key.size = unit(2,"line"), axis.text = element_text(size=15)) +
# scale_color_discrete(name =expression(underline("Group of 10 Parts"))) +theme(legend.title.align = 1, legend.title = element_text(size=15)) +
# guides(color = guide_legend(override.aes = list(size = 3)))



# Torque
po_s1_torque_plot <-
  ggplot(df_po10_s1, aes(PartCount, Torque, group = PartCount)) +
  stat_boxplot(geom ='errorbar')+
  geom_boxplot(alpha = 0.25, outlier.colour = "red")+
  geom_jitter(aes(color=as.factor(PartCount)), outlier.shape=NA, position=position_jitter(0.25), size=0.5)+
  stat_summary(fun = "median", geom = "point", shape = 23, size = 3, fill="blue") +
  stat_summary(fun = "mean", geom = "point", shape = 21, size = 3, fill="red",  colour="black" )+
  scale_y_continuous(limits = quantile(df_po10_s1$Torque, c(0.1, 0.9)))+
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

po_c1_torque_plot <-
  ggplot(df_po10_s1, aes(PartCount, DriveTorque, group = PartCount)) +
  stat_boxplot(geom ='errorbar')+
  geom_boxplot(alpha = 0.25, outlier.colour = "red")+
  geom_jitter(aes(color=as.factor(PartCount)), outlier.shape=NA, position=position_jitter(0.25), size=0.5)+
  stat_summary(fun = "median", geom = "point", shape = 23, size = 3, fill="blue") +
  stat_summary(fun = "mean", geom = "point", shape = 21, size = 3, fill="red",  colour="black" )+
  scale_y_continuous(limits = quantile(df_po10_s1$DriveTorque, c(0.1, 0.9)))+
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
po_s1_load_plot <-
  ggplot(df_po10_s1, aes(PartCount, Load, group = PartCount)) +
  stat_boxplot(geom ='errorbar')+
  geom_boxplot(alpha = 0.25, outlier.colour = "red")+
  geom_jitter(aes(color=as.factor(PartCount)), outlier.shape=NA, position=position_jitter(0.25), size=0.5)+
  stat_summary(fun = "median", geom = "point", shape = 23, size = 3, fill="blue") +
  stat_summary(fun = "mean", geom = "point", shape = 21, size = 3, fill="red",  colour="black" )+
  scale_y_continuous(limits = quantile(df_po10_s1$Load, c(0.1, 0.9)))+
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

# Load
po_c1_load_plot <-
  ggplot(df_po10_s1, aes(PartCount, DriveLoad, group = PartCount)) +
  stat_boxplot(geom ='errorbar')+
  geom_boxplot(alpha = 0.25, outlier.colour = "red")+
  geom_jitter(aes(color=as.factor(PartCount)), outlier.shape=NA, position=position_jitter(0.25), size=0.5)+
  stat_summary(fun = "median", geom = "point", shape = 23, size = 3, fill="blue") +
  stat_summary(fun = "mean", geom = "point", shape = 21, size = 3, fill="red",  colour="black" )+
  scale_y_continuous(limits = quantile(df_po10_s1$DriveLoad, c(0.1, 0.9)))+
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
po_s1_rpm_plot <-
  ggplot(df_po10_s1, aes(PartCount, RPM, group = PartCount)) +
  stat_boxplot(geom ='errorbar')+
  geom_boxplot(alpha = 0.25, outlier.colour = "red")+
  geom_jitter(aes(color=as.factor(PartCount)), outlier.shape=NA, position=position_jitter(0.25), size=0.5)+
  stat_summary(fun = "median", geom = "point", shape = 23, size = 3, fill="blue") +
  stat_summary(fun = "mean", geom = "point", shape = 21, size = 3, fill="red",  colour="black" )+
  scale_y_continuous(limits = quantile(df_po10_s1$RPM, c(0.1, 0.9)))+
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
po_s1_fr_plot <-
  ggplot(df_po10_s1, aes(PartCount, FR, group = PartCount)) +
  stat_boxplot(geom ='errorbar')+
  geom_boxplot(alpha = 0.25, outlier.colour = "red")+
  geom_jitter(aes(color=as.factor(PartCount)), outlier.shape=NA, position=position_jitter(0.25), size=0.5)+
  stat_summary(fun = "median", geom = "point", shape = 23, size = 3, fill="blue") +
  stat_summary(fun = "mean", geom = "point", shape = 21, size = 3, fill="red",  colour="black" )+
  scale_y_continuous(limits = quantile(df_po10_s1$FR, c(0.1, 0.9)))+
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
po_s1_cl_spread <-
  df_po10_s1 %>%
  ggplot(aes(x =CL , fill= factor(PartCount))) +
  geom_density(aes(y = ..density..), adjust = 1, alpha = 0.5) +
  theme_bw() +
  facet_wrap(~PartCount, ncol=1) +
  theme(legend.position = "none") +
  scale_fill_brewer(palette = "Dark2") + scale_x_continuous(limits = c(0.002776, 0.00278)) +
  labs(x=expression(bold("Chip Load")),
       y=expression(bold("Density")),
       title=expression(bold("Theoretical Chip Load Density Plots for Finish Splines Tool over 1 shift on 11/02/22"))) +
  theme(plot.title=element_text(size=18.), plot.subtitle = element_text(size=14), legend.text = element_text(size=15), legend.key.size = unit(2,"line"), axis.text = element_text(size=15)) +
  # scale_color_discrete(name =expression(underline("Group of 10 Parts"))) +theme(legend.title.align = 1, legend.title = element_text(size=15)) +
  guides(color = guide_legend(override.aes = list(size = 3)))




#--------
# S4
#--------
length(po_Y0_S4)
nrow(po_Y0_S4)

df_po10_s4<-po_Y0_S4
df_po10_s4$PartCount <- count2
unique(df_po10_s4$PartCount)
length(df_po10_s4$PartCount)

#Power
po_s4_power_plot <-
  ggplot(df_po10_s4, aes(PartCount, Power, group = PartCount)) +
  stat_boxplot(geom ='errorbar')+
  geom_boxplot(alpha = 0.25, outlier.colour = "red")+
  geom_jitter(aes(color=as.factor(PartCount)), outlier.shape=NA, position=position_jitter(0.25), size=0.5)+
  stat_summary(fun = "median", geom = "point", shape = 23, size = 3, fill="blue") +
  stat_summary(fun = "mean", geom = "point", shape = 21, size = 3, fill="red",  colour="black" )+
  scale_y_continuous(limits = quantile(df_po10_s4$Power, c(0.1, 0.9)))+
  theme_minimal()+
  theme(legend.position = "none")+
  scale_fill_brewer(palette="Dark2")+
  scale_x_discrete(name ="Time", limits=c(time))+theme(legend.position = "none")
# +labs(x=expression(bold("Part Count in groups of 10")),
#      y=expression(bold("S1 Power (W)")),
#      title=expression(bold("S1 Power for Chamdrill 8D Tool over 1 shift on 11/02/22")),
#      subtitle=expression("Mean = red circle; Median = blue diamond")) +
# theme(plot.title=element_text(size=18.), plot.subtitle = element_text(size=14), legend.text = element_text(size=15), legend.key.size = unit(2,"line"), axis.text = element_text(size=15)) +
# scale_color_discrete(name =expression(underline("Group of 10 Parts"))) +theme(legend.title.align = 1, legend.title = element_text(size=15)) +
# guides(color = guide_legend(override.aes = list(size = 3)))

po_s4_current_plot <-
  ggplot(df_po10_s4, aes(PartCount, Current, group = PartCount)) +
  stat_boxplot(geom ='errorbar')+
  geom_boxplot(alpha = 0.25, outlier.colour = "red")+
  geom_jitter(aes(color=as.factor(PartCount)), outlier.shape=NA, position=position_jitter(0.25), size=0.5)+
  stat_summary(fun = "median", geom = "point", shape = 23, size = 3, fill="blue") +
  stat_summary(fun = "mean", geom = "point", shape = 21, size = 3, fill="red",  colour="black" )+
  scale_y_continuous(limits = quantile(df_po10_s4$Current, c(0.1, 0.9)))+
  theme_minimal()+
  theme(legend.position = "none")+
  scale_fill_brewer(palette="Dark2")+
  scale_x_discrete(name ="Time", limits=c(time))+theme(legend.position = "none")
# +labs(x=expression(bold("Part Count in groups of 10")),
#      y=expression(bold("S1 Current (A)")),
#      title=expression(bold("S1 Current for Chamdrill 8D Tool over 1 shift on 11/02/22")),
#      subtitle=expression("Mean = red circle; Median = blue diamond")) +
# theme(plot.title=element_text(size=18.), plot.subtitle = element_text(size=14), legend.text = element_text(size=15), legend.key.size = unit(2,"line"), axis.text = element_text(size=15)) +
# scale_color_discrete(name =expression(underline("Group of 10 Parts"))) +theme(legend.title.align = 1, legend.title = element_text(size=15)) +
# guides(color = guide_legend(override.aes = list(size = 3)))



# Torque
po_s4_torque_plot <-
  ggplot(df_po10_s4, aes(PartCount, Torque, group = PartCount)) +
  stat_boxplot(geom ='errorbar')+
  geom_boxplot(alpha = 0.25, outlier.colour = "red")+
  geom_jitter(aes(color=as.factor(PartCount)), outlier.shape=NA, position=position_jitter(0.25), size=0.5)+
  stat_summary(fun = "median", geom = "point", shape = 23, size = 3, fill="blue") +
  stat_summary(fun = "mean", geom = "point", shape = 21, size = 3, fill="red",  colour="black" )+
  scale_y_continuous(limits = quantile(df_po10_s4$Torque, c(0.1, 0.9)))+
  theme_minimal()+
  theme(legend.position = "none")+
  scale_fill_brewer(palette="Dark2")+
  scale_x_discrete(name ="Time", limits=c(time))+theme(legend.position = "none")
# +labs(x=expression(bold("Part Count in groups of 10")),
#      y=expression(bold("S1 Torque (Nm)")),
#      title=expression(bold("S1 Torque for Chamdrill 8D Tool over 1 shift on 11/02/22")),
#      subtitle=expression("Mean = red circle; Median = blue diamond")) +
# theme(plot.title=element_text(size=18.), plot.subtitle = element_text(size=14), legend.text = element_text(size=15), legend.key.size = unit(2,"line"), axis.text = element_text(size=15)) +
# scale_color_discrete(name =expression(underline("Group of 10 Parts"))) +theme(legend.title.align = 1, legend.title = element_text(size=15)) +
# guides(color = guide_legend(override.aes = list(size = 3)))


# Load
po_s4_load_plot <-
  ggplot(df_po10_s4, aes(PartCount, Load, group = PartCount)) +
  stat_boxplot(geom ='errorbar')+
  geom_boxplot(alpha = 0.25, outlier.colour = "red")+
  geom_jitter(aes(color=as.factor(PartCount)), outlier.shape=NA, position=position_jitter(0.25), size=0.5)+
  stat_summary(fun = "median", geom = "point", shape = 23, size = 3, fill="blue") +
  stat_summary(fun = "mean", geom = "point", shape = 21, size = 3, fill="red",  colour="black" )+
  scale_y_continuous(limits = quantile(df_po10_s4$Load, c(0.1, 0.9)))+
  theme_minimal()+
  theme(legend.position = "none")+
  scale_fill_brewer(palette="Dark2")+
  scale_x_discrete(name ="Time", limits=c(time)) +theme(legend.position = "none")
#+ labs(x=expression(bold("Part Count in groups of 10")),
#      y=expression(bold("S1 Load (%)")),
#      title=expression(bold("S1 Load for Chamdrill 8D Tool over 1 shift on 11/02/22")),
#      subtitle=expression("Mean = red circle; Median = blue diamond")) +
# theme(plot.title=element_text(size=18.), plot.subtitle = element_text(size=14), legend.text = element_text(size=15), legend.key.size = unit(2,"line"), axis.text = element_text(size=15)) +
# scale_color_discrete(name =expression(underline("Group of 10 Parts"))) +theme(legend.title.align = 1, legend.title = element_text(size=15)) +
# guides(color = guide_legend(override.aes = list(size = 3)))


# RPM
po_s4_rpm_plot <-
  ggplot(df_po10_s4, aes(PartCount, RPM, group = PartCount)) +
  stat_boxplot(geom ='errorbar')+
  geom_boxplot(alpha = 0.25, outlier.colour = "red")+
  geom_jitter(aes(color=as.factor(PartCount)), outlier.shape=NA, position=position_jitter(0.25), size=0.5)+
  stat_summary(fun = "median", geom = "point", shape = 23, size = 3, fill="blue") +
  stat_summary(fun = "mean", geom = "point", shape = 21, size = 3, fill="red",  colour="black" )+
  scale_y_continuous(limits = quantile(df_po10_s4$RPM, c(0.1, 0.9)))+
  theme_minimal()+
  theme(legend.position = "none")+
  scale_fill_brewer(palette="Dark2")+
  scale_x_discrete(name ="Time", limits=c(time))+theme(legend.position = "none")
#+labs(x=expression(bold("Part Count in groups of 10")),
#      y=expression(bold("S1 RPM")),
#      title=expression(bold("S1 RPM for Chamdrill 8D Tool over 1 shift on 11/02/22")),
#      subtitle=expression("Mean = red circle; Median = blue diamond")) +
# theme(plot.title=element_text(size=18.), plot.subtitle = element_text(size=14), legend.text = element_text(size=15), legend.key.size = unit(2,"line"), axis.text = element_text(size=15)) +
# scale_color_discrete(name =expression(underline("Group of 10 Parts"))) +theme(legend.title.align = 1, legend.title = element_text(size=15)) +
# guides(color = guide_legend(override.aes = list(size = 3)))

# Feed Rate
po_s4_fr_plot <-
  ggplot(df_po10_s4, aes(PartCount, FR, group = PartCount)) +
  stat_boxplot(geom ='errorbar')+
  geom_boxplot(alpha = 0.25, outlier.colour = "red")+
  geom_jitter(aes(color=as.factor(PartCount)), outlier.shape=NA, position=position_jitter(0.25), size=0.5)+
  stat_summary(fun = "median", geom = "point", shape = 23, size = 3, fill="blue") +
  stat_summary(fun = "mean", geom = "point", shape = 21, size = 3, fill="red",  colour="black" )+
  scale_y_continuous(limits = quantile(df_po10_s4$FR, c(0.1, 0.9)))+
  theme_minimal()+
  theme(legend.position = "none")+
  scale_fill_brewer(palette="Dark2")+
  scale_x_discrete(name ="Time", limits=c(time)) +theme(legend.position = "none")
# +labs(x=expression(bold("Part Count in groups of 10")),
#      y=expression(bold("S1 Feed Rate (inch/min)")),
#      title=expression(bold("S1 Power for Chamdrill 8D Tool over 1 shift on 11/02/22")),
#      subtitle=expression("Mean = red circle; Median = blue diamond")) +
# theme(plot.title=element_text(size=18.), plot.subtitle = element_text(size=14), legend.text = element_text(size=15), legend.key.size = unit(2,"line"), axis.text = element_text(size=15)) +
# scale_color_discrete(name =expression(underline("Group of 10 Parts"))) +theme(legend.title.align = 1, legend.title = element_text(size=15)) +
# guides(color = guide_legend(override.aes = list(size = 3)))

# Chip Load
po_s4_cl_spread <-
  df_po10_s4 %>%
  ggplot(aes(x =CL , fill= factor(PartCount))) +
  geom_density(aes(y = ..density..), adjust = 1, alpha = 0.5) +
  theme_bw() +
  facet_wrap(~PartCount, ncol=1) +
  theme(legend.position = "none") +
  scale_fill_brewer(palette = "Dark2") + scale_x_continuous(limits = c(0.002776, 0.00278))


#--------
# TEST
#--------

test<-ggarrange(po_s1_current_plot, po_s1_torque_plot, ncol=1)
ggarrange(po_s1_cl_spread, test, widths = c(1.5,2))
