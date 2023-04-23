#-------------------------------------------------------------------
#checking what RPM bands the tools operate in and no of occurrences
#-------------------------------------------------------------------
fullrad2<-filter(fullRad, `fields_General_0_Part Count` == 2) #filtering for one part (number 2)
fullrad2_rpm <- fullrad2[ , grepl("RPM", names( fullrad2 ) ) ] #sort for columns with RPM in column title for partcount = 2
unique(fullrad2_rpm$`fields_S4_0_Desired RPM`)
table(fullrad2_rpm$`fields_S4_0_Desired RPM`)
table(fullrad2$fields_Channels_0_Msg)

fullrad2500<-filter(fullRad, `fields_S4_0_Desired RPM`== 2500, fields_Channels_0_Msg == "TURN MIDDLE GRVS MSP") #clean whole tool data set for single RPM band of 2500 and correct process
length(unique(fullrad2500$`fields_General_0_Part Count`)) # checking that all parts have experienced this RPM

#-------------------------
#plotting (and) stats
#-------------------------
#df_fr_spindle1 <- fullrad2500[ , grepl("fields_S1", names( fullrad2500) ) ] #for future reference if needed
#df_fr_spindle4 <- fullrad2500[ , grepl("fields_S4", names( fullrad2500) ) ] #for future reference if needed

fr_2500<-setNames(data.frame(fullrad2500$Date), "Date")
fr_2500$Time<-fullrad2500$Time
fr_2500$PartCount<-as.numeric(fullrad2500$`fields_General_0_Part Count`)
fr_2500$DrivePower <- as.numeric(fullrad2500$fields_Channels_0_DrivePower)
fr_2500$DriveCurrent <- as.numeric(fullrad2500$fields_Channels_0_DriveCurrent)
fr_2500$DriveTorque <- as.numeric(fullrad2500$fields_Channels_0_DriveTorque)
fr_2500$DriveLoad <- as.numeric(fullrad2500$fields_Channels_0_DriveLoad)
fr_2500$DriveTemp<-as.numeric(fullrad2500$`fields_Channels_0_Drive Temperature`)
fr_2500$Name<-as.factor(fullrad2500$fields_Tool_0_tool_name)
fr_2500$Type<-as.factor("Mill")
fr_2500$Life<-as.numeric(fullrad2500$fields_Tool_0_tool_life_min)
fr_2500$X<-as.numeric(fullrad2500$fields_Channels_0_X_Coord)
fr_2500$Y<-as.numeric(fullrad2500$fields_Channels_0_Y_Coord)
fr_2500$Z<-as.numeric(fullrad2500$fields_Channels_0_Z_Coord)

fr_2500S1<-setNames(data.frame(as.factor(matrix("S1", nrow = nrow(fr_2500)))), "Spindle")
fr_2500S1$DRPM<-as.numeric(fullrad2500$`fields_S1_0_Desired RPM`)
fr_2500S1$RPM<-as.numeric(fullrad2500$fields_S1_0_RPM)
fr_2500S1$Load<-as.numeric(fullrad2500$fields_S1_0_Load)
fr_2500S1$Power<-as.numeric(fullrad2500$fields_S1_0_Power)
fr_2500S1$Torque<-as.numeric(fullrad2500$fields_S1_0_Torque)
fr_2500S1$Current<-as.numeric(fullrad2500$fields_S1_0_Current)
fr_2500S1$FR<-as.numeric(fullrad2500$`fields_S1_0_Feed Rate`)
fr_2500S1$DFR<-as.numeric(fullrad2500$`fields_S1_0_Desired Feed Rate`)
fr_2500S1$DCL<-as.numeric(fullrad2500$`fields_S1_0_Desired RPM`)/as.numeric(fullrad2500$`fields_S1_0_Desired Feed Rate`)
fr_2500S1$CL<-as.numeric(fullrad2500$`fields_S1_0_RPM`)/as.numeric(fullrad2500$`fields_S1_0_Feed Rate`)

fr_2500S4<-setNames(data.frame(as.factor(matrix("S4", nrow = nrow(fr_2500)))), "Spindle")
fr_2500S4$DRPM<-as.numeric(fullrad2500$`fields_S4_0_Desired RPM`)
fr_2500S4$RPM<-as.numeric(fullrad2500$fields_S4_0_RPM)
fr_2500S4$Load<-as.numeric(fullrad2500$fields_S4_0_Load)
fr_2500S4$Power<-as.numeric(fullrad2500$fields_S4_0_Power)
fr_2500S4$Torque<-as.numeric(fullrad2500$fields_S4_0_Torque)
fr_2500S4$Current<-as.numeric(fullrad2500$fields_S4_0_Current)
fr_2500S4$DFR<-as.numeric(fullrad2500$`fields_S4_0_Desired Feed Rate`)
fr_2500S4$FR<-as.numeric(fullrad2500$`fields_S4_0_Feed Rate`)
fr_2500S4$DCL<-as.numeric(fullrad2500$`fields_S4_0_Desired RPM`)/as.numeric(fullrad2500$`fields_S4_0_Desired Feed Rate`)
fr_2500S4$CL<-as.numeric(fullrad2500$`fields_S4_0_RPM`)/as.numeric(fullrad2500$`fields_S4_0_Feed Rate`)

fr_2500S1$DCL[is.nan(fr_2500S1$DCL)] <-0
fr_2500S1$CL[is.nan(fr_2500S1$CL)] <-0

fr_2500S4$DCL[is.nan(fr_2500S4$DCL)] <-0
fr_2500S4$CL[is.nan(fr_2500S4$CL)] <-0

fr_2500_S1<-cbind(fr_2500, fr_2500S1)
fr_2500_S4<-cbind(fr_2500, fr_2500S4)
df_fr<-rbind(fr_2500_S1, fr_2500_S4)

plot_ly(df_fr, x = ~PartCount, y = ~DriveLoad, type = 'scatter',  mode = 'markers')

#---------------------------------------------------------------------------------------------------------------

fr_Scurr<-fr_2500_S1%>%
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

fr_Sload<-fr_2500_S1%>%
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

fr_STorque<-fr_2500_S1%>%
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

fr_DTorque<-fr_2500%>%
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

fr_DTemp<-fr_2500%>%
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


fr_S4_curr<-fr_2500_S4%>%
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

fr_S4_load<-fr_2500_S4%>%
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

fr_S4_Torque<-fr_2500_S4%>%
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
plot(fr_S4_curr$Mean) + lines(predict(lm(fr_S4_curr$Median~fr_S4_curr$PartCount)),col='green')
plot(fr_S4_curr$Median) + lines(predict(lm(fr_S4_curr$Median~fr_S4_curr$PartCount)),col='blue')

a<-filter(fr_2500_S1, fr_2500_S1$PartCount=='10')
b<-filter(fr_2500_S1, fr_2500_S1$PartCount=='20')
c<-filter(fr_2500_S1, fr_2500_S1$PartCount=='30')
d<-filter(fr_2500_S1, fr_2500_S1$PartCount=='40')
e<-filter(fr_2500_S1, fr_2500_S1$PartCount=='50')
f<-filter(fr_2500_S1, fr_2500_S1$PartCount=='60')
g<-filter(fr_2500_S1, fr_2500_S1$PartCount=='70')

a<-filter(fr_2500_S4, fr_2500_S4$PartCount=='10')
b<-filter(fr_2500_S4, fr_2500_S4$PartCount=='20')
c<-filter(fr_2500_S4, fr_2500_S4$PartCount=='30')
d<-filter(fr_2500_S4, fr_2500_S4$PartCount=='40')
e<-filter(fr_2500_S4, fr_2500_S4$PartCount=='50')
f<-filter(fr_2500_S4, fr_2500_S4$PartCount=='60')
g<-filter(fr_2500_S4, fr_2500_S4$PartCount=='70')


# par(mfrow=c(7,1))
# plot(density(a$DriveTorque), xlim=c(0, 5))
# plot(density(b$DriveTorque), xlim=c(0, 5))
# plot(density(c$DriveTorque), xlim=c(0, 5))
# plot(density(d$DriveTorque), xlim=c(0, 5))
# plot(density(e$DriveTorque), xlim=c(0, 5))
# plot(density(f$DriveTorque), xlim=c(0, 5))
# plot(density(g$DriveTorque), xlim=c(0, 5))

aa<-cbind(as.numeric(a$Torque), a$PartCount)
dd<-cbind(as.numeric(d$Torque), d$PartCount)
gg<-cbind(as.numeric(g$Torque), g$PartCount)
h<-data.frame(rbind(dd, gg))

kable(head(h))
h$X2<-as.factor(h$X2)
names(h)[2]<-"PartCount"

ggplot(h, aes(x = X1, fill = PartCount)) +
  geom_density(alpha = 0.5) +
  xlim(5, 15)+
  labs(x=expression(bold("S1 Torque")), y=expression(bold("Density")),
       title=expression(bold("Spread of Torque values for Full Rad 118 Tool: 10th & 70th"))) +
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

plotly_curr_S1<-plot_ly(fr_Scurr, x = ~PartCount, y = ~Median, type = 'scatter')
s1curr_plot<-add_lines(plotly_curr_S1, x=~PartCount, y=predict(lm(fr_Scurr$Median~fr_Scurr$PartCount)), name="Linear Trendline", showlegend = F)
s1curr_plot = layout(s1curr_plot,  annotations = s1cu, title = "Median Values of Current Draw in Spindle over 74 parts using Finish Splines Milling Tool")
print(s1curr_plot)

plotly_load_S1<-plot_ly(fr_Sload, x = ~PartCount, y = ~Median, type = 'scatter')
s1load_plot<-add_lines(plotly_load_S1, x=~PartCount, y=predict(lm(fr_Sload$Median~fr_Sload$PartCount)), name="Linear Trendline", showlegend = F)
s1load_plot = layout(s1load_plot,  annotations = s1lo, title = "Median Values of Load on Spindle over 74 parts using Finish Splines Milling Tool")
print(s1load_plot)

plotly_torque_S1<-plot_ly(fr_STorque, x = ~PartCount, y = ~Median, type = 'scatter')
#s1torque_plot<-add_lines(plotly_torque_S1, x=~PartCount, y=predict(lm(fr_STorque$Median~poly(fr_STorque$PartCount,2,raw=TRUE))), name="Linear Trendline", showlegend = F)
s1torque_plot<-add_lines(plotly_torque_S1, x=~PartCount, y=predict(lm(fr_STorque$Median~fr_STorque$PartCount)), name="Linear Trendline", showlegend = F)
s1torque_plot = layout(s1torque_plot,  annotations = s1to, title = "Median Values of Torque on Spindle over 74 parts using Finish Splines Milling Tool")
print(s1torque_plot)


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


plotly_curr_S4<-plot_ly(fr_S4_curr, x = ~PartCount, y = ~Median, type = 'scatter')
s4curr_plot<-add_lines(plotly_curr_S4, x=~PartCount, y=predict(lm(fr_S4_curr$Median~poly(fr_S4_curr$PartCount,2,raw=TRUE))), name="Linear Trendline", showlegend = F)
s4curr_plot = layout(s4curr_plot, annotations = s4cu, title = "Median Values of Current Draw in Chuck over 74 parts using Finish Splines Milling Tool")
print(s4curr_plot)

plotly_load_S4<-plot_ly(fr_S4_load, x = ~PartCount, y = ~Median, type = 'scatter')
s4load_plot<-add_lines(plotly_load_S4, x=~PartCount, y=predict(lm(fr_S4_load$Median~poly(fr_S4_load$PartCount,2,raw=TRUE))), name="Linear Trendline", showlegend = F)
s4load_plot = layout(s4load_plot, annotations = s4lo, title = "Median Values of Load on Chuck over 74 parts using Finish Splines Milling Tool")
print(s4load_plot)

plotly_torque_S4<-plot_ly(fr_S4_Torque, x = ~PartCount, y = ~Median, type = 'scatter')
s4torque_plot<-add_lines(plotly_torque_S4, x=~PartCount, y=predict(lm(fr_S4_Torque$Median~poly(fr_S4_Torque$PartCount,2,raw=TRUE))), name="Linear Trendline", showlegend = F)
s4torque_plot = layout(s4torque_plot, annotations = s4to, title = "Median Values of Torque on Chuck over 74 parts using Finish Splines Milling Tool")
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
  layout(title = list(text="<b>Median Values for Finish Splines Tool over 'Life'</b>",
                      font = t,
                      y = 1, x = 0.5,
                      xanchor = 'center',
                      yanchor =  'top'),
         showlegend =FALSE,
         text=sprintf("<b>%s</b>", 1:10),
         plot_bgcolor='#e5ecf6')

fig

#-----------------------------------------
count1 <-c()
for (val in fr_2500_S1$PartCount){
  if(val<10){
    count1<-append(count1,1)
  }else if(val>=10 & val<20){
    count1<-append(count1,2)
  }else if(val>=20 & val<30){
    count1<-append(count1,3)
  }else if(val>=30 & val <40){
    count1<-append(count1,4)
  }else if(val>=40 & val <50){
    count1<-append(count1,5)
  }else if(val>=50 & val<60){
    count1<-append(count1,6)
  }else if(val>=60 & val<70){
    count1<-append(count1,7)
  }else if(val>=70 & val<80){
    count1<-append(count1,8)
  }
}

nrow(fr_2500_S1)

fr10<-fr_2500_S1
fr10$PartCount<-count1
unique(fr10$PartCount)
nrow(fr10)
kable(head(fr10))

fscurr10<-fr10 %>%
  summarize(
    Mean = mean(Current),
    Median  = median(Current),
    Mode = Mode(Current),
    Q1 = quantile(Current, 0.25),
    Q3 = quantile(Current, 0.75))

kable(fscurr10)


dftime<-which(fr10$PartCount != dplyr::lag(fr10$PartCount))
timez<-fr10[dftime,]$Time
time<-append(fr10$Time[1], timez)
time

#----------------------------------------------

#--------
# S1
#--------

length(fr_2500_S1)
nrow(fr_2500_S1)

df_fr10_s1<-fr_2500_S1
df_fr10_s1$PartCount <- count1
unique(df_fr10_s1$PartCount)
length(df_fr10_s1$PartCount)

#Power
fs_s1_power_plot <-
  ggplot(df_fs10_s1, aes(PartCount, Power, group = PartCount)) +
  stat_boxplot(geom ='errorbar')+
  geom_boxplot(alpha = 0.5)+
  geom_jitter(aes(color=as.factor(PartCount)), outlier.shape=NA, position=position_jitter(0.25), size=0.5)+
  stat_summary(fun = "median", geom = "point", shape = 23, size = 3, fill="blue") +
  stat_summary(fun = "mean", geom = "point", shape = 21, size = 3, fill="red",  colour="black" )+
  scale_y_continuous(limits = quantile(df_fs10_s1$Power, c(0.1, 0.9)))+
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

fs_c1_power_plot <-
  ggplot(df_fs10_s1, aes(PartCount, DrivePower, group = PartCount)) +
  stat_boxplot(geom ='errorbar')+
  geom_boxplot(alpha = 0.5)+
  geom_jitter(aes(color=as.factor(PartCount)), outlier.shape=NA, position=position_jitter(0.25), size=0.5)+
  stat_summary(fun = "median", geom = "point", shape = 23, size = 3, fill="blue") +
  stat_summary(fun = "mean", geom = "point", shape = 21, size = 3, fill="red",  colour="black" )+
  scale_y_continuous(limits = quantile(df_fs10_s1$DrivePower, c(0.1, 0.9)))+
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

fs_s1_current_plot <-
  ggplot(df_fs10_s1, aes(PartCount, Current, group = PartCount)) +
  stat_boxplot(geom ='errorbar')+
  geom_boxplot(alpha = 0.25, outlier.colour = "red")+
  geom_jitter(aes(color=as.factor(PartCount)), outlier.shape=NA, position=position_jitter(0.25), size=0.5)+
  stat_summary(fun = "median", geom = "point", shape = 23, size = 3, fill="blue") +
  stat_summary(fun = "mean", geom = "point", shape = 21, size = 3, fill="red",  colour="black" )+
  scale_y_continuous(limits = quantile(df_fs10_s1$Current, c(0.1, 0.9)))+
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

fs_c1_current_plot <-
  ggplot(df_fs10_s1, aes(PartCount, DriveCurrent, group = PartCount)) +
  stat_boxplot(geom ='errorbar')+
  geom_boxplot(alpha = 0.25, outlier.colour = "red")+
  geom_jitter(aes(color=as.factor(PartCount)), outlier.shape=NA, position=position_jitter(0.25), size=0.5)+
  stat_summary(fun = "median", geom = "point", shape = 23, size = 3, fill="blue") +
  stat_summary(fun = "mean", geom = "point", shape = 21, size = 3, fill="red",  colour="black" )+
  scale_y_continuous(limits = quantile(df_fs10_s1$DriveCurrent, c(0.1, 0.9)))+
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
fs_s1_torque_plot <-
  ggplot(df_fs10_s1, aes(PartCount, Torque, group = PartCount)) +
  stat_boxplot(geom ='errorbar')+
  geom_boxplot(alpha = 0.25, outlier.colour = "red")+
  geom_jitter(aes(color=as.factor(PartCount)), outlier.shape=NA, position=position_jitter(0.25), size=0.5)+
  stat_summary(fun = "median", geom = "point", shape = 23, size = 3, fill="blue") +
  stat_summary(fun = "mean", geom = "point", shape = 21, size = 3, fill="red",  colour="black" )+
  scale_y_continuous(limits = quantile(df_fs10_s1$Torque, c(0.1, 0.9)))+
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

fs_c1_torque_plot <-
  ggplot(df_fs10_s1, aes(PartCount, DriveTorque, group = PartCount)) +
  stat_boxplot(geom ='errorbar')+
  geom_boxplot(alpha = 0.25, outlier.colour = "red")+
  geom_jitter(aes(color=as.factor(PartCount)), outlier.shape=NA, position=position_jitter(0.25), size=0.5)+
  stat_summary(fun = "median", geom = "point", shape = 23, size = 3, fill="blue") +
  stat_summary(fun = "mean", geom = "point", shape = 21, size = 3, fill="red",  colour="black" )+
  scale_y_continuous(limits = quantile(df_fs10_s1$DriveTorque, c(0.1, 0.9)))+
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
fs_s1_load_plot <-
  ggplot(df_fs10_s1, aes(PartCount, Load, group = PartCount)) +
  stat_boxplot(geom ='errorbar')+
  geom_boxplot(alpha = 0.25, outlier.colour = "red")+
  geom_jitter(aes(color=as.factor(PartCount)), outlier.shape=NA, position=position_jitter(0.25), size=0.5)+
  stat_summary(fun = "median", geom = "point", shape = 23, size = 3, fill="blue") +
  stat_summary(fun = "mean", geom = "point", shape = 21, size = 3, fill="red",  colour="black" )+
  scale_y_continuous(limits = quantile(df_fs10_s1$Load, c(0.1, 0.9)))+
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
fs_c1_load_plot <-
  ggplot(df_fs10_s1, aes(PartCount, DriveLoad, group = PartCount)) +
  stat_boxplot(geom ='errorbar')+
  geom_boxplot(alpha = 0.25, outlier.colour = "red")+
  geom_jitter(aes(color=as.factor(PartCount)), outlier.shape=NA, position=position_jitter(0.25), size=0.5)+
  stat_summary(fun = "median", geom = "point", shape = 23, size = 3, fill="blue") +
  stat_summary(fun = "mean", geom = "point", shape = 21, size = 3, fill="red",  colour="black" )+
  scale_y_continuous(limits = quantile(df_fs10_s1$DriveLoad, c(0.1, 0.9)))+
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
fs_s1_rpm_plot <-
  ggplot(df_fs10_s1, aes(PartCount, RPM, group = PartCount)) +
  stat_boxplot(geom ='errorbar')+
  geom_boxplot(alpha = 0.25, outlier.colour = "red")+
  geom_jitter(aes(color=as.factor(PartCount)), outlier.shape=NA, position=position_jitter(0.25), size=0.5)+
  stat_summary(fun = "median", geom = "point", shape = 23, size = 3, fill="blue") +
  stat_summary(fun = "mean", geom = "point", shape = 21, size = 3, fill="red",  colour="black" )+
  scale_y_continuous(limits = quantile(df_fs10_s1$RPM, c(0.1, 0.9)))+
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
fs_s1_fr_plot <-
  ggplot(df_fs10_s1, aes(PartCount, FR, group = PartCount)) +
  stat_boxplot(geom ='errorbar')+
  geom_boxplot(alpha = 0.25, outlier.colour = "red")+
  geom_jitter(aes(color=as.factor(PartCount)), outlier.shape=NA, position=position_jitter(0.25), size=0.5)+
  stat_summary(fun = "median", geom = "point", shape = 23, size = 3, fill="blue") +
  stat_summary(fun = "mean", geom = "point", shape = 21, size = 3, fill="red",  colour="black" )+
  scale_y_continuous(limits = quantile(df_fs10_s1$FR, c(0.1, 0.9)))+
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
fs_s1_cl_spread <-
  df_fs10_s1 %>%
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

count2 <-c()
for (val in fr_2500_S4$PartCount){
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

length(fr_2500_S4)
nrow(fr_2500_S4)

df_fr10_s4<-fr_2500_S4
df_fr10_s4$PartCount <- count2
unique(df_fr10_s4$PartCount)
length(df_fr10_s4$PartCount)

#Power
fr_s4_power_plot <-
  ggplot(df_fr10_s4, aes(PartCount, Power, group = PartCount)) +
  stat_boxplot(geom ='errorbar')+
  geom_boxplot(alpha = 0.25, outlier.colour = "red")+
  geom_jitter(aes(color=as.factor(PartCount)), outlier.shape=NA, position=position_jitter(0.25), size=0.5)+
  stat_summary(fun = "median", geom = "point", shape = 23, size = 3, fill="blue") +
  stat_summary(fun = "mean", geom = "point", shape = 21, size = 3, fill="red",  colour="black" )+
  scale_y_continuous(limits = quantile(df_fr10_s4$Power, c(0.1, 0.9)))+
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

fr_s4_current_plot <-
  ggplot(df_fr10_s4, aes(PartCount, Current, group = PartCount)) +
  stat_boxplot(geom ='errorbar')+
  geom_boxplot(alpha = 0.25, outlier.colour = "red")+
  geom_jitter(aes(color=as.factor(PartCount)), outlier.shape=NA, position=position_jitter(0.25), size=0.5)+
  stat_summary(fun = "median", geom = "point", shape = 23, size = 3, fill="blue") +
  stat_summary(fun = "mean", geom = "point", shape = 21, size = 3, fill="red",  colour="black" )+
  scale_y_continuous(limits = quantile(df_fr10_s4$Current, c(0.1, 0.9)))+
  theme_minimal()+
  theme(legend.position = "none")+
  scale_fill_brewer(palette="Dark2")+
  scale_x_discrete(name ="Time", limits=c(time))+theme(legend.position = "none")
# +labs(x=expression(bold("Part Count in groups of 10")),
#      y=expression(bold("S1 Current (A)")),
#      title=expression(bold("S1 Current for Finish Splines Tool over 1 shift on 11/02/22")),
#      subtitle=expression("Mean = red circle; Median = blue diamond")) +
# theme(plot.title=element_text(size=18.), plot.subtitle = element_text(size=14), legend.text = element_text(size=15), legend.key.size = unit(2,"line"), axis.text = element_text(size=15)) +
# scale_color_discrete(name =expression(underline("Group of 10 Parts"))) +theme(legend.title.align = 1, legend.title = element_text(size=15)) +
# guides(color = guide_legend(override.aes = list(size = 3)))



# Torque
fr_s4_torque_plot <-
  ggplot(df_fr10_s4, aes(PartCount, Torque, group = PartCount)) +
  stat_boxplot(geom ='errorbar')+
  geom_boxplot(alpha = 0.25, outlier.colour = "red")+
  geom_jitter(aes(color=as.factor(PartCount)), outlier.shape=NA, position=position_jitter(0.25), size=0.5)+
  stat_summary(fun = "median", geom = "point", shape = 23, size = 3, fill="blue") +
  stat_summary(fun = "mean", geom = "point", shape = 21, size = 3, fill="red",  colour="black" )+
  scale_y_continuous(limits = quantile(df_fr10_s4$Torque, c(0.1, 0.9)))+
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
fr_s4_load_plot <-
  ggplot(df_fr10_s4, aes(PartCount, Load, group = PartCount)) +
  stat_boxplot(geom ='errorbar')+
  geom_boxplot(alpha = 0.25, outlier.colour = "red")+
  geom_jitter(aes(color=as.factor(PartCount)), outlier.shape=NA, position=position_jitter(0.25), size=0.5)+
  stat_summary(fun = "median", geom = "point", shape = 23, size = 3, fill="blue") +
  stat_summary(fun = "mean", geom = "point", shape = 21, size = 3, fill="red",  colour="black" )+
  scale_y_continuous(limits = quantile(df_fr10_s4$Load, c(0.1, 0.9)))+
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
fr_s4_rpm_plot <-
  ggplot(df_fr10_s4, aes(PartCount, RPM, group = PartCount)) +
  stat_boxplot(geom ='errorbar')+
  geom_boxplot(alpha = 0.25, outlier.colour = "red")+
  geom_jitter(aes(color=as.factor(PartCount)), outlier.shape=NA, position=position_jitter(0.25), size=0.5)+
  stat_summary(fun = "median", geom = "point", shape = 23, size = 3, fill="blue") +
  stat_summary(fun = "mean", geom = "point", shape = 21, size = 3, fill="red",  colour="black" )+
  scale_y_continuous(limits = quantile(df_fr10_s4$RPM, c(0.1, 0.9)))+
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
fr_s4_fr_plot <-
  ggplot(df_fr10_s4, aes(PartCount, FR, group = PartCount)) +
  stat_boxplot(geom ='errorbar')+
  geom_boxplot(alpha = 0.25, outlier.colour = "red")+
  geom_jitter(aes(color=as.factor(PartCount)), outlier.shape=NA, position=position_jitter(0.25), size=0.5)+
  stat_summary(fun = "median", geom = "point", shape = 23, size = 3, fill="blue") +
  stat_summary(fun = "mean", geom = "point", shape = 21, size = 3, fill="red",  colour="black" )+
  scale_y_continuous(limits = quantile(df_fr10_s4$FR, c(0.1, 0.9)))+
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
fr_s4_cl_spread <-
  df_fr10_s4 %>%
  ggplot(aes(x =CL , fill= factor(PartCount))) +
  geom_density(aes(y = ..density..), adjust = 1, alpha = 0.5) +
  theme_bw() +
  facet_wrap(~PartCount, ncol=1) +
  theme(legend.position = "none") +
  scale_fill_brewer(palette = "Dark2") + scale_x_continuous(limits = c(0.002, 0.003))


fr_s4_torque_spread <-
  df_fr10_s4 %>%
  ggplot(aes(x =Torque , fill= factor(PartCount))) +
  geom_density(aes(y = ..density..), adjust = 1, alpha = 0.5) +
  theme_bw() +
  facet_wrap(~PartCount, ncol=1) +
  theme(legend.position = "none") +
  scale_fill_brewer(palette = "Dark2") + scale_x_continuous(limits = c(0.002776, 0.00278))

#--------
# TEST
#--------

test<-ggarrange(fr_s1_current_plot, fr_s1_torque_plot ncol=1)
ggarrange(fr_s1_cl_spread, test, widths = c(1.5,2))

