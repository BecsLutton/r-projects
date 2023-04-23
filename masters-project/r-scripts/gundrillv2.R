
#-------------------------------------------------------------------
#checking what RPM bands the tools operate in and no of occurrences
#-------------------------------------------------------------------
gundrill2<-filter(gundrill,
                  `fields_General_0_Part Count` == 2,   #filtering for one part (number 2)
                  `fields_Channels_0_Program Status`==3) #filtering for active program (status = 2)

gundrill2_rpm <- gundrill2[, grepl("RPM", names(gundrill2))] #sort for columns with RPM in column title for partcount = 2

unique(gundrill2_rpm$`fields_S1_0_Desired RPM`)
unique(gundrill2_rpm$`fields_S4_0_Desired RPM`)

table(gundrill2_rpm$`fields_S1_0_Desired RPM`)
table(gundrill2_rpm$`fields_S4_0_Desired RPM`)
table(gundrill2$fields_Channels_0_Msg)
table(gundrill2$`fields_S4_0_Desired Feed Rate`)
table(gundrill2$`fields_General_0_Axes Status`)

gundrill300<-filter(gundrill,
                    `fields_S1_0_Desired RPM`== 300,
                    fields_Channels_0_Msg == "Drilling",
                    `fields_Channels_0_Desired Feed Rate`=="216000",
                    `fields_Channels_0_Program Status`==3,
                    `fields_General_0_Axes Status`==0,)
                  #  between(gundrill$`fields_General_0_Part Count`, 16,69))

length(unique(gundrill300$`fields_General_0_Part Count`)) # checking that all parts have experienced this RPM (54)

#-------------------------
#plotting (and) stats
#-------------------------
#df_g_spindle1 <- gundrill300[ , grepl("fields_S1", names( gundrill300) ) ] #for future reference if needed
#df_g_spindle4 <- gundrill300[ , grepl("fields_S4", names( gundrill300) ) ] #for future reference if needed

#-----------------------------
#  DRIVE VARIABLES
#-----------------------------
g_300<-setNames(data.frame(gundrill300$Date), "Date")
g_300$Time<-gundrill300$Time
g_300$PartCount<-as.numeric(gundrill300$`fields_General_0_Part Count`)
g_300$DrivePower <- as.numeric(gundrill300$fields_Channels_0_DrivePower)
g_300$DriveCurrent <- as.numeric(gundrill300$fields_Channels_0_DriveCurrent)
g_300$DriveTorque <- as.numeric(gundrill300$fields_Channels_0_DriveTorque)
g_300$DriveLoad <- as.numeric(gundrill300$fields_Channels_0_DriveLoad)
g_300$DriveTemp<-as.numeric(gundrill300$`fields_Channels_0_Drive Temperature`)
g_300$Name<-as.factor(gundrill300$fields_Tool_0_tool_name)
g_300$ToolNo<-as.factor(gundrill300$fields_Tool_0_tool_no)
g_300$Type<-as.factor("Mill")
g_300$Life<-as.numeric(gundrill300$fields_Tool_0_tool_life_min)
g_300$X<-as.numeric(gundrill300$fields_Channels_0_X_Coord)
g_300$Y<-as.numeric(gundrill300$fields_Channels_0_Y_Coord)
g_300$Z<-as.numeric(gundrill300$fields_Channels_0_Z_Coord)

#-----------------------------
#  S1 VARIABLES
#-----------------------------
g_300_S1<-setNames(data.frame(as.factor(matrix("S1", nrow = nrow(g_300)))), "Spindle")
g_300_S1$DRPM<-as.numeric(gundrill300$`fields_S1_0_Desired RPM`)
g_300_S1$RPM<-as.numeric(gundrill300$fields_S1_0_RPM)
g_300_S1$Load<-as.numeric(gundrill300$fields_S1_0_Load)
g_300_S1$Power<-as.numeric(gundrill300$fields_S1_0_Power)
g_300_S1$Torque<-as.numeric(gundrill300$fields_S1_0_Torque)
g_300_S1$Current<-as.numeric(gundrill300$fields_S1_0_Current)
g_300_S1$FR<-as.numeric(gundrill300$`fields_S1_0_Feed Rate`)
g_300_S1$DFR<-as.numeric(gundrill300$`fields_S1_0_Desired Feed Rate`)
g_300_S1$DCL<-as.numeric(gundrill300$`fields_S1_0_Desired RPM`)/as.numeric(gundrill300$`fields_S1_0_Desired Feed Rate`)
g_300_S1$CL<-as.numeric(gundrill300$`fields_S1_0_RPM`)/as.numeric(gundrill300$`fields_S1_0_Feed Rate`)

g_300_S1$DCL[is.nan(g_300_S1$DCL)] <-0
g_300_S1$CL[is.nan(g_300_S1$CL)] <-0

#-----------------------------
#  S4 VARIABLES
#-----------------------------
g_300_S4<-setNames(data.frame(as.factor(matrix("S4", nrow = nrow(g_300)))), "Spindle")
g_300_S4$DRPM<-as.numeric(gundrill300$`fields_S4_0_Desired RPM`)
g_300_S4$RPM<-as.numeric(gundrill300$fields_S4_0_RPM)
g_300_S4$Load<-as.numeric(gundrill300$fields_S4_0_Load)
g_300_S4$Power<-as.numeric(gundrill300$fields_S4_0_Power)
g_300_S4$Torque<-as.numeric(gundrill300$fields_S4_0_Torque)
g_300_S4$Current<-as.numeric(gundrill300$fields_S4_0_Current)
g_300_S4$DFR<-as.numeric(gundrill300$`fields_S4_0_Desired Feed Rate`)
g_300_S4$FR<-as.numeric(gundrill300$`fields_S4_0_Feed Rate`)
g_300_S4$DCL<-as.numeric(gundrill300$`fields_S4_0_Desired RPM`)/as.numeric(gundrill300$`fields_S4_0_Desired Feed Rate`)
g_300_S4$CL<-as.numeric(gundrill300$`fields_S4_0_RPM`)/as.numeric(gundrill300$`fields_S4_0_Feed Rate`)

g_300_S4$DCL[is.nan(g_300_S4$DCL)] <-0
g_300_S4$CL[is.nan(g_300_S4$CL)] <-0

g_300_S1<-cbind(g_300, g_300_S1)
g_300_S4<-cbind(g_300, g_300_S4)

plot_ly(g_300_S1, x = ~PartCount, y = ~abs(Torque), type = 'scatter',  mode = 'markers')


a<-filter(g_300_S1, g_300_S1$PartCount=='16')
b<-filter(g_300_S1, g_300_S1$PartCount=='22')
c<-filter(g_300_S1, g_300_S1$PartCount=='28')
d<-filter(g_300_S1, g_300_S1$PartCount=='34')
e<-filter(g_300_S1, g_300_S1$PartCount=='40')
f<-filter(g_300_S1, g_300_S1$PartCount=='46')
g<-filter(g_300_S1, g_300_S1$PartCount=='52')
h<-filter(g_300_S1, g_300_S1$PartCount=='58')
k<-filter(g_300_S1, g_300_S1$PartCount=='64')
l<-filter(g_300_S1, g_300_S1$PartCount=='69')


aa<-cbind(as.numeric(a$Torque), a$PartCount)
bb<-cbind(as.numeric(b$Torque), b$PartCount)
cc<-cbind(as.numeric(c$Torque), c$PartCount)
dd<-cbind(as.numeric(d$Torque), d$PartCount)
ee<-cbind(as.numeric(e$Torque), e$PartCount)
ff<-cbind(as.numeric(f$Torque), f$PartCount)
gg<-cbind(as.numeric(g$Torque), g$PartCount)
hh<-cbind(as.numeric(h$Torque), h$PartCount)
kk<-cbind(as.numeric(k$Torque), k$PartCount)
ll<-cbind(as.numeric(l$Torque), l$PartCount)

m<-data.frame(rbind(aa,bb,cc,dd,ee,ff,gg,kk,ll))
m$X2<-as.factor(m$X2)
names(m)[2]<-"PartCount"
kable(head(m))


m %>%
  ggplot(aes(x =abs(X1), fill=PartCount)) +
  geom_density(aes(y = ..density..), adjust = 1, alpha = 0.5)+
  theme_bw()+
  facet_wrap(~PartCount, ncol=1) +
  theme(plot.title = element_text(hjust = 0.5,
                                  size = 18, face="bold"),
        legend.position = "none") +
  # scale_fill_brewer(palette="Set1") +
  labs(x = expression(bold("S1 Torque")),
       y = expression(bold("Density")),
       title="Gundrill Density Plots of Torque for Each Group of 6 Parts")


m2<-data.frame(rbind(bb,ff))
m2$X2<-as.factor(m2$X2)
names(m2)[2]<-"PartCount"

m2 %>%
  ggplot(aes(x = X1, fill = PartCount)) +
  geom_density(alpha = 0.5) +
  xlim(-10, 3)+
  labs(x=expression(bold("S1 Torque")), y=expression(bold("Density")),
       title=expression(bold("Spread of Torque values for Gundrill Tool: 16th, 40th & 69th"))) +
  theme_minimal() +
  theme(plot.title=element_text(hjust=0.5, size=14))


#---------------------------------------------------------------------------------------------------------------


#----------- DRIVE

g_DPower<-g_300%>%
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

g_DCurrent<-g_300%>%
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

g_DLoad<-g_300%>%
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

g_DTorque<-g_300%>%
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

g_DTemp<-g_300%>%
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

#----------- S1

g_Scurr<-g_300_S1%>%
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

g_Sload<-g_300_S1%>%
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

g_STorque<-g_300_S1%>%
  group_by(PartCount) %>%
  summarise(
    Mean = mean((Torque), trim=0.1),
    Median  = median((Torque)),
    Mode = Mode(Torque),
    Q1 = quantile(Torque, 0.25),
    Q3 = quantile(Torque, 0.75),
    IQR = Q3-Q1,
    SD = sd(Torque),
    SD_trim = sd_trim(Torque, trim=0.1),
    mad=mad(Torque, constant = 1.4826),
    size = length(Torque))



#----------- S4


g_S4_curr<-g_300_S4%>%
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

g_S4_load<-g_300_S4%>%
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

g_S4_Torque<-g_300_S4%>%
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
plot(g_Scurr$Mean) + lines(predict(lm(g_Scurr$Mean~g_Scurr$PartCount)),col='green')
plot(g_Scurr$Median) + lines(predict(lm(g_Scurr$Median~g_Scurr$PartCount)),col='blue')

#--------------------------------
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

plotly_curr_S1<-plot_ly(g_Scurr, x = ~PartCount, y = ~Median, type = 'scatter')
s1curr_plot<-add_lines(plotly_curr_S1, x=~PartCount, y=predict(lm(g_Scurr$Median~poly(g_Scurr$PartCount, 3, raw=TRUE))), name="Linear Trendline", showlegend = F)
s1curr_plot = layout(s1curr_plot,  annotations = s1cu, title = "Median Values of Current Draw in Spindle over 74 parts using Chamdrill 8D Drill Tool")
print(s1curr_plot)

plotly_load_S1<-plot_ly(g_Sload, x = ~PartCount, y = ~Median, type = 'scatter')
s1load_plot<-add_lines(plotly_load_S1, x=~PartCount, y=predict(lm(g_Sload$Median~poly(g_Sload$PartCount, 3, raw=TRUE))), name="Linear Trendline", showlegend = F)
s1load_plot = layout(s1load_plot,  annotations = s1lo, title = "Median Values of Load on Spindle over 74 parts using Chamdrill 8D Drill Tool")
print(s1load_plot)

plotly_torque_S1<-plot_ly(g_STorque, x = ~PartCount, y = ~Median, type = 'scatter')
s1torque_plot<-add_lines(plotly_torque_S1, x=~PartCount, y=predict(lm(g_STorque$Median~poly(g_STorque$PartCount,3,raw=TRUE))), name="Linear Trendline", showlegend = F)
s1torque_plot = layout(s1torque_plot,  annotations = s1to, title = "Median Values of Torque on Spindle over 74 parts using Chamdrill 8D Drill Tool")
print(s1torque_plot)

plot_ly(g_Sload, x = ~PartCount, y = ~abs(Mean),
        type = 'scatter',
        mode = 'lines',
        line = list(color='rgb(255,127,14)'),
        name = "Mean Torque S1") %>%
  add_trace(y = ~abs(Q1),
            type = 'scatter',
            mode = 'lines',
            line = list(color = 'transparent'),
            name = 'Q1')  %>%
  add_trace(y = ~abs(Q3),
            type = 'scatter',
            mode = 'lines',
            fill = 'tonexty',
            fillcolor='rgba(255,127,14,0.2)',
            line = list(color = 'transparent'),
            name = 'Q3')


plot_ly(g_STorque, x = ~PartCount, y = ~abs(Mean), type = 'scatter',  mode = 'lines+markers')

plot_ly(g_STorque, x = ~PartCount, y = ~abs(Mean), type = 'scatter',  mode = 'lines+markers') %>%
  add_trace(y = ~mad, type = 'scatter',  mode = 'lines', name = "mad", line = list(color = "red"))

plot_ly(g_STorque, x = ~PartCount, y = ~abs(Mean), type = 'scatter',  mode = 'lines+markers') %>%
  add_trace(y = ~mad, type = 'scatter',  mode = 'lines+markers', name = "mad", line = list(color = "red"), marker = list(color = "red")) %>%
  add_trace(y = ~abs(Median), type = 'scatter',  mode = 'lines+markers', name = "median")
plot_ly(g_STorque, x = ~PartCount, y = ~SD_trim, type = 'scatter',  mode = 'lines+markers')


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


plotly_curr_S4<-plot_ly(g_S4_curr, x = ~PartCount, y = ~Median, type = 'scatter')
s4curr_plot<-add_lines(plotly_curr_S4, x=~PartCount, y=predict(lm(g_S4_curr$Median~poly(g_S4_curr$PartCount,3,raw=TRUE))), name="Linear Trendline", showlegend = F)
s4curr_plot = layout(s4curr_plot, annotations = s4cu, title = "Median Values of Current Draw in Chuck over 74 parts using Chamdrill 8D Drill Tool")
print(s4curr_plot)

plotly_load_S4<-plot_ly(g_S4_load, x = ~PartCount, y = ~Median, type = 'scatter')
s4load_plot<-add_lines(plotly_load_S4, x=~PartCount, y=predict(lm(g_S4_load$Median~poly(g_S4_load$PartCount,3,raw=TRUE))), name="Linear Trendline", showlegend = F)
s4load_plot = layout(s4load_plot, annotations = s4lo, title = "Median Values of Load on Chuck over 74 parts using Chamdrill 8D Drill Tool")
print(s4load_plot)

plotly_torque_S4<-plot_ly(g_S4_Torque, x = ~PartCount, y = ~Median, type = 'scatter')
s4torque_plot<-add_lines(plotly_torque_S4, x=~PartCount, y=predict(lm(g_S4_Torque$Median~poly(g_S4_Torque$PartCount,3,raw=TRUE))), name="Linear Trendline", showlegend = F)
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

df_g_s1<-filter(g_300_S1, between(g_300_S1$PartCount, 16,69))
df_g_s4<-filter(g_300_S4, between(g_300_S4$PartCount, 16,69))

count1 <-c()
for (val in df_g_s1$PartCount){
  if(val<22){
    count1<-append(count1,16)
  }else if(val>=22 & val<28){
    count1<-append(count1,22)
  }else if(val>=28 & val<34){
    count1<-append(count1,28)
  }else if(val>=34 & val<40){
    count1<-append(count1,34)
  }else if(val>=40 & val<46){
    count1<-append(count1,40)
  }else if(val>=46 & val<52){
    count1<-append(count1,46)
  }else if(val>=52 & val<58){
    count1<-append(count1,52)
  }else if(val>=58 & val<64){
    count1<-append(count1,58)
  }else if(val>=64 & val<70){
    count1<-append(count1,64)
  }
}

#--------
# S1
#--------

length(df_g_s1)
nrow(df_g_s1)


df_g_s1$PartCount <- count1
unique(df_g_s1$PartCount)
length(df_g_s1$PartCount)

dftime<-which(df_g_s1$PartCount != dplyr::lag(df_g_s1$PartCount))
timez<-df_g_s1[dftime,]$Time
time<-append(df_g_s1$Time[1], timez)
time

#Power
g_s1_power_plot <-
  ggplot(df_g_s1, aes(PartCount, Power, group = PartCount)) +
  stat_boxplot(geom ='errorbar')+
  geom_boxplot(alpha = 0.5)+
  geom_jitter(aes(color=as.factor(PartCount)), position=position_jitter(0.25), size=0.5)+
  stat_summary(fun = "median", geom = "point", shape = 23, size = 3, fill="blue") +
  stat_summary(fun = "mean", geom = "point", shape = 21, size = 3, fill="red",  colour="black" )+
  scale_y_continuous(limits = quantile(df_g_s1$Power, c(0.1, 0.9)))+
  theme_minimal()+
  theme(legend.position = "none")+
  scale_fill_brewer(palette="Dark2")#+  scale_x_discrete(name ="Time", limits=unique(time))+theme(legend.position = "none")
# +labs(x=expression(bold("Part Count in groups of 10")),
#      y=expression(bold("S1 Power (W)")),
#      title=expression(bold("S1 Power for Finish Splines Tool over 1 shift on 11/02/22")),
#      subtitle=expression("Mean = red circle; Median = blue diamond")) +
# theme(plot.title=element_text(size=18.), plot.subtitle = element_text(size=14), legend.text = element_text(size=15), legend.key.size = unit(2,"line"), axis.text = element_text(size=15)) +
# scale_color_discrete(name =expression(underline("Group of 10 Parts"))) +theme(legend.title.align = 1, legend.title = element_text(size=15)) +
# guides(color = guide_legend(override.aes = list(size = 3)))

g_c1_power_plot <-
  ggplot(df_g_s1, aes(PartCount, DrivePower, group = PartCount)) +
  stat_boxplot(geom ='errorbar')+
  geom_boxplot(alpha = 0.5)+
  geom_jitter(aes(color=as.factor(PartCount)), position=position_jitter(0.25), size=0.5)+
  stat_summary(fun = "median", geom = "point", shape = 23, size = 3, fill="blue") +
  stat_summary(fun = "mean", geom = "point", shape = 21, size = 3, fill="red",  colour="black" )+
  #scale_y_continuous(limits = quantile(df_g_s1$DrivePower, c(0.1, 0.9)))+
  theme_minimal()+
  theme(legend.position = "none")+
  scale_fill_brewer(palette="Dark2")#+  scale_x_discrete(name ="Time", limits=unique(time))+theme(legend.position = "none")
# +labs(x=expression(bold("Part Count in groups of 10")),
#      y=expression(bold("S1 Power (W)")),
#      title=expression(bold("S1 Power for Finish Splines Tool over 1 shift on 11/02/22")),
#      subtitle=expression("Mean = red circle; Median = blue diamond")) +
# theme(plot.title=element_text(size=18.), plot.subtitle = element_text(size=14), legend.text = element_text(size=15), legend.key.size = unit(2,"line"), axis.text = element_text(size=15)) +
# scale_color_discrete(name =expression(underline("Group of 10 Parts"))) +theme(legend.title.align = 1, legend.title = element_text(size=15)) +
# guides(color = guide_legend(override.aes = list(size = 3)))

g_s1_current_plot <-
  ggplot(df_g_s1, aes(PartCount, abs(Current), group = PartCount)) +
  stat_boxplot(geom ='errorbar')+
  geom_boxplot(alpha = 0.25, outlier.colour = "red")+
  geom_jitter(aes(color=as.factor(PartCount)), position=position_jitter(0.25), size=0.5)+
  stat_summary(fun = "median", geom = "point", shape = 23, size = 3, fill="blue") +
  stat_summary(fun = "mean", geom = "point", shape = 21, size = 3, fill="red",  colour="black" )+
  #scale_y_continuous(limits = quantile(abs(df_g_s1$Current), c(0.1, 0.9)))+
  theme_minimal()+
  theme(legend.position = "none")+
  scale_fill_brewer(palette="Dark2")#+  scale_x_discrete(name ="Time", limits=unique(time))+theme(legend.position = "none")
# +labs(x=expression(bold("Part Count in groups of 10")),
#      y=expression(bold("S1 Current (A)")),
#      title=expression(bold("S1 Current for Finish Splines Tool over 1 shift on 11/02/22")),
#      subtitle=expression("Mean = red circle; Median = blue diamond")) +
# theme(plot.title=element_text(size=18.), plot.subtitle = element_text(size=14), legend.text = element_text(size=15), legend.key.size = unit(2,"line"), axis.text = element_text(size=15)) +
# scale_color_discrete(name =expression(underline("Group of 10 Parts"))) +theme(legend.title.align = 1, legend.title = element_text(size=15)) +
# guides(color = guide_legend(override.aes = list(size = 3)))

g_c1_current_plot <-
  ggplot(df_g_s1, aes(PartCount, abs(DriveCurrent), group = PartCount)) +
  stat_boxplot(geom ='errorbar')+
  geom_boxplot(alpha = 0.25, outlier.colour = "red")+
  geom_jitter(aes(color=as.factor(PartCount)), position=position_jitter(0.25), size=0.5)+
  stat_summary(fun = "median", geom = "point", shape = 23, size = 3, fill="blue") +
  stat_summary(fun = "mean", geom = "point", shape = 21, size = 3, fill="red",  colour="black" )+
 # scale_y_continuous(limits = quantile(abs(df_g_s1$DriveCurrent), c(0.1, 0.9)))+
  theme_minimal()+
  theme(legend.position = "none")+
  scale_fill_brewer(palette="Dark2")#+  scale_x_discrete(name ="Time", limits=unique(time))+theme(legend.position = "none")
# +labs(x=expression(bold("Part Count in groups of 10")),
#      y=expression(bold("S1 Current (A)")),
#      title=expression(bold("S1 Current for Finish Splines Tool over 1 shift on 11/02/22")),
#      subtitle=expression("Mean = red circle; Median = blue diamond")) +
# theme(plot.title=element_text(size=18.), plot.subtitle = element_text(size=14), legend.text = element_text(size=15), legend.key.size = unit(2,"line"), axis.text = element_text(size=15)) +
# scale_color_discrete(name =expression(underline("Group of 10 Parts"))) +theme(legend.title.align = 1, legend.title = element_text(size=15)) +
# guides(color = guide_legend(override.aes = list(size = 3)))



# Torque
g_s1_torque_plot <-
  ggplot(df_g_s1, aes(PartCount, abs(Torque), group = PartCount)) +
  stat_boxplot(geom ='errorbar')+
  geom_boxplot(alpha = 0.25, outlier.colour = "red")+
  geom_jitter(aes(color=as.factor(PartCount)), position=position_jitter(0.25), size=0.5)+
  stat_summary(fun = "median", geom = "point", shape = 23, size = 3, fill="blue") +
  stat_summary(fun = "mean", geom = "point", shape = 21, size = 3, fill="red",  colour="black" )+
 # scale_y_continuous(limits = quantile(abs(df_g_s1$Torque), c(0.1, 0.9)))+
  theme_minimal()+
  theme(legend.position = "none")+
  scale_fill_brewer(palette="Dark2")#+  scale_x_discrete(name ="Time", limits=unique(time))+theme(legend.position = "none")
# +labs(x=expression(bold("Part Count in groups of 10")),
#      y=expression(bold("S1 Torque (Nm)")),
#      title=expression(bold("S1 Torque for Finish Splines Tool over 1 shift on 11/02/22")),
#      subtitle=expression("Mean = red circle; Median = blue diamond")) +
# theme(plot.title=element_text(size=18.), plot.subtitle = element_text(size=14), legend.text = element_text(size=15), legend.key.size = unit(2,"line"), axis.text = element_text(size=15)) +
# scale_color_discrete(name =expression(underline("Group of 10 Parts"))) +theme(legend.title.align = 1, legend.title = element_text(size=15)) +
# guides(color = guide_legend(override.aes = list(size = 3)))

g_c1_torque_plot <-
  ggplot(df_g_s1, aes(PartCount, abs(DriveTorque), group = PartCount)) +
  stat_boxplot(geom ='errorbar')+
  geom_boxplot(alpha = 0.25, outlier.colour = "red")+
  geom_jitter(aes(color=as.factor(PartCount)), position=position_jitter(0.25), size=0.5)+
  stat_summary(fun = "median", geom = "point", shape = 23, size = 3, fill="blue") +
  stat_summary(fun = "mean", geom = "point", shape = 21, size = 3, fill="red",  colour="black" )+
  #scale_y_continuous(limits = quantile(abs(df_g_s1$DriveTorque), c(0.1, 0.9)))+
  theme_minimal()+
  theme(legend.position = "none")+
  scale_fill_brewer(palette="Dark2")#+  scale_x_discrete(name ="Time", limits=unique(time))+theme(legend.position = "none")
# +labs(x=expression(bold("Part Count in groups of 10")),
#      y=expression(bold("S1 Torque (Nm)")),
#      title=expression(bold("S1 Torque for Finish Splines Tool over 1 shift on 11/02/22")),
#      subtitle=expression("Mean = red circle; Median = blue diamond")) +
# theme(plot.title=element_text(size=18.), plot.subtitle = element_text(size=14), legend.text = element_text(size=15), legend.key.size = unit(2,"line"), axis.text = element_text(size=15)) +
# scale_color_discrete(name =expression(underline("Group of 10 Parts"))) +theme(legend.title.align = 1, legend.title = element_text(size=15)) +
# guides(color = guide_legend(override.aes = list(size = 3)))


# Load
g_s1_load_plot <-
  ggplot(df_g_s1, aes(PartCount, Load, group = PartCount)) +
  stat_boxplot(geom ='errorbar')+
  geom_boxplot(alpha = 0.25, outlier.colour = "red")+
  geom_jitter(aes(color=as.factor(PartCount)), position=position_jitter(0.25), size=0.5)+
  stat_summary(fun = "median", geom = "point", shape = 23, size = 3, fill="blue") +
  stat_summary(fun = "mean", geom = "point", shape = 21, size = 3, fill="red",  colour="black" )+
 # scale_y_continuous(limits = quantile(df_g_s1$Load, c(0.1, 0.9)))+
  theme_minimal()+
  theme(legend.position = "none")+
  scale_fill_brewer(palette="Dark2")#+  scale_x_discrete(name ="Time", limits=unique(time))+theme(legend.position = "none")
#+ labs(x=expression(bold("Part Count in groups of 10")),
#      y=expression(bold("S1 Load (%)")),
#      title=expression(bold("S1 Load for Finish Splines Tool over 1 shift on 11/02/22")),
#      subtitle=expression("Mean = red circle; Median = blue diamond")) +
# theme(plot.title=element_text(size=18.), plot.subtitle = element_text(size=14), legend.text = element_text(size=15), legend.key.size = unit(2,"line"), axis.text = element_text(size=15)) +
# scale_color_discrete(name =expression(underline("Group of 10 Parts"))) +theme(legend.title.align = 1, legend.title = element_text(size=15)) +
# guides(color = guide_legend(override.aes = list(size = 3)))

# Load
g_c1_load_plot <-
  ggplot(df_g_s1, aes(PartCount, DriveLoad, group = PartCount)) +
  stat_boxplot(geom ='errorbar')+
  geom_boxplot(alpha = 0.25, outlier.colour = "red")+
  geom_jitter(aes(color=as.factor(PartCount)), position=position_jitter(0.25), size=0.5)+
  stat_summary(fun = "median", geom = "point", shape = 23, size = 3, fill="blue") +
  stat_summary(fun = "mean", geom = "point", shape = 21, size = 3, fill="red",  colour="black" )+
 # scale_y_continuous(limits = quantile(df_g_s1$DriveLoad, c(0.1, 0.9)))+
  theme_minimal()+
  theme(legend.position = "none")+
  scale_fill_brewer(palette="Dark2")#+  scale_x_discrete(name ="Time", limits=unique(time))+theme(legend.position = "none")
#+ labs(x=expression(bold("Part Count in groups of 10")),
#      y=expression(bold("S1 Load (%)")),
#      title=expression(bold("S1 Load for Finish Splines Tool over 1 shift on 11/02/22")),
#      subtitle=expression("Mean = red circle; Median = blue diamond")) +
# theme(plot.title=element_text(size=18.), plot.subtitle = element_text(size=14), legend.text = element_text(size=15), legend.key.size = unit(2,"line"), axis.text = element_text(size=15)) +
# scale_color_discrete(name =expression(underline("Group of 10 Parts"))) +theme(legend.title.align = 1, legend.title = element_text(size=15)) +
# guides(color = guide_legend(override.aes = list(size = 3)))


# RPM
g_s1_rpm_plot <-
  ggplot(df_g_s1, aes(PartCount, RPM, group = PartCount)) +
  stat_boxplot(geom ='errorbar')+
  geom_boxplot(alpha = 0.25, outlier.colour = "red")+
  geom_jitter(aes(color=as.factor(PartCount)), position=position_jitter(0.25), size=0.5)+
  stat_summary(fun = "median", geom = "point", shape = 23, size = 3, fill="blue") +
  stat_summary(fun = "mean", geom = "point", shape = 21, size = 3, fill="red",  colour="black" )+
 # scale_y_continuous(limits = quantile(df_g_s1$RPM, c(0.1, 0.9)))+
  theme_minimal()+
  theme(legend.position = "none")+
  scale_fill_brewer(palette="Dark2")#+  scale_x_discrete(name ="Time", limits=unique(time))+theme(legend.position = "none")
#+labs(x=expression(bold("Part Count in groups of 10")),
#      y=expression(bold("S1 RPM")),
#      title=expression(bold("S1 RPM for Finish Splines Tool over 1 shift on 11/02/22")),
#      subtitle=expression("Mean = red circle; Median = blue diamond")) +
# theme(plot.title=element_text(size=18.), plot.subtitle = element_text(size=14), legend.text = element_text(size=15), legend.key.size = unit(2,"line"), axis.text = element_text(size=15)) +
# scale_color_discrete(name =expression(underline("Group of 10 Parts"))) +theme(legend.title.align = 1, legend.title = element_text(size=15)) +
# guides(color = guide_legend(override.aes = list(size = 3)))

# Feed Rate
g_s1_fr_plot <-
  ggplot(df_g_s1, aes(PartCount, FR, group = PartCount)) +
  stat_boxplot(geom ='errorbar')+
  geom_boxplot(alpha = 0.25, outlier.colour = "red")+
  geom_jitter(aes(color=as.factor(PartCount)), position=position_jitter(0.25), size=0.5)+
  stat_summary(fun = "median", geom = "point", shape = 23, size = 3, fill="blue") +
  stat_summary(fun = "mean", geom = "point", shape = 21, size = 3, fill="red",  colour="black" )+
  scale_y_continuous(limits = quantile(df_g_s1$FR, c(0.01, 0.99)))+
  theme_minimal()+
  theme(legend.position = "none")+
  scale_fill_brewer(palette="Dark2")#+  scale_x_discrete(name ="Time", limits=unique(time))+theme(legend.position = "none")
# +labs(x=expression(bold("Part Count in groups of 10")),
#      y=expression(bold("S1 Feed Rate (inch/min)")),
#      title=expression(bold("S1 Power for Finish Splines Tool over 1 shift on 11/02/22")),
#      subtitle=expression("Mean = red circle; Median = blue diamond")) +
# theme(plot.title=element_text(size=18.), plot.subtitle = element_text(size=14), legend.text = element_text(size=15), legend.key.size = unit(2,"line"), axis.text = element_text(size=15)) +
# scale_color_discrete(name =expression(underline("Group of 10 Parts"))) +theme(legend.title.align = 1, legend.title = element_text(size=15)) +
# guides(color = guide_legend(override.aes = list(size = 3)))

# Chip Load
g_s1_cl_plot <-
  ggplot(df_g_s1, aes(PartCount, CL, group = PartCount)) +
  stat_boxplot(geom ='errorbar')+
  geom_boxplot(alpha = 0.25, outlier.colour = "red")+
  geom_jitter(aes(color=as.factor(PartCount)), position=position_jitter(0.25), size=0.5)+
  stat_summary(fun = "median", geom = "point", shape = 23, size = 3, fill="blue") +
  stat_summary(fun = "mean", geom = "point", shape = 21, size = 3, fill="red",  colour="black" )+
 # scale_y_continuous(limits = quantile(df_g_s1$FR, c(0.1, 0.9)))+
  theme_minimal()+
  theme(legend.position = "none")+
  scale_fill_brewer(palette="Dark2")#+  scale_x_discrete(name ="Time", limits=unique(time))+theme(legend.position = "none")
# +labs(x=expression(bold("Part Count in groups of 10")),
#      y=expression(bold("S1 Feed Rate (inch/min)")),
#      title=expression(bold("S1 Power for Finish Splines Tool over 1 shift on 11/02/22")),
#      subtitle=expression("Mean = red circle; Median = blue diamond")) +
# theme(plot.title=element_text(size=18.), plot.subtitle = element_text(size=14), legend.text = element_text(size=15), legend.key.size = unit(2,"line"), axis.text = element_text(size=15)) +
# scale_color_discrete(name =expression(underline("Group of 10 Parts"))) +theme(legend.title.align = 1, legend.title = element_text(size=15)) +
# guides(color = guide_legend(override.aes = list(size = 3)))

g_s1_cl_spread <-
  df_g_s1 %>%
  ggplot(aes(x =CL , fill= factor(PartCount))) +
  geom_density(aes(y = ..density..), adjust = 1, alpha = 0.5) +
  theme_bw() +
  facet_wrap(~PartCount, ncol=1) +
  theme(legend.position = "none") +
  scale_fill_brewer(palette = "Set1") + scale_x_continuous(limits = c(0.002755, 0.002795)) +
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
for (val in df_g_s4$PartCount){
  if(val<22){
    count2<-append(count2,16)
  }else if(val>=22 & val<28){
    count2<-append(count2,22)
  }else if(val>=28 & val<34){
    count2<-append(count2,28)
  }else if(val>=34 & val<40){
    count2<-append(count2,34)
  }else if(val>=40 & val<46){
    count2<-append(count2,40)
  }else if(val>=46 & val<52){
    count2<-append(count2,46)
  }else if(val>=52 & val<58){
    count2<-append(count2,52)
  }else if(val>=58 & val<64){
    count2<-append(count2,58)
  }else if(val>=64 & val<70){
    count2<-append(count2,64)
  }
}

length(df_g_s4)
unique(df_g_s4$PartCount)
df_g_s4$PartCount <- count2
unique(df_g_s4$PartCount)
length(df_g_s4$PartCount)


#Power
g_s4_power_plot <-
  ggplot(df_g_s4, aes(PartCount, Power, group = PartCount)) +
  stat_boxplot(geom ='errorbar')+
  geom_boxplot(alpha = 0.25, outlier.colour = "red")+
  geom_jitter(aes(color=as.factor(PartCount)),  position=position_jitter(0.25), size=0.5)+
  stat_summary(fun = "median", geom = "point", shape = 23, size = 3, fill="blue") +
  stat_summary(fun = "mean", geom = "point", shape = 21, size = 3, fill="red",  colour="black" )+
  #scale_y_continuous(limits = quantile(df_g_s4$Power, c(0.1, 0.9)))+
  theme_minimal()+
  theme(legend.position = "none")+
  scale_fill_brewer(palette="Dark2")
  #scale_x_discrete(name ="Time", limits=unique(time))+theme(legend.position = "none")
# +labs(x=expression(bold("Part Count in groups of 10")),
#      y=expression(bold("S1 Power (W)")),
#      title=expression(bold("S1 Power for Chamdrill 8D Tool over 1 shift on 11/02/22")),
#      subtitle=expression("Mean = red circle; Median = blue diamond")) +
# theme(plot.title=element_text(size=18.), plot.subtitle = element_text(size=14), legend.text = element_text(size=15), legend.key.size = unit(2,"line"), axis.text = element_text(size=15)) +
# scale_color_discrete(name =expression(underline("Group of 10 Parts"))) +theme(legend.title.align = 1, legend.title = element_text(size=15)) +
# guides(color = guide_legend(override.aes = list(size = 3)))

g_s4_current_plot <-
  ggplot(df_g_s4, aes(PartCount, abs(Current), group = PartCount)) +
  stat_boxplot(geom ='errorbar')+
  geom_boxplot(alpha = 0.25, outlier.colour = "red")+
  geom_jitter(aes(color=as.factor(PartCount)), position=position_jitter(0.25), size=0.5)+
  stat_summary(fun = "median", geom = "point", shape = 23, size = 3, fill="blue") +
  stat_summary(fun = "mean", geom = "point", shape = 21, size = 3, fill="red",  colour="black" )+
 # scale_y_continuous(limits = quantile(abs(df_g_s4$Current), c(0.1, 0.9)))+
  theme_minimal()+
  theme(legend.position = "none")+
  scale_fill_brewer(palette="Dark2")#  scale_x_discrete(name ="Time", limits=unique(time))+theme(legend.position = "none")
# +labs(x=expression(bold("Part Count in groups of 10")),
#      y=expression(bold("S1 Current (A)")),
#      title=expression(bold("S1 Current for Chamdrill 8D Tool over 1 shift on 11/02/22")),
#      subtitle=expression("Mean = red circle; Median = blue diamond")) +
# theme(plot.title=element_text(size=18.), plot.subtitle = element_text(size=14), legend.text = element_text(size=15), legend.key.size = unit(2,"line"), axis.text = element_text(size=15)) +
# scale_color_discrete(name =expression(underline("Group of 10 Parts"))) +theme(legend.title.align = 1, legend.title = element_text(size=15)) +
# guides(color = guide_legend(override.aes = list(size = 3)))



# Torque
g_s4_torque_plot <-
  ggplot(df_g_s4, aes(PartCount, abs(Torque), group = PartCount)) +
  stat_boxplot(geom ='errorbar')+
  geom_boxplot(alpha = 0.25, outlier.colour = "red")+
  geom_jitter(aes(color=as.factor(PartCount)), position=position_jitter(0.25), size=0.5)+
  stat_summary(fun = "median", geom = "point", shape = 23, size = 3, fill="blue") +
  stat_summary(fun = "mean", geom = "point", shape = 21, size = 3, fill="red",  colour="black" )+
  #scale_y_continuous(limits = quantile(abs(df_g_s4$Torque), c(0.1, 0.9)))+
  theme_minimal()+
  theme(legend.position = "none")+
  scale_fill_brewer(palette="Dark2")#  scale_x_discrete(name ="Time", limits=unique(time))+theme(legend.position = "none")
# +labs(x=expression(bold("Part Count in groups of 10")),
#      y=expression(bold("S1 Torque (Nm)")),
#      title=expression(bold("S1 Torque for Chamdrill 8D Tool over 1 shift on 11/02/22")),
#      subtitle=expression("Mean = red circle; Median = blue diamond")) +
# theme(plot.title=element_text(size=18.), plot.subtitle = element_text(size=14), legend.text = element_text(size=15), legend.key.size = unit(2,"line"), axis.text = element_text(size=15)) +
# scale_color_discrete(name =expression(underline("Group of 10 Parts"))) +theme(legend.title.align = 1, legend.title = element_text(size=15)) +
# guides(color = guide_legend(override.aes = list(size = 3)))


# Load
g_s4_load_plot <-
  ggplot(df_g_s4, aes(PartCount, Load, group = PartCount)) +
  stat_boxplot(geom ='errorbar')+
  geom_boxplot(alpha = 0.25, outlier.colour = "red")+
  geom_jitter(aes(color=as.factor(PartCount)), position=position_jitter(0.25), size=0.5)+
  stat_summary(fun = "median", geom = "point", shape = 23, size = 3, fill="blue") +
  stat_summary(fun = "mean", geom = "point", shape = 21, size = 3, fill="red",  colour="black" )+
 # scale_y_continuous(limits = quantile(df_g_s4$Load, c(0.1, 0.9)))+
  theme_minimal()+
  theme(legend.position = "none")+
  scale_fill_brewer(palette="Dark2")#  scale_x_discrete(name ="Time", limits=unique(time))+theme(legend.position = "none")
#+ labs(x=expression(bold("Part Count in groups of 10")),
#      y=expression(bold("S1 Load (%)")),
#      title=expression(bold("S1 Load for Chamdrill 8D Tool over 1 shift on 11/02/22")),
#      subtitle=expression("Mean = red circle; Median = blue diamond")) +
# theme(plot.title=element_text(size=18.), plot.subtitle = element_text(size=14), legend.text = element_text(size=15), legend.key.size = unit(2,"line"), axis.text = element_text(size=15)) +
# scale_color_discrete(name =expression(underline("Group of 10 Parts"))) +theme(legend.title.align = 1, legend.title = element_text(size=15)) +
# guides(color = guide_legend(override.aes = list(size = 3)))


# RPM
g_s4_rpm_plot <-
  ggplot(df_g_s4, aes(PartCount, RPM, group = PartCount)) +
  stat_boxplot(geom ='errorbar')+
  geom_boxplot(alpha = 0.25, outlier.colour = "red")+
  geom_jitter(aes(color=as.factor(PartCount)), position=position_jitter(0.25), size=0.5)+
  stat_summary(fun = "median", geom = "point", shape = 23, size = 3, fill="blue") +
  stat_summary(fun = "mean", geom = "point", shape = 21, size = 3, fill="red",  colour="black" )+
  #scale_y_continuous(limits = quantile(df_g_s4$RPM, c(0.1, 0.9)))+
  theme_minimal()+
  theme(legend.position = "none")+
  scale_fill_brewer(palette="Dark2")#  scale_x_discrete(name ="Time", limits=unique(time))+theme(legend.position = "none")
#+labs(x=expression(bold("Part Count in groups of 10")),
#      y=expression(bold("S1 RPM")),
#      title=expression(bold("S1 RPM for Chamdrill 8D Tool over 1 shift on 11/02/22")),
#      subtitle=expression("Mean = red circle; Median = blue diamond")) +
# theme(plot.title=element_text(size=18.), plot.subtitle = element_text(size=14), legend.text = element_text(size=15), legend.key.size = unit(2,"line"), axis.text = element_text(size=15)) +
# scale_color_discrete(name =expression(underline("Group of 10 Parts"))) +theme(legend.title.align = 1, legend.title = element_text(size=15)) +
# guides(color = guide_legend(override.aes = list(size = 3)))

# Feed Rate
g_s4_fr_plot <-
  ggplot(df_g_s4, aes(PartCount, FR, group = PartCount)) +
  stat_boxplot(geom ='errorbar')+
  geom_boxplot(alpha = 0.25, outlier.colour = "red")+
  geom_jitter(aes(color=as.factor(PartCount)), position=position_jitter(0.25), size=0.5)+
  stat_summary(fun = "median", geom = "point", shape = 23, size = 3, fill="blue") +
  stat_summary(fun = "mean", geom = "point", shape = 21, size = 3, fill="red",  colour="black" )+
  #scale_y_continuous(limits = quantile(df_g_s4$FR, c(0.1, 0.9)))+
  theme_minimal()+
  theme(legend.position = "none")+
  scale_fill_brewer(palette="Dark2")#  scale_x_discrete(name ="Time", limits=unique(time))+theme(legend.position = "none")
# +labs(x=expression(bold("Part Count in groups of 10")),
#      y=expression(bold("S1 Feed Rate (inch/min)")),
#      title=expression(bold("S1 Power for Chamdrill 8D Tool over 1 shift on 11/02/22")),
#      subtitle=expression("Mean = red circle; Median = blue diamond")) +
# theme(plot.title=element_text(size=18.), plot.subtitle = element_text(size=14), legend.text = element_text(size=15), legend.key.size = unit(2,"line"), axis.text = element_text(size=15)) +
# scale_color_discrete(name =expression(underline("Group of 10 Parts"))) +theme(legend.title.align = 1, legend.title = element_text(size=15)) +
# guides(color = guide_legend(override.aes = list(size = 3)))

# Chip Load
g_s4_cl_plot <-
  ggplot(df_g_s4, aes(PartCount, CL, group = PartCount)) +
  stat_boxplot(geom ='errorbar')+
  geom_boxplot(alpha = 0.25, outlier.colour = "red")+
  geom_jitter(aes(color=as.factor(PartCount)), position=position_jitter(0.25), size=0.5)+
  stat_summary(fun = "median", geom = "point", shape = 23, size = 3, fill="blue") +
  stat_summary(fun = "mean", geom = "point", shape = 21, size = 3, fill="red",  colour="black" )+
  # scale_y_continuous(limits = quantile(df_g_s1$FR, c(0.1, 0.9)))+
  theme_minimal()+
  theme(legend.position = "none")+
  scale_fill_brewer(palette="Dark2")#  scale_x_discrete(name ="Time", limits=unique(time))+theme(legend.position = "none")
# +labs(x=expression(bold("Part Count in groups of 10")),
#      y=expression(bold("S1 Feed Rate (inch/min)")),
#      title=expression(bold("S1 Power for Finish Splines Tool over 1 shift on 11/02/22")),
#      subtitle=expression("Mean = red circle; Median = blue diamond")) +
# theme(plot.title=element_text(size=18.), plot.subtitle = element_text(size=14), legend.text = element_text(size=15), legend.key.size = unit(2,"line"), axis.text = element_text(size=15)) +
# scale_color_discrete(name =expression(underline("Group of 10 Parts"))) +theme(legend.title.align = 1, legend.title = element_text(size=15)) +
# guides(color = guide_legend(override.aes = list(size = 3)))


g_s4_cl_spread <-
  df_g_s4 %>%
  ggplot(aes(x =CL , fill= factor(PartCount))) +
  geom_density(aes(y = ..density..), adjust = 1, alpha = 0.5) +
  theme_bw() +
  facet_wrap(~PartCount, ncol=1) +
  theme(legend.position = "none") +
  scale_fill_brewer(palette = "Dark2") + scale_x_continuous(limits = c(0.002776, 0.00278))


#--------
# TEST
#--------

test<-ggarrange(g_s1_current_plot, g_s1_torque_plot, ncol=1)
ggarrange(g_s1_cl_spread, test, widths = c(1.5,2))

