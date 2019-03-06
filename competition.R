
library(readxl)
library(ggplot2)
library(RColorBrewer)
library(stringr)

#prepare data
data1<-readxl::read_xlsx("WDVP2.xlsx")
colnames(data1)<-c("country","ISO","GINI","HPI","HDI","WH_report","SEDA","health_exp_per_person","edu_exp_per_person","unemployment","polit_right_score","civil_lib_score","polit_stability","gov_effect","control_corrup","jud_effect","gov_integr")
data1[data1 == "-"] <- NA
data1 <- subset(data1, select = -c(ISO))

#cleaning data
data<-data1[!is.na(data1$GINI)&!is.na(data1$HPI)&!is.na(data1$HDI)&!is.na(data1$WH_report)&!is.na(data1$SEDA)&
                   !is.na(data1$health_exp_per_person)&!is.na(data1$edu_exp_per_person)&!is.na(data1$unemployment)&!is.na(data1$polit_right_score)&!is.na(data1$civil_lib_score)&
              !is.na(data1$polit_stability)&!is.na(data1$gov_effect)&!is.na(data1$control_corrup)&!is.na(data1$jud_effect)&!is.na(data1$gov_integr),]

data$GINI<-as.numeric(data$GINI)
data$HPI<-as.numeric(data$HPI)
data$HDI<-as.numeric(data$HDI)
data$WH_report<-as.numeric(data$WH_report)
data$SEDA<-as.numeric(data$SEDA)
data$health_exp_per_person<-as.numeric(data$health_exp_per_person)
data$edu_exp_per_person<-as.numeric(data$edu_exp_per_person)
data$unemployment<-as.numeric(data$unemployment)
data$polit_right_score<-as.numeric(data$polit_right_score)
data$civil_lib_score<-as.numeric(data$civil_lib_score)
data$polit_stability<-as.numeric(data$polit_stability)
data$gov_effect<-as.numeric(data$gov_effect)
data$control_corrup<-as.numeric(data$control_corrup)
data$jud_effect<-as.numeric(data$jud_effect)
data$gov_integr<-as.numeric(data$gov_integr)

data_df<-data.frame(data)
rownames(data_df) <- data_df$country
data_df$country<-NULL
df1<-data.frame(t(data_df))
df1$id=seq(1, nrow(df1))

#proper colnames
rownames(df1)<-c("GINI","HPI","HDI","WHI","SEDA","Health","Education","Unemployment","Political \n rights","Civil \n liberties","Political \n stability","Government \n effectiveness","Control of \n corruption","Judicial \n effectiveness","Government \n integrity")

#add groups
df1= cbind(df1, group=c( rep('A', 5), rep('B', 5), rep('C', 5)))

# Set a number of 'empty bar' to add at the end of each group
empty_bar=1
to_add = data.frame( matrix(NA, empty_bar*nlevels(df1$group), ncol(df1)) )
colnames(to_add) = colnames(df1)
to_add$group=rep(levels(df1$group), each=empty_bar)
df1=rbind(df1, to_add)
df1 <- df1[order(df1$group),]
df1$id=seq(1, nrow(df1))

# Get the name and the y position of each label
label_data=df1
number_of_bar=nrow(label_data)
angle= 90 - 360 * (label_data$id-0.5) /number_of_bar
label_data$hjust<-ifelse(angle < -90, 1, 0)
label_data$angle<-ifelse(angle < -90, angle+180, angle) 

#make a plot
windowsFonts(Helvetica = windowsFont("Helvetica")) 
p = ggplot(label_data, aes(x=as.factor(id), y=Poland, fill=group)) +  
  geom_bar(stat="identity", alpha=1) +
  ylim(-100,120) +
  theme_minimal() +
  theme(
    legend.position = "none",
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    plot.margin = unit(rep(-1,4), "cm") 
  ) +
  coord_polar() + 
  geom_text(data=label_data, aes(x=id, y=Poland+10, label=rownames(label_data), hjust=hjust), 
           color="darkgrey", family="Helvetica", fontface="bold",alpha=1, size=5, angle= label_data$angle, inherit.aes = FALSE )

plot<-p + scale_fill_manual(values=c("#15262E", "#15262E", "#15262E"))

plot

#rank
rank_data <- subset(df1, select = -c(id, group))
rank_means<-colMeans(rank_data, na.rm=TRUE)
rank_data=rbind(rank_data, rank_means)
rank_data1 <- rank_data[,order(rank_data[19,])]
colnames(rank_data1[19,])
rank_data1[19,]

