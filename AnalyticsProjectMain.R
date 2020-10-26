
##Cleaning Data##
##Sugar Data##
install.packages("readxl") #install packages 
library("readxl") #loading xl packages 
sugardata<-read_excel("Sugaredited.xlsx", sheet = "sugar") #accessing data
names(sugardata) #name the variables 
View(sugardata) #view the dataset 
summary(sugardata) #view the summary statistics 
na.omit(sugardata) #omit values that had NA variables 
sugardata<-na.omit(sugardata) #I got rid of 29 observation 
sugardata_1<-subset(sugardata,select=-c(1,3,4)) #got rid of unnecessary variables 
head(sugardata_1) #showing top results 1-6
View(sugardata_1) #viewing the data set
sugardata_2<-subset(sugardata_1, sugar>146)#only looking at data for top 10 sugar consumption
summary(sugardata_2)
View(sugardata_2) #showing top ten results 

##Making a bar graph for sugar consumption##
##Plot for sugar in grams for the top 10 countries##
library(ggplot2) #loading ggplot 
S.data<-data.frame( ##setting the data for the y and x axis 
  name =c("CUB ", "DNK", "CRI", "BRB", "COL", "MLT", "NZL", "CHE", "TTO", "USA") , #setting the values of the x axis
  value =c(147.375,151.000,151.875,158.000,160.125,160.250,162.250,162.500,165.625,173.125) #setting the values for the y axis 
)
ggplot(S.data, aes(x =name, y = value)) + ##giving the axis correct names 
   ggtitle("Average Sugar Consumption Per Person \n(g per day)(2006-2013)") + #giving the graph a title 
  theme(plot.title = element_text(hjust = 0.5))+ #centering the graph a title 
  labs(y = "Sugar (gm)", x = "Country") + #giving the graph x and y varibles 
  geom_text(aes(label=value), position=position_dodge(width=0.9), vjust=-0.25) + #Putting the y values on top of each bar 
geom_bar(stat = "identity",color = "#6f14ff", fill="#d3b8ff", width = 0.6) #changing the color of the bars and setting the color for the fill and outline
d1<-density(S.data$value)
plot(d1)

##Life expectancy Data##
lifedata<-read_excel("lifeedited.xlsx", sheet = "life") #accessing data
names(lifedata) #name the variables 
View(lifedata) #view the dataset 
summary(lifedata) #view the summary statistics 
lifedata_1<-subset(lifedata,select=-c(1,2,3,4,5,6,7,8,9,10,11,12,13)) #I got rid of every variable except country and average  
View(lifedata_1) #viewing the data set 
na.omit(lifedata_1) #omitting values that have NA results 
lifedata_2<-na.omit(lifedata_1) #saving the omitted data set 
View(lifedata_2) #viewing the data set 

##Making the grpah for Life expectancy based on the 10 countries from sugar consumption
library(ggplot2) #loading ggplot 
L.data<-data.frame( ##setting the data for the y and x axis 
  name =c("TTO ", "COL", "CUB", "BRB", "USA", "CRI", "DNK", "MLT", "NZL", "CHE") , #setting the values of the x axis
  value =c(71.802,75.80, 78.24,78.32,78.35,78.71,79.07,80.47,80.68,82.21))
ggplot(L.data, aes(x =name, y = value)) + ##giving the axis correct names 
  ggtitle("Average Life expectancy (yrs.) (2006-2013)") + #giving the graph a title 
  theme(plot.title = element_text(hjust = 0.5))+ #centering the graph a title 
  labs(y = "Years", x = "Country") + #giving the graph x and y varibles 
  geom_text(aes(label=value), position=position_dodge(width=0.9), vjust=-0.25) + #Putting the y values on top of each bar 
  geom_bar(stat = "identity",color = "#F44336", fill="#FFCDD2", width = 0.6) #changing the color of the bars and setting the color for the fill and outline
d2<-density(L.data$value) #checking distribution values 
plot(d2)


##GDP DATA##
GDPdata<-read_excel("GDPedited.xlsx", sheet = "GDP") #accessing data
names(GDPdata) #name the variables 
summary(GDPdata) #view the summary statistics 
na.omit(GDPdata) #omit values that had NA variables 
GDPdata<-na.omit(GDPdata) #saving omitted data under a variable 
View(GDPdata) #view data
GDPdata_1<-subset(GDPdata,select=-c(1,2,3,4,5,6,7,8,9,10,11,12,13)) #selecting specific columns from excel 
View(GDPdata_1) #view data 

##Making graph for GDP($) data based on the same 
library(ggplot2) #loading ggplot 
GDP.data<-data.frame( ##setting the data for the y and x axis 
  name =c("BRB ", "MLT", "TTO", "CRI", "CUB", "NZL", "COL", "DNK", "CHE", "USA") ,  #setting the values of the x axis
  value =c(4.57,8.72,23.46,35.78,64.73,148.23,28.12,33.71,58.83,15118.10))
ggplot(GDP.data, aes(x =name, y = value)) + ##giving the axis correct names 
  ggtitle("Average GDP in dollars (2006-2013)") + #giving the graph a title 
  theme(plot.title = element_text(hjust = 0.5))+ #centering the graph a title 
  labs(y = "Dollar (billions)", x = "Country") + #giving the graph x and y varibles 
  geom_text(aes(label=value), position=position_dodge(width=0.9), vjust=-0.25) + #Putting the y values on top of each bar 
  geom_bar(stat = "identity",color = "#1A237E", fill="#C5CAE9", width = 0.6) #changing the color of the bars and setting the color for the fill and outline
d3<-density(GDP.data$value) #checking the ditribution 
plot(d3)


##PerCap ##
percap<-read_excel("percapedited.xls", sheet = "percap") #accessing data
names(percap) #name the variables 
View(percap) #view the dataset 
summary(percap) #view the summary statistics 
na.omit(percap) #omit values that had NA variables 
percap<-na.omit(percap)#saving omitted variables  
percap_1<-subset(percap,select=-c(1,2,3,4,5,6,7,8,9,10,11,12,13)) #taking out any unnecessary variables
View(percap_1) #viewing data to make sure it looks okay 

###Making a graph for GDP Percapita (%)##
library(ggplot2) #loading ggplot 
Percap.data<-data.frame( ##setting the data for the y and x axis 
  name =c("BRB","DNK", "USA","NZL","CHE","MLT","TTO","CRI", "COL", "CUB") ,  #setting the values of the x axis
  value =c(-0.492, 0.028, 0.439, 0.781, 0.886, 1.723, 2.384, 3.112, 3.669, 4.458))
ggplot(Percap.data, aes(x =name, y = value)) + ##giving the axis correct names 
  ggtitle("Average GDP per capita (%) (2006-2013)") + #giving the graph a title 
  theme(plot.title = element_text(hjust = 0.5))+ #centering the graph a title 
  labs(y = "Percent", x = "Country") + #giving the graph x and y varibles 
  geom_text(aes(label=value), position=position_dodge(width=0.7), vjust=-0.25) + #Putting the y values on top of each bar 
  geom_bar(stat = "identity",color = "#B2DFDB", fill="#00BFA5", width = 0.6) #changing the color of the bars and setting the color for the fill and outline
d4<-density(Percap.data$value) #checking distribution 
plot(d4)

 


