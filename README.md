# GRIP_TheSparksFoundation

Superstore <- read.csv("C:/Users/hp/Documents/The Sparks Foundation/SampleSuperstore.csv", header = TRUE) 
library(ggplot2) 
library(dplyr) 
library(corrplot) 

str(Superstore) 
View(Superstore) 
colSums(is.na(Superstore)) # To check if there are any missing values 
Superstore %>% distinct() ->SUPERSTORE # Remove duplicate rows if there are any 

summary(SUPERSTORE) 
table(SUPERSTORE$Country) 
table(SUPERSTORE$Region) 
table(SUPERSTORE$State) 
table(SUPERSTORE$Segment) 
table(SUPERSTORE$Category) 
table(SUPERSTORE$Sub.Category) 
table(SUPERSTORE$Ship.Mode) 

ggplot(SUPERSTORE, aes(Quantity))+ geom_bar(col= "blue", fill= "skyblue")+ scale_x_continuous(breaks= seq(0, 14, by= 1))+ labs(title= "QUANTITY", y= "Count")+ theme(plot.title = element_text(hjust=.5)) 
table(SUPERSTORE$Quantity) 

aggregate(SUPERSTORE$Sales, list(SUPERSTORE$Region), sum) -> Sales_Region 
ggplot(Sales_Region, aes(reorder(Group.1, -x), x))+ geom_col(col= "blue", fill= "skyblue")+ scale_y_continuous(labels = scales::comma)+ labs(title = "SALES BY REGION", x="Region", y="Sales") +theme(plot.title = element_text(hjust=.5)) 

aggregate(SUPERSTORE$Profit, list(SUPERSTORE$Region), sum) -> Profit_Region 
ggplot(Profit_Region, aes(reorder(Group.1, -x), x))+ geom_col(col= "blue", fill= "skyblue")+ labs(title = "PROFIT BY REGION", x= "Region", y="Profit") +theme(plot.title = element_text(hjust=.5)) 

aggregate(SUPERSTORE$Sales, list(SUPERSTORE$State), sum) -> Sales_State 
ggplot(Sales_State, aes(reorder(Group.1, -x), x))+geom_col(col= "blue", fill= "skyblue")+ scale_y_continuous(labels = scales::comma)+ theme(axis.text.x = element_text(angle=90, hjust = 1, vjust = 1))+ labs(title = "SALES BY STATE", x="State", y="Sales") +theme(plot.title = element_text(hjust=.5)) 

aggregate(SUPERSTORE$Profit, list(SUPERSTORE$State), sum) -> Profit_State 
ggplot(Profit_State, aes(reorder(Group.1, -x), x))+ geom_col(col= "blue", fill= "skyblue")+ labs(title = "PROFIT BY STATE", x= "State", y= "Profit") +theme(plot.title = element_text(hjust=.5))+ theme(axis.text.x = element_text(angle=90, hjust = 1, vjust = 1)) 

aggregate(SUPERSTORE$Sales, list(SUPERSTORE$Segment), sum) -> Sales_Segment 
ggplot(Sales_Segment, aes(reorder(Group.1, -x), x))+geom_col(col= "blue", fill= "skyblue")+ labs(title = "SALES BY SEGMENT", x="Segment", y="Sales") +theme(plot.title = element_text(hjust=.5)) 

aggregate(SUPERSTORE$Profit, list(SUPERSTORE$Segment), sum) -> Profit_Segment 
ggplot(Profit_Segment, aes(reorder(Group.1, -x), x))+ geom_col(col= "blue", fill= "skyblue")+ labs(title = "PROFIT BY SEGMENT", x= "Segment", y= "Profit") +theme(plot.title = element_text(hjust=.5))+ scale_y_continuous(labels = scales::comma) 

aggregate(SUPERSTORE$Sales, list(SUPERSTORE$Category), sum) -> Sales_Category 
ggplot(Sales_Category, aes(reorder(Group.1, -x), x))+ geom_col(col= "blue", fill= "skyblue")+ scale_y_continuous(labels = scales::comma)+ labs(title = "SALES BY CATEGORY", x="Category", y="Sales") +theme(plot.title = element_text(hjust=.5)) 

aggregate(SUPERSTORE$Profit, list(SUPERSTORE$Category), sum) -> Profit_Category 
ggplot(Profit_Category, aes(reorder(Group.1, -x), x))+ geom_col(col= "blue", fill= "skyblue")+ labs(title = "PROFIT BY CATEGORY", x= "Category", y= "Profit") +theme(plot.title = element_text(hjust=.5)) 

aggregate(SUPERSTORE$Sales, list(SUPERSTORE$Sub.Category), sum) -> Sales_SubCategory 
ggplot(Sales_SubCategory, aes(reorder(Group.1, -x), x))+ geom_col(col= "blue", fill= "skyblue")+ scale_y_continuous(labels = scales::comma)+ theme(axis.text.x = element_text(angle=90, hjust = 1, vjust = 1))+ labs(title = "SALES BY SUB-CATEGORY", x="Sub-Category", y="Sales") +theme(plot.title = element_text(hjust=.5)) 

aggregate(SUPERSTORE$Profit, list(SUPERSTORE$Sub.Category), sum) -> Profit_SubCategory 
ggplot(Profit_SubCategory, aes(reorder(Group.1, -x), x))+ geom_col(col= "blue", fill= "skyblue")+ labs(title = "PROFIT BY SUB-CATEGORY", x= "Sub-Category", y= "Profit") +theme(plot.title = element_text(hjust=.5))+ theme(axis.text.x = element_text(angle=90, hjust = 1, vjust = 1)) 

aggregate(SUPERSTORE$Sales, list(SUPERSTORE$Ship.Mode), sum) -> Sales_ShipMode 
ggplot(Sales_ShipMode, aes(reorder(Group.1, -x), x))+geom_col(col= "blue", fill= "skyblue")+ scale_y_continuous(labels = scales::comma)+ labs(title = "SALES BY MODE OF SHIPPING", x="Mode of Shipping", y="Sales") +theme(plot.title = element_text(hjust=.5)) 

aggregate(SUPERSTORE$Profit, list(SUPERSTORE$Ship.Mode), sum) -> Profit_ShipMode 
ggplot(Profit_ShipMode, aes(reorder(Group.1, -x), x))+ geom_col(col= "blue", fill= "skyblue")+ labs(title = "PROFIT BY MODE OF SHIPPING", x= "Mode of Shipping", y= "Profit") +theme(plot.title = element_text(hjust=.5)) 



ggplot(SUPERSTORE, aes(Sales, Profit))+ geom_point(size=1.3, alpha=0.3)+ geom_smooth(method="lm", se=FALSE)+ scale_y_continuous(breaks= seq(-7000, 9000, by= 1000))+ labs(title= "SCATTERPLOT FOR SALES AND PROFIT")+ theme(plot.title = element_text(hjust=.5)) 
ggplot(SUPERSTORE, aes(Discount, Profit))+geom_point(alpha= 0.3)+ geom_smooth(method="lm", se=FALSE)+scale_y_continuous(breaks= seq(-7000, 9000, by= 1000))+ scale_x_continuous(breaks= seq(0, .8, by= .1))+ labs(title= "SCATTERPLOT FOR DISCOUNT AND PROFIT")+ theme(plot.title = element_text(hjust=.5)) 



SUPERSTORE1 <- subset(SUPERSTORE, select = c(Sales, Profit, Discount, Quantity)) 
cor(SUPERSTORE1) -> Correlation_Matrix # Correlation matrix 
round(Correlation_Matrix, 2) -> Correlation_Matrix 
corrplot(Correlation_Matrix, method = "color", tl.col = "black", addCoef.col = "black", tl.srt = 45)
