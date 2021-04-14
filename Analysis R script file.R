



library(dummies)
library(GGally)
library(gplots)

toyota <- read.csv(file = "C:/Users/UParekh/Desktop/Udit/ToyotaCorolla.csv", header = TRUE)



#a
#The categorical variables in this dataset are: Model, FUel_type, Color, Met_Color, Mfr_Month, Mfr_Year, Automatic, Mfr_Guarantee, BOVAG_Guarantee, ABS, Airbag_1 , Airbag_2 , Airco , Automatic_airco , Boardcomputer , CD_Player , Central_Lock , Powered_Windows, Power_Steering, Radio, Mistlamps, Sport_Model, Backseat_Divider, Metallic_Rim, Radio_cassette , Parking assistance system, Tow_Bar

#b
# A Dummy variable or Indicator Variable is an artificial variable created to represent an attribute with two or more distinct categories/levels.A variable with N categories will be converted to N or N-1 dummy variables. We create dummy variables for categorical data as yes or no which are converted to 0 and 1.

#c
# N or N-1 variables are required to capture the information in a categorical variable

#d

toyota$Mfg_Year <- factor(toyota$Mfg_Year)

toyotadummy <- dummy.data.frame(toyota[,-c(1,2,5,6,11,15)])


#One of the example for this is: The Fuel_Type variable which has three category, Petrol, Diesel and CNG. This results in three different dummy variables Fuel_Type_Petrol, Fuel_Type_Diesel and Fuel_Type_CNG

#e

#Correlation Matrix
Cor_Matrix <- round(cor(toyotadummy),2)




#After analysing Variables which are not important seeing the Correlation matrix excluded-
#Radio_Cassette,Tow_Bar, Parking_Assistant,Mettalic_Rim, Backseat_Divider
#Sports_Model,Mistlamps,BOVAG_Guarantee, Mfr_Guarantee, Automatic, Met_Color
#Radio and radio_cassette have high correlation 0.99 correlation so only radio is considered
Toyota_a <- toyotadummy[,-c(8,9,15,16,29,30,31,32,33,34,35)]


#Heatmap to analyse all data
heatmap.2(cor(Toyota_a), Rowv = FALSE, Colv = FALSE, dendrogram = "none",
          cellnote = round(cor(Toyota_a),2),
          notecol = "black", key = FALSE, trace = 'none', margins = c(10,10))

# All continuous variables
Toyota_Co.df <- toyotadummy[,c(1,2,3,7,10,11,12,13,14,17)]


#Heat Map for only continuous variables


heatmap.2(cor(Toyota_Co.df), Rowv = FALSE, Colv = FALSE, dendrogram = "none",
          cellnote = round(cor(Toyota_Co.df),2),
          notecol = "black", key = FALSE, trace = 'none', margins = c(10,10))


#After analysing data other highly Correalted variables which are dropped:-
#CC, Doors, Gears, Guarantee period

Toyota_Drop.df <- Toyota_Co.df[,-c(5,6,7,10)]



#Heat Map of only relevant continuous variables
#Continous Variables relevant- Price,Age,KM,Quaterly_Tax,Weight,HP


heatmap.2(cor(Toyota_Drop.df), Rowv = FALSE, Colv = FALSE, dendrogram = "none",
          cellnote = round(cor(Toyota_Drop.df),2),
          notecol = "black", key = FALSE, trace = 'none', margins = c(10,10))



y<-Toyota_Drop.df

#Only Continuous variables for Correlation matrix:-
Cor_Matrix <- round(cor(y),2)


#Matrix scatter plot Continous variables 
ggpairs(Toyota_Drop.df)

#After the analysis of several variables. The corelation between multiple variables in considered:
#Age_08_04 is negatively correlated with price (-0.88): This means when the age of the car is more the price of the car goes down.KM is negatively correlated with price(-0.57): When the km increases similar to age the price of the car decreases. Weight is positively correlated with price(0.58): Weight of the Car is positively correlated with the Price (higher price higher weight). KM is positively correlated with Age_08_04( 0.51): Other than price, KM and Age of Car is positively stating a increasing relationship for both the factors when one increases. Weight is positively correlated with quarterly tax(0.63) : Quarterly Road Tax collected is positively related to Weight stating higher Car weight higher the Quarterly road tax.

```