#load in data
ListingPrice <- read.csv("County_MedianListingPrice_AllHomes.csv")
SoldPrice <- read.csv("County_MedianSoldPrice_AllHomes.csv")

#get rid of layers in the states
States <- as.character(ListingPrice$State)
StatesSold <- as.character(SoldPrice$State)

#declare listing and sold prices variables for CA
ListingCA <- data.frame(ListingPrice[0,])
SoldCA <- data.frame(SoldPrice[0,])

#loop to only keep listings and sold houses in California
for(n in 1:3078)
{
  if(grepl("CA", States[n])){
    ListingCA[n,] <- ListingPrice[n,]
  }
  if(grepl("CA", StatesSold[n])){
    SoldCA[n,] <- SoldPrice[n,]
  }
}

#omit NAs
ListingCA <- na.omit(ListingCA)
SoldCA <- na.omit(SoldCA)

#ListingCA has repeats, so we need to get rid of those
ListingCA <- unique(ListingCA)

#get rid of columns that give extra information about region for easier plotting
PListingCA <- ListingCA[c(1, 6:82)]
PSoldCA <- SoldCA[c(1, 6:234)]

#separate Santa Clara data
SCSold <- PSoldCA[6,]
SCSold <- SCSold[c(2:230)]

#put Santa Clara in graphing format
Date = 0
Price = 0
for(n in 1:229)
{
  Date[n] <- names(SCSold)[n]
  Price[n] <- SCSold[1,n]
}
SCSoldGraph <- data.frame(Date, Price)
SCSoldGraph$Date <- as.numeric(SCSoldGraph$Date)

#put date in correct format
a = 23956/12
b = 1/12
for(n in 1:229)
{
  SCSoldGraph$Date[n] <- a + (b*(n-1))
}

#make graph with ggplot
install.packages("ggplot2")
library(ggplot2) 
graph1 <- ggplot(data=SCSoldGraph, aes(x = Date, y = Price)) + geom_point(colour = "red") 
graph1 + labs(title = "Price Changes in Santa Clara Homes", y = "Median Price in Dollars")

#extract Santa Clara data
SCListing <- PListingCA[6,]
SCListing <- SCListing[c(2:78)]

#take out the columns in SCSold that don't have corresponding values in SCListing
SCSoldCut <- SCSold[c(151:227)]

#put Santa Clara comparison in graphing format
Date = 0
Listing = 0
Sold = 0
for(n in 1:77)
{
  Date[n] <- names(SCSoldCut)[n]
  Listing[n] <- SCListing[1,n]
  Sold[n] <- SCSoldCut[1,n]
}
SCComparisonGraph <- data.frame(Date, Listing, Sold)
SCComparisonGraph$Date <- as.numeric(SCComparisonGraph$Date)

#change date to correct format
a = 24106/12
b = 1/12
for(n in 1:77)
{
  SCComparisonGraph$Date[n] <- a + (b*(n-1))
}

#make graph with ggplot to contrast listing and sold prices in Santa Clara
graph2 <- ggplot() +
  # blue plot
  geom_point(data=SCComparisonGraph, aes(x=Date, y=Listing), colour = "blue") + 
  #geom_smooth(data=SCComparisonGraph, aes(x=Date, y=Listing), fill="blue",
   #           colour="darkblue", size=1) +
  # red plot
  geom_point(data=SCComparisonGraph, aes(x=Date, y=Sold), colour = "red")# + 
  #geom_smooth(data=SCComparisonGraph, aes(x=Date, y=Sold), fill="red",
   #           colour="red", size=1)
graph2 + labs(title = "Listing and Sold Prices in Santa Clara Homes", y = "Listing(Blue) vs. Sold(Red) Price in Dollars")

#bring in rental data
RentalPrice <- read.csv("County_MedianRentalPrice_AllHomes.csv")

#separate Santa Clara rental data
SCRent <- RentalPrice[17,]
SCRent <- SCRent[c(6:68)]

#bring in sold data for comparison
SCSoldRentComp <- SCSold[c(167:229)]

#find mortgage prices for sold homes with 4% interest rate

Mortgage = 0
i = 0.04/12 
for(n in 1:63)
{
  Mortgage[n] <- ((SCSoldRentComp[1,n]-50000)*(i*(1 + i)^360)/(((1 + i)^360)-1) + ((SCSoldRentComp[1,n]*0.0125)/12))
}

#put data in correct format
Date = 0
Rent = 0
for(n in 1:63)
{
  Date[n] <- names(SCRent)[n]
  Rent[n] <- SCRent[1,n]
}
SCRentGraph <- data.frame(Date, Rent, Mortgage)
SCRentGraph$Date <- as.numeric(SCRentGraph$Date)

#change date to correct format
a = 24122/12
b = 1/12
for(n in 1:63)
{
  SCRentGraph$Date[n] <- a + (b*(n-1))
}

#make graph with ggplot to contrast listing and sold prices in Santa Clara
graph3 <- ggplot() +
  # blue plot
  geom_point(data=SCRentGraph, aes(x=Date, y=Rent), colour = "green") + 
  # red plot
  geom_point(data=SCRentGraph, aes(x=Date, y=Mortgage), colour = "violet")
graph3 + labs(title = "Renting vs Mortgage & Property Tax Monthly Prices in Santa Clara Homes", y = "Rent(Green) vs. Mortgage&Tax(Violet) Price in Dollars")

#Use sold data to predict median home price in 30 years
lm(formula = Price~Date, data = SCSoldGraph)

#check accuracy
summary(lm(formula = Price~Date, data = SCSoldGraph))
#Coefficients:
#                Estimate Std. Error t value Pr(>|t|)    
#  (Intercept) -4.479e+07  1.909e+06  -23.46   <2e-16 ***
#  Date         2.259e+04  9.517e+02   23.73   <2e-16 ***
#significant p-value, so good

#Coefficients:
#  (Intercept)         Date  
#    -44792323        22588

PriceThirty = (-44792323 + 22588*(24544/12))
#1407666

#make a linear regression plot that shows the expected rise in value
Date = 0
Price = 0
for(n in 1:229)
{
  Date[n] <- names(SCSold)[n]
  Price[n] <- SCSold[1,n]
}
Date[230] = (24544/12)
Price[230] = PriceThirty
SCPredictGraph <- data.frame(Date, Price)
SCPredictGraph$Date <- as.numeric(SCPredictGraph$Date)

a = 23956/12
b = 1/12
for(n in 1:229)
{
  SCPredictGraph$Date[n] <- a + (b*(n-1))
}
SCPredictGraph$Date[230] = (24544/12)

graph4 <- ggplot(data=SCPredictGraph, aes(x = Date, y = Price)) + geom_point(colour = "red") + geom_smooth(method=lm, se=FALSE, fullrange=T, colour = "red") 
graph4 + labs(title = "Predicted Rise in Santa Clara Home Prices", y = "Line of Predicted Median Price in Dollars")

#show total payment with a mortgage and how that compares to expected house value
TotalPayment = (SCRentGraph$Mortgage[63]*360)
#1552382
Total = TotalPayment - PriceThirty
#144715.3

#predict rent values for the next 30 years
lm(formula = Rent~Date, data = SCRentGraph)
TotalRent = 0
MonthlyRent = 0
for(n in 1:360)
{
  TotalRent = (TotalRent + (-279116.0 + ((24183+n)/12)*139.8))
  MonthlyRent[n] <- (-279116.0 + ((24183+n)/12)*139.8)
}

#check for significance
summary(lm(formula = Rent ~ Date, data = SCRentGraph))
#significant p-values
#Coefficients:
#                Estimate Std. Error t value Pr(>|t|)    
#  (Intercept) -279116.01   47264.39  -5.905 1.68e-07 ***
#  Date            139.79      23.48   5.953 1.40e-07 ***

#graphical representation
graph5 <- ggplot(data=SCRentGraph, aes(x = Date, y = Rent), colour = "green") + geom_point(colour = "green") + geom_smooth(method=lm, se=FALSE, fullrange=T, colour = "green") 
graph5 + labs(title = "Predicted Rise in Santa Clara Rent Prices", y = "Line of Predicted Median Rent Price in Dollars") + xlim(2010, 2046)

TotalRent
#1698759

#let's see what happens if we don't even consider the changing price of rent average
OptimisticRent = (SCRentGraph$Rent[63])*360
OptimisticRent
#1017000

#show how much is spent overall in a bar graph
Overall <- data.frame(type = factor(c("Rent", "Mortgage", "Predicted Median Home Value")), 
                      total = c(TotalRent, TotalPayment, PriceThirty))

graph6 <- ggplot(data = Overall, aes(x=type, y=total, fill=type)) +
  geom_bar(stat="identity")
graph6 + labs(title = "What You Paid After 30 Years", x = "", y = "Price in Dollars") + 
  scale_fill_manual(values=c("green", "violet", "red")) + guides(fill=FALSE)

DifferenceOverall <- data.frame(type = factor(c("Rent", "Mortgage")), 
                      total = c(TotalRent, Total))

#show the difference of how much you "lost" in housing after 30 years
graph7 <- ggplot(data = DifferenceOverall, aes(x=type, y=total, fill=type)) +
  geom_bar(stat="identity")
graph7 + labs(title = "How Much is Actually Spent After 30 Years (2015 to 2045 Prediction)", x = "", y = "Price in Dollars") +
  scale_fill_manual(values=c("green", "violet")) + guides(fill=FALSE)



