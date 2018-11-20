

CustomerAcqusition <- read.csv("F:\\BA\\R case studies\\R case study 1 (Banking)\\Customer Acqusition.csv")
Repayment <- read.csv("F:\\BA\\R case studies\\R case study 1 (Banking)\\Repayment.csv")
Spend <- read.csv("F:\\BA\\R case studies\\R case study 1 (Banking)\\spend.csv")

#Question no. 1(a)

CustomerAcqusition$Age<- ifelse(CustomerAcqusition$Age < 18,mean(CustomerAcqusition$Age),
                                CustomerAcqusition$Age)


#Question no. 1(b) 

require(dplyr)
Repayment <- Repayment[c(1,2,3,4)]
Repayment <- Repayment[1:1500,] 
Repayment[is.na(Repayment)] <- 1

Spendgroupby <- Spend %>% group_by(Customer) %>% dplyr::summarise(SpendAmount=sum(Amount))

Repaymentgroupby <- Repayment %>% group_by(Customer) %>% dplyr::summarise(RepaymentAmount=sum(Amount))


CustomerAcqusition1 <- merge(x=CustomerAcqusition,y=Repaymentgroupby,all = T) #merging
CustomerAcqusition1 <- merge(x=CustomerAcqusition1,y=Spendgroupby,all=T)
CustomerAcqusition1 <- CustomerAcqusition1[c(2,1,3:10)]
CustomerAcqusition1 <- CustomerAcqusition1 %>% arrange(No) #arrangement
CustomerAcqusition1 <- dplyr::rename(CustomerAcqusition1,SL.No.=No)

CustomerAcqusition <- CustomerAcqusition1  #Just for future use

CustomerAcqusition1$SpendAmount <- ifelse(CustomerAcqusition1$SpendAmount > CustomerAcqusition1$Limit,
                                      CustomerAcqusition1$Limit/2,CustomerAcqusition1$SpendAmount)

#Question no. 1(c)
CustomerAcqusition1$RepaymentAmount <- ifelse(CustomerAcqusition1$RepaymentAmount > CustomerAcqusition1$Limit,
                                          CustomerAcqusition1$Limit,CustomerAcqusition1$RepaymentAmount)

#-----------------------------------------------------------------------------------------




#Question no.2(a)

NoofDistintCustomer <- length(CustomerAcqusition1$Customer)
DistinctCustomer <- print(CustomerAcqusition1$Customer[length(!duplicated(CustomerAcqusition1$Customer))])


#Question no.2(b)


Categories <- Spend %>% group_by(Type) %>% dplyr::summarise(SumSpendPerType=sum(Amount))
NO.ofCategories <- print(length(Categories$Type))
DistinctCategories <- print(Categories$Type[length(Categories$Type)])



#Question no.2(C)

require(lubridate)

Spend$Month <- as.Date(Spend$Month,format= "%d-%b-%y")
Spend$Month_Year<-format(as.Date(Spend$Month),"%Y-%m")       #Important
AverageMonthlySpend <- Spend %>% group_by(Customer,Month_Year) %>% dplyr::summarise(MonthlySpend=sum(Amount))



#Question no.2(d)


Repayment$Month <- as.Date(Repayment$Month,format="%d-%b-%y")
Repayment$Month_Year <- format(as.Date(Repayment$Month),"%Y-%m")
AverageMonthlyRepayment <- Repayment %>% group_by(Customer,Month_Year) %>% dplyr::summarise(MonthlyRepayment=sum(Amount))


#Question no.2(e)

Spend$MonthNAME <- lubridate::month(Spend$Month,label=T,abbr=T)
Spend$Year <- lubridate::year(Spend$Month)
MonthlySpend <- Spend %>% group_by(Month_Year) %>% dplyr::summarise(TotalSpend=sum(Amount))
MonthlySpend$Profit <- (MonthlySpend$TotalSpend*0.029)



#Question no.2(f)

Categories <- Categories %>%dplyr::arrange(desc(SumSpendPerType))
HighUsageTYPE <- print(head(Categories$Type))



#Question no.2(g)

SpendCityWise <- CustomerAcqusition %>% group_by(City) %>% 
            dplyr::summarise(SpendAmountbyCity=sum(SpendAmount))

SpendCityWise$Profit <- (SpendCityWise$SpendAmountbyCity*0.029)
CityWithMaxProfit <- print(SpendCityWise[which.max(SpendCityWise$SpendAmountbyCity),])



#Question no.2(h) #create age group

CustomerAcqusition$AgeCat <- ifelse(CustomerAcqusition$Age<=30,"Young",
                                    ifelse(CustomerAcqusition$Age<=45,"Elder","Senior"))
AgeGroup <- CustomerAcqusition %>% group_by(AgeCat) %>% dplyr::summarise(SpendAmountbyAge=sum(SpendAmount))
AgeCat <- print(AgeGroup[which.max(AgeGroup$SpendAmountbyAge),])



#Question no.2(i)

Segment <- CustomerAcqusition %>% group_by(Segment) %>% dplyr::summarise(SpendAmountbySegment=sum(SpendAmount))
Segmentwise <- print(Segment[which.max(Segment$SpendAmountbySegment),])



#Question no.2(j)

Segment1 <- CustomerAcqusition %>% group_by(Segment) %>% dplyr::summarise(RepaymentAmountbySegment=sum(RepaymentAmount))
Segment1 <- merge(x=Segment,y=Segment1,all = T)
Segment1$Profit <- (Segment1$RepaymentAmountbySegment - Segment1$SpendAmountbySegment)
ProfitableSegment <- print(Segment1[which.max(Segment1$Profit),])



#Question no.2(k)

Repaymentgroupby <- Repaymentgroupby %>% dplyr::arrange(desc(RepaymentAmount))
Top10Customers <- print(Repaymentgroupby$Customer[c(1:10)])



#-----------------------------------------------------------------------------------------


#Question no.3


require(ggplot2)

CitywiseSpend <- CustomerAcqusition[c(2,4)]
CitywiseSpend1 <- Spend %>% group_by(Customer,Type,Year) %>% summarise(CitywiseSpend = sum(Amount))
CitywiseSpend <- merge(x=CitywiseSpend,y=CitywiseSpend1,all = T)

CitywiseSpend2 <- CitywiseSpend %>% group_by(City,Type,Year) %>% summarise(CitywiseSum= sum(CitywiseSpend))
CitywiseSpend2 <- CitywiseSpend2 %>% dplyr::arrange(Year)

PlotCitywiseSpend <- ggplot(data =CitywiseSpend2) + aes(x = Year, y = CitywiseSum, fill = Type ) +
                       geom_bar(stat = "identity", position = "dodge") + facet_grid(.~City)
print(PlotCitywiseSpend)
#-----------------------------------------------------------------------------------------


#Question no.4(a)

MonthlySpendCat <- Spend %>% group_by(Type,MonthNAME,Year) %>% dplyr::summarise(MonthlySpend=sum(Amount))
YearlySpendCat <- Spend %>% group_by(Type,Year) %>% dplyr::summarise(YearlySpend=sum(Amount))

MonthlyComparison <- ggplot(data=MonthlySpendCat) + aes(x = MonthNAME,y = MonthlySpend,fill=Type) +
                            geom_bar(stat = "identity",position = "dodge") + facet_grid(.~Year)

print(MonthlyComparison)
Yearlycomparison <- ggplot(data=YearlySpendCat) + aes(x = Year,y = YearlySpend,fill=Type) + 
                                      geom_bar(stat = "identity",colour="black") 
print(Yearlycomparison)

#Question no.4(b)

Table1 <- CustomerAcqusition[c(2,4)]
Table2 <- Spend%>% group_by(Customer,Month_Year,MonthNAME) %>% dplyr::summarise(MonthlySpendCity = sum(Amount))
MonthlySpendCity <- merge(x=Table1,y=Table2,all=T)

PlotMonthlySpendCity <- ggplot(data = MonthlySpendCity) + 
                    aes(x = Month_Year, y = MonthlySpendCity,fill=City) +
                    geom_bar(stat = "identity",colour ="Black") +
                     theme(text = element_text(size = 20),
                      axis.text.x= element_text(angle = 90,hjust = 1))
print(PlotMonthlySpendCity)


#Question no.4(c)

YearlySpendonAirTkt <- Spend %>% group_by(Type,Year) %>% summarise(AmountType = sum(Amount))
YearlySpendonAirTkt <- YearlySpendonAirTkt[YearlySpendonAirTkt$Type == "AIR TICKET",]
PlotAirTkt <- ggplot(data = YearlySpendonAirTkt) + aes(x = Year,y = AmountType, fill=Year) + 
                      geom_bar(stat = "identity",colour = "Black" )
print(PlotAirTkt)
 
#Question no.4(d)

MonthlySpendpattern <- Spend %>% group_by(Year,MonthNAME) %>% summarise(Amount = sum(Amount))

PlotMonthlyPattern <- ggplot(data = MonthlySpendpattern) + aes(x = MonthNAME,y = Amount,fill = MonthNAME) +
            geom_bar(stat = "identity",colour="Black",position = "Dodge") + facet_grid(.~Year)
print(PlotMonthlyPattern)


#----------------------------------------------------------------------------------------- 

#Question no. 5

Aqusitition <- CustomerAcqusition %>% group_by(Customer,City) %>% summarise()

Repayment$Year <- lubridate::year(Repayment$Month)
Repayment$MonthNAME <- lubridate::month(Repayment$Month,label=T,abbr=T)

MergedSpendDAta <- Spend %>% group_by(Customer,Type,MonthNAME,Year) %>% summarise(Spend=sum(Amount,na.rm = T))
MergedRepaymentDAta <- Repayment %>% group_by(Customer,MonthNAME,Year) %>% summarise(Repayment=sum(Amount,na.rm = T))
FirstMerge <- merge(MergedSpendDAta,MergedRepaymentDAta,all = T)
FinalMerge <- merge(FirstMerge,Aqusitition,all = T)

#Customers, spend, repayment,categories,Year, month Input as category and time period

Func_Top_Cust <- function(x){
  print(FinalMerge$City[(!duplicated(FinalMerge$City))])
  City <- readline("Please Enter the city you want from above: ")
  City <- as.character(City)
  
  print(FinalMerge$Type[!duplicated(FinalMerge$Type)])
  Type <- readline("Please Enter the type from above:  ")
  Type <- as.character(Type)
  
  print(c('Month','Year'))
  MonOrYear <- readline("Please type Month/Year: ")
  MonOrYear <- as.character(MonOrYear)
  
  if(MonOrYear == 'Month'){
    print(FinalMerge$MonthNAME[(!duplicated(FinalMerge$MonthNAME))])
    Mon <- readline("Please provide Month  from above:  ")
    Mon <- as.character(Mon)
    Top <- FinalMerge[which(FinalMerge$MonthNAME == Mon & FinalMerge$Type == Type & FinalMerge$City == City),]
    # return(Data)
    return(Top)
  }
  else{
    print(FinalMerge$Year[(!duplicated(FinalMerge$Year))])
    Year <- readline("Please provide year from above:  ")
    Year <- as.numeric(Year)
    Top <- FinalMerge[which(FinalMerge$Year == Year & FinalMerge$Type == Type & FinalMerge$City == City),]
    return(Top)
    
  }
  
}

Func_Top_Cust(FinalMerge)




