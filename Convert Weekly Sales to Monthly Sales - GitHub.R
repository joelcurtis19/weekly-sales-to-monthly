library(tidyverse)
library(lubridate)

weekly_path <- "Retail Weekly POS Sales.csv"

weekly <- read_csv(weekly_path)

names(weekly) <- make.names(colnames(weekly))

#create beginning of week column
weekly$Week.Begin <- weekly$Week.Ending - 6

#create end of month and beginning of month columns
weekly$EOMonth <- weekly$Week.Ending # end of month initiation
day(weekly$EOMonth) <- days_in_month(weekly$EOMonth) # end of month
weekly$BOMonth <- weekly$EOMonth # beginning of month initiation
day(weekly$BOMonth) <- days_in_month(weekly$BOMonth) - (days_in_month(weekly$BOMonth) - 1) #subtract n-1 days in month from days in month


#create days in current month column
weekly$Days.In.Current.Month <- ifelse(
  weekly$Week.Begin < weekly$BOMonth, #if the beginning of week is before the begin of month
  weekly$Week.Ending - weekly$BOMonth + 1, #take the week ending and subtract first day of month and add back 1
  7) #otherwise input 7 days of week in current month

#create a lagged sales column, and use days in current month as multiplier
#order by sku and then week
weekly <- weekly %>%
  arrange(SKU, Week.Ending)#ensuring data is sorted by sku then week
weekly$SKU.Start <- ifelse(lag(weekly$SKU) == weekly$SKU, FALSE, TRUE)#creating indicator to determine start of new week
weekly$SKU.Start[1] <- TRUE #set the first row of df to TRUE to indicate 'new week'

#determine numbers in each week belonging to current month
weekly <- weekly %>%
  mutate(Current.Dollars = Dollars * Days.In.Current.Month/7,#determine dollars belonging to current month
         Current.Units = Units * Days.In.Current.Month/7) #determine units belonging to current month

#determine numbers in each week belonging to previous month
weekly <- weekly %>%
  mutate(Previous.Dollars = Dollars - Current.Dollars, #determine dollars belonging to previous month
         Previous.Units = Units - Current.Units) #determine units belonging to previous month

#determine the figure that should be added to current months units and dollars
weekly <- weekly %>%
  mutate(Add.Dollars = ifelse(SKU.Start == TRUE, 0, lag(Previous.Dollars)),#figure to add to current month dollars
         Add.Units = ifelse(SKU.Start == TRUE, 0, lag(Previous.Units)))#figure to add to current month units

#add the existing current month figures to the figures previously calculated
weekly <- weekly %>%
  mutate(Dollars = Current.Dollars + Add.Dollars, #creating estimated dollars for current month
         Units = Current.Units + Add.Units) #created estimated units for current month

#columns to select for monthly df
monthly_select <- c("SKU",
                    "Dollars",             
                    "Units",
                    "BOMonth",
                    "EOMonth")

#creating monthly df from transformed weekly data
monthly <- weekly %>%
  select(monthly_select)%>% #selecting columns
  group_by(SKU, EOMonth, BOMonth)%>%#grouping by SKU and month
  summarize(Dollars = sum(Dollars), #summing dollars
            Units = sum(Units)) #summing units



#removes months that are only partial months
max_month <- max(monthly$EOMonth)#this is the latest month

monthly <- if(max(weekly$Week.Ending) == max_month){#checks if the latest month in weekly data is equal to latest month in monthly data
  monthly#if true then select monthly df
}else{
  monthly %>% #if not true then filter out any EOMonth that are equal to or greate than the max month
    filter(EOMonth < max_month)
} 

#the monthly df is now complete
#each row in monthly df represents an estimate of a specific sku's sales in the given month

