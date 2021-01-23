
library(tidyverse)
library(readxl)
library(DescTools)
library(knitr)

setwd("/Users/gbatta/Box Sync/Econ 160 - Spring 2020/Data/Module 1")



Dupont_BS=read_xls("Analytics_mindset_case_studies_DuPont.xls",sheet="Balance Sheet Data",col_names=TRUE)
as_tibble(Dupont_BS)

# If I want to set NA to zero for numerical variables
#positions <- c(6:ncol(Dupont_BS))
#Dupont_BS<-Dupont_BS %>% mutate_at(positions,funs(replace_na(.,0)))
# Dump Financial Statement variable; replace with metadata
Dupont_BS<-Dupont_BS %>% mutate('Financial Statement' = NULL)
positions <- c(5:ncol(Dupont_BS))
for (i in positions) {            
  comment(Dupont_BS[[i]]) <- "Balance Sheet"      
}

Dupont_IS=read_xls("Analytics_mindset_case_studies_DuPont.xls",sheet="Income Statement Data",col_names=TRUE)
as_tibble(Dupont_IS)

# If I want to set NA to zero for numerical variables
#positions <- c(4:ncol(Dupont_IS))
#Dupont_IS<-Dupont_IS %>% mutate_at(positions,funs(replace_na(.,0)))
# Dump Financial Statement variable; replace with metadata
Dupont_IS<-Dupont_IS %>% mutate('Financial Statement' = NULL)
positions <- c(5:ncol(Dupont_IS))
for (i in positions) {            
  comment(Dupont_IS[[i]]) <- "Income Statement"      
}

#Merge the two datasets
Dupont<-Dupont_BS %>% inner_join(Dupont_IS, by = c("Ticker","Year"))
#Remove the old datasets
rm("Dupont_IS","Dupont_BS")


#Part II, Q1. Go through different math functions. Note that some produce a vector, e.g. log:
sum(Dupont$"Total Assets")
Dupont<-rename(Dupont,Total_Assets=`Total Assets`)

#Detour into subsetting and summary command
summary(Dupont$Total_Assets)
#What if I want the median?
summary(Dupont$Total_Assets)["Median"] 
#or get it based on position within the resulting table
summary(Dupont$Total_Assets)[3] 

#But this won't give me a number! Class is table:
class(summary(Dupont$Total_Assets)["Median"])
#To get a number, need to subset:
summary(Dupont$Total_Assets)[["Median"]]


#Part II, Q2: 
n_distinct(Dupont$Ticker)

#Part II, Q3:
#Unless you store results somewhere, result will just appear in the console
#Note that unlike cheat sheet, I didn't need to put the name of the dataset up-front
#Note also that group_by is always followed by summarize. If you need to compute other groups, you can ungroup then group
#So we will create an object to store this result in

Dupont %>% filter(Year==2013) %>% group_by(Industry) %>%  summarize(count = n_distinct(Ticker))




# This just verifies that Q2 sum == Q3 sum
# For floating point numbers, can also use near, e.g. near(sqrt(4),2,.001)
sum(firms_by_industry$count)==n_distinct(Dupont$Ticker)

#Part II, Q4:
Dupont %>% group_by(Industry) %>%  
  filter(Year==2013) %>% summarize(Sum_sales = sum(`Net Revenues`)) %>% 
  arrange(desc(Sum_sales))


Dupont %>% group_by(Name) %>% summarize(rev=sum(`Net Revenues`,na.rm=TRUE)) %>% 
  arrange(desc(rev))

Dupont %>% group_by(Name) %>% summarize(rev=sum(`Net Revenues`,na.rm=TRUE)) %>% 
  arrange(desc(rev)) %>% slice(1:5)

Dupont  %>% group_by(Name) %>% summarize(rev=sum(`Net Revenues`,na.rm=TRUE)) %>% 
  top_n(1,rev)


# Part II, Q5. Let them attempt this. Tell them they have to first sum 
# Net Revenues by firm.
# Also  tell them they can use the 'top_n' function`


Dupont %>% group_by(Name) %>% summarize(total_firm_sales=sum(`Net Revenues`)) %>% 
  arrange(desc(total_firm_sales)) %>% head(1)

sums<-Dupont %>% group_by(Name) %>% summarize(sum(`Net Revenues`)) %>% top_n(sums,5)
top_n(sums,5)
  


Dupont %>% group_by(Name) %>%   summarize(Sum_sales = sum(`Net Revenues`)) %>% 
  top_n(3,Sum_sales)

# Get 3rd highest value
#Dupont %>% group_by(Name) %>%   summarize(Sum_sales = sum(`Net Revenues`)) %>% 
#  top_n(3,Sum_sales) %>% summarize(nth(Sum_sales,3))


#Alternative way
Dupont %>% group_by(Name) %>%   summarize(Sum_sales = sum(`Net Revenues`)) %>% 
  arrange(desc(Sum_sales)) %>% slice(1)    



#Part III (preliminaries)
#Part III, Q1

#I use case_when below because when Net Revenues == 0, it fouls up mean calculations in summarize (will produce a NaN)

#Dupont<-Dupont %>% arrange(Ticker,Year) %>%
#  mutate(ROE=case_when(`Common Equity`>0 ~ `Net Income`/`Common Equity`)) %>%
#  mutate(ROEavebal=case_when(Ticker==lag(Ticker) ~ 
#          `Net Income`/((`Common Equity`+lag(`Common Equity`))/2))) 

Dupont<-Dupont %>% arrange(Ticker,Year) %>%
  mutate(ROE=case_when(`Common Equity`>0 ~ `Net Income`/`Common Equity`)) %>% 
  mutate(PM = case_when(`Net Revenues`>0 ~ `Net Income`/`Net Revenues`)) %>% 
  mutate(AT = `Net Revenues`/Total_Assets) %>% 
  mutate(FL = case_when(`Common Equity`>0 ~ Total_Assets/`Common Equity`))


#Have them construct the rest of the ratios on their own. 
#Show them the summarize_at command (generally)
#Tell them they should do a case_when for Net Revenues>0 for PM
#Tell them they should also do a case_when for Common Equity<0 for FL




Dupont_all_years<-select(Dupont,Name,Industry,Year,ROE,PM,AT,FL)

write.csv(Dupont_all_years,"Dupont_all_years.csv",row.names=FALSE)


#Show them the summarize_at command
# kable is part of the knitr package, which must be installed
# Documentation of kable here: 
# https://www.rdocumentation.org/packages/knitr/versions/1.21/topics/kable


Dupont %>% filter(Year==2015) %>%  group_by(Industry) %>% summarize_at(c("ROE","PM","AT","FL"),
                                               tibble::lst(median),na.rm=TRUE) 



Dupont  %>% group_by(Industry) %>% summarize_at( c("ROE","PM","AT","FL"),
                                                 list(median=median,mean=mean ),na.rm=TRUE ) 


# Sort from highest to lowest ROE, within industry for 2015
Dupont %>% filter(Year==2015) %>% arrange(Industry,desc(ROE)) %>% 
  select(Industry,Name,ROE,PM,AT,FL) %>% kable(format="rst",digits=3)

# Just get the top 3 by ROE, by industry for 2015
Dupont %>% filter(Year==2015) %>% arrange(Industry,desc(ROE)) %>% 
  select(Industry,Name,ROE,PM,AT,FL) %>% group_by(Industry) %>% top_n(1,ROE)%>% kable(format="rst",digits=3)



#Part III, Q4
#First get to firms sorted, by industry, form highest to lowest
#first get to arrange. Notice that there's a lot of stuff we don't need; that's where select comes in handy
#Next show them group_by follwed by mutate
ROE_sort<-Dupont %>% filter(Year==2015) %>% arrange(Industry,desc(ROE)) %>% 
  select(Industry,Name,ROE,PM,AT,FL) 



#Next, ask them to extract high ROE firms. 
ROE_sort %>% group_by(Industry) %>% arrange(desc(ROE)) %>% 
  top_n(1,ROE) %>% kable(format="rst",digits=3)  


# Ask them about what's driving the high ROE for these firms?
# I'd want to investigate: Is it more about ROA or FL?

ROE_sort %>%  group_by(Industry) %>% mutate(FL_median=median(FL,na.rm=TRUE))  %>%
  mutate(ROA=PM*AT) %>% mutate(ROA_median=median(ROA,na.rm=TRUE)) %>% 
  arrange(desc(ROE)) %>%  top_n(1,ROE) %>% kable(format="rst",digits=3)  



#Class 3 - Intro to visualization - Basically, following book's example
#    ggplot(data = <DATA>) + 
#      <GEOM_FUNCTION>(mapping = aes(<MAPPINGS>))

#Plot turnover/PM scatterplot
ggplot(data=ROE_sort) +
  geom_point(mapping=aes(x=PM, y=AT))

#Can also specify to ignore missing values:
ggplot(data=ROE_sort) +
  geom_point(mapping=aes(x=PM, y=AT),na.rm=TRUE)

#Let's look at the profit margin histogram:    
ggplot(data = ROE_sort) + 
  geom_histogram(mapping=aes(x=PM),na.rm=TRUE)

#Bad outliers!! Get rid of them in ROE_sort
ROE_sort_mod<-ROE_sort %>% filter(PM<1.0 & PM>0.0) 

#Also can winsorize. #This is from the DescTools package:
ROE_sort_mod<-ROE_sort_mod %>% mutate(ROEw1=Winsorize(ROE,probs = c(0.03, 0.97),na.rm=TRUE))  

write.csv(ROE_sort_mod,"ROE_sort_mod.csv",row.names=FALSE)


#Scatterplot looks more normal
ggplot(data=ROE_sort_mod) +
  geom_point(mapping=aes(x=PM, y=AT))



#Next, let's color industries:
ggplot(data=ROE_sort_mod) +
  geom_point(mapping=aes(x=PM, y=AT, color=Industry))

#Next, let's size the industries--this controls transparency:
ggplot(data=ROE_sort_mod) +
  geom_point(mapping=aes(x=PM, y=AT, size=Industry))


#Next, let's alpha the industries--this controls transparency:
ggplot(data=ROE_sort_mod) +
  geom_point(mapping=aes(x=PM, y=AT, alpha=Industry))

#Next, let's shape industries:
ggplot(data=ROE_sort_mod) +
  geom_point(mapping=aes(x=PM, y=AT, shape=Industry))

#Next, let's try facet wraps:
ggplot(data=ROE_sort_mod) +
  geom_point(mapping=aes(x=PM, y=AT)) +
  facet_wrap(~ Industry,nrow=2)



#Can also use `vars()` instead of `~`
ggplot(data=ROE_sort_mod) +
  geom_point(mapping=aes(x=PM, y=AT)) +
  facet_wrap(vars(Industry),nrow=2)

#Lots to unpack here! Ask students what they'd do next to investigate.
ggplot(data=subset(ROE_sort_mod,Industry=="Finance" )) +
  geom_point(mapping=aes(x=PM, y=AT))

ggplot(data=subset(ROE_sort_mod,Industry=="Consumer Services" )) +
  geom_point(mapping=aes(x=PM, y=AT))

#Now, try geom_smooth. Why are standard errors so wide out on the right?
ggplot(data= ROE_sort_mod ) +
  geom_smooth(mapping=aes(x=PM, y=AT)) 

#We can overlay scatterplot:      
ggplot(data= ROE_sort_mod ) +
  geom_smooth(mapping=aes(x=PM, y=AT)) +
  geom_point(mapping=aes(x=PM, y=AT))

#a cleaner way to do the same thing above
ggplot(data= ROE_sort_mod ,mapping=aes(x=PM, y=AT)) +
  geom_smooth() +
  geom_point()

#Next, let's do separate lines for each industry    
ggplot(data= ROE_sort_mod) +
  geom_smooth(mapping=aes(x=PM, y=AT,linetype=Industry))

#Maybe filter out Finance and Consumer Services?    
ggplot(data= filter(ROE_sort_mod,Industry!="Finance", Industry!="Consumer Services") ) +
  geom_smooth(mapping=aes(x=PM, y=AT,linetype=Industry))

#Hard to see clearly, let's add color
ggplot(data= filter(ROE_sort_mod,Industry!="Finance", Industry!="Consumer Services") ) +
  geom_smooth(mapping=aes(x=PM, y=AT,linetype=Industry,color=Industry))

#Could also just distinguish solely by color    
ggplot(data= filter(ROE_sort_mod,Industry!="Finance", Industry!="Consumer Services") ) +
  geom_smooth(mapping=aes(x=PM, y=AT,color=Industry))

#Now let's look at Consumer Services separately    
ggplot(data= filter(ROE_sort_mod,Industry=="Consumer Services")) +
  geom_smooth(mapping=aes(x=PM, y=AT,linetype=Industry))

#Can also do separate mapping for different geoms!:    
#Here, we're also filtering out the huge PM value:
ggplot(data= filter(ROE_sort_mod,PM<.5),mapping=aes(x=PM, y=AT) ) +
  geom_smooth() +
  geom_point(mapping=aes(color=Industry))

#Can subset data for different geoms, too
ggplot(data= filter(ROE_sort_mod,PM<.5),mapping=aes(x=PM, y=AT) ) +
  geom_smooth(data=filter(ROE_sort_mod,Industry=="Consumer Services",PM<.5),se=FALSE) +
  geom_point(mapping=aes(color=Industry))

#Plot numbers of firm-years in each industry
#The new count variable produced is a stat, short for
#'statistical transformation'. Rather than using raw values
#like in scatterplots, it creates new variables. 
#Histograms, boxplots, and smoothers (as above) are types of statistical transformations.

ggplot(data = ROE_sort_mod) + 
  geom_bar(mapping = aes(x = Industry))


ggplot(data = ROE_sort_mod) + 
  stat_count(mapping = aes(x = Industry))



#Plot numbers of firm-years in each industry, as a proportion of total
#Two dots around the 'prop' stat to distinguish from any variable named
#prop in your dataset
#Group=1 just says get proportions among all the data points, not just in the industry

ggplot(data = ROE_sort_mod) + 
  geom_bar(mapping = aes(x = Industry,y=..prop..,group=1))

#If I don't specify group, I get this...  
ggplot(data = ROE_sort_mod) + 
  geom_bar(mapping = aes(x = Industry,y=..prop.. ))

#Box-and-whiskers-ish
ggplot(data=ROE_sort_mod) +
  stat_summary(
    mapping=aes(x=Industry,y=ROEw1),
    fun.ymin=min,
    fun.ymax=max,
    fun.y=median
  )
#Actual box-and-whiskers. Black bar is median, ends of box are 
#Q25 and Q75, ends of whiskers extend from hinge to smallest (largest) 
#value at most 1.5*IQR. Anything b eyond that poin is plotted individually.
ggplot(data=filter(ROE_sort_mod,ROE<1)) +
  geom_boxplot(
    mapping=aes(x=Industry,y=ROE),
  )



# HOMEWORK
# Q1
# Here is an answer via a mutate, group_by, summarize sequence.

Dupont  %>% 
  mutate(ROE_2013=case_when(Year==2013 ~ ROE)) %>%  
  mutate(ROE_2015=case_when(Year==2015 ~ ROE)) %>% select(Ticker,Industry,Year,ROE_2013,ROE_2015) %>% 
  group_by(Industry) %>% summarize_at(vars(ROE_2013,ROE_2015),list(median=median),na.rm=TRUE) %>% 
  mutate(diff=ROE_2015_median-ROE_2013_median)

# Here is a different way to approach, using the lag() function
Dupont_all_years %>% group_by(Industry,Year) %>% 
  summarize(ROE=median(ROE,na.rm=TRUE)) %>% 
  mutate(ROE_diff=ifelse(Year==2015 & lag(Year,2)==2013,ROE-lag(ROE,2),NA)) %>% 
  filter(is.na(ROE_diff)==FALSE) %>% select(Industry,ROE_diff) %>% 
  arrange(desc(ROE_diff))


# This gets to the second part of question 1. Here, I want to get the difference for all of the ratios
# in which case, I need to use mutate_at. One thing about mutate_at is that I need to feed it 
# a function if I want a "custom" calculation like computing differences in lagged rows
# Note that I did group_by twice. By doing group_by(Industry), I can ensure that the lag()
# is only getting lagged values from within each industry.

lag2 <- function(x)   x - lag(x,2)
winsor<-function(x)   Winsorize(x,probs=c(.01,.99),na.rm=TRUE)


change_in_median_ROE<-Dupont %>% group_by(Industry,Year) %>% 
  summarize_at(vars(ROE,PM,AT,FL),list(median),na.rm=TRUE)   %>% group_by(Industry) %>% 
  mutate_at(vars(ROE,PM,AT,FL),list(delta=lag2)) %>% filter(Year==2015) %>% 
  select(-c(ROE,PM,AT,FL,Year)) %>% arrange(desc(ROE_delta))

double<- function(x) x*2

Dupont %>% mutate_at(vars(ROE,PM),list(double=double)) %>% 
  select(Ticker,Year,ROE,ROE_double,PM,PM_double)




ggplot(data=change_in_median_ROE )+
  geom_point( aes(x=Industry,y=ROE_delta))


#Here's a more cleaned up version, that 1) orders by medianROEgrowth, 2) makes the industry
# names not overlap and 3) labels each datapoint by its ROE growth value
ggplot(data=change_in_median_ROE,aes(x=reorder(Industry,ROE_delta),y=ROE_delta) )+
  geom_point( )+
  labs(x = "Industry",y="Median ROE growth") +
  scale_x_discrete(labels=function(x) sub(" "," \n",x,fixed=TRUE))+
  geom_text(aes(label=round(ROE_delta,3),hjust=-0.2, vjust=1.1))



# ADDITIONAL APPLICATION OF MUTATE_AT:
# Let's say I want to calculate the difference between a firm's ROE and its median Industry-Year ROE.
# We have to create a function again. There' a lot of na.rm structure around this: 
# First, we set na.rm=TRUE as a default of the new median_diff function we created
# Then, we use that default value in the base R median() function

median_diff<-function(x,na.rm=TRUE ) (x - median(x ,na.rm=na.rm))

Dupont %>% group_by(Industry,Year) %>% 
  mutate_at(vars(ROE,PM,AT,FL),list(delta=median_diff,median=median ),na.rm=TRUE)   %>% 
  select(Name,Ticker,Year,Industry, ROE,ROE_median,ROE_delta,PM_delta,AT_delta,FL_delta) %>% arrange(Ticker,Year)



# Q2 
# First, find the top 3rd value of AT, for negative PM
# The following gives you a tibble consisting of a column name "AT" and a value. 
# We need to use the double bracketing to get the actual value. Quite annoying!
AT3rd<-Dupont %>% filter(PM>-10 & PM<0 & Year==2015)  %>% select(AT) %>% 
  arrange(desc(AT)) %>% top_n(3,AT) %>% slice(3)

AT3rd[["AT"]]

ggplot(data=filter(Dupont,PM>-10   & Year==2015),mapping=aes(x=PM,y=AT))+
  geom_point()+
  geom_text(aes(label=ifelse(PM<0 & AT>=AT3rd[["AT"]],Name,'')))


# ADDITIONAL EXAMPLE
# What if we want to ID whether high leverage, non-financial firms tend to have lower
# asset turnover?
# High leverage, let's say, is greater than the 90th percentile for non-financial firms
# Can use the quantile command for this, e.g. if you want the 10th quantile:
# quantile(FL,.1,na.rm=TRUE)

Dupont_all_years<-Dupont_all_years %>% filter(PM>-10 & Industry!="Finance") %>% 
  mutate(q=quantile(FL,.9,na.rm=TRUE)) %>% 
  mutate(HiFL=ifelse(FL>q,TRUE,FALSE))  

ggplot(data=filter(Dupont_all_years,PM>-10 & Industry!="Finance" & is.na(FL)==FALSE  ),mapping=aes(x=PM,y=AT))+
  geom_point(aes(color=HiFL))

