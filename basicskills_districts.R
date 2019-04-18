#This script is for working with the full Basic Skills revenue and expenditure data
#this encompasses both compensatory revenue and English Learner revenue
#We received data on the revenue side (the "basicskills" file) on April 18, 2019 from MDE
#The UFARS file we had received earlier and that shows all the spending (program code 219 is spending for English Learners)




#install.packages("pastecs")
library(pastecs)

library(readr) #importing csv files
library(dplyr) #general analysis 
library(ggplot2) #making charts
library(lubridate) #date functions
library(reshape2) #use this for melt function to create one record for each team
library(tidyr)
library(janitor) #use this for doing crosstabs
library(scales) #needed for stacked bar chart axis labels
library(knitr) #needed for making tables in markdown page
library(htmltools)#this is needed for Rstudio to display kable and other html code
library(rmarkdown)
library(kableExtra)
library(ggthemes)
library(stringr)
library(RMySQL)
library(readxl) #for importing Excel files
library(DT) #needed for making  searchable sortable data tble
library(waffle)
library(foreign) #for importing SPSS files
library(jsonlite) #for exporting JSON
library(car)
library(aws.s3) #for loading to AWS server
options(scipen=999)
library(scales)



# Import Basic Skills revenue ---------------------------------------------

basicskills <-  read_csv('./data/basicskills_revenue_import.csv', col_types=cols(`District Number`=col_character(), `District Type`=col_character()))%>%
  clean_names() %>% mutate(districtid=paste(district_number, district_type, '000', sep='-'))

#winnow down to key fields
basicskills2 <-  basicskills %>% select(-district_number, -district_type, -district)

#normalize the data so there is one row for each variable for each district/year
basicskills3 <-  melt(basicskills2, id.vars='districtid') 

#add columns for the year and the variable type
basicskills3 <-  basicskills3 %>% mutate(datayr=substr(variable, 2, 6),
                                         yr= as.numeric(paste('20',substr(variable,5,6), sep='')),
                                         type=substr(variable, 8,100))


# import UFARS ------------------------------------------------------------

ufars06_18 <-  read_csv('./data/ufars06_18.csv', 
                        col_types=cols(.default=col_character(), tot_amt=col_double()))%>% rename(datayear=dat_yer,
                                                                                                  districtnum=dst_num,
                                                                                                  disttype=dst_tye,
                                                                                                  fund=fun_num,
                                                                                                  organization=ogz_num,
                                                                                                  program=prg_num,
                                                                                                  finance=fna_num,
                                                                                                  object=obj_num,
                                                                                                  course=crs_num,
                                                                                                  schoolclass=unt_cls)

#this is the list of codes (not needed for this analysis, though)
codes <-  read_excel("./data/UFARS/09-ListofCodes 2019.1.xlsx", sheet="CODES", range="A1:D730")


# import from mysql -------------------------------------------------------
#this imports names and other info on the districts


con <- dbConnect(RMySQL::MySQL(), host = Sys.getenv("host"), dbname="Schools",user= Sys.getenv("userid"), password=Sys.getenv("pwd"))

#list the tables in the database we've connected to
#dbListTables(con)

#list the fields in the table; change "mytablename" to the name of the table you're trying to connect to
#dbListFields(con,'mytablename')


#Pull DistrictList table
data1 <- dbSendQuery(con, "select * from DistrictList")

#assign it to a new data frame
district_list <- fetch(data1, n=-1)

dbClearResult(data1)


#disconnect connection
dbDisconnect(con)
rm(data1)


#clean up district_list data frame 
district_list <- district_list %>% clean_names() %>% rename(district_name=organization)


#add some fields to the ufars data
#need to limit to finance code 317 (basic skills) because they accidentally gave us some bad records
ufars06_18 <- ufars06_18 %>%
  filter(finance=='317') %>% 
  mutate(schoolid=paste(districtnum, disttype, organization, sep="-"),
         yr=as.integer(str_sub(datayear,4,6))+2000,
         districtid=paste(str_sub(schoolid,1,7),'000',sep="-"))


#split UFARS into two files
#note we're excluding the 2005-06 data from UFARS cause we don't have matching revenue data
#first one is for English Learner spending under Basic Skills
el_spent <-  ufars06_18 %>% filter(disttype=='01' | disttype=='03', program=='219', yr>2006) %>% 
  group_by(yr, districtid) %>% summarise(el_spent= sum(tot_amt))

#second one is for all non-EL spending under Basic Skills
comp_spent <-  ufars06_18 %>% filter(disttype=='01' | disttype=='03', program!='219', yr>2006) %>% 
  group_by(yr, districtid) %>% summarise(comp_spent= sum(tot_amt))



#These split the revenue side into separate batches
#English learner revenue
el_revenue <-  basicskills3 %>% filter(type=='el_revenue') %>% select(yr, districtid, el_rev=value)

#EL concentration revenue (a small boost for high concentration of EL kids)
el_conc_rev <-  basicskills3 %>% filter(type=='el_concentration_revenue') %>% select(yr, districtid, el_conc_rev=value)

#compensatory revenue
#see file called "compensatory_revenue_bysite_06_18.csv" for calculations in how this was allocated
comp_rev <-  basicskills3 %>% filter(type=='total_compensatory_revenue') %>% select(yr, districtid, compensatory_rev=value)

#total basic skills revenue (compensatory plus both English Learner buckets)
tot_basicskills <-  basicskills3 %>% filter(type=='total_basic_skills_revenue') %>% select(yr, districtid, basicskills_rev=value)



#Start putting everything together

#Join total basic skills revenue with compensatory revenue
match <-  left_join(tot_basicskills, comp_rev, by=c("districtid"="districtid", "yr"="yr"))

#Join what we have so far with EL revenue
match <-  left_join(match, el_revenue, by=c("districtid"="districtid", "yr"="yr"))

#Join what we have so far with EL concentration revenue
match <-  left_join(match, el_conc_rev, by=c("districtid"="districtid", "yr"="yr"))

#Join what we have so far with compensatory spending
match <-  full_join(match, comp_spent, by=c("districtid"="districtid", "yr"="yr"))


#Join what we have so far with EL spending
#also filter out districts that have NULL in the basicskills revenue column
match <-  full_join(match, el_spent, by=c("districtid"="districtid", "yr"="yr")) %>% filter(basicskills_rev!='NA')

#filter out districts that have 0 basic skills revenue
match <-  match %>% filter(basicskills_rev!=0)




#add districtname and other info about the district
compare_districts <-  left_join(match, district_list %>% select(id_number, district_name, county, metro7county, location), by=c("districtid"="id_number"))



#fill in null values
compare_districts$compensatory_rev[is.na(compare_districts$compensatory_rev)] <- 0
compare_districts$el_rev[is.na(compare_districts$el_rev)] <- 0
compare_districts$el_conc_rev[is.na(compare_districts$el_conc_rev)] <- 0
compare_districts$comp_spent[is.na(compare_districts$comp_spent)] <- 0
compare_districts$el_spent[is.na(compare_districts$el_spent)] <- 0


#add fields

#calculate total spent
#different between spending and revenue
#pct spent is the percentage of revenue that was spent 
compare_districts <-  compare_districts %>% mutate(tot_spent = comp_spent+el_spent,
                                                   diff=round(basicskills_rev-tot_spent,2),
                                                   pctspent = (tot_spent/basicskills_rev)*100)


#add a column that puts that pct spending into buckets
compare_districts <-  compare_districts %>% 
  mutate(scope = case_when(pctspent==0 ~ 'none',
                           pctspent==100~'100%',
                           pctspent>100 ~ 'over spent',
                           pctspent<100  ~'under spent',
                           TRUE ~'check'))

#calculate the difference in spending as a percentage of revenue
#then put them into buckets
#anything that is 15% or more over or under are potentially problematic (according to MDE)
compare_districts <- compare_districts %>%  mutate( diffpct=round((diff/basicskills_rev)*100,1),
                                                    diffscope = case_when(diffpct>=14.49~'over by 15% or more',
                                                                          diffpct<14.49 & diffpct>9.49~'over by 10%-14%',
                                                                          diffpct<=9.49 & diffpct>0 ~'over by less than 10%',
                                                                          diffpct==0 ~'even',
                                                                          diffpct<0 & diffpct> -9.49 ~'under by less than 10%',
                                                                          diffpct> -14.49 & diffpct< -9.49~'under by 10-14%%',
                                                                          diffpct<= -14.49~'under by 15% or more',
                                                                      TRUE~'something went wrong'))


#Count up how many fell in each bucket in 2018
compare_districts %>% filter(yr==2018) %>% group_by(diffscope) %>% summarise(count=n())




# AUTO-GENERATE CHARTS -------------------------------------------------------------


#this generates list of the  districts that are over/under by 15% plus St. Paul & Minneapolis
districts <-  compare_districts %>% 
  filter(yr==2018, diffscope=='over by 15% or more' | diffscope=='under by 15% or more' | districtid=='0625-01-000' | districtid=='0001-03-000') %>% ungroup() %>% select(district_name) %>% distinct() 



#this generates charts and data files for all the districts in districts df
#puts them in sub-directory called "district_exports"
for (i in 1:nrow(districts)){
  
  district = districts$district[i]
  
  g1_data <-  gather(compare_districts %>% 
                       filter(district_name==district) %>% 
                       ungroup()%>% 
                       select(yr, basicskills_rev, tot_spent), type, amount, basicskills_rev:tot_spent)
  
  plot <- ggplot(g1_data, aes(yr, amount, fill=type))+
    geom_bar(stat = "identity", position = 'dodge') +
    scale_y_continuous(labels=dollar_format())+
    scale_x_continuous(breaks=c(2007:2018, 1))+
    scale_fill_manual(name=NULL,
                      values=c("#00559c", "#6c7176"),
                      breaks=c("basicskills_rev", "tot_spent"),
                      labels=c("Revenue", "Expenditure"))+
    theme_hc()+
    labs(title = district, 
         subtitle = "Basic Skills revenue and spending",
         caption = "Star Tribune analysis",
         x="Ending fiscal year",
         y="")
  plot
  
  plotname <-  paste('./district_exports/', district, 'graphic', sep='_')
  
  ggsave(paste(plotname, '.jpg'), plot,width=8, height=5, units="in", dpi="print" )
  
  df <-  compare_districts %>% 
    filter(district_name==district) %>% 
    ungroup() %>% 
    select(yr, district_name, compensatory_rev, el_rev, el_conc_rev, basicskills_rev, comp_spent, el_spent, tot_spent)
  
  datafilename <-  paste('./district_exports/', district, 'data', sep='_')
  
  write.csv(df, paste(datafilename, '.csv'), row.names=FALSE)
  
}




# MAKE CHARTS MANUALLY ----------------------------------------------------

#use code below to create a chart for a single district and then export manually


district2 = 'Anoka-Hennepin Public School District'

g2_data <-  gather(compare_districts %>% 
                     filter(district_name==district2) %>% 
                     ungroup()%>% 
                     select(yr, basicskills_rev, tot_spent), type, amount, basicskills_rev:tot_spent)

plot2 <- ggplot(g2_data, aes(yr, amount, fill=type))+
  geom_bar(stat = "identity", position = 'dodge') +
  scale_y_continuous(labels=dollar_format())+
  scale_x_continuous(breaks=c(2007:2018, 1))+
  scale_fill_manual(name=NULL,
                    values=c("#00559c", "#6c7176"),
                    breaks=c("basicskills_rev", "tot_spent"),
                    labels=c("Revenue", "Expenditure"))+
  theme_hc()+
  labs(title = district2, 
       subtitle = "Basic Skills revenue and spending",
       caption = "Star Tribune analysis",
       x="Ending fiscal year",
       y="")
plot2

compare_districts %>% filter(diffscope=='over by 15% or more', yr==2018) %>% select(district_name)
