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





#additional compensatory -- pilot, one-time and early learning 
additional_comp <-  read_xlsx('./data/Additional district level compensatory data.xlsx', sheet='Compensatory Revenue Values', range='B4:K6611') %>% 
  clean_names() %>% mutate(yr=as.integer(str_sub(year,4,6))+2000, districtid=paste(district_number_type, '000', sep='-'))



#match datasets

basics_skills_by_district <-  full_join(additional_comp %>% select(yr, districtid, district_name, compensatory_by_site, x1st_year_vpk_srp_compensatory, compensatory_one_time, compensatory_pilot, total_compensatory), 
                                        el_revenue, by=c("yr"="yr", "districtid"="districtid"))

basics_skills_by_district <-  left_join(basics_skills_by_district, el_conc_rev, by=c("yr"="yr", "districtid"="districtid"))

basics_skills_by_district$el_rev[is.na(basics_skills_by_district$el_rev)] <-  0
basics_skills_by_district$el_conc_rev[is.na(basics_skills_by_district$el_conc_rev)] <-  0


foronline <-  basics_skills_by_district %>% filter(yr==2018, district_name!='NA') %>%  
  mutate(district_name=toupper(district_name), el_total = el_rev+el_conc_rev, pilot = case_when(compensatory_pilot>0~'y', TRUE~'n'), basicskills_total = total_compensatory+el_total) %>%
  select(district_name, pilot, total_compensatory, el_total, basicskills_total)

write.csv(foronline, './output/district_totals_2018_foronline.csv', row.names=FALSE)





#this is revenue by building for all district types
#this needs to be updated when MDE sends new data
revenue <-  read_csv('./data/compensatory_revenue_bysite_06_18.csv') %>% 
  clean_names() %>% 
  mutate(schoolid=paste(district_number, district_type, site_number, sep="-"),
         yr=as.integer(str_sub(year,4,6))+2000,
         districtid=paste(str_sub(schoolid,1,7),'000',sep="-"))



#grab only the district type 2 and 7 compensatory revenue - summarized to district level
#this needs to be updated when MDE sends new data
comp_rev_charters <-  revenue %>% filter(district_type=='02' | district_type=='07') %>% group_by(districtid, yr) %>% summarize(comp_rev_total = sum(revenue))

#this is EL revenue for charter schools - district types 2 and 7
el_rev_charters <-  read_csv('./data/LEPTypes2and7.csv', col_types=cols(dst_num=col_character(), dst_tye=col_character(),
                                                                        lep_rev=col_double(), lep_cnc_rev=col_double())) %>% 
  clean_names() %>% 
  mutate(districtid = paste(dst_num, dst_tye, '000', sep="-"),
         total_el = lep_rev+lep_cnc_rev,
         yr=as.integer(str_sub(dat_yer,4,6))+2000)


#all charter school basic skills revenue

charters_rev <-  left_join(comp_rev_charters, el_rev_charters, by=c("yr"="yr", "districtid"="districtid") ) %>%
  mutate(basicskills_total = total_el + comp_rev_total)


charters_rev %>% filter(dst_tye=='07') %>%  group_by(yr) %>% summarise(count=n(), tot= sum(basicskills_total), el = sum(total_el), comp=sum(comp_rev_total))






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


write.csv(compare_districts, './output/districts_basicskills_totals.csv', row.names=FALSE)
















                                                                                                  