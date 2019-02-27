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


codes <-  read_excel("./data/UFARS/09-ListofCodes 2019.1.xlsx", sheet="CODES", range="A1:D728")



# import from mysql -------------------------------------------------------



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


#Pull mobility data 
data2 <- dbSendQuery(con, "select schoolid, fiscalyear as yr, midyr_pct as mobility, schoolclassification
from mobility")

#assign it to a new data frame
mobility <- fetch(data2, n=-1)

dbClearResult(data2)



#Pull race data 
data3 <- dbSendQuery(con, "select schoolid, schoolyear as yr,  pctminority, totalstudents
from enroll_race where schoolyear>=2007")

#assign it to a new data frame
race <- fetch(data3, n=-1)

dbClearResult(data3)


#Pull teacher data 
data4 <- dbSendQuery(con, "select idnumber as schoolid, concat('20', right(schoolyr,2)) as yr, totfte
from teacher_demographics where distnum<>'9999'")

#assign it to a new data frame
teachers <- fetch(data4, n=-1)

dbClearResult(data4)


#Pull special enrollment data (free lunch and ELL)
data5 <- dbSendQuery(con, "select schoolid, concat('20', right(datayear,2)) as yr,k12enr, freek12, redk12, lepidentifiedk12, lepservedk12
                     from enroll_special
                     where grade='All Grades' and datayear not like '9%'
                     having yr>2006")

#assign it to a new data frame
special <- fetch(data5, n=-1)

dbClearResult(data5)



#Pull SchoolList table
data6 <- dbSendQuery(con, "select * from SchoolList")

#assign it to a new data frame
school_list <- fetch(data6, n=-1)

dbClearResult(data6)



#disconnect connection
dbDisconnect(con)



# clean up mysql data -----------------------------------------------------


special$k12enr[is.na(special$k12enr)] <- 0
special$freek12[is.na(special$freek12)] <- 0
special$redk12[is.na(special$redk12)] <- 0
special$lepidentifiedk12[is.na(special$lepidentifiedk12)] <- 0
special$lepservedk12[is.na(special$lepservedk12)] <- 0


#clean up district_list data frame and add a districtid number
district_list <- district_list %>% clean_names() 

school_list <-  school_list %>% clean_names()


#change the yr variable to integer in a new field and drop the yr variable
race <- race %>% mutate(schoolyr=as.integer(yr)) %>% select(-yr)


mobility <- mobility %>% mutate(schoolyr=as.integer(yr)) %>% select(-yr)

special <- special %>% mutate(schoolyr=as.integer(yr)) %>% select(-yr)

teachers <- teachers %>% mutate(schoolyr=as.integer(yr)) %>% select(-yr)

# import text files -------------------------------------------------------



#open enrollment
openenroll <-  read_csv('./data/openenroll.csv')%>% group_by(districtid, yr) %>%
  summarise(enroll=sum(enrolled),
            leaving=sum(LeavingToTrad)+sum(LeavingToCharter),
            coming=sum(ComingIn),
            resident=sum(residents)) %>% 
  mutate(pctleving=leaving/resident,
         pctcoming=coming/enroll)


#attendance
attend <-  read_csv('./data/consistent_attendance_northstar.csv') %>% 
  mutate(schoolid=paste(districtnumber, districttype, schoolnumber, sep="-"))



#compensatory revenue / poverty concentration
#this one needs districtid attached to grab location information from district_list
revenue <-  read_csv('./data/compensatory_revenue_bysite_06_18.csv') %>% 
  clean_names() %>% 
  mutate(schoolid=paste(district_number, district_type, site_number, sep="-"),
         yr=as.integer(str_sub(year,4,6))+2000,
         districtid=paste(str_sub(schoolid,1,7),'000',sep="-"))





#mca data
math <-  read_csv('./data/math_scores.csv', col_types=cols(.default="c", totaltested=col_integer(),
                                                    level3=col_integer(),
                                                    level4=col_integer())) %>% 
  mutate(math_totalproficient=level3+level4,
         math_pctproficient=math_totalproficient/totaltested,
         yr=as.integer(str_sub(datayear,4,6))+2000) %>% 
  filter(yr>=2007)



read <-  read_csv('./data/read_scores.csv', col_types=cols(.default="c", totaltested=col_integer(),
                                                           level3=col_integer(),
                                                           level4=col_integer())) %>% 
  mutate(read_totalproficient=level3+level4,
         read_pctproficient=read_totalproficient/totaltested,
         yr=as.integer(str_sub(datayear,4,6))+2000) %>% 
  filter(yr>=2007)



rm(data1)
rm(data2)
rm(data3)
rm(data4)
rm(data5)
rm(data6)


# IDENTIFY SCHOOLS IN NEED ------------------------------------------------

#sets a level (1 through 4) depending on math proficiency percentage
#for simplicity, I cut it at quartiles, with the highest number (4) being the most in need
math <-  math %>% mutate(need_level = case_when(math_pctproficient<.25~4,
                                                math_pctproficient>=.25 & math_pctproficient<.5~3,
                                                math_pctproficient>=.5 & math_pctproficient<.75~2,
                                                math_pctproficient>=.75~1))


#create a column that counts that number of kids below proficiency
#(levels 1 and 2 added together)
math <-  math %>% mutate(number_in_need = totaltested-math_totalproficient)


#math %>% group_by(yr) %>% summarise(in_need=sum(number_in_need))




# MATCH MONEY -------------------------------------------------------------

#to simplify this, going to limit data down to 2017-18 school year
#And only district types of 1, 3 and 7

math2018 <-  math %>% filter(yr==2018) %>%  select(schoolid, yr, need_level, totaltested, number_in_need, math_pctproficient) %>% 
  mutate(dist_type=str_sub(schoolid,6,7))%>% filter(dist_type=='01' | dist_type=='03' | dist_type=='07')

names(revenue)

revenue <- revenue %>% filter(yr>2006) %>%  select(schoolid, districtid, district_number, district_type, district_name, site_number, site_name, yr, fall_enrollment, free_lunch_count, reduced_lunch_count,
                                                   adjusted_count, concentration, factor, pupil_units,
                                                   revenue_per_adjusted_count, revenue) %>% 
  rename(students_yr_prior=fall_enrollment)




df <-  left_join(revenue, school_list %>% 
                   select(school_id, metro7county, location, school_name, school_location_county_name, classification, grades, school_type), by=c("schoolid"="school_id"))


#names(df)

revenue2018 <- df %>% filter(yr==2018, district_type=='01' | district_type=='03' | district_type=='07') %>%
  mutate(grades2= str_trim(grades))


#organization code 005 = districtwide spending

ufars06_18 <- ufars06_18 %>%   mutate(schoolid=paste(districtnum, disttype, organization, sep="-"),
                                     yr=as.integer(str_sub(datayear,4,6))+2000,
                                     districtid=paste(str_sub(schoolid,1,7),'000',sep="-"))



#this excludes the program 219, English Learner spending
ufars2018 <-  ufars06_18 %>% 
  filter(program!='219', datayear=='17-18', disttype=='01' | disttype=='03' | disttype=='07')%>%
  group_by(yr, schoolid, disttype, districtid) %>%
  summarise(tot_spent = sum(tot_amt))




match2018 <- left_join(revenue2018, ufars2018 %>% select(schoolid, tot_spent), by=c("schoolid"="schoolid"))

match2018 <- left_join(match2018, math2018, by=c("schoolid"="schoolid"))

match2018 %>% group_by(need_level) %>% summarise(tot_revenue = sum(revenue),adjusted_rev=sum(revenue_per_adjusted_count), total_spent=sum(tot_spent), count=n(), pupils=sum(adjusted_count))

match2018$tot_spent[is.na(match2018$tot_spent)] <- 0
match2018$revenue[is.na(match2018$revenue)] <- 0


match2018 <-  match2018 %>% mutate(poverty_level = case_when(concentration>=.8~'very high',
                                                             concentration>=.6 & concentration<.8~'high',
                                                             concentration>=.4 & concentration<.6~'medium',
                                                             concentration>=.2 & concentration<.4~'low',
                                                             concentration<.2~'very low'))


#match2018 %>% filter(tot_spent>0) %>% group_by(poverty_level) %>%
#  summarise(tot_revenue = sum(revenue),adjusted_rev=sum(revenue_per_adjusted_count), total_spent=sum(tot_spent), count=n(), pupils=sum(adjusted_count))


match2018 <-  match2018 %>% mutate(rev_per_need = if_else(is.na(revenue), 0, round_half_up(as.double(revenue/adjusted_count))),
                                   spend_per_need = if_else(is.na(tot_spent), 0, round_half_up(as.double(tot_spent/adjusted_count))))





