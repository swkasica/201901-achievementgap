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


# ADD HERE -- import UFARS data -------------------------------------------





# combine tables ----------------------------------------------------------
#table list and fields to match on
#checked that all the yr fields are numeric; schoolid fields are character



#district_list (IDnumber)


#revenue (schoolid, yr)
#attend (schoolid, datayr)

#mobility (schoolid, schoolyr)
#race (schoolid, schoolyr)
#special(schoolid, schoolyr)
#teachers (schoolid, schoolyr)


#math (schoolid, yr)
#read (schoolid, yr)





#openenroll -- match on districtID & yr




revenue <- revenue %>% filter(yr>2006) %>%  select(schoolid, districtid, district_number, district_type, district_name, site_number, site_name, yr, fall_enrollment, free_lunch_count, reduced_lunch_count,
                              adjusted_count, concentration, factor, pupil_units,
                              revenue_per_adjusted_count, revenue) %>% 
  rename(students_yr_prior=fall_enrollment)




df <-  left_join(revenue, school_list %>% 
                   select(school_id, metro7county, location, school_name, school_location_county_name, classification, grades, school_type), by=c("schoolid"="school_id"))




#this is the only one that didn't match to district_list
#df %>% filter(districtid=='1465-35-000') 

#the attend table only has data for 2016-2017-2018
df <-  left_join(df, attend %>% select(schoolid, datayear,consisten_attendance, studentcount, schoolclassifcation), by=c("schoolid"="schoolid", "yr"="datayr") )



df <-  left_join(df, mobility, by=c("schoolid"="schoolid", "yr"="schoolyr"))



df <-  left_join(df, race, by=c("schoolid"="schoolid", "yr"="schoolyr"))


df <-  left_join(df, attend %>% select(schoolid, datayr, consisten_attendance), by=c("schoolid"="schoolid", "yr"="datayr"))

df <-  left_join(df, special, by=c("schoolid"="schoolid", "yr"="schoolyr"))

df <-  left_join(df, teachers, by=c("schoolid"="schoolid", "yr"="schoolyr"))

df <-  left_join(df, math %>% select(schoolid, yr, math_totalproficient, math_pctproficient), by=c("schoolid"="schoolid", "yr"="yr"))

df <-  left_join(df, read %>% select(schoolid, yr, read_totalproficient, read_pctproficient), by=c("schoolid"="schoolid", "yr"="yr"))

#write.csv(df, 'test.csv', row.names=FALSE)


df$totalstudents[is.na(df$totalstudents)] <- 0
df$adjusted_count[is.na(df$adjusted_count)] <- 0


df %>% filter(yr==2018) %>% group_by(school_type) %>% summarise(rev=sum(revenue), students=sum(adjusted_count)) %>%
  mutate(perstudent=rev/students) %>% 
  arrange(desc(rev))


df2018 <- df %>%  filter(yr==2018)

df2018 %>% filter(is.na(mobility)) %>% group_by(school_type) %>% summarise(count=n())
