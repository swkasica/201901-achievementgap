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




df2018 <- df %>%  filter(yr==2018)


### Merge UFARS with school_list

names(ufars06_18)
names(school_list)
names(district_list)

district_list <-  district_list %>% rename(districtname=organization)


#add district information and codes
ufars06_18 <- left_join(ufars06_18, district_list %>% 
                          select(district_number, district_type, districtname, metro7county, location),
                        by=c("districtnum"="district_number",
                             "disttype"="district_type"))%>%   mutate(schoolid=paste(districtnum, disttype, organization, sep="-"),
                                                                      yr=as.integer(str_sub(datayear,4,6))+2000,
                                                                      districtid=paste(str_sub(schoolid,1,7),'000',sep="-"))


#ufars data has multiple rows per school per year
#revenue data has one row per school per year

#school name information is stored in revenue
#but do we have the same schools in revenue as in the ufars data? (possibly not)

#might need to ask MDE for a school lookup table (district number, districttype, school number, school name and school classification)

#or try getting codes from mca table in mysql (see if I have more school info in there that what I have stored in school list)


#filter out the English Language Learner spending (program=219)
ufars_summary <- ufars06_18 %>% 
  filter(program!='219')%>% group_by(datayear, yr, districtname, districtid, metro7county, location, organization) %>% summarise(tot_spent = sum(tot_amt))

match_rev_ufars<-  left_join(revenue %>% 
                               select(schoolid, districtid, site_number, yr, revenue), ufars_summary %>% select(yr, districtid, organization, tot_spent),
                             by=c("districtid"="districtid", "site_number"="organization", "yr"="yr")) 


match_ufars_rev<-  left_join(ufars_summary %>% select(yr, districtid, organization, tot_spent),
                             revenue %>% 
                               select(schoolid, districtid, site_number, yr, revenue), 
                             by=c("districtid"="districtid", "organization"="site_number", "yr"="yr")) 



ufars_by_district <- ufars06_18 %>%
  filter(program!='219') %>% group_by(yr, districtid) %>% summarise(spending=sum(tot_amt))

rev_by_district <-  revenue %>% group_by(yr, districtid) %>% summarise(rev=sum(revenue))

match_rev_ufars_district <- left_join(rev_by_district, ufars_by_district, by=c("districtid"="districtid", "yr"="yr"))

write.csv(match_rev_ufars_district, 'match_test.csv', row.names = FALSE)


mpls <- match_rev_ufars %>% filter(districtid=='0001-03-000') %>% select(yr, site_number, revenue, tot_spent)
sp <-  match_rev_ufars %>% filter(districtid=='0625-01-000') %>% select(yr, site_number, revenue, tot_spent)


sp_ufars_rev <-  match_ufars_rev%>% filter(districtid=='0625-01-000') %>% select(yr, organization, revenue, tot_spent)
