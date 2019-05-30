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


#need compare_districts table that is created in the basicskills_districts.R file



# AUTO-GENERATE CHARTS -------------------------------------------------------------


#this generates list of the  districts that are over/under by 15% plus St. Paul & Minneapolis
districts <-  compare_districts %>% 
  filter(yr==2018, diffscope=='over by 15% or more' | diffscope=='under by 15% or more' | districtid=='0625-01-000' | districtid=='0001-03-000') %>% ungroup() %>% select(district_name) %>% distinct() 



#this generates charts and data files for all the districts in districts df
#puts them in sub-directory called "district_exports"
for (i in 1:nrow(districts)){
  
  district_selected = districts$district[i]
  
  g1_data <-  gather(compare_districts %>% 
                       filter(district_name==district_selected) %>% 
                       ungroup()%>% 
                       select(yr, total_basic_skills_revenue, tot_spent), type, amount, total_basic_skills_revenue:tot_spent)
  
  plot <- ggplot(g1_data, aes(yr, amount, fill=type))+
    geom_bar(stat = "identity", position = 'dodge') +
    scale_y_continuous(labels=dollar_format())+
    scale_x_continuous(breaks=c(2007:2018, 1))+
    scale_fill_manual(name=NULL,
                      values=c("#00559c", "#6c7176"),
                      breaks=c("total_basic_skills_revenue", "tot_spent"),
                      labels=c("Revenue", "Expenditure"))+
    theme_hc()+
    labs(title = district_selected, 
         subtitle = "Basic Skills revenue and spending",
         caption = "Star Tribune analysis",
         x="Ending fiscal year",
         y="")
  plot
  
  plotname <-  paste('./district_exports/', district_selected, 'graphic', sep='_')
  
  ggsave(paste(plotname, '.jpg'), plot,width=8, height=5, units="in", dpi="print" )
  
  df <-  compare_districts %>% 
    filter(district_name==district_selected) %>% 
    ungroup() %>% 
    select(yr, district_name, total_compensatory_revenue, el_revenue, el_concentration_revenue, total_basic_skills_revenue, comp_spent, el_spent, tot_spent)
  
  datafilename <-  paste('./district_exports/', district_selected, 'data', sep='_')
  
  write.csv(df, paste(datafilename, '.csv'), row.names=FALSE)
  
}



# MAKE CHARTS MANUALLY ----------------------------------------------------

#use code below to create a chart for a single district and then export manually


district2 = 'MONTGOMERY-LONSDALE SCHOOL DISTRICT'

g2_data <-  gather(compare_districts %>% 
                     filter(district_name==district2) %>% 
                     ungroup()%>% 
                     select(yr, total_basic_skills_revenue, tot_spent), type, amount, total_basic_skills_revenue:tot_spent)

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



