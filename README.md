Data source: Minnesota Department of Education

This is the code used to analyze MDE data on the "Basic Skills" revenues that school districts receive to support under-achieving students and English Learners. This amounts to more than $600 million per year, however our reporting found that there's very little oversight into how this money is spent despite a state law that specifies not only how the money can be spent, but also that districts should report how it was spent and how it is affecting achievement. 

Data: 
This analysis is based on several different files that ended up needing to be cobbled together because some of the initial files received were missing key elements, either because we didn't ask for it (not knowing we would need it) or because MDE didn't include it. All of these files were received as a result of Data Practices Act requests (they aren't readily available on MDE Data Center site)

1) compensatory_revenue_bysite_06_18.csv -- Compensatory revenue by school/site for 2005-06 through 2017-18. This shows the calculations used to determine how much compensatory revenue each school earned each year. To get district totals, use the files below. This is supposed to include all district types, including charter schools. Best use for this file is to see how much each school/site generated and then compare that to what was spent there. 

2) Basicskills_revenue_import.csv -- This is rolled up to the district level but only for district types 1 and 3, showing total compensatory revenue and the English Learner revenue and English Learner concentration revenue that each district received each year, 2006-07 through 2017-18. Note: the compensatory totals are missing pilot project money and the one-time payments that occurred in 2013 (see #4 below)

3) LEPTypes2and7.csv -- We asked for this to supplement the two files above for charter schools. The first file has compensatory revenue for charter schools and this file has the English Learner revenues for 2006-07 through 2017-18.

4) additional_compensatory_data.csv -- This is a revision of district-level compensatory totals to include the amounts some districts received as part of a pilot program and amounts received in a one-time "bonus" that occurred in 2013. It only affects the traditional school districts (types 1 and 3)

5) UFARs_06-18.csv -- This is spending data from the UFARS financial reporting system. It's supposed to only include records that had the finance code 317 (for Basic Skills) however MDE accidentally left some other records in (my code weeds them out as part of the import). This is at the school/site level, but can be summarized up to the district level. This is what we used to determine how much Basic Skills money (both compensatory and English Learner) were spent in each district. The data proved insufficient, though, because we found so many districts spending far more than they got -- they told us that they used other revenue sources and labeled the expenses as Basic Skills because the money was being used to support under-achieving students. This data originally came as separate files for each fiscal year. I put them together into one csv file manually.


Scripts:
1) basicskills_districts.R  -- this is the main file used to cobble together the various files. It compares revenues and spending for traditional districts and then puts them into buckets by how far apart these two figures were. (We didn't do this for charters because charters are except from the reporting requirements about how the money is spent) It also generates a file for an online lookup tool to show how much each district (including charters) received in Basic Skills revenue and English Learner revenue. 

2) generate_charts.R -- this is code to auto-generate charts for each school district, comparing how much they received in compensatory revenue to how much they spent. The filter at the top of the chart identifies only certain districts to do this for. The charts are output to a folder called "district_exports". In addition to a ggplot chart, it also spits out a csv file with the underlying data.

3) richfield.Rmd -- this code drilled down into Richfield schools and specifically Centennial Elementary, which was used as an example in the story. 


