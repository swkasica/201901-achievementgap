library(readr) #importing csv files

#install.packages("devtools")
#devtools::install_github("andrewbtran/muckrakr")

library(muckrakr) #this is needed for merging csvs in a directory (bulk_csv)

#The original files from MDE are in the UFARS directory
#They came as Excel files; I padded columns with the leading zeros that had been lost
#and then converted them to csv files
#This command below merges them into one big file and puts a new csv in the data directory


bulk_csv(folder="./data/UFARS", export="./data/ufars06_18.csv")


