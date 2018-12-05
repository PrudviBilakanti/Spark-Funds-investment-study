library(tidyr)
library(dplyr)
library(stringr)


#Checkpoints - Part 1 

#Checkpoint 1: Data Cleaning 1

#read companies and rounds data from the given datasets.
companies <- read.delim('companies.txt', header=TRUE, sep='\t', stringsAsFactors = F, encoding = 'Latin-1')

rounds2 <- read.csv('rounds2.csv',stringsAsFactors = F, encoding = 'Latin-1')


#lets change the permalink and company_permalink to lower letters which helps us compare the columns and merge 2 df
# the 2 dataframes.

companies$permalink = tolower(companies$permalink)
rounds2$company_permalink = tolower(rounds2$company_permalink)

# How many unique companies are present in rounds2?
# n_distinct method returns the number of unique elements in a column
n_distinct(companies$permalink)
# 66368

#How many unique companies are present in companies?
n_distinct(rounds2$company_permalink)
# 66368

#In the companies data frame, which column can be used as the unique key for each company? Write the name of the column.
#permalink

#Are there any companies in the rounds2 file which are not present in companies? Answer yes or no: Y/N
sum(!(rounds2$company_permalink %in% companies$permalink)) 
# %in% operation returns "True" if a value from rounds2 is found in companies permalink. 
# negating it to find which are not present in second column
# sum returns " ZERO " which means the number of companies in companies df and rounds df are equal.
# " No " there are no companies that are in rounds2 and not in companies df


#Merge the two data frames so that all variables (columns) in the companies frame are added to the rounds2 data frame. 
#Name the merged frame master_frame. How many observations are present in master_frame?

# merge rounds2 and companies df by their unique ID's
# Use merge function to merge dataframe with the common ID in both the datasets.
master_frame <- merge(companies, rounds2, by.x = 'permalink', by.y = 'company_permalink', all = T)

# There are 114949 observations in master_frame.


# Data cleansing  #

# There are missing values in the raised_amount_usd column in master dataframe.
# Missing Values are more than 10% of dataset and we cannot afford to lose as we may suffer Loss of quality info and get biased results
# Below is an example of missing values for a company.

#/organization/36kr	/funding-round/5cb773d6450904c837e83c07d0192796	venture	A	1/7/2012	1573976
#/ORGANIZATION/36KR	/funding-round/9282dbf72ef2cc932556e99888916f2f	angel		  1/4/2011	
#/organization/36kr	/funding-round/e27661055554c1ca2741c507c87f4f3e	venture	D	15-10-2015

# As given in the instructions, that if a company has reached the venture stage, 
# it would have already passed through the angel or seed stage/s.
# but for some reason the funding amount was missing in certain rows

# We have decided to replace the missing values with its respective funding type mean value.
# This way the mean value of funding type doesnt change over all.

# Below command cal average for each funding type and replace NA with its respective funding type mean value.


master_frame$raised_amount_usd <-  with(master_frame, ave(raised_amount_usd, funding_round_type,
                                                            FUN = function(x) replace(x, is.na(x), mean(x, na.rm = TRUE))))


################################################################################################

#Checkpoint 2: Funding Type Analysis


#Calculate the average investment amount for each of the four funding types (venture, angel, seed, and private equity) 
# Below peice of code returns the avg fund recieved by selected funding type

master_frame %>%
  filter(funding_round_type == "venture" | funding_round_type == "angel" | funding_round_type == "seed" | funding_round_type == "private_equity") %>%
  group_by(funding_round_type) %>%
  summarise(avgfunding = mean(raised_amount_usd))

# funding_round_type avgfunding
# <chr>                   <dbl>
# 1 angel                  958694
# 2 private_equity       73308593
# 3 seed                   719818
# 4 venture              11748949

# Based on the average investment amount calculated above, which investment type do you think is the most suitable for Spark Funds?

# considering the amount Spark Funds wants to invest 5M to 15M USD, " Venture " fund type would be the right choice.


################################################################################################


#Checkpoint - Part 2
#Checkpoint 3: COUNTRY ANALYSIS


#Spark Funds wants to see the top nine countries which have received the highest total funding (across ALL sectors for the chosen investment type)

# Master data frame has missing values in country_code column. 
# Missing values total count is less than 10% of the dataset and we are considering to filter them.
# As it doesnt add any value to know about amount invested in country with blank value.

#filter the df by venture fund type and ignore country code with blank values, 
#group by country
#summariase the avg amount invested in each country 
#Then Sort by avg fund in Decending order
#pick top9 entries from the result df


top9 <- master_frame %>%
  filter(funding_round_type == 'venture' & country_code != "") %>%       
  group_by(country_code) %>%                        
  summarise(totfundbycountry = sum(raised_amount_usd)) %>%  
  arrange(.,desc(totfundbycountry)) %>%            
  head(9)                                        
                                                 


# From the above results, Top 3 english countries are as below
#1  Top English-speaking country - USA
#2  Second English-speaking country - GBR
#3  Third English-speaking country - IND


################################################################################################


#Checkpoint 4: Sector Analysis 1

# Extract the primary sector of each category list from the category_list column.
# Using seperate function to get primary sector. specifying remove=F retains the original
#column.
# Dropping "extra" values as we are retaining original category_list and our focus is to extract and store the primary sector.

master_frame <- 
  separate(master_frame, category_list, into = c("primary_sector"), sep = "\\|", remove=F, extra='drop',fill = "right")

# Use the mapping file 'mapping.csv' to map each primary sector to one of the eight main sectors.
# Read mapping csv into a df.

sector_mapping <- read.csv('mapping.csv', stringsAsFactors = F, check.names=FALSE) 

#convert df from wide to long format.
sector_mapping <- gather(sector_mapping, main_sector, sectorvalue, 2:10)

#get rid of extra columns and rows.
sector_mapping <- sector_mapping[!(sector_mapping$sectorvalue == 0),]
sector_mapping <- sector_mapping[, -3]

# Data cleansing in sector_mapping df. 

# It is observed that there are some misspelled sector names in category_list column.
# Replace the value '0' with 'na' characters. gsub function comes handy for this task.
sector_mapping$category_list <- gsub(0,'na', sector_mapping$category_list)

# Converting Primary_sector in master_frame and category_list in sector_mapping to lowercase

master_frame$primary_sector <- tolower(master_frame$primary_sector)
sector_mapping$category_list <- tolower(sector_mapping$category_list)

# now use sector mapping df to map the primary_sector to main_sector

#use "all.x=T" in merge if you want to retain non matching rows from master_frame.
# Here we will get rid of those values as there is no mapping of primary to main sector and our focus
#is to find the investments in 8 main_sectors
# ~ 41 unique primary sectors does not have mapping to any of the 8 main sector.

master_df <- merge(master_frame,sector_mapping, by.x="primary_sector", by.y = "category_list")  

# Removing rows in master_df with "Blanks" as main_sector as we are interested to know investments
#in 8 main sectors.

master_df <- master_df[!(master_df$main_sector == 'Blanks'),]

# Now we have data related to 8 main sectors.

################################################################################################

#Checkpoint 5: Sector Analysis 2

#Create three separate data frames D1, D2 and D3 
#for each of the three countries containing the observations of funding type FT falling within the 5-15 million USD range

#D1 df for Country USA, Venture fund type between 5M to 15M USD

D1 <- master_df %>%
  filter(country_code == "USA" & 
           between(raised_amount_usd, 5000000, 15000000) & 
           funding_round_type == "venture")

#D2 df for Country GBR, Venture fund type between 5M to 15M USD

D2 <- master_df %>%
  filter(country_code == "GBR" & 
           between(raised_amount_usd, 5000000, 15000000) & 
           funding_round_type == "venture")

#D3 df for Country IND, Venture fund type  between 5M to 15M USD

D3 <- master_df %>%
  filter(country_code == "IND" & 
           between(raised_amount_usd, 5000000, 15000000) & 
           funding_round_type == "venture")


#Calculate total number of investment for each main_sector
# Creating intermediate df to cal total no of investments and total amount invested in each sector in each country
# This intermediate df's will then be merged with its respective country df 


# total no of investments and total amount invested in each sector in Country 1

#Calculate total number of investment for each main_sector
D1_tot_no_investments <- D1 %>%
  group_by(main_sector) %>%
  summarise(no_of_invest_by_sector = n())

#Calculate total amount of investment in each main_sector

D1_tot_amount_invested <- D1 %>%
  group_by(main_sector) %>%
  summarise(tot_amount_invest_by_sector = sum(raised_amount_usd)) 

# Merge tot_no_investments and tot_amount_invested with D1 dataframe.
  
D1 <- merge(D1,D1_tot_no_investments,by='main_sector',all.x = T)
D1 <- merge(D1,D1_tot_amount_invested,by='main_sector',all.x = T)


# total no of investments and total amount invested in each sector in Country 2

#Calculate total number of investments in each main_sector in Country 2

D2_tot_no_investments <- D2 %>%
  group_by(main_sector) %>%
  summarise(no_of_invest_by_sector = n())

#Calculate total amount of investment in each main_sector

D2_tot_amount_invested <- D2 %>%
  group_by(main_sector) %>%
  summarise(tot_amount_invest_by_sector = sum(raised_amount_usd)) 

# Merge tot_no_investments and tot_amount_invested with D2 dataframe.

D2 <- merge(D2,D2_tot_no_investments,by='main_sector',all.x = T)
D2 <- merge(D2,D2_tot_amount_invested,by='main_sector',all.x = T)


# Total no. of investments and total amount invested in each sector in Country 3

#Calculate total number of investment in each main_sector

D3_tot_no_investments <- D3 %>%
  group_by(main_sector) %>%
  summarise(no_of_invest_by_sector = n())

#Calculate total amount of investment for each main_sector

D3_tot_amount_invested <- D3 %>%
  group_by(main_sector) %>%
  summarise(tot_amount_invest_by_sector = sum(raised_amount_usd)) 

# Merge tot_no_investments and tot_amount_invested with D3 dataframe.

D3 <- merge(D3,D3_tot_no_investments,by='main_sector',all.x = T)
D3 <- merge(D3,D3_tot_amount_invested,by='main_sector',all.x = T)



############################################################################################

# Table 5.1 : Sector-wise Investment Analysis


# 1. Total number of Investments (count) 

# Country 1
D1_tot_no_investments %>%
  summarise(sum(no_of_invest_by_sector))
#14154

#Country 2
D2_tot_no_investments %>%
  summarise(sum(no_of_invest_by_sector))
#845

#Country 3
D3_tot_no_investments %>%
  summarise(sum(no_of_invest_by_sector))
#493


# 2.Total amount of investment (USD)
# Country 1
D1_tot_amount_invested %>%
  summarise(sum(tot_amount_invest_by_sector))
#132324149924 $

# Country 2
D2_tot_amount_invested %>%
  summarise(sum(tot_amount_invest_by_sector))
#8010843296

# Country 3
D3_tot_amount_invested %>%
  summarise(sum(tot_amount_invest_by_sector))
#4888120208

# 3.Top Sector name (no. of investment-wise)
# Country 1
D1_tot_no_investments$main_sector[sapply(D1_tot_no_investments[,c('no_of_invest_by_sector')],which.max)]
#Others

# Country 2
D2_tot_no_investments$main_sector[sapply(D2_tot_no_investments[,c('no_of_invest_by_sector')],which.max)]
#Others

# Country 3
D3_tot_no_investments$main_sector[sapply(D3_tot_no_investments[,c('no_of_invest_by_sector')],which.max)]
#Others

# 4.Second Sector name (no. of investment-wise)
#Country 1
D1_tot_no_investments$main_sector[which(D1_tot_no_investments$no_of_invest_by_sector == sort(D1_tot_no_investments$no_of_invest_by_sector,decreasing = TRUE)[2])]
# "Social, Finance, Analytics, Advertising"

#Country 2
D2_tot_no_investments$main_sector[which(D2_tot_no_investments$no_of_invest_by_sector == sort(D2_tot_no_investments$no_of_invest_by_sector,decreasing = TRUE)[2])]
# "Social, Finance, Analytics, Advertising"

#Country 3
D3_tot_no_investments$main_sector[which(D3_tot_no_investments$no_of_invest_by_sector == sort(D3_tot_no_investments$no_of_invest_by_sector,decreasing = TRUE)[2])]
# "Social, Finance, Analytics, Advertising"

# 5.Third Sector name (no. of investment-wise)

# Country 1
D1_tot_no_investments$main_sector[which(D1_tot_no_investments$no_of_invest_by_sector == sort(D1_tot_no_investments$no_of_invest_by_sector,decreasing = TRUE)[3])]
#"Cleantech / Semiconductors"

# Country 2
D2_tot_no_investments$main_sector[which(D2_tot_no_investments$no_of_invest_by_sector == sort(D2_tot_no_investments$no_of_invest_by_sector,decreasing = TRUE)[3])]
# "Cleantech / Semiconductors"

# Country 3
D3_tot_no_investments$main_sector[which(D3_tot_no_investments$no_of_invest_by_sector == sort(D3_tot_no_investments$no_of_invest_by_sector,decreasing = TRUE)[3])]
# "News, Search and Messaging"


# 6.Number of investments in top sector (3)
# Country 1
D1_tot_no_investments$no_of_invest_by_sector[which(D1_tot_no_investments$main_sector == 'Others')]
#3403

# Country 2
D2_tot_no_investments$no_of_invest_by_sector[which(D2_tot_no_investments$main_sector == 'Others')]
#211

# Country 3
D3_tot_no_investments$no_of_invest_by_sector[which(D3_tot_no_investments$main_sector == 'Others')]
#158

# 7.Number of investments in second sector (4)
# Country 1
D1_tot_no_investments$no_of_invest_by_sector[which(D1_tot_no_investments$main_sector == "Social, Finance, Analytics, Advertising")]
#3290

# Country 2
D2_tot_no_investments$no_of_invest_by_sector[which(D2_tot_no_investments$main_sector == "Social, Finance, Analytics, Advertising")]
#193

# Country 3
D3_tot_no_investments$no_of_invest_by_sector[which(D3_tot_no_investments$main_sector == "Social, Finance, Analytics, Advertising")]
#106


# 8.Number of investments in third sector (5)

# Country 1
D1_tot_no_investments$no_of_invest_by_sector[which(D1_tot_no_investments$main_sector == "Cleantech / Semiconductors")]
#2622

# Country 2
D2_tot_no_investments$no_of_invest_by_sector[which(D2_tot_no_investments$main_sector == "Cleantech / Semiconductors")]
#154

# Country 3
D3_tot_no_investments$no_of_invest_by_sector[which(D3_tot_no_investments$main_sector == "News, Search and Messaging")]
#76


# 9.For point 3 (top sector count-wise), which company received the highest investment?

# Country 1

D1 %>%
  filter(main_sector == 'Others') %>%
  arrange(.,desc(raised_amount_usd)) %>%            
  head(1) %>%
  ungroup() %>%
  select(.,name)

#BetterWorks

# Country 2

D2 %>%
  filter(main_sector == 'Others') %>%
  arrange(.,desc(raised_amount_usd)) %>%            
  head(1) %>%
  ungroup() %>%
  select(.,name)

#Notonthehighstreet

# Country 3

D3 %>%
  filter(main_sector == 'Others') %>%
  arrange(.,desc(raised_amount_usd)) %>%            
  head(1) %>%
  ungroup() %>%
  select(.,name)

#K-12 Techno Services



# For point 4 (second best sector count-wise), which company received the highest investment?

# Country 1
  
D1 %>%
  filter(main_sector == "Social, Finance, Analytics, Advertising") %>%
  arrange(.,desc(raised_amount_usd)) %>%            
  head(1) %>%
  ungroup() %>%
  select(.,name)
#AdRoll

# Country 2

D2 %>%
  filter(main_sector == "Social, Finance, Analytics, Advertising") %>%
  arrange(.,desc(raised_amount_usd)) %>%            
  head(1) %>%
  ungroup() %>%
  select(.,name)
# myThings

# Country 3

D3 %>%
  filter(main_sector == "Social, Finance, Analytics, Advertising") %>%
  arrange(.,desc(raised_amount_usd)) %>%            
  head(1) %>%
  ungroup() %>%
  select(.,name)
# myThings

