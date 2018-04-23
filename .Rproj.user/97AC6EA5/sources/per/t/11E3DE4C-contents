library(tidyverse)
library(stringi)
library(zipcode)

# I downloaded each disclosure separatelty (on March 12) and then uploaded them to OpenRefine to make sure most lines are in standard format. Then I downloaded it back as a csv called cleaned_contribs_new. 

#Now I'm uplopading them into R
cleaned_contributions_new <- read_csv("cleaned_contribs_new.csv")
#let's see what we got. 
glimpse(cleaned_contributions_new)
summary(cleaned_contributions_new)

#############TIME FOR ANALYSIS##############

#before we break it up by candidate, let's see where the money is coming from to all candidates within the state of Maryland.

#let's start exploring where in the world this money is coming from...We can do that by extracting zip codes. 
#searches for all number combos that are either 5 digits long or 5 digits followed by 4 digits with a dash in between.
contributions_all_zips1 <- stri_extract_all_regex(cleaned_contributions_new$`Contributor Address`, "(?<!\\d)(\\d{5}(?:[-\\s]\\d{4})?)\\b")
contributions_all_5digitzips <- stri_extract_all_regex(contributions_all_zips1, "\\d{5}")
#I was getting some extra 5 digit combos from addresses, so I used this lapply command to just grab the last one, since the zip is always last. 
contributions_all_zips2 <- map(contributions_all_5digitzips, function(x) x[length(x)])
#creates the dataframe hogan_c as new hogan_c
contributions_all_zipz_df <- data_frame(contributions_all_zips2)
#changes the list to a numeric value. This helps the group by function which do4sn't like lists.
contributions_all_zipz_df %>% mutate_if(is.list, as.character) -> contributions_all_zipz_df
#changes the column header in the new dataframe to zip_codes. this will be helpful later on because it is unique. 
colnames(contributions_all_zipz_df)[which(names(contributions_all_zipz_df) == "contributions_all_zips2")] <- "zip_codes"
#binds it to the hogan contributions database
newall_contribswithzips <- cbind(contributions_all_zipz_df, cleaned_contributions_new)


#ok, so now that we have our new list with the added zip codes, we can go through and group them and count them. 




##########################4##########################

#now let's combine with the zip codfes database so we can group by state and county 

#pulls the database of zip codes/state information 
data(zipcode)
View(zipcode) 
#creates a merged list of zip codes and the hogan information. 
allcontribs_state_list_fromzips <-  left_join(newall_contribswithzips, zipcode, by =c("zip_codes" = "zip"))

write_csv(allcontribs_state_list_fromzips, "allcontributionswithzips.csv")

## so now that we've got the two tables merged, It's time to sort out the Maryland contributors. 

all_contribs_from_maryland <- allcontribs_state_list_fromzips %>% 
  filter(state == "MD")




################# HOGAN! ############


###############1################# 
#OK, so this one was way more complicated than I had hoped, but it searches for numbers that are grouped into 5 digits and sometimes are separated by a - sign or a dash. Then adds it to a list, which I converted to a numeric, grouped, sorte and then summed contributions. 


hogan_contribs_all <- cleaned_contributions_new  %>% 
  #here we're creating a dataframe of all entries that have the receiving committee equaling Hogan. And it excludes in-kind and refund/rebate information
  filter(`Receiving Committee` == "Hogan  Larry for Governor") %>%
  filter(`Contribution Type` != "In-Kind") %>%
  filter(`Contribution Type` != "Coordinated In-Kind") %>%
  filter(`Contribution Type` != "Refund/Rebate")
  View(hogan_contribs_all)
#this also creates the a dataframe of rebartes and inkind donations that I pulled out of the contributions list. Just to see if I see something interesting.
hogan_inkind_rebates_all <- cleaned_contributions_new  %>% 
  #here we're creating a dataframe of all entries that have the receiving committee equaling Hogan. And it excludes in-kind and refund/rebate information
  filter(`Receiving Committee` == "Hogan  Larry for Governor") %>%
  filter(`Contribution Type` == "In-Kind" | `Contribution Type` == "Coordinated In-Kind" | `Contribution Type` == "Refund/Rebate")
  View(hogan_inkind_rebates_all)


##############################2####################
# So as a safeguard, let's look at Hogan's contributions and see if they match up with the Baltimore Sun's numbers and the campaign's DISCLOSURE report.....not for antything but they seem like a competent group of journalists
sum(hogan_contribs_all$`Contribution Amount`)
#4852449 is what I got AND 643690.1 is what I got for in-kind....If you add up the in kind donations of $643,690.13 and $4,852,448.70 from his annual report, you get 5.495 million. 
sum(hogan_inkind_rebates_all$`Contribution Amount`)

#########################3#################

#let's start exploring where in the world this money is coming from...We can do that by extracting zip codes. 
#searches for all number combos that are either 5 digits long or 5 digits followed by 4 digits with a dash in between.
hogan_zips1 <- stri_extract_all_regex(hogan_contribs_all$`Contributor Address`, "(?<!\\d)(\\d{5}(?:[-\\s]\\d{4})?)\\b")
hogan_5digitzips <- stri_extract_all_regex(hogan_zips1, "\\d{5}")
#I was getting some extra 5 digit combos from addresses, so I used this lapply command to just grab the last one, since the zip is always last. 
hogan_zips2 <- map(hogan_5digitzips, function(x) x[length(x)])
#creates the dataframe hogan_c as new hogan_c
hogan_zips_df <- data_frame(hogan_zips2)
#changes the list to a numeric value. This helps the group by function which do4sn't like lists.
hogan_zips_df %>% mutate_if(is.list, as.character) -> hogan_zips_df
#changes the column header in the new dataframe to zip_codes. this will be helpful later on because it is unique. 
colnames(hogan_zips_df)[which(names(hogan_zips_df) == "hogan_zips2")] <- "zip_codes"
#binds it to the hogan contributions database
newhogan_contribswithzips <- cbind(hogan_zips_df, hogan_contribs_all)
#ok, so now that we have our new list with the added zip codes, we can go through and group them and count them. 
count_hogan_zip_codes <- newhogan_contribswithzips %>% 
  #the colum we are grouping 
  group_by(zip_codes)  %>%
  #creates a count with the row name being count. The n value is the number of times it occurs
  summarise(count = n(), total = sum(`Contribution Amount`,  na.rm = TRUE)) %>%
  #arrange the list in descending order
  arrange(desc(total)) 
View(count_hogan_zip_codes)
count_hogan_zip_codes[1:10,]

##########################4##########################

#now let's group by state and see where this money is coming from by joining this data with a helpful database of zip codes that is part of the zipcode library.....which you should install, or you're not going anywhere pat'na. PS: zipcode also has a cleaning feature, but it didn't work for my purposes. Thus the code above. 

#pulls the database of zip codes/state information 
data(zipcode)
View(zipcode) 
#creates a merged list of zip codes and the hogan information. 
hogan_state_list_fromzips <-  left_join(newhogan_contribswithzips, zipcode, by =c("zip_codes" = "zip"))


##########################5###########################
#so let's count it up and see what we got. I'm grouping by city and state, but that can be changed, if needed. 

count_hogan_states <- hogan_state_list_fromzips %>% 
  #the colum we are grouping 
  group_by(state, city)  %>%
  #creates a count with the row name being count. The n value is the number of times it occurs
  summarise(count = n(), total = sum(`Contribution Amount`,  na.rm = TRUE)) %>%
  #arrange the list in descending order
  arrange(desc(total)) 
View(count_hogan_states)
count_hogan_states[1:10,]


###########INKIND ANALYSIS HOGAN ############### 
#I decided to pull out the inkind donations Hogan got, just because it was so many 


#let's start exploring where in the world this money is coming from...We can do that by extracting zip codes. 
#searches for all number combos that are either 5 digits long or 5 digits followed by 4 digits with a dash in between.
hogan_inkind_zips1 <- stri_extract_all_regex(hogan_inkind_rebates_all$`Contributor Address`, "(?<!\\d)(\\d{5}(?:[-\\s]\\d{4})?)\\b")
hogan_inkind_5digitzips <- stri_extract_all_regex(hogan_inkind_zips1, "\\d{5}")
#I was getting some extra 5 digit combos from addresses, so I used this lapply command to just grab the last one, since the zip is always last. 
hogan_inkind_zips2 <- map(hogan_inkind_5digitzips, function(x) x[length(x)])
#creates the dataframe hogan_c as new hogan_c
hogan_inkind_zips_df <- data_frame(hogan_inkind_zips2)
#changes the list to a numeric value. This helps the group by function which do4sn't like lists.
hogan_inkind_zips_df %>% mutate_if(is.list, as.character) -> hogan_inkind_zips_df
#changes the column header in the new dataframe to zip_codes. this will be helpful later on because it is unique. 
colnames(hogan_inkind_zips_df)[which(names(hogan_inkind_zips_df) == "hogan_inkind_zips2")] <- "zip_codes"
#binds it to the hogan contributions database
newhogan_inkind_contribswithzips <- cbind(hogan_inkind_zips_df, hogan_inkind_rebates_all)
#ok, so now that we have our new list with the added zip codes, we can go through and group them and count them. 
count_hogan_inkind_zip_codes <- newhogan_inkind_contribswithzips %>% 
  #the colum we are grouping 
  group_by(zip_codes)  %>%
  #creates a count with the row name being count. The n value is the number of times it occurs
  summarise(count = n(), total = sum(`Contribution Amount`,  na.rm = TRUE)) %>%
  #arrange the list in descending order
  arrange(desc(total)) 
View(count_hogan_inkind_zip_codes)
count_hogan_inkind_zip_codes[1:10,]

##########################4##########################

#now let's group by state and see where this money is coming from by joining this data with a helpful database of zip codes that is part of the zipcode library.....which you should install, or you're not going anywhere pat'na. PS: zipcode also has a cleaning feature, but it didn't work for my purposes. Thus the code above. 

#pulls the database of zip codes/state information 
data(zipcode)
View(zipcode) 
#creates a merged list of zip codes and the hogan information. 
hogan_state_inkind_list_fromzips <-  left_join(newhogan_inkind_contribswithzips, zipcode, by =c("zip_codes" = "zip"))

#just a quickie to see where this money is coming from and how much. 
hogan_inkind_count_and_sum <- hogan_state_inkind_list_fromzips %>%
  group_by(city, `Contributor Name`)  %>%
  #creates a count with the row name being count. The n value is the number of times it occurs
  summarise(count = n(), total = sum(`Contribution Amount`)) %>%
  #arrange the list in descending order
  arrange(desc(total)) 
View(hogan_inkind_count_and_sum)
hogan_inkind_count_and_sum[1:10,]
  

##########################5###########################
#so let's count it up and see what we got. I'm grouping by city and state, but that can be changed, if needed. 

count_hogan_states <- hogan_state_list_fromzips %>% 
  #the colum we are grouping 
  group_by(state, city)  %>%
  #creates a count with the row name being count. The n value is the number of times it occurs
  summarise(count = n(), total = sum(`Contribution Amount`,  na.rm = TRUE)) %>%
  #arrange the list in descending order
  arrange(desc(total)) 
View(count_hogan_states)
count_hogan_states[1:10,]

##################################



#So  looks like Baltimore and Annapolis are top.
#1    MD            Baltimore   664 415797.0
#2    MD            Annapolis   580 256182.5
#3    MD Lutherville Timonium   346 156239.2
#4    DC           Washington    89 133593.0
#5    MD               Towson   249 125061.0

#######################5a##################
#because there's so muhc money let's back up and just sort by states. That way we can see what's what as far as totals go. 
count_hogan_just_states <- hogan_state_list_fromzips %>% 
  #the colum we are grouping 
  group_by(state)  %>%
  #creates a count with the row name being count. The n value is the number of times it occurs
  summarise(count = n(), total = sum(`Contribution Amount`,  na.rm = TRUE)) %>%
  #arrange the list in descending order
  arrange(desc(total)) 
View(count_hogan_just_states)
count_hogan_just_states[1:10,]

#Interesting. Maryland contributed much more,  

#1    MD 12667 4739710.51
#2    DC    89  133593.00
#3    NJ    43  122995.00
#4    VA   100  112775.64
#5    PA    64   91455.00

133593.00/5440505

##########################6###########################


# now let's examine what these people's employer is. This categorty is a bit anemic, because many of the fields are left blank, but why not try, amirite? 
count_hogan_states_jobs <- hogan_state_list_fromzips %>% 
  #the colum we are grouping 
  group_by(city, `Employer Name`)  %>%
  #creates a count with the row name being count. The n value is the number of times it occurs
  summarise(count = n(), total = sum(`Contribution Amount`)) %>%
  #arrange the list in descending order
  arrange(desc(total)) 
View(count_hogan_states_jobs)
count_hogan_states_jobs[1:10,]

#Meh, too many nulls to really be helpful But 157 retirees in Baltimore gave 27956.00 and 189 retirees in Annapolis gave 27828.00


###############7#################

#Let's zoom back out then. Let's just group by employer name. 

hogan_contribsby_employer1 <- hogan_contribs_all %>%
  #groups the values by the employer.name, condensing all duplicates. 
  group_by(`Employer Name`) %>%
  #sums the values from the contribution amount column and places it into the group by we requested above. 
  summarise(total = sum(`Contribution Amount`)) %>%
  #arrange the list in descending order
  arrange(desc(total))
#show me what I just did
View(hogan_contribsby_employer1 )
hogan_contribsby_employer1[1:10,]
#FINDING: aha, now we're getting somewhere.......kind of. The NAs have it, but retirees and self-employed people gave a serious chunk of change. 
#1                          <NA> 3342723.26
#2                       Retired  390287.54
#3                 Self-Employed  141086.35
#4             State Of Maryland   27104.17
#5                 Self Employed   16695.00

###############8#################

hogan_contribsby_occupation <- hogan_contribs_all %>%
  #the colum we are grouping 
  group_by(`Employer Occupation`) %>%
  #creates a count with the row name being count. The n value is the number of times it occurs
  summarise(total = sum(`Contribution Amount`,  na.rm = TRUE)) %>%
  #arrange the list in descending order
  arrange(desc(total))
#show me what I just did
View(hogan_contribsby_occupation)
hogan_contribsby_occupation[1:10,]
#FINDING: again the NA category is overwhelming, but the service industry, financial and retired people had a strong showing. 
#1                   <NA> 3285598.26
#2              Financial  571330.35
#3                Retired  386707.54
#4       Service Industry  204551.75
#5                  Legal  163546.00


###############9################
#Hey big spender, let's look at all the contributions ordered by amount....

hogan_contribs_sunmmed_arranged<- hogan_contribs_all %>%
  group_by(`Contributor Name`) %>%
  #sums the values from the contribution amount column and places it into the group by we requested above. 
  summarise(total = sum(`Contribution Amount`)) %>%
  #arrange the list in descending order
  arrange(desc(total))
#show me what I just did
View(hogan_contribs_sunmmed_arranged)
hogan_contribs_sunmmed_arranged[1:10,]
#AHA, in addition to the Republican Party being a huge contributor, BB&T and Wells Fargo, Kinsley Anne W. 12000.00, Gioioso Wayne R. 9500.00
#1                              Wells Fargo 18152.12
#2                                     BB&T 16769.68
#3                         Kinsley  Anne W. 12000.00
#4                        Gioioso  Wayne R.  9500.00
#5                          Gardyn  Simpson  7500.00
################################10#########################

#OK, finally, let's look at the contribution amounts grouped by contribution type.

hogan_contribution_type  <- hogan_contribs_all %>%
  #the colum we are grouping 
  group_by(`Contributor Type`)  %>%
  #creates a count with the row name being count. The n value is the number of times it occurs
  summarise(count = n(), total = sum(`Contribution Amount`,  na.rm = TRUE)) %>%
  #arrange the list in descending order
  arrange(desc(total)) 
View(hogan_contribution_type)
hogan_contribution_type[1:10,]

#What I'm seeing is that Hogan is getting more than half of his money from individual donors. 
#1                  Individual 11950 3283165.26
#2 Business/Group/Organization   998 1396025.51
#3               Party Central   155  589156.51
#4           Federal Committee    58  131893.00
#5               PAC Committee    45   70892.77
#6         Candidate Committee    31   22920.78
#7              Political Club     2    2085.00


#################11##############

#I'd also like to look at big contributions versus big contributions. I will do that with these two functions. Small contribs, for contrinbutions less than 250 and big contributions more than 2500.

count_hogan_smallcontribs <- hogan_state_list_fromzips %>% 
  #the colum we are grouping 
  filter(`Contribution Amount` < 250 )  %>%
  #creates a count with the row name being count. The n value is the number of times it occurs
  summarise(count = n(), total = sum(`Contribution Amount`)) %>%
  #arrange the list in descending order
  arrange(desc(total)) 
count_hogan_smallcontribs[1:2]

count_hogan_bigcontribs <- hogan_state_list_fromzips %>% 
  #the colum we are grouping 
  filter(`Contribution Amount` > 2500 )  %>%
  #creates a count with the row name being count. The n value is the number of times it occurs
  summarise(count = n(), total = sum(`Contribution Amount`)) %>%
  #arrange the list in descending order
  arrange(desc(total)) 
count_hogan_bigcontribs[1:2]





#############TIME FOR ANALYSIS##############
################# baker! ############


###############1################# 
#OK, so this one was way more complicated than I had hoped, but it searches for numbers that are grouped into 5 digits and sometimes are separated by a - sign or a dash. Then adds it to a list, which I converted to a numeric, grouped, sorte and then summed contributions. 


baker_contribs_all <- cleaned_contributions_new  %>% 
  #here we're creating a dataframe of all entries that have the receiving committee equaling baker. And it excludes in-kind and refund/rebate information
  filter(`Receiving Committee` == "Baker  Rushern III Friends Of-Comm For Pol Change (CPC)") %>%
  filter(`Contribution Type` != "In-Kind") %>%
  filter(`Contribution Type` != "Coordinated In-Kind") %>%
  filter(`Contribution Type` != "Refund/Rebate")
View(baker_contribs_all)
#this also creates the a dataframe of rebartes and inkind donations that I pulled out of the contributions list. Just to see if I see something interesting.
baker_inkind_rebates_all <- cleaned_contributions_new  %>% 
  #here we're creating a dataframe of all entries that have the receiving committee equaling baker. And it excludes in-kind and refund/rebate information
  filter(`Receiving Committee` == "Baker  Rushern III Friends Of-Comm For Pol Change (CPC)") %>%
  filter(`Contribution Type` == "In-Kind" | `Contribution Type` == "Refund/Rebate" | `Contribution Type` == "Coordinated In-Kind")
View(baker_inkind_rebates_all)


##############################2####################

# So as a safeguard, let's look at baker's contributions a
sum(baker_contribs_all$`Contribution Amount`)
#1053796 is what I got....and the campaign reported $1,053,796.28. Not bad.  
sum(baker_inkind_rebates_all$`Contribution Amount`)
#20614.87

#what's weird, is that baker didn't get a lot of large contribs, nor did he get a lot of small contribs. What's the average contribution amount?

mean(baker_contribs_all$`Contribution Amount`)
#########################3#################

#let's start exploring where in the world this money is coming from...We can do that by extracting zip codes. 
#searches for all number combos that are either 5 digits long or 5 digits followed by 4 digits with a dash in between.
baker_zips1 <- stri_extract_all_regex(baker_contribs_all$`Contributor Address`, "(?<!\\d)(\\d{5}(?:[-\\s]\\d{4})?)\\b")
#now we strip out the 5 digit zips, separating them from the 4 digits...that makes the zipcode database work
baker_5digitzips <- stri_extract_all_regex(baker_zips1, "\\d{5}")
#I was getting some extra 5 digit combos from addresses, so I used this lapply command to just grab the last one, since the zip is always last. 
baker_zips2 <- map(baker_5digitzips, function(x) x[length(x)])
#creates the dataframe baker_c as new baker_c
baker_zips_df <- data_frame(baker_zips2)
#changes the list to a numeric value. This helps the group by function which do4sn't like lists.
baker_zips_df %>% mutate_if(is.list, as.character) -> baker_zips_df
#changes the column header in the new dataframe to zip_codes. this will be helpful later on because it is unique. 
colnames(baker_zips_df)[which(names(baker_zips_df) == "baker_zips2")] <- "zip_codes"
#binds it to the baker contributions database
newbaker_contribswithzips <- cbind(baker_zips_df, baker_contribs_all)
#ok, so now that we have our new list with the added zip codes, we can go through and group them and count them. 
count_baker_zip_codes <- newbaker_contribswithzips %>% 
  #the colum we are grouping 
  group_by(zip_codes)  %>%
  #creates a count with the row name being count. The n value is the number of times it occurs
  summarise(count = n(), total = sum(`Contribution Amount`,  na.rm = TRUE)) %>%
  #arrange the list in descending order
  arrange(desc(total)) 
View(count_baker_zip_codes)
count_baker_zip_codes[1:10,]

#So from what I can tell, and I'm going to have to put this into Carto to get a better idea....or I can join with some zip codes. 
# 1     20854    40 64770.00
#2     20721    95 42619.00
# 3     20706    45 30054.00
#4     20001    35 29683.00
#5     20774    79 23811.18

##########################4##########################

#now let's group by state and see where this money is coming from by joining this data with a helpful database of zip codes that is part of the zipcode library.....which you should install, or you're not going anywhere pat'na. PS: zipcode also has a cleaning feature, but it didn't work for my purposes. Thus the code above. 

#pulls the database of zip codes/state information 
data(zipcode)
View(zipcode) 
#creates a merged list of zip codes and the baker information. 
baker_state_list_fromzips <-  left_join(newbaker_contribswithzips, zipcode, by =c("zip_codes" = "zip"))


##########################5###########################
#so let's count it up and see what we got. I'm grouping by city and state, but that can be changed, if needed. 

count_baker_states <- baker_state_list_fromzips %>% 
  #the colum we are grouping 
  group_by(state, city)  %>%
  #creates a count with the row name being count. The n value is the number of times it occurs
  summarise(count = n(), total = sum(`Contribution Amount`,  na.rm = TRUE)) %>%
  #arrange the list in descending order
  arrange(desc(total)) 
View(count_baker_states)
count_baker_states[1:10,]





#So  looks like DC is top of the heap......Bowie and Potomac are a distant second
#1    DC     Washington   237 200322.14
#2    MD          Bowie   196  81142.00
#3    MD        Potomac    40  64770.00
#4    MD Upper Marlboro   149  40971.18
#5    MD    Hyattsville   120  35309.41

 81142.00  +  40971.18 +  35309.41



#######################5a##################
#because there's so muhc money from DC let's back up and just sort by states. That way we can see what's what as far as totals go. 
count_baker_just_states <- baker_state_list_fromzips %>% 
  #the colum we are grouping 
  group_by(state)  %>%
  #creates a count with the row name being count. The n value is the number of times it occurs
  summarise(count = n(), total = sum(`Contribution Amount`,  na.rm = TRUE)) %>%
  #arrange the list in descending order
  arrange(desc(total)) 
View(count_baker_just_states)
count_baker_just_states[1:10,]

#Interesting. Maryland contributed much more, but DC VA and NY all did give some money 
#1    MD  1274 672038.13
#2    DC   238 200572.14
#3    VA    78  55999.00
#4    NY    20  27674.00
#5    FL    11  16925.00

200572.14/1053796 
#Unlike Hogsan, whose second biggest chunk of change was about 2 percent. This is more like 20 percent from DC
##########################6###########################


# now let's examine what these people's employer is. This categorty is a bit anemic, because many of the fields are left blank, but why not try, amirite? 
count_baker_states_jobs <- baker_state_list_fromzips %>% 
  #the colum we are grouping 
  group_by(city, `Employer Name`)  %>%
  #creates a count with the row name being count. The n value is the number of times it occurs
  summarise(count = n(), total = sum(`Contribution Amount`)) %>%
  #arrange the list in descending order
  arrange(desc(total)) 
View(count_baker_states_jobs)
count_baker_states_jobs[1:10]

#Meh, too many nulls to really be helpful, but got a few to look into. 
#4       Melville Napoli Shkolnik  PLLC     3 18000.00
#9        Potomac     Total Wine & More     3 14000.00
#10      Rockville    Berman Enterprises     7 14000.00


###############7#################

#Let's zoom back out then. Let's just group by employer name. 

baker_contribsby_employer1 <- baker_contribs_all %>%
  #groups the values by the employer.name, condensing all duplicates. 
  group_by(`Employer Name`) %>%
  #sums the values from the contribution amount column and places it into the group by we requested above. 
  summarise(total = sum(`Contribution Amount`)) %>%
  #arrange the list in descending order
  arrange(desc(total))
#show me what I just did
View(baker_contribsby_employer1 )
baker_contribsby_employer1[1:10,]
#FINDING: Hmm unfortunately the NAs are a little too overwhelming to be totally useful, but it's interesting that Prince George's County employees gave 23k. 
#1                   <NA> 398513.69
#2 Prince George's County  23490.18
#3  Napoli Shkolnik  PLLC  18000.00
#4                Retired  16250.00
#5      Total Wine & More  15500.00
#6          Self-Employed  15142.41
###############8#################

baker_contribsby_occupation <- baker_contribs_all %>%
  #the colum we are grouping 
  group_by(`Employer Occupation`) %>%
  #creates a count with the row name being count. The n value is the number of times it occurs
  summarise(total = sum(`Contribution Amount`,  na.rm = TRUE)) %>%
  #arrange the list in descending order
  arrange(desc(total))
#show me what I just did
View(baker_contribsby_occupation)
baker_contribsby_occupation[1:10,]
#FINDING:Unfortunately OTHER is the big winner and NA is close behind. Not much use here.  
#1                         Other 662387.6
#2                          <NA> 382443.7
#3                     Education   2070.0
#4                         Legal   1735.0
#5                    Government   1390.0
#6                    Healthcare   1250.0


###############9################
#Hey big spender, let's look at all the contributions ordered by amount....

baker_contribs_sunmmed_arranged<- baker_contribs_all %>%
  group_by(`Contributor Name`) %>%
  #sums the values from the contribution amount column and places it into the group by we requested above. 
  summarise(total = sum(`Contribution Amount`)) %>%
  #arrange the list in descending order
  arrange(desc(total))
#show me what I just did
View(baker_contribs_sunmmed_arranged)
baker_contribs_sunmmed_arranged[1:10,]
#some people I don't know....probably should do some googling on these. 
#1                      Meltzer  Alan L. 12000
#2 Law Office Of Isaac H. Marks  Sr. LLC  7500
#3                         Albert  Derek  6000
#4                    BE Investments LLC  6000
#5                        Berman  Dennis  6000

################################10#########################

#OK, finally, let's look at the contribution amounts grouped by contribution type.

baker_contribution_type  <- baker_contribs_all %>%
  #the colum we are grouping 
  group_by(`Contributor Type`)  %>%
  #creates a count with the row name being count. The n value is the number of times it occurs
  summarise(count = n(), total = sum(`Contribution Amount`,  na.rm = TRUE)) %>%
  #arrange the list in descending order
  arrange(desc(total)) 
View(baker_contribution_type)
baker_contribution_type[1:10,]

#What I'm seeing is that baker is getting a lot of money from business, less from indificuals than Hogan.
#1                                      Individual  1537 752748.1
#2                     Business/Group/Organization   204 273548.1
#3 Unregistered Out-of-State Non-Federal Committee     3   9000.0
#4                             Candidate Committee     2   7000.0
#5                                   PAC Committee     5   6500.0
#6                               Federal Committee     2   4000.0


baker_pg_County  <- baker_contribs_all %>%
#the colum we are grouping 
filter(`Employer Name` == "Prince George's County") 
View(baker_pg_County)

#################11##############

#I'd also like to look at big contributions versus big contributions. I will do that with these two functions. Small contribs, for contrinbutions less than 250 and big contributions more than 2500.

count_baker_smallcontribs <- baker_state_list_fromzips %>% 
  #the colum we are grouping 
  filter(`Contribution Amount` < 250 )  %>%
  #creates a count with the row name being count. The n value is the number of times it occurs
  summarise(count = n(), total = sum(`Contribution Amount`)) %>%
  #arrange the list in descending order
  arrange(desc(total)) 
count_baker_smallcontribs[1:2]

count_baker_bigcontribs <- baker_state_list_fromzips %>% 
  #the colum we are grouping 
  filter(`Contribution Amount` > 2500 )  %>%
  #creates a count with the row name being count. The n value is the number of times it occurs
  summarise(count = n(), total = sum(`Contribution Amount`)) %>%
  #arrange the list in descending order
  arrange(desc(total)) 
count_baker_bigcontribs[1:2]



################# jealous! ############


###############1################# 
#OK, so this one was way more complicated than I had hoped, but it searches for numbers that are grouped into 5 digits and sometimes are separated by a - sign or a dash. Then adds it to a list, which I converted to a numeric, grouped, sorte and then summed contributions. 


jealous_contribs_all <- cleaned_contributions_new  %>% 
  #here we're creating a dataframe of all entries that have the receiving committee equaling jealous. And it excludes in-kind and refund/rebate information
  filter(`Receiving Committee` == "Jealous  Ben Friends of") %>%
  filter(`Contribution Type` != "In-Kind") %>%
  filter(`Contribution Type` != "Coordinated In-Kind") %>%
  filter(`Contribution Type` != "Refund/Rebate")
View(jealous_contribs_all)
#this also creates the a dataframe of rebartes and inkind donations that I pulled out of the contributions list. Just to see if I see something interesting.
jealous_inkind_rebates_all <- cleaned_contributions_new  %>% 
  #here we're creating a dataframe of all entries that have the receiving committee equaling jealous. And it excludes in-kind and refund/rebate information
  filter(`Receiving Committee` == "Jealous  Ben Friends of") %>%
  filter(`Contribution Type` == "In-Kind" | `Contribution Type` == "Refund/Rebate" | `Contribution Type` == "Coordinated In-Kind")
View(jealous_inkind_rebates_all)


##############################2####################

# So as a safeguard, let's look at jealous's contributions and see if they match up with the Baltimore Sun's numbers.....not for antything but they seem like a competent grtoup of journalists. 
sum(jealous_contribs_all$`Contribution Amount`)
#1249316 is what I got....and the Sun listed the total as 1,500,000. but the original disclosure says 1,250,229.46 And I summed both the in kind and conrribution amount and got 1265546
sum(jealous_inkind_rebates_all$`Contribution Amount`)
#16229.5
#########################3#################

#let's start exploring where in the world this money is coming from...We can do that by extracting zip codes. 
#searches for all number combos that are either 5 digits long or 5 digits followed by 4 digits with a dash in between.
jealous_zips1 <- stri_extract_all_regex(jealous_contribs_all$`Contributor Address`, "(?<!\\d)(\\d{5}(?:[-\\s]\\d{4})?)\\b")
#let's break out the 5 digiters. Those 4 digits complicate things. 
jealous_5digitzips <- stri_extract_all_regex(jealous_zips1, "\\d{5}")
#I was getting some extra 5 digit combos from addresses, so I used this lapply command to just grab the last one, since the zip is always last. 
jealous_zips2 <- map(jealous_5digitzips, function(x) x[length(x)])
#creates the dataframe jealous_c as new jealous_c
jealous_zips_df <- data_frame(jealous_zips2)
#changes the list to a numeric value. This helps the group by function which do4sn't like lists.
jealous_zips_df %>% mutate_if(is.list, as.character) -> jealous_zips_df
#changes the column header in the new dataframe to zip_codes. this will be helpful later on because it is unique. 
colnames(jealous_zips_df)[which(names(jealous_zips_df) == "jealous_zips2")] <- "zip_codes"
#binds it to the jealous contributions database
newjealous_contribswithzips <- cbind(jealous_zips_df, jealous_contribs_all)
#ok, so now that we have our new list with the added zip codes, we can go through and group them and count them. 
count_jealous_zip_codes <- newjealous_contribswithzips %>% 
  #the colum we are grouping 
  group_by(zip_codes)  %>%
  #creates a count with the row name being count. The n value is the number of times it occurs
  summarise(count = n(), total = sum(`Contribution Amount`,  na.rm = TRUE)) %>%
  #arrange the list in descending order
  arrange(desc(total)) 
View(count_jealous_zip_codes)
count_jealous_zip_codes[1:10,]

# I'm going to have to put this into Carto to get a better idea...
##########################4##########################

#now let's group by state and see where this money is coming from by joining this data with a helpful database of zip codes that is part of the zipcode library.....which you should install, or you're not going anywhere pat'na. PS: zipcode also has a cleaning feature, but it didn't work for my purposes. Thus the code above. 

#pulls the database of zip codes/state information 
data(zipcode)
View(zipcode) 
#creates a merged list of zip codes and the jealous information. 
jealous_state_list_fromzips <-  left_join(newjealous_contribswithzips, zipcode, by =c("zip_codes" = "zip"))


##########################5###########################
#so let's count it up and see what we got. I'm grouping by city and state, but that can be changed, if needed. 

count_jealous_states <- jealous_state_list_fromzips %>% 
  #the colum we are grouping 
  group_by(state, city)  %>%
  #creates a count with the row name being count. The n value is the number of times it occurs
  summarise(count = n(), total = sum(`Contribution Amount`,  na.rm = TRUE)) %>%
  #arrange the list in descending order
  arrange(desc(total)) 
View(count_jealous_states)
count_jealous_states[1:10,]


#So  looks like MR Jealous is getting a lot of his contriubutions from San Fran
#1    CA San Francisco   220 175394.83
#2    NY      New York   313 124811.72
#3    CA       Oakland   144  71518.50
#4    CA   Los Angeles   118  60241.68
#5    DC    Washington   230  53739.63

#######################5a##################
#because there's so muhc money from these other places. I'm sorting bty state as well. 
count_jealous_just_states <- jealous_state_list_fromzips %>% 
  #the colum we are grouping 
  group_by(state)  %>%
  #creates a count with the row name being count. The n value is the number of times it occurs
  summarise(count = n(), total = sum(`Contribution Amount`,  na.rm = TRUE)) %>%
  #arrange the list in descending order
  arrange(desc(total)) 
View(count_jealous_just_states)
count_jealous_just_states[1:10,]

#Interesting. California donors contributed much more, than Maryland and New York.
#1    CA  2207 607945.73
#2    MD  1800 177496.27
#3    NY   871 162049.03
#4    DC   230  53739.63
#5    WA   376  25190.59

607945.73/1265546   #0.4803822
#Using the number that Jealous' campaign diosclosed, about half of his campaign donations are coming from California. 
##########################6###########################


# now let's examine what these people's employer is. This categorty is a bit anemic, because many of the fields are left blank, but why not try, amirite? 
count_jealous_states_jobs <- jealous_state_list_fromzips %>% 
  #the colum we are grouping 
  group_by(city, `Employer Name`)  %>%
  #creates a count with the row name being count. The n value is the number of times it occurs
  summarise(count = n(), total = sum(`Contribution Amount`)) %>%
  #arrange the list in descending order
  arrange(desc(total)) 
View(count_jealous_states_jobs)
count_jealous_states_jobs[1:4]

#Less nulls, bnut a lot of unemployed people in San Fran are giving money. Let's look into this more. 
#1 San Francisco             Not Employed    18 48500.00
#2 San Francisco            Self-Employed    11 27500.00
#3      New York                     <NA>   264 22460.72
#4    Santa Cruz Mills Family Enterprises     4 20000.00
#5       Oakland             Not Employed     8 16000.00
#6      New York             Not Employed    11 14625.00

###############7#################

#Let's zoom back out then. Let's just group by employer name. 

jealous_contribsby_employer1 <- jealous_contribs_all %>%
  #groups the values by the employer.name, condensing all duplicates. 
  group_by(`Employer Name`) %>%
  #sums the values from the contribution amount column and places it into the group by we requested above. 
  summarise(total = sum(`Contribution Amount`)) %>%
  #arrange the list in descending order
  arrange(desc(total))
#show me what I just did
View(jealous_contribsby_employer1 )
jealous_contribsby_employer1[1:10,]
#FINDING: Hmm unfortunately the NAs are a little too overwhelming to be totally useful, but this not employed categorty did give a lot of money. 
#1                            <NA> 268427
#2                    Not Employed 201972
#3                   Self-Employed 104100
#4        Mills Family Enterprises  20000
#5 United Security Financial Corp.  18000
###############8#################

jealous_contribsby_occupation <- jealous_contribs_all %>%
  #the colum we are grouping 
  group_by(`Employer Occupation`) %>%
  #creates a count with the row name being count. The n value is the number of times it occurs
  summarise(total = sum(`Contribution Amount`,  na.rm = TRUE)) %>%
  #arrange the list in descending order
  arrange(desc(total))
#show me what I just did
View(jealous_contribsby_occupation)
jealous_contribsby_occupation[1:10,]
#FINDING:Unfortunately NA is the big winner but financial is a big part of the pie.  
#1                          <NA> 268427
#2                     Financial 171305
#3                    Unemployed 160222
#4                         Legal  82200
#5        Arts and Entertainment  81750

#using the number he submitted on his disclosure, those marked as financial gave 13% of total moneys. 

171305/1265546

###############9################
#Hey big spender, let's look at all the contributions ordered by amount....

jealous_contribs_sunmmed_arranged<- jealous_contribs_all %>%
  group_by(`Contributor Name`) %>%
  #sums the values from the contribution amount column and places it into the group by we requested above. 
  summarise(total = sum(`Contribution Amount`)) %>%
  #arrange the list in descending order
  arrange(desc(total))
#show me what I just did
View(jealous_contribs_sunmmed_arranged)
jealous_contribs_sunmmed_arranged[1:10,]
#some people I don't know....probably should do some googling on these. 
#1                    Sandler  Gretchen 18000
#2                Devereaux-Mills  Anne 15000
#3               Lamkins  Phaedra Ellis  9000
#4                    Stephenson  Keith  6015
#5 1199 SEIU- NYS Political Action Fund  6000
################################10#########################

#OK, finally, let's look at the contribution amounts grouped by contribution type.

jealous_contribution_type  <- jealous_contribs_all %>%
  #the colum we are grouping 
  group_by(`Contributor Type`)  %>%
  #creates a count with the row name being count. The n value is the number of times it occurs
  summarise(count = n(), total = sum(`Contribution Amount`,  na.rm = TRUE)) %>%
  #arrange the list in descending order
  arrange(desc(total)) 
View(jealous_contribution_type)
jealous_contribution_type[1:10,]

#What I'm seeing is that jealous is getting a lot of money from business, less from indificuals than Hogan.
#1                                      Individual  9677 1214417
#2                                     Labor Union     3   10000
#3   Registered Out-of-State Non-Federal Committee     2   10000
#4                               Federal Committee     1    6000
#5 Unregistered Out-of-State Non-Federal Committee     1    6000
  
 #wow almost all came from individual donations.  

#################11##############

count_jealous_smallcontribs <- jealous_state_list_fromzips %>% 
  #the colum we are grouping 
  filter(`Contribution Amount` < 250 )  %>%
  #creates a count with the row name being count. The n value is the number of times it occurs
  summarise(count = n(), total = sum(`Contribution Amount`)) %>%
  #arrange the list in descending order
  arrange(desc(total)) 
View(count_jealous_smallcontribs)
count_jealous_smallcontribs[1:2]

count_jealous_bigcontribs <- jealous_state_list_fromzips %>% 
  #the colum we are grouping 
  filter(`Contribution Amount` > 2500 )  %>%
  #creates a count with the row name being count. The n value is the number of times it occurs
  summarise(count = n(), total = sum(`Contribution Amount`)) %>%
  #arrange the list in descending order
  arrange(desc(total)) 
View(count_jealous_bigcontribs)
count_jealous_bigcontribs[1:2]


  
  
################# shea! ############


###############1################# 
#OK, so this one was way more complicated than I had hoped, but it searches for numbers that are grouped into 5 digits and sometimes are separated by a - sign or a dash. Then adds it to a list, which I converted to a numeric, grouped, sorte and then summed contributions. 


shea_contribs_all <- cleaned_contributions_new  %>% 
  #here we're creating a dataframe of all entries that have the receiving committee equaling shea. And it excludes in-kind and refund/rebate information
  filter(`Receiving Committee` == "Jim Shea For Maryland") %>%
  filter(`Contribution Type` != "In-Kind") %>%
  filter(`Contribution Type` != "Coordinated In-Kind") %>%
  filter(`Contribution Type` != "Refund/Rebate")
View(shea_contribs_all)
#this also creates the a dataframe of rebartes and inkind donations that I pulled out of the contributions list. Just to see if I see something interesting.
shea_inkind_rebates_all <- cleaned_contributions_new  %>% 
  #here we're creating a dataframe of all entries that have the receiving committee equaling shea. And it excludes in-kind and refund/rebate information
  filter(`Receiving Committee` == "Jim Shea For Maryland") %>%
  filter(`Contribution Type` == "In-Kind" | `Contribution Type` == "Refund/Rebate" | `Contribution Type` == "Coordinated In-Kind")
View(shea_inkind_rebates_all)


##############################2####################

# So as a safeguard, let's look at shea's contributions and see if they match up with the Baltimore Sun's numbers.....not for antything but they seem like a competent grtoup of journalists. 
sum(shea_contribs_all$`Contribution Amount`)
#2017775 is what I got....and the disclosure states $2,016,524.73  So I subtracted the ( Federal Committees $1,250.00) and got 2016525. 
sum(shea_inkind_rebates_all$`Contribution Amount`)
#54814.21
#########################3#################

#let's start exploring where in the world this money is coming from...We can do that by extracting zip codes. 
#searches for all number combos that are either 5 digits long or 5 digits followed by 4 digits with a dash in between.
shea_zips1 <- stri_extract_all_regex(shea_contribs_all$`Contributor Address`, "(?<!\\d)(\\d{5}(?:[-\\s]\\d{4})?)\\b")
#break out the 5 digit strings from everyrthing else. REGEXPRESSIONS 
shea_5digitzips <- stri_extract_all_regex(shea_zips1, "\\d{5}")
#I was getting some extra 5 digit combos from addresses, so I used this lapply command to just grab the last one, since the zip is always last. 
shea_zips2 <- map(shea_5digitzips, function(x) x[length(x)])
#creates the dataframe shea_c as new shea_c
shea_zips_df <- data_frame(shea_zips2)
#changes the list to a numeric value. This helps the group by function which do4sn't like lists.
shea_zips_df %>% mutate_if(is.list, as.character) -> shea_zips_df
#changes the column header in the new dataframe to zip_codes. this will be helpful later on because it is unique. 
colnames(shea_zips_df)[which(names(shea_zips_df) == "shea_zips2")] <- "zip_codes"
#binds it to the shea contributions database
newshea_contribswithzips <- cbind(shea_zips_df, shea_contribs_all)
#ok, so now that we have our new list with the added zip codes, we can go through and group them and count them. 
count_shea_zip_codes <- newshea_contribswithzips %>% 
  #the colum we are grouping 
  group_by(zip_codes)  %>%
  #creates a count with the row name being count. The n value is the number of times it occurs
  summarise(count = n(), total = sum(`Contribution Amount`,  na.rm = TRUE)) %>%
  #arrange the list in descending order
  arrange(desc(total)) 
View(count_shea_zip_codes)
count_shea_zip_codes[1:10,]

#So from what I can tell, and I'm going to have to put this into Carto 

##########################4##########################

#now let's group by state and see where this money is coming from by joining this data with a helpful database of zip codes that is part of the zipcode library.....which you should install, or you're not going anywhere pat'na. PS: zipcode also has a cleaning feature, but it didn't work for my purposes. Thus the code above. 

#pulls the database of zip codes/state information 
data(zipcode)
View(zipcode) 
#creates a merged list of zip codes and the shea information. 
shea_state_list_fromzips <-  left_join(newshea_contribswithzips, zipcode, by =c("zip_codes" = "zip"))


##########################5###########################
#so let's count it up and see what we got. I'm grouping by city and state, but that can be changed, if needed. 

count_shea_states <- shea_state_list_fromzips %>% 
  #the colum we are grouping 
  group_by(state, city)  %>%
  #creates a count with the row name being count. The n value is the number of times it occurs
  summarise(count = n(), total = sum(`Contribution Amount`,  na.rm = TRUE)) %>%
  #arrange the list in descending order
  arrange(desc(total)) 
View(count_shea_states)
count_shea_states[1:10,]


#So  looks like Maryland is top of the heap.....
#1    MD         Owings Mills    40 532100.00
#2    MD            Baltimore   372 320456.00
#3    DC           Washington    80 117850.00
#4    MD Lutherville Timonium    51  58480.18
#5    MD               Towson    83  57461.80

#######################5a##################
#Let's zoom out and just look at states.
count_shea_just_states <- shea_state_list_fromzips %>% 
  #the colum we are grouping 
  group_by(state)  %>%
  #creates a count with the row name being count. The n value is the number of times it occurs
  summarise(count = n(), total = sum(`Contribution Amount`,  na.rm = TRUE)) %>%
  #arrange the list in descending order
  arrange(desc(total)) 
View(count_shea_just_states)
count_shea_just_states[1:10,]

#Interesting. Maryland contributed much more, but DC VA and NY all did give some money 
#1    MD   941 1352576
#2    VA   103  132030
#3    DC    80  117850
#4    CA    59   97393
#5    NY    59   90700
1352576/ 2016525
132030/ 2016525
#6% came from VA
#Unlike Hogsan, whose second biggest chunk of change was about 2 percent. 
##########################6###########################


# now let's examine what these people's employer is. This categorty is a bit anemic, because many of the fields are left blank, but why not try, amirite? 
count_shea_states_jobs <- shea_state_list_fromzips %>% 
  #the colum we are grouping 
  group_by(city, `Employer Name`)  %>%
  #creates a count with the row name being count. The n value is the number of times it occurs
  summarise(count = n(), total = sum(`Contribution Amount`)) %>%
  #arrange the list in descending order
  arrange(desc(total)) 
View(count_shea_states_jobs)
count_shea_states_jobs[1:3]

#Lots of this Venable firm popping up all over the place. LEt's sort it out in more depth. 

count_shea_smallcontribs <- shea_state_list_fromzips %>% 
  #the colum we are grouping 
  filter(`Contribution Amount` < 250 )  %>%
  #creates a count with the row name being count. The n value is the number of times it occurs
  summarise(count = n(), total = sum(`Contribution Amount`)) %>%
  #arrange the list in descending order
  arrange(desc(total)) 
View(count_shea_smallcontribs)
count_shea_smallcontribs[1:2]

count_shea_bigcontribs <- shea_state_list_fromzips %>% 
  #the colum we are grouping 
  filter(`Contribution Amount` > 2500 )  %>%
  #creates a count with the row name being count. The n value is the number of times it occurs
  summarise(count = n(), total = sum(`Contribution Amount`)) %>%
  #arrange the list in descending order
  arrange(desc(total)) 
count_shea_bigcontribs[1:2]

###############7#################

#Let's zoom back out then. Let's just group by employer name. 

shea_contribsby_employer1 <- shea_contribs_all %>%
  #groups the values by the employer.name, condensing all duplicates. 
  group_by(`Employer Name`) %>%
  #sums the values from the contribution amount column and places it into the group by we requested above. 
  summarise(total = sum(`Contribution Amount`)) %>%
  #arrange the list in descending order
  arrange(desc(total))
#show me what I just did
View(shea_contribsby_employer1 )
shea_contribsby_employer1[1:10,]
#FINDING: Hmm this venable law firm sure gave a lot of money.....
#1                              <NA> 672559.2
#2                       Venable LLP 314568.0
#3                           Venable 269400.0
#4                           Retired 105850.0
#5                      Not Employed  76450.0

#so 583968 csame from venableemployees. thats 28% of total
###############8#################

shea_contribsby_occupation <- shea_contribs_all %>%
  #the colum we are grouping 
  group_by(`Employer Occupation`) %>%
  #creates a count with the row name being count. The n value is the number of times it occurs
  summarise(total = sum(`Contribution Amount`,  na.rm = TRUE)) %>%
  #arrange the list in descending order
  arrange(desc(total))
#show me what I just did
View(shea_contribsby_occupation)
shea_contribsby_occupation[1:10,]
#FINDING:LEGAL. Wow big category. 
#1                         Legal 792618.0
#2                          <NA> 655559.2
#3                    Unemployed 119449.0
#4                       Retired 115350.0
#5                     Financial  68600.0


###############9################
#Hey big spender, let's look at all the contributions ordered by amount....

shea_contribs_sunmmed_arranged<- shea_contribs_all %>%
  group_by(`Contributor Name`) %>%
  #sums the values from the contribution amount column and places it into the group by we requested above. 
  summarise(total = sum(`Contribution Amount`)) %>%
  #arrange the list in descending order
  arrange(desc(total))
#show me what I just did
View(shea_contribs_sunmmed_arranged)
shea_contribs_sunmmed_arranged[1:10,]
#some people I don't know....Hmm A Jim Shea kicked in 500k. That's interesting....let's see if that's a candidte donation.
#1         Shea  James  L. 5e+05 
#2    Augustine  Norman R. 6e+03
#3      Baldridge  Douglas 6e+03
#4       Bell  Catharine F 6e+03
#5           Bell  Christy 6e+03



################################10#########################

#OK, finally, let's look at the contribution amounts grouped by contribution type.

shea_contribution_type  <- shea_contribs_all %>%
  #the colum we are grouping 
  group_by(`Contributor Type`)  %>%
  #creates a count with the row name being count. The n value is the number of times it occurs
  summarise(count = n(), total = sum(`Contribution Amount`,  na.rm = TRUE)) %>%
  #arrange the list in descending order
  arrange(desc(total)) 
View(shea_contribution_type)
shea_contribution_type[1:10,]

#What I'm seeing is that shea did indeed plow 500,000 into his own campaign. But looking at the entries catalogging that data, he put down no emplyer name or occupation. So it dshouldn't mess with any of my other calculations. 
#1                  Individual  1458 1481136.39
#2            Self (Candidate)     4  500000.00
#3 Business/Group/Organization    31   35388.77
#4           Federal Committee     2    1250.00


#################11##############

count_shea_smallcontribs <- shea_state_list_fromzips %>% 
  #the colum we are grouping 
  filter(`Contribution Amount` < 250 )  %>%
  #creates a count with the row name being count. The n value is the number of times it occurs
  summarise(count = n(), total = sum(`Contribution Amount`)) %>%
  #arrange the list in descending order
  arrange(desc(total)) 
count_shea_smallcontribs[1:2]

count_shea_bigcontribs <- shea_state_list_fromzips %>% 
  #the colum we are grouping 
  filter(`Contribution Amount` > 2500 )  %>%
  #creates a count with the row name being count. The n value is the number of times it occurs
  summarise(count = n(), total = sum(`Contribution Amount`)) %>%
  #arrange the list in descending order
  arrange(desc(total)) 
count_shea_bigcontribs[1:2]



################# kamenetz! ############


###############1################# 
#OK, so this one was way more complicated than I had hoped, but it searches for numbers that are grouped into 5 digits and sometimes are separated by a - sign or a dash. Then adds it to a list, which I converted to a numeric, grouped, sorte and then summed contributions. 


kamenetz_contribs_all <- cleaned_contributions_new  %>% 
  #here we're creating a dataframe of all entries that have the receiving committee equaling kamenetz. And it excludes in-kind and refund/rebate information
  filter(`Receiving Committee` == "Kamenetz  (Kevin) Committee For") %>%
  filter(`Contribution Type` != "In-Kind") %>%
  filter(`Contribution Type` != "Coordinated In-Kind") %>%
  filter(`Contribution Type` != "Refund/Rebate")
View(kamenetz_contribs_all)
#this also creates the a dataframe of rebartes and inkind donations that I pulled out of the contributions list. Just to see if I see something interesting.
kamenetz_inkind_rebates_all <- cleaned_contributions_new  %>% 
  #here we're creating a dataframe of all entries that have the receiving committee equaling kamenetz. And it excludes in-kind and refund/rebate information
  filter(`Receiving Committee` == "Kamenetz  (Kevin) Committee For") %>%
  filter(`Contribution Type` == "In-Kind" | `Contribution Type` == "Refund/Rebate" |  `Contribution Type` == "Coordinated In-Kind")
View(kamenetz_inkind_rebates_all)


##############################2####################

# So as a safeguard, let's look at kamenetz's contributions and see if they match up with the Baltimore Sun's numbers and the campaign's disclosure.
sum(kamenetz_contribs_all$`Contribution Amount`)
#1041282 is what I got....The campaign said it collected 1066700.13 and got  10752.45  in in-kind for a totl of  $1,055,948
sum(kamenetz_inkind_rebates_all$`Contribution Amount`)
# 36170.34. $25k came from a rebate to Iowa company GPS Impact
#########################3#################

#let's start exploring where in the world this money is coming from...We can do that by extracting zip codes. 
#searches for all number combos that are either 5 digits long or 5 digits followed by 4 digits with a dash in between.
kamenetz_zips1 <- stri_extract_all_regex(kamenetz_contribs_all$`Contributor Address`, "(?<!\\d)(\\d{5}(?:[-\\s]\\d{4})?)\\b")
#break out the 5 digit strings from everyrthing else. REGEXPRESSIONS 
kamenetz_5digitzips <- stri_extract_all_regex(kamenetz_zips1, "\\d{5}")
#I was getting some extra 5 digit combos from addresses, so I used this lapply command to just grab the last one, since the zip is always last. 
kamenetz_zips2 <- map(kamenetz_5digitzips, function(x) x[length(x)])
#creates the dataframe kamenetz_c as new kamenetz_c
kamenetz_zips_df <- data_frame(kamenetz_zips2)
#changes the list to a numeric value. This helps the group by function which do4sn't like lists.
kamenetz_zips_df %>% mutate_if(is.list, as.character) -> kamenetz_zips_df
#changes the column header in the new dataframe to zip_codes. this will be helpful later on because it is unique. 
colnames(kamenetz_zips_df)[which(names(kamenetz_zips_df) == "kamenetz_zips2")] <- "zip_codes"
#binds it to the kamenetz contributions database
newkamenetz_contribswithzips <- cbind(kamenetz_zips_df, kamenetz_contribs_all)
#ok, so now that we have our new list with the added zip codes, we can go through and group them and count them. 
count_kamenetz_zip_codes <- newkamenetz_contribswithzips %>% 
  #the colum we are grouping 
  group_by(zip_codes)  %>%
  #creates a count with the row name being count. The n value is the number of times it occurs
  summarise(count = n(), total = sum(`Contribution Amount`,  na.rm = TRUE)) %>%
  #arrange the list in descending order
  arrange(desc(total)) 
View(count_kamenetz_zip_codes)
count_kamenetz_zip_codes[1:10,]

##########################4##########################

#now let's group by state and see where this money is coming from by joining this data with a helpful database of zip codes that is part of the zipcode library.....which you should install, or you're not going anywhere pat'na. PS: zipcode also has a cleaning feature, but it didn't work for my purposes. Thus the code above. 

#pulls the database of zip codes/state information 
data(zipcode)
View(zipcode) 
#creates a merged list of zip codes and the kamenetz information. 
kamenetz_state_list_fromzips <-  left_join(newkamenetz_contribswithzips, zipcode, by =c("zip_codes" = "zip"))


##########################5###########################
#so let's count it up and see what we got. I'm grouping by city and state, but that can be changed, if needed. 

count_kamenetz_states <- kamenetz_state_list_fromzips %>% 
  #the colum we are grouping 
  group_by(state, city)  %>%
  #creates a count with the row name being count. The n value is the number of times it occurs
  summarise(count = n(), total = sum(`Contribution Amount`,  na.rm = TRUE)) %>%
  #arrange the list in descending order
  arrange(desc(total)) 
View(count_kamenetz_states)
count_kamenetz_states[1:10,]


#Baltimoore was responsible for a lot of money. Same with the others. All top 5 are Maryland. 
#1    MD            Baltimore   212 229068.00
#2    MD Lutherville Timonium    68 111853.00
#3    MD               Towson    73  83805.00
#4    MD           Pikesville    38  82025.00
#5    MD         Owings Mills    36  64705.00

#######################5a##################
#because there's so muhc money from DC let's back up and just sort by states. That way we can see what's what as far as totals go. 
count_kamenetz_just_states <- kamenetz_state_list_fromzips %>% 
  #the colum we are grouping 
  group_by(state)  %>%
  #creates a count with the row name being count. The n value is the number of times it occurs
  summarise(count = n(), total = sum(`Contribution Amount`,  na.rm = TRUE)) %>%
  #arrange the list in descending order
  arrange(desc(total)) 
View(count_kamenetz_just_states)
count_kamenetz_just_states[1:10,]

#Interesting. Maryland contributers paid much more, but DC CT and IL all did give some money 
#1    MD   862 974200.2
#2    VA    10  14550.0
#3    CT     5  11000.0
#4    IL     5   8000.0
#5    PA     8   7750.0

14550.0/ 1041282
#Like Hogsan, Kamenetz' second biggest chunk of change was about 1 percent.
##########################6###########################


# now let's examine what these people's employer is. This categorty is a bit anemic, because many of the fields are left blank, but why not try, amirite? 
count_kamenetz_states_jobs <- kamenetz_state_list_fromzips %>% 
  #the colum we are grouping 
  group_by(city, `Employer Name`)  %>%
  #creates a count with the row name being count. The n value is the number of times it occurs
  summarise(count = n(), total = sum(`Contribution Amount`)) %>%
  #arrange the list in descending order
  arrange(desc(total)) 
View(count_kamenetz_states_jobs)
count_kamenetz_states_jobs[1:4]

#all nulls. So much for that. 


###############7#################

#Let's zoom back out then. Let's just group by employer name. 

kamenetz_contribsby_employer1 <- kamenetz_contribs_all %>%
  #groups the values by the employer.name, condensing all duplicates. 
  group_by(`Employer Name`) %>%
  #sums the values from the contribution amount column and places it into the group by we requested above. 
  summarise(total = sum(`Contribution Amount`)) %>%
  #arrange the list in descending order
  arrange(desc(total))
#show me what I just did
View(kamenetz_contribsby_employer1 )
kamenetz_contribsby_employer1[1:10,]
#FINDING: Hmm unfortunately the NAs are a little too overwhelming to be totally useful, but it's interesting that self-employed people and two companies gave significant chunks of money. 
#1                             <NA> 482699.8
#2                              N/A  69535.0
#3                    Self-Employed  23100.0
#4 David S. Brown Enterprises  Ltd.  12000.0
#5         The Southernland Company  12000.0
#6                   Garson Law LLC  10000.0
###############8#################

kamenetz_contribsby_occupation <- kamenetz_contribs_all %>%
  #the colum we are grouping 
  group_by(`Employer Occupation`) %>%
  #creates a count with the row name being count. The n value is the number of times it occurs
  summarise(total = sum(`Contribution Amount`,  na.rm = TRUE)) %>%
  #arrange the list in descending order
  arrange(desc(total))
#show me what I just did
View(kamenetz_contribsby_occupation)
kamenetz_contribsby_occupation[1:10,]
#FINDING:The Real Estate and Legal categories were sizable. But of course NAs were huge too.  
#1                          <NA> 482684.8
#2                   Real Estate 119000.0
#3                         Legal 109675.0
#4 Administrative and Management 106472.5
#5                       Retired  56040.0
#6         Construction Services  36750.0


###############9################
#Hey big spender, let's look at all the contributions ordered by amount....

kamenetz_contribs_sunmmed_arranged<- kamenetz_contribs_all %>%
  group_by(`Contributor Name`) %>%
  #sums the values from the contribution amount column and places it into the group by we requested above. 
  summarise(total = sum(`Contribution Amount`)) %>%
  #arrange the list in descending order
  arrange(desc(total))
#show me what I just did
View(kamenetz_contribs_sunmmed_arranged)
kamenetz_contribs_sunmmed_arranged[1:10,]
#some people I don't know....probably should do some googling on these. 
#1           Academy Crossing LP  6000
#2             Altfeld  David E.  6000
#3            Angelos  Georgia K  6000
#4                Angelos  Louis  6000
#5               Blum  Marc Paul  6000
################################10#########################

#OK, finally, let's look at the contribution amounts grouped by contribution type.

kamenetz_contribution_type  <- kamenetz_contribs_all %>%
  #the colum we are grouping 
  group_by(`Contributor Type`)  %>%
  #creates a count with the row name being count. The n value is the number of times it occurs
  summarise(count = n(), total = sum(`Contribution Amount`,  na.rm = TRUE)) %>%
  #arrange the list in descending order
  arrange(desc(total)) 
View(kamenetz_contribution_type)
kamenetz_contribution_type[1:10,]

#What I'm seeing is that kamenetz is getting a lot of money from business, mroe than half from individuals.
#1                                      Individual   587 577579.5
#2                     Business/Group/Organization   313 443952.8
#3                                   PAC Committee    12   9750.0
#4                             Candidate Committee     1   6000.0
#5                              Spouse (Candidate)     1   2000.0
#6                               Federal Committee     1   1000.0
#7 Unregistered Out-of-State Non-Federal Committee     1   1000.0



#################11##############


#I'd also like to look at big contributions versus big contributions. I will do that with these two functions. Small contribs, for contrinbutions less than 250 and big contributions more than 2500.


count_kamenetz_smallcontribs <- kamenetz_state_list_fromzips %>% 
  #the colum we are grouping 
  filter(`Contribution Amount` < 250 )  %>%
  #creates a count with the row name being count. The n value is the number of times it occurs
  summarise(count = n(), total = sum(`Contribution Amount`)) %>%
  #arrange the list in descending order
  arrange(desc(total)) 
count_kamenetz_smallcontribs[1:2]

count_kamenetz_bigcontribs <- kamenetz_state_list_fromzips %>% 
  #the colum we are grouping 
  filter(`Contribution Amount` > 2500 )  %>%
  #creates a count with the row name being count. The n value is the number of times it occurs
  summarise(count = n(), total = sum(`Contribution Amount`)) %>%
  #arrange the list in descending order
  arrange(desc(total)) 
count_kamenetz_bigcontribs[1:2]



################# madaleno! ############



###############1################# 
#OK, so this one was way more complicated than I had hoped, but it searches for numbers that are grouped into 5 digits and sometimes are separated by a - sign or a dash. Then adds it to a list, which I converted to a numeric, grouped, sorte and then summed contributions. 


madaleno_contribs_all <- cleaned_contributions_new  %>% 
  #here we're creating a dataframe of all entries that have the receiving committee equaling madaleno. And it excludes in-kind and refund/rebate information
  filter(`Receiving Committee` == "Madaleno  Richard Marylanders For") %>%
  filter(`Contribution Type` != "In-Kind") %>%
  filter(`Contribution Type` != "Coordinated In-Kind") %>%
  filter(`Contribution Type` != "Refund/Rebate")
View(madaleno_contribs_all)
#this also creates the a dataframe of rebartes and inkind donations that I pulled out of the contributions list. Just to see if I see something interesting.
madaleno_inkind_rebates_all <- cleaned_contributions_new  %>% 
  #here we're creating a dataframe of all entries that have the receiving committee equaling madaleno. And it excludes in-kind and refund/rebate information
  filter(`Receiving Committee` == "Madaleno  Richard Marylanders For") %>%
  filter(`Contribution Type` == "In-Kind" | `Contribution Type` == "Refund/Rebate" |  `Contribution Type` == "Coordinated In-Kind")
View(madaleno_inkind_rebates_all)


##############################2####################

# So as a safeguard, let's look at madaleno's contributions and see if they match up with the Baltimore Sun's numbers.....not for antything but they seem like a competent grtoup of journalists. 
sum(madaleno_contribs_all$`Contribution Amount`)
#439862 is what I got....and their campaign information also said $439,862.00. Not bad. 
sum(madaleno_inkind_rebates_all$`Contribution Amount`)
#3085.1
#########################3#################

#let's start exploring where in the world this money is coming from...We can do that by extracting zip codes
#searches for all number combos that are either 5 digits long or 5 digits followed by 4 digits with a dash in between.
madaleno_zips1 <- stri_extract_all_regex(madaleno_contribs_all$`Contributor Address`, "(?<!\\d)(\\d{5}(?:[-\\s]\\d{4})?)\\b")
#pull out the 5 digit strings from the rest.
madaleno_5digitzips <- stri_extract_all_regex(madaleno_zips1, "\\d{5}")
#I was getting some extra 5 digit combos from addresses, so I used this lapply command to just grab the last one, since the zip is always last. 
madaleno_zips2 <- map(madaleno_5digitzips , function(x) x[length(x)])
#creates the dataframe madaleno_c as new madaleno_c
madaleno_zips_df <- data_frame(madaleno_zips2)
#changes the list to a numeric value. This helps the group by function which do4sn't like lists.
madaleno_zips_df %>% mutate_if(is.list, as.character) -> madaleno_zips_df
#changes the column header in the new dataframe to zip_codes. this will be helpful later on because it is unique. 
colnames(madaleno_zips_df)[which(names(madaleno_zips_df) == "madaleno_zips2")] <- "zip_codes"
#binds it to the madaleno contributions database
newmadaleno_contribswithzips <- cbind(madaleno_zips_df, madaleno_contribs_all)
#ok, so now that we have our new list with the added zip codes, we can go through and group them and count them. 
count_madaleno_zip_codes <- newmadaleno_contribswithzips %>% 
  #the colum we are grouping 
  group_by(zip_codes)  %>%
  #creates a count with the row name being count. The n value is the number of times it occurs
  summarise(count = n(), total = sum(`Contribution Amount`,  na.rm = TRUE)) %>%
  #arrange the list in descending order
  arrange(desc(total)) 
View(count_madaleno_zip_codes)
count_madaleno_zip_codes[1:10,]


##########################4##########################

#now let's group by state and see where this money is coming from by joining this data with a helpful database of zip codes that is part of the zipcode library.....which you should install, or you're not going anywhere pat'na. PS: zipcode also has a cleaning feature, but it didn't work for my purposes. Thus the code above. 

#pulls the database of zip codes/state information 
data(zipcode)
View(zipcode) 
#creates a merged list of zip codes and the madaleno information. 
madaleno_state_list_fromzips <-  left_join(newmadaleno_contribswithzips, zipcode, by =c("zip_codes" = "zip"))


##########################5###########################
#so let's count it up and see what we got. I'm grouping by city and state, but that can be changed, if needed. 

count_madaleno_states <- madaleno_state_list_fromzips %>% 
  #the colum we are grouping 
  group_by(state, city)  %>%
  #creates a count with the row name being count. The n value is the number of times it occurs
  summarise(count = n(), total = sum(`Contribution Amount`,  na.rm = TRUE)) %>%
  #arrange the list in descending order
  arrange(desc(total)) 
View(count_madaleno_states)
count_madaleno_states[1:10,]


#So Interesting. We're looking closer to DC. Odd NA, but most of the money is from kensington....a lot. 
#1    MD    Kensington   130 146074
#2    MD   Chevy Chase   204  54450
#3    MD Silver Spring   261  48634
#4    MD      Bethesda   142  35815
#5    MD       Potomac    35  18527

#######################5a##################
#because there's so muhc money from DC let's back up and just sort by states. That way we can see what's what as far as totals go. 
count_madaleno_just_states <- madaleno_state_list_fromzips %>% 
  #the colum we are grouping 
  group_by(state)  %>%
  #creates a count with the row name being count. The n value is the number of times it occurs
  summarise(count = n(), total = sum(`Contribution Amount`,  na.rm = TRUE)) %>%
  #arrange the list in descending order
  arrange(desc(total)) 
View(count_madaleno_just_states)
count_madaleno_just_states[1:10,]

#Interesting. Maryland contributed much more, but DC VA and NY all did give some money 
#1    MD  1194 382996
#2    DC    75  18256
#3    TX     8   7975
#4    NY    21   7048
#5    CA    13   6930



##########################6###########################


# now let's examine what these people's employer is. This categorty is a bit anemic, because many of the fields are left blank, but why not try, amirite? 
count_madaleno_states_jobs <- madaleno_state_list_fromzips %>% 
  #the colum we are grouping 
  group_by(city, `Employer Name`)  %>%
  #creates a count with the row name being count. The n value is the number of times it occurs
  summarise(count = n(), total = sum(`Contribution Amount`)) %>%
  #arrange the list in descending order
  arrange(desc(total)) 
View(count_madaleno_states_jobs)
count_madaleno_states_jobs[1:4]

#Meh, too many nulls to really be helpful, but got a few to look into. 
#9   Chevy Chase American University     2   6000
#10   Chevy Chase     U.S. Government     2   6000


###############7#################

#Let's zoom back out then. Let's just group by employer name. 

madaleno_contribsby_employer1 <- madaleno_contribs_all %>%
  #groups the values by the employer.name, condensing all duplicates. 
  group_by(`Employer Name`) %>%
  #sums the values from the contribution amount column and places it into the group by we requested above. 
  summarise(total = sum(`Contribution Amount`)) %>%
  #arrange the list in descending order
  arrange(desc(total))
#show me what I just did
View(madaleno_contribsby_employer1 )
madaleno_contribsby_employer1[1:10,]
#FINDING: Hmm unfortunately the NAs are a little too overwhelming to be totally useful, but it's interesting that Prince George's County employees gave 23k. 
#1                        <NA> 309500
#2               Self-Employed  13200
#3               Self Employed  10000
#4         American University   6500
#5   Maryland General Assembly   6000
###############8#################

madaleno_contribsby_occupation <- madaleno_contribs_all %>%
  #the colum we are grouping 
  group_by(`Employer Occupation`) %>%
  #creates a count with the row name being count. The n value is the number of times it occurs
  summarise(total = sum(`Contribution Amount`,  na.rm = TRUE)) %>%
  #arrange the list in descending order
  arrange(desc(total))
#show me what I just did
View(madaleno_contribsby_occupation)
madaleno_contribsby_occupation[1:10,]
#FINDING:Unfortunately NA and other are runaways. But retired is a somewhat significant category. 
#1                          <NA> 249525
#2                         Other  46650
#3                       Retired  39975
#4                         Legal  35902
#5 Administrative and Management  18100


###############9################
#Hey big spender, let's look at all the contributions ordered by amount....

madaleno_contribs_sunmmed_arranged<- madaleno_contribs_all %>%
  group_by(`Contributor Name`) %>%
  #sums the values from the contribution amount column and places it into the group by we requested above. 
  summarise(total = sum(`Contribution Amount`)) %>%
  #arrange the list in descending order
  arrange(desc(total))
#show me what I just did
View(madaleno_contribs_sunmmed_arranged)
madaleno_contribs_sunmmed_arranged[1:10,]
#some people I don't know....that Richard Madaleno name sounds pretty familiar......
##1 MADALENO  RICHARD  STUART 120000
#2         Eisenstadt  Linda   6000
#3           Hostetler  Eric   6000
#4              Jones  Rolin   6000
#5             Lublin  David   6000
################################10#########################

#OK, finally, let's look at the contribution amounts grouped by contribution type.

madaleno_contribution_type  <- madaleno_contribs_all %>%
  #the colum we are grouping 
  group_by(`Contributor Type`)  %>%
  #creates a count with the row name being count. The n value is the number of times it occurs
  summarise(count = n(), total = sum(`Contribution Amount`,  na.rm = TRUE)) %>%
  #arrange the list in descending order
  arrange(desc(total)) 
View(madaleno_contribution_type)
madaleno_contribution_type[1:10,]

#What I'm seeing is that madaleno is getting lots from individuals, but he gave himself a big chunk of change too
#1                                    Individual  1389 300112
#2                              Self (Candidate)     1 120000
#3                   Business/Group/Organization     8   8000
#4                            Spouse (Candidate)     1   4000
#5                           Candidate Committee     4   3750


#################11##############


#I'd also like to look at big contributions versus big contributions. I will do that with these two functions. Small contribs, for contrinbutions less than 250 and big contributions more than 2500.


count_madaleno_smallcontribs <- madaleno_state_list_fromzips %>% 
  #the colum we are grouping 
  filter(`Contribution Amount` < 250 )  %>%
  #creates a count with the row name being count. The n value is the number of times it occurs
  summarise(count = n(), total = sum(`Contribution Amount`)) %>%
  #arrange the list in descending order
  arrange(desc(total)) 
count_madaleno_smallcontribs[1:2]

count_madaleno_bigcontribs <- madaleno_state_list_fromzips %>% 
  #the colum we are grouping 
  filter(`Contribution Amount` > 2500 )  %>%
  #creates a count with the row name being count. The n value is the number of times it occurs
  summarise(count = n(), total = sum(`Contribution Amount`)) %>%
  #arrange the list in descending order
  arrange(desc(total)) 
count_madaleno_bigcontribs[1:2]

################# ross! ############


###############1################# 
#OK, so this one was way more complicated than I had hoped, but it searches for numbers that are grouped into 5 digits and sometimes are separated by a - sign or a dash. Then adds it to a list, which I converted to a numeric, grouped, sorte and then summed contributions. 


ross_contribs_all <- cleaned_contributions_new  %>% 
  #here we're creating a dataframe of all entries that have the receiving committee equaling ross. And it excludes in-kind and refund/rebate information
  filter(`Receiving Committee` == "Ross  Alec for Maryland") %>%
  filter(`Contribution Type` != "In-Kind") %>%
  filter(`Contribution Type` != "Coordinated In-Kind") %>%
  filter(`Contribution Type` != "Refund/Rebate")
View(ross_contribs_all)
#this also creates the a dataframe of rebartes and inkind donations that I pulled out of the contributions list. Just to see if I see something interesting.
ross_inkind_rebates_all <- cleaned_contributions_new  %>% 
  #here we're creating a dataframe of all entries that have the receiving committee equaling ross. And it excludes in-kind and refund/rebate information
  filter(`Receiving Committee` == "Ross  Alec for Maryland") %>%
  filter(`Contribution Type` == "In-Kind" | `Contribution Type` == "Refund/Rebate" |  `Contribution Type` == "Coordinated In-Kind")
View(ross_inkind_rebates_all)


##############################2####################

# So as a safeguard, let's look at ross's contributions and see if they match up with the Baltimore Sun's numbers.....not for antything but they seem like a competent grtoup of journalists. 
sum(ross_contribs_all$`Contribution Amount`)
#1050653 is what I got....and their campaign information said $1,057,246.94.  Sun reports 1million flat
sum(ross_inkind_rebates_all$`Contribution Amount`)
#15596.26

#########################3#################

#let's start exploring where in the world this money is coming from...We can do that by extracting zip codes
#searches for all number combos that are either 5 digits long or 5 digits followed by 4 digits with a dash in between.
ross_zips1 <- stri_extract_all_regex(ross_contribs_all$`Contributor Address`, "(?<!\\d)(\\d{5}(?:[-\\s]\\d{4})?)\\b")
#pull out the 5 digit strings from the rest.
ross_5digitzips <- stri_extract_all_regex(ross_zips1, "\\d{5}")
#I was getting some extra 5 digit combos from addresses, so I used this lapply command to just grab the last one, since the zip is always last. 
ross_zips2 <- map(ross_5digitzips , function(x) x[length(x)])
#creates the dataframe ross_c as new ross_c
ross_zips_df <- data_frame(ross_zips2)
#changes the list to a numeric value. This helps the group by function which do4sn't like lists.
ross_zips_df %>% mutate_if(is.list, as.character) -> ross_zips_df
#changes the column header in the new dataframe to zip_codes. this will be helpful later on because it is unique. 
colnames(ross_zips_df)[which(names(ross_zips_df) == "ross_zips2")] <- "zip_codes"
#binds it to the rosscontributions database
newross_contribswithzips <- cbind(ross_zips_df, ross_contribs_all)
#ok, so now that we have our new list with the added zip codes, we can go through and group them and count them. 
count_ross_zip_codes <- newross_contribswithzips %>% 
  #the colum we are grouping 
  group_by(zip_codes)  %>%
  #creates a count with the row name being count. The n value is the number of times it occurs
  summarise(count = n(), total = sum(`Contribution Amount`,  na.rm = TRUE)) %>%
  #arrange the list in descending order
  arrange(desc(total)) 
View(count_ross_zip_codes)
count_ross_zip_codes[1:10,]


##########################4##########################

#now let's group by state and see where this money is coming from by joining this data with a helpful database of zip codes that is part of the zipcode library.....which you should install, or you're not going anywhere pat'na. PS: zipcode also has a cleaning feature, but it didn't work for my purposes. Thus the code above. 

#pulls the database of zip codes/state information 
data(zipcode)
View(zipcode) 
#creates a merged list of zip codes and the rossinformation. 
ross_state_list_fromzips <-  left_join(newross_contribswithzips, zipcode, by =c("zip_codes" = "zip"))


##########################5###########################
#so let's count it up and see what we got. I'm grouping by city and state, but that can be changed, if needed. 

count_ross_states <- ross_state_list_fromzips %>% 
  #the colum we are grouping 
  group_by(state, city)  %>%
  #creates a count with the row name being count. The n value is the number of times it occurs
  summarise(count = n(), total = sum(`Contribution Amount`,  na.rm = TRUE)) %>%
  #arrange the list in descending order
  arrange(desc(total)) 
View(count_ross_states)
count_ross_states[1:10,]


#So Interesting. New York and San Francisco providing a lot of his money. DC after that
#1    NY      New York   175 201444.22
#2    CA San Francisco    82 107319.15
#3    DC    Washington   181  57997.73
#4    MD     Baltimore   168  48272.49
#5    MD   Chevy Chase    54  33303.26

#######################5a##################
#because there's so muhc money from DC let's back up and just sort by states. That way we can see what's what as far as totals go. 
count_ross_just_states <- ross_state_list_fromzips %>% 
  #the colum we are grouping 
  group_by(state)  %>%
  #creates a count with the row name being count. The n value is the number of times it occurs
  summarise(count = n(), total = sum(`Contribution Amount`,  na.rm = TRUE)) %>%
  #arrange the list in descending order
  arrange(desc(total)) 
View(count_ross_just_states)
count_ross_just_states[1:10,]

#Interesting. Maryland contributed much more, but DC VA and NY all did give some money 
#1    CA   282 367278.37
#2    NY   224 225212.00
#3    MD   544 204195.76
#4    DC   181  57997.73
#5    VA    75  37201.84
#between ca and ny 56% of cash to campaign
(367278.37 + 225212.00)/1050653
# 19 percent of total contributions comes from MD
204195.76/1050653 
##########################6###########################


# now let's examine what these people's employer is. This categorty is a bit anemic, because many of the fields are left blank, but why not try, amirite? 
count_ross_states_jobs <- ross_state_list_fromzips %>% 
  #the colum we are grouping 
  group_by(city, `Employer Name`)  %>%
  #creates a count with the row name being count. The n value is the number of times it occurs
  summarise(count = n(), total = sum(`Contribution Amount`)) %>%
  #arrange the list in descending order
  arrange(desc(total)) 
View(count_ross_states_jobs)
count_ross_states_jobs[1:4]

#Meh, too many nulls to really be helpful, but got a few to look into. 
#1      New York    Self-Employed     5 18500.00

#5    Burlingame Geodesic Capital     2 12000.00

###############7#################

#Let's zoom back out then. Let's just group by employer name. 

ross_contribsby_employer1 <- ross_contribs_all %>%
  #groups the values by the employer.name, condensing all duplicates. 
  group_by(`Employer Name`) %>%
  #sums the values from the contribution amount column and places it into the group by we requested above. 
  summarise(total = sum(`Contribution Amount`)) %>%
  #arrange the list in descending order
  arrange(desc(total))
#show me what I just did
View(ross_contribsby_employer1 )
ross_contribsby_employer1[1:10,]
#FINDING: Hmm unfortunately the NAs are a little too overwhelming to be totally useful, but it's interesting that Prince George's County employees gave 23k. 
#1             <NA> 141626.9
#2    Self-Employed  38864.4
#3          Retired  37228.3
#4    Self Employed  28369.1
#5         LinkedIn  19000.0
#6     Not Employed  12500.0
###############8#################

ross_contribsby_occupation <- ross_contribs_all %>%
  #the colum we are grouping 
  group_by(`Employer Occupation`) %>%
  #creates a count with the row name being count. The n value is the number of times it occurs
  summarise(total = sum(`Contribution Amount`,  na.rm = TRUE)) %>%
  #arrange the list in descending order
  arrange(desc(total))
#show me what I just did
View(ross_contribsby_occupation)
ross_contribsby_occupation[1:10,]
#FINDING:Unfortunately NA and other are runaways. 


###############9################
#Hey big spender, let's look at all the contributions ordered by amount....

ross_contribs_sunmmed_arranged<- ross_contribs_all %>%
  group_by(`Contributor Name`) %>%
  #sums the values from the contribution amount column and places it into the group by we requested above. 
  summarise(total = sum(`Contribution Amount`)) %>%
  #arrange the list in descending order
  arrange(desc(total))
#show me what I just did
View(ross_contribs_sunmmed_arranged)
ross_contribs_sunmmed_arranged[1:10,]
#some people I don't know....
#1       Cerf  Sigrid 12000.00
#2   McKinnon  Andrew  6005.29
#3     Allen  Herbert  6000.00
#4    Barzun  Matthew  6000.00
#5     Behrens  Bruce  6000.00
################################10#########################

#OK, finally, let's look at the contribution amounts grouped by contribution type.

ross_contribution_type  <- ross_contribs_all %>%
  #the colum we are grouping 
  group_by(`Contributor Type`)  %>%
  #creates a count with the row name being count. The n value is the number of times it occurs
  summarise(count = n(), total = sum(`Contribution Amount`,  na.rm = TRUE)) %>%
  #arrange the list in descending order
  arrange(desc(total)) 
View(ross_contribution_type)
ross_contribution_type[1:10,]

#What I'm seeing is that rossis getting lots from individuals, but he gave himself a big chunk of change too
#1                                      Individual  1529 1007708.89
#2                     Business/Group/Organization    16   31944.71
#3 Unregistered Out-of-State Non-Federal Committee     1    5999.00
#4                                   PAC Committee     2    3000.00
#5                                Self (Candidate)     1    2000.00

#################11##############


#I'd also like to look at big contributions versus big contributions. I will do that with these two functions. Small contribs, for contrinbutions less than 250 and big contributions more than 2500.


count_ross_smallcontribs <- ross_state_list_fromzips %>% 
  #the colum we are grouping 
  filter(`Contribution Amount` < 250 )  %>%
  #creates a count with the row name being count. The n value is the number of times it occurs
  summarise(count = n(), total = sum(`Contribution Amount`)) %>%
  #arrange the list in descending order
  arrange(desc(total)) 
count_ross_smallcontribs[1:2]

count_ross_bigcontribs <- ross_state_list_fromzips %>% 
  #the colum we are grouping 
  filter(`Contribution Amount` > 2500 )  %>%
  #creates a count with the row name being count. The n value is the number of times it occurs
  summarise(count = n(), total = sum(`Contribution Amount`)) %>%
  #arrange the list in descending order
  arrange(desc(total)) 
count_ross_bigcontribs[1:2]

################# vignarajah! ############


###############1################# 
#OK, so this one was way more complicated than I had hoped, but it searches for numbers that are grouped into 5 digits and sometimes are separated by a - sign or a dash. Then adds it to a list, which I converted to a numeric, grouped, sorte and then summed contributions. 


vignarajah_contribs_all <- cleaned_contributions_new  %>% 
  #here we're creating a dataframe of all entries that have the receiving committee equaling vignarajah. And it excludes in-kind and refund/rebate information
  filter(`Receiving Committee` == "Vignarajah  Krish for Maryland") %>%
  filter(`Contribution Type` != "In-Kind") %>%
  filter(`Contribution Type` != "Coordinated In-Kind") %>%
  filter(`Contribution Type` != "Refund/Rebate")
View(vignarajah_contribs_all)
#this also creates the a dataframe of rebartes and inkind donations that I pulled out of the contributions list. Just to see if I see something interesting.
vignarajah_inkind_rebates_all <- cleaned_contributions_new  %>% 
  #here we're creating a dataframe of all entries that have the receiving committee equaling vignarajah. And it excludes in-kind and refund/rebate information
  filter(`Receiving Committee` == "Vignarajah  Krish for Maryland") %>%
  filter(`Contribution Type` == "In-Kind" | `Contribution Type` == "Refund/Rebate" |  `Contribution Type` == "Coordinated In-Kind")
View(vignarajah_inkind_rebates_all)


##############################2####################

# So as a safeguard, let's look at vignarajah's contributions and see if they match up with the Baltimore Sun's numbers.....not for antything but they seem like a competent grtoup of journalists. 
sum(vignarajah_contribs_all$`Contribution Amount`)
#434747.2 is what I got....and their campaign information also said $434,747.18. Ding! 
sum(vignarajah_inkind_rebates_all$`Contribution Amount`)
#49070
#########################3#################

#let's start exploring where in the world this money is coming from...We can do that by extracting zip codes
#searches for all number combos that are either 5 digits long or 5 digits followed by 4 digits with a dash in between.
vignarajah_zips1 <- stri_extract_all_regex(vignarajah_contribs_all$`Contributor Address`, "(?<!\\d)(\\d{5}(?:[-\\s]\\d{4})?)\\b")
#pull out the 5 digit strings from the rest.
vignarajah_5digitzips <- stri_extract_all_regex(vignarajah_zips1, "\\d{5}")
#I was getting some extra 5 digit combos from addresses, so I used this lapply command to just grab the last one, since the zip is always last. 
vignarajah_zips2 <- map(vignarajah_5digitzips , function(x) x[length(x)])
#creates the dataframe vignarajah_c as new vignarajah_c
vignarajah_zips_df <- data_frame(vignarajah_zips2)
#changes the list to a numeric value. This helps the group by function which do4sn't like lists.
vignarajah_zips_df %>% mutate_if(is.list, as.character) -> vignarajah_zips_df
#changes the column header in the new dataframe to zip_codes. this will be helpful later on because it is unique. 
colnames(vignarajah_zips_df)[which(names(vignarajah_zips_df) == "vignarajah_zips2")] <- "zip_codes"
#binds it to the vignarajahcontributions database
newvignarajah_contribswithzips <- cbind(vignarajah_zips_df, vignarajah_contribs_all)
#ok, so now that we have our new list with the added zip codes, we can go through and group them and count them. 
count_vignarajah_zip_codes <- newvignarajah_contribswithzips %>% 
  #the colum we are grouping 
  group_by(zip_codes)  %>%
  #creates a count with the row name being count. The n value is the number of times it occurs
  summarise(count = n(), total = sum(`Contribution Amount`,  na.rm = TRUE)) %>%
  #arrange the list in descending order
  arrange(desc(total)) 
View(count_vignarajah_zip_codes)
count_vignarajah_zip_codes[1:10,]


##########################4##########################

#now let's group by state and see where this money is coming from by joining this data with a helpful database of zip codes that is part of the zipcode library.....which you should install, or you're not going anywhere pat'na. PS: zipcode also has a cleaning feature, but it didn't work for my purposes. Thus the code above. 

#pulls the database of zip codes/state information 
data(zipcode)
View(zipcode) 
#creates a merged list of zip codes and the vignarajahinformation. 
vignarajah_state_list_fromzips <-  left_join(newvignarajah_contribswithzips, zipcode, by =c("zip_codes" = "zip"))


##########################5###########################
#so let's count it up and see what we got. I'm grouping by city and state, but that can be changed, if needed. 

count_vignarajah_states <- vignarajah_state_list_fromzips %>% 
  #the colum we are grouping 
  group_by(state, city)  %>%
  #creates a count with the row name being count. The n value is the number of times it occurs
  summarise(count = n(), total = sum(`Contribution Amount`,  na.rm = TRUE)) %>%
  #arrange the list in descending order
  arrange(desc(total)) 
View(count_vignarajah_states)
count_vignarajah_states[1:10,]


#So Interesting.Columbia Maryland....6 donors for 100k? I smell self-funding. 
#1    MD    Columbia     6 100950
#2    NY    New York    87  51861
#3    DC  Washington   109  30195
#4    MA   Cambridge     8  12600
#5    CA Bakersfield     2  12000


################################10#########################

#OK, I'm going to go ahead and see who these people are....

vignarajah_contribution_type  <- vignarajah_contribs_all %>%
  #the colum we are grouping 
  group_by(`Contributor Type`)  %>%
  #creates a count with the row name being count. The n value is the number of times it occurs
  summarise(count = n(), total = sum(`Contribution Amount`,  na.rm = TRUE)) %>%
  #arrange the list in descending order
  arrange(desc(total)) 
View(vignarajah_contribution_type)
vignarajah_contribution_type[1:10,]

#yep the candidate gave 100k to themselves. 
#1                  Individual   930 334147.2
#2            Self (Candidate)     1 100000.0
#3 Business/Group/Organization     2    600.0

#######################5a##################
#because there's so muhc money from DC let's back up and just sort by states. That way we can see what's what as far as totals go. 
count_vignarajah_just_states <- vignarajah_state_list_fromzips %>% 
  #the colum we are grouping 
  group_by(state)  %>%
  #creates a count with the row name being count. The n value is the number of times it occurs
  summarise(count = n(), total = sum(`Contribution Amount`,  na.rm = TRUE)) %>%
  #arrange the list in descending order
  arrange(desc(total)) 
View(count_vignarajah_just_states)
count_vignarajah_just_states[1:10,]

#Interesting. Maryland contributed much more (100k is from the candidate), but CA, DC, and NY all did give some money....
#1    MD   334 168291.2
#2    NY   134  72926.0
#3    CA    93  48611.0
#4    DC   109  30195.0
#5    MA    25  21445.0


##########################6###########################


# now let's examine what these people's employer is. This categorty is a bit anemic, because many of the fields are left blank, but why not try, amirite? 
count_vignarajah_states_jobs <- vignarajah_state_list_fromzips %>% 
  #the colum we are grouping 
  group_by(city, `Employer Name`)  %>%
  #creates a count with the row name being count. The n value is the number of times it occurs
  summarise(count = n(), total = sum(`Contribution Amount`)) %>%
  #arrange the list in descending order
  arrange(desc(total)) 
View(count_vignarajah_states_jobs)
count_vignarajah_states_jobs[1:4]

#Meh, too many nulls to really be helpful, but got a few to look into. 
#2    New York                       Self-Employed     7  12600
#5 Bakersfield Comprehensive Blood & Cancer Center     1   6000
#6 Bakersfield                       Self-Employed     1   6000
###############7#################

#Let's zoom back out then. Let's just group by employer name. 

vignarajah_contribsby_employer1 <- vignarajah_contribs_all %>%
  #groups the values by the employer.name, condensing all duplicates. 
  group_by(`Employer Name`) %>%
  #sums the values from the contribution amount column and places it into the group by we requested above. 
  summarise(total = sum(`Contribution Amount`)) %>%
  #arrange the list in descending order
  arrange(desc(total))
#show me what I just did
View(vignarajah_contribsby_employer1 )
vignarajah_contribsby_employer1[1:10,]
#FINDING: Hmm unfortunately the NAs are a little too overwhelming to be totally useful, but it's interesting that Prince George's County employees gave 23k. 
#1                                <NA> 178187.2
#2                       Self-Employed  29310.0
#3                        Not Employed  17499.0
#4                             Retired  15200.0
#5              Caravel Management LLC   6500.0
###############8#################

vignarajah_contribsby_occupation <- vignarajah_contribs_all %>%
  #the colum we are grouping 
  group_by(`Employer Occupation`) %>%
  #creates a count with the row name being count. The n value is the number of times it occurs
  summarise(total = sum(`Contribution Amount`,  na.rm = TRUE)) %>%
  #arrange the list in descending order
  arrange(desc(total))
#show me what I just did
View(vignarajah_contribsby_occupation)
vignarajah_contribsby_occupation[1:10,]
#FINDING:Unfortunately NA and other are such runaways it's not even worth putting here. 


###############9################
#Hey big spender, let's look at all the contributions ordered by amount....

vignarajah_contribs_sunmmed_arranged<- vignarajah_contribs_all %>%
  group_by(`Contributor Name` ) %>%
  #sums the values from the contribution amount column and places it into the group by we requested above. 
  summarise(total = sum(`Contribution Amount`)) %>%
  #arrange the list in descending order
  arrange(desc(total))
#show me what I just did
View(vignarajah_contribs_sunmmed_arranged)
vignarajah_contribs_sunmmed_arranged[1:10,]
#some people I don't know....Mario Batali. Heard that name. 

#1 VIGNARAJAH  KRISHANTI 1e+05
#2         Batali  Mario 6e+03
#3      Blaustein  Susan 6e+03
#4        Boone  Cecilia 6e+03
#5         Harmon  James 6e+03

#################11##############


#I'd also like to look at big contributions versus big contributions. I will do that with these two functions. Small contribs, for contrinbutions less than 250 and big contributions more than 2500.


count_vignarajah_smallcontribs <- vignarajah_state_list_fromzips %>% 
  #the colum we are grouping 
  filter(`Contribution Amount` < 250 )  %>%
  #creates a count with the row name being count. The n value is the number of times it occurs
  summarise(count = n(), total = sum(`Contribution Amount`)) %>%
  #arrange the list in descending order
  arrange(desc(total)) 
count_vignarajah_smallcontribs[1:2]

count_vignarajah_bigcontribs <- vignarajah_state_list_fromzips %>% 
  #the colum we are grouping 
  filter(`Contribution Amount` > 2500 )  %>%
  #creates a count with the row name being count. The n value is the number of times it occurs
  summarise(count = n(), total = sum(`Contribution Amount`)) %>%
  #arrange the list in descending order
  arrange(desc(total)) 
count_vignarajah_bigcontribs[1:2]

#what's weird, is that baker didn't get a lot of large contribs, nor did he get a lot of small contribs. What's the average contribution amount?

mean(baker_contribs_all$`Contribution Amount`)
#########################3#################

#below I've just done al the means ot see them in one place. 
mean(baker_contribs_all$`Contribution Amount`)
mean(shea_contribs_all$`Contribution Amount`)
mean(vignarajah_contribs_all$`Contribution Amount`)
mean(kamenetz_contribs_all$`Contribution Amount`)
mean(madaleno_contribs_all$`Contribution Amount`)
mean(ross_contribs_all$`Contribution Amount`)
mean(jealous_contribs_all$`Contribution Amount`)


#looks at the unique conrributors to the campaign
count_ross_contribs <- ross_state_list_fromzips %>% 
  #the colum we are grouping 
  distinct(`Contributor Name` ) %>%
  #creates a count with the row name being count. The n value is the number of times it occurs
  summarise(count = n()) %>%
  #arrange the list in descending order
  arrange(desc(count)) 
count_ross_contribs[1:10,]