library(tidyverse)
library(stringi)
library(zipcode)
library(purrr)
library(lubridate)
#this data was downloaded from the MD BOE on Sept. 5. It was cleaned with Openrefine to standardize some of the addresses and names. Hogan and Jealous informaiton downloaded between 1/1/15-12/31/2018-- Omalley information from 1/1/2007 -12/31/2010


#read in spreadsheets
r_hogan_contributions <- read_csv("hogan_contribs_cycle_18.csv")
r_jealous_contributions <- read_csv("jealous_contribs_cycle_18.csv")
r_omalley_contributions <- read_csv("omalley_contribs_cycle_10.csv")

all_contributions <- rbind(r_hogan_contributions, r_jealous_contributions, r_omalley_contributions)


#quick calculation to see how much money raised
sum(hogan_contributions$`Contribution Amount`, na.rm = T)
sum(jealous_contributions$`Contribution Amount`, na.rm = T)
sum(omalley_contributions$`Contribution Amount`, na.rm = T)


#############TIME FOR ANALYSIS##############

all_contributions_new <- all_contributions %>% 
  #here we're creating a dataframe of all entries from the two receiving committees. It excludes in-kind and refund/rebate information
 # filter(`Contribution Type` != "In-Kind") %>%
#  filter(`Contribution Type` != "Coordinated In-Kind") %>%
  filter(`Contribution Type` != "Refund/Rebate")


#this also creates the a dataframe of rebartes and inkind donations that I pulled out of the contributions list. Just to see if I see something interesting.
inkind_rebates_all <- all_contributions %>% 
  #here we're creating a dataframe of all entries that have the receiving committee equaling jealous. And it excludes in-kind and refund/rebate information
  filter(`Contribution Type` == "In-Kind" | `Contribution Type` == "Refund/Rebate" | `Contribution Type` == "Coordinated In-Kind") %>%
group_by(`Receiving Committee`, `Contribution Type`, `Contributor Name`) %>%
summarise(count = n(), total_contrib = sum(`Contribution Amount`))

#let's start exploring where in the world this money is coming from...We can do that by extracting zip codes. 
#searches for all number combos that are either 5 digits long or 5 digits followed by 4 digits with a dash in between.
contributions_all_zips1 <- stri_extract_all_regex(all_contributions_new$`Contributor Address`, "(?<!\\d)(\\d{5}(?:[-\\s]\\d{4})?)\\b")
contributions_all_5digitzips <- stri_extract_all_regex(contributions_all_zips1, "\\d{5}")
#I was getting some extra 5 digit combos from addresses, so I used this lapply command to just grab the last one, since the zip is always last. 
contributions_all_zips2 <- map(contributions_all_5digitzips, function(x) x[length(x)])
#creates the dataframe Jealous_contributions_new as new contributions_all_zipz_df
contributions_all_zipz_df <- data_frame(contributions_all_zips2)
#changes the list to a numeric value. This helps the group by function which do4sn't like lists.
contributions_all_zipz_df %>% mutate_if(is.list, as.character) -> contributions_all_zipz_df
#changes the column header in the new dataframe to zip_codes. this will be helpful later on because it is unique. 
colnames(contributions_all_zipz_df)[which(names(contributions_all_zipz_df) == "contributions_all_zips2")] <- "zip_codes"
#binds it to the hogan contributions database
newall_contribswithzips <- cbind(contributions_all_zipz_df, all_contributions_new)


#ok, so now that we have our new list with the added zip codes, we can go through and group them and count them. 

##########################4##########################

#now let's combine with the zip codes database so we can group by state and county 

#pulls the database of zip codes/state information 
data(zipcode)
#creates a merged list of zip codes and the hogan information. 
allcontribs_state_list_fromzips <-  left_join(newall_contribswithzips, zipcode, by =c("zip_codes" = "zip"))


#so before we do the basic calculations, let's take a minute and do something a little different. Let's loook at the people who gave to both Omalley and to Hogan and the people who gave to Omalley and Jealous. Then let's compare the differences. 

#To do this, i've settled on a semi join, which will return all rows from x where there are matching values in y, keeping just columns from x. A semi join differs from an inner join because an inner join will return one row of x for each matching row of y, where a semi join will never duplicate rows of x. This is important, because I need to make sure that I'm not getting repeats like I would get in an inner join. That would create inaccurate numbers of donation funds. 
#Beacuase of the fact that it only grabs X rows, I have to run the reverse join to get the y equivelant

#the columns that must match in x and y for it to return a result. 


#Now we have to create three separate datasets of candidate information to join them back together. 
hogan_contributions  <- allcontribs_state_list_fromzips %>%
  #here we're creating a dataframe of all entries from the two receiving committees. It excludes in-kind and refund/rebate information
  filter(`Receiving Committee` == "Hogan  Larry for Governor" )


jealous_contributions <- allcontribs_state_list_fromzips %>%
  #here we're creating a dataframe of all entries from the two receiving committees. It excludes in-kind and refund/rebate information
  filter(`Receiving Committee` == "Jealous  Ben Friends of" )


omalley_contributions <- allcontribs_state_list_fromzips %>%
  #here we're creating a dataframe of all entries from the two receiving committees. It excludes in-kind and refund/rebate information
  filter(`Receiving Committee` == "O'Malley  Martin Friends Of" )

#our the fields we will be joining on are contributor name and zip code

requirements <- c("Contributor Name", "zip_codes")

#this finds contributiors to jealous who also gave to omalley
jealous_omalley <- semi_join(jealous_contributions, omalley_contributions, by = (requirements), ignorecase = T)
#this finds contributors who gave to hogan that also gave to jealous
hogan_jealous <- semi_join(hogan_contributions, jealous_contributions, by = (requirements), ignorecase = T) 
#this finds contributors who gave to jealous and also gave to hogan
jealous_hogan <- semi_join(jealous_contributions,hogan_contributions, by = (requirements),ignorecase = T) 
#so this finds contributors who gave to hogan that also gave to omalley  
hogan_omalley <-  semi_join(hogan_contributions, omalley_contributions, by = (requirements), ignorecase = T) 

#For expediency's sake, we're going to summarize who gave to hogan and o'malley 
sum_hogan_omalley <- hogan_omalley %>%
group_by( `Contribution Date`,`Receiving Committee`,`Contributor Name`, zip_codes, `Contributor Address`, ) %>%
summarise(count = n(), total_contrib = sum(`Contribution Amount`))
#how muhc total?
sum(sum_hogan_omalley$total_contrib)


#now it groups the donations by receiving committee, zip and name and gives a number of donations and contribution amount.
sum_jealous_omalley <- jealous_omalley %>%
  group_by( `Receiving Committee`,zip_codes, `Contributor Address`,`Contributor Name`, `Contribution Date`, state, city) %>%
summarise(count = n(), total_contrib = sum(`Contribution Amount`))


#now give me my results in a csv.


#write_csv(sum_hogan_omalley, "sum_hogan_omalley.csv")
#write_csv(sum_jealous_omalley, "sum_jealous_omalley.csv")


#so I got a tip that some of those contributions donated after the election. So I've gone back and decided to pull a few more details from Hogan's filings, like the date of the donation and what state the contribution came from
sum_hogan_omalley_checked <- read_csv("sum_hogan_omalley_checked.csv")

test_hogan_omalley <- left_join(sum_hogan_omalley_checked, hogan_omalley, by = c("Contributor Address" = "Contributor Address", "Contributor Name" = "Contributor Name"))
sum(test_hogan_omalley$`Contribution Amount`, na.rm = T)

write_csv(test_hogan_omalley, "hogan_omalley_with_dates.csv")


jealous_omalley$zip_codes <- as.integer(jealous_omalley$zip_codes)

class(jealous_omalley$zip_codes )
class(sum_jealous_omalley_checked$zip_codes)

test_jealous_omalley <- left_join(sum_jealous_omalley_checked, jealous_omalley, by = c("zip_codes" = "zip_codes", "Contributor Name" = "Contributor Name"))
sum(test_jealous_omalley$`Contribution Amount`)

#write_csv(test_jealous_omalley, "jealous_omalley_with_states.csv")



#One last thing. I'm interested in seeing if Jealous has made a pivot and brought in a bunch of money from Maryland,instead of other states. So I'll run a few queries to see. And then I can compare it to Hogan. 

#now, just to look at some other little things, we're going to create a row that will say whether the donor is from maryland or not  
jealous_contributions <- jealous_contributions %>%
  mutate(md_notmd = if_else(state == "MD", "MD", "not_MD")) 

#Hey big spender, let's look at all the contributions grouped by whether its a Maryland or outside Maryland contribution and betwen a date range....the last filing period.

#firtst we need to mame the date a date field
jealous_contributions$`Contribution Date` <- as.Date(jealous_contributions$`Contribution Date`,"%m/%d/%y")


count_jealous_states <- jealous_contributions %>% 
  #the colum we are grouping 
  filter(`Contribution Date` >= as.Date("2018-06-11") & `Contribution Date` <= as.Date("2018-08-21"))  %>%
  group_by(md_notmd )  %>%
  #creates a count with the row name being count. The n value is the number of times it occurs
  summarise(count = n(), total = sum(`Contribution Amount`,  na.rm = TRUE)) %>%
  #arrange the list in descending order
  arrange(desc(total)) 
View(count_jealous_states)


#now, just to look at some other little things, we're going to create a row that will say whether the donor is from maryland or not  

hogan_contributions <- hogan_contributions %>%
  mutate(md_notmd = if_else(state == "MD", "MD", "not_MD")) 

#Hey big spender, let's look at all the contributions ordered by amount, state and period....

count_hogan_states <- hogan_contributions %>% 
  #the colum we are grouping 
  group_by(md_notmd, `Filing Period` )  %>%
  #creates a count with the row name being count. The n value is the number of times it occurs
  summarise(count = n(), total = sum(`Contribution Amount`,  na.rm = TRUE)) %>%
  #arrange the list in descending order
  arrange(desc(total)) 
View(count_hogan_states)



#we know that Hogan got public funds during his first run for governor. Now let's see how quickly people started giving him money afterward. 
hogan_contributions$`Contribution Date` <- as.Date(hogan_contributions$`Contribution Date`,"%m/%d/%y")

#Sorts out the number of contributions by a certain date. I'm using this to ascertain how much was donated to Hogan at certain times during his early years as Governor. 
hogan_contributions %>%
filter(`Contribution Date` < as.Date("2016-01-21")) %>%
summarise( total = sum(`Contribution Amount`))



######The below queries and other code are other exploratory queries that I ended up not using. But are here for future reference. ###########



#says hey, if the contributions are less than or = to $20 say really small, <= 200 just say small. Else, say large.  
small_large  <- allcontribs_state_list_fromzips %>%
mutate(large_small = if_else(`Contribution Amount`  <= 20, "really_small", if_else(`Contribution Amount`  <= 200, "just_small", "large")))  


#In this evaluation, we're looking at the number of donors and the amount of contributions given to Jealous and whne they gave. We can now compare the amount of money given by small donors in the pre primary and the pre general.
eva_all <- small_large %>%
  group_by(`Receiving Committee`, `Filing Period`, md_notmd, large_small) %>%
  summarise(count = n(), total_contrib = sum(`Contribution Amount`))

#And just for fun, let's run the state totals for each state, so we can see where the money both in and out of state is coming from for the most recent filing period. 
state_numbers <- small_large %>%
  filter( `Filing Period` == "2018 Gubernatorial Pre-General1 Report") %>%
  group_by(`Receiving Committee`, state) %>%
  summarise(count = n(), total_contrib = sum(`Contribution Amount`))

contrib_type  <- small_large %>%
  #the colum we are grouping 
  filter( `Filing Period` == "2018 Gubernatorial Pre-General1 Report") %>%
  group_by(`Receiving Committee`,`Contributor Type`)  %>%
  #creates a count with the row name being count. The n value is the number of times it occurs
  summarise(count = n(), total = sum(`Contribution Amount`)) %>%
  #arrange the list in descending order
  arrange(desc(total)) 



################

################

################

#This ended up being not neccessary, but But this purr function is sweet at combining several dataframes together when not needing to left/right join. Now we can look at all the data from above queries together. Just saving for later.
library(purrr)
Master_list <-   
  list(nonmd_contributors, md_contributors, indiv_nonmd_contribs_count, indiv_md_contribs_count) %>% 
  reduce(left_join, by = c("Filing Period"))
NULL


## so now that we've got the two tables merged, It's time to see when donations have been coming into the campaign. This turned out to be somewhat not useful. But it may be helpful later, so I stuck it at the end. 
month_contributions <- allcontribs_state_list_fromzips

#makes the contributions a date field
month_contributions$`Contribution Date` <- as.Date(month_contributions$`Contribution Date`, "%m/%d/%Y")
#now I'm filtering all the dates after 2018.
month_contributions <- month_contributions %>% 
  filter(month_contributions$`Contribution Date` >= as.Date("2018-1-1"))

#makes the contributions a date field
month_contributions$`Contribution Date` <- as.Date(month_contributions$`Contribution Date`, "%m/%d/%Y")
#now I'm filtering all the dates after 2018.
month_contributions <- month_contributions %>%
  mutate(month = format(month_contributions$`Contribution Date`, "%m"))



    
