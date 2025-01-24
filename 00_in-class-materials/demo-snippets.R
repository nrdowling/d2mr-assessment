library(tidyverse)

##############################################
############# WEEK 2 CLASS 1 #################
##############################################

# Defining a simple function
add.two <- function(x) {return(x + 2)}

# The names don't matter
# Assign it to any name and call the arguments whatever you want
sdfkjsdf <- function(kgguw) {return(kgguw + 2)}

# Slightly more complicated, including other functions in a function
add.two2 <- function(x) {return(paste(as.character(x), "two"))}

# What counts as "returning"?

fnc_paste_return <- function(argument1, argument2) {
    return(paste(argument1, argument2))
}

fnc_paste <- function(argument1, argument2) {
    paste(argument1, argument2)
}

fnc_paste_store <- function(argument1, argument2) {
    myvalue <- paste(argument1, argument2)
}

fnc_paste_return("hello", "world")
fnc_paste("hello", "world")
fnc_paste_store("hello", "world")


#### See groceries.qmd for all the grocer's partner conditional/functions shenanigans! ####


for (i in c(2,4,8,16)) {print(i)}

for (i in c(1,2,3,4)) {print(2^i)}

timestwo <- function(number) {x <- number*2
    print(x) 
}


i <- 1
while(i <= 4) {
    print(2^i)
    i <- i + 1}

# Hello world demo

print("Hello world!")

print(paste("Hello", "world!")) # Default separator is a space

print(paste("Hello", "world!", sep = "-")) # Make it anything

print(paste0("Hello", "world!")) # Or nothing

print(paste0("Hello", " ", "world", "!")) # Break it up with more than two arguments



##############################################
############# WEEK 3 CLASS 1/2 ###############
##############################################

### readr (and related packages): READ AND WRITE DATA ###


## REFERENCE: Key functions from readr, readxl, googlesheets4 #
## functions listed for reference (won't run without filepath string arguments)
# 
# # library(readr) # Note: readr included in library(tidyverse) or can be loaded alone
# read_delim()
# read_csv()
# read_csv2()
# 
# # library(readxl)
# read_excel_csv()
# read_excel_csv2()
# read_tsv()
# 
# # library(googlesheets4)
# read_sheet()
# 
# # readr can also export tibbles to tabular files  # with similar syntax to how it imports
# 
# # library(readr) # Note: readr included in library(tidyverse) or can be loaded alone
# write_delim()
# write_csv()
# write_csv2()
# 
# # library(readxl)
# write_excel_csv()
# write_excel_csv2()
# write_tsv()
# 
# # library(googlesheets4)
# write_sheet()


### IN-CLASS READR EXERCISE ##

# 1. Pull the most recent commit of the d2mr-assessment repo (seems like you've done that if you're here!)
# 2. Find the excel spreadsheet: `in-class-materials/data/MM data.xlsx`
# 3. Load the readxl library


# 4. Use the read_excel() function to assign the data to a tibble called mmdata


# 5. Examine your data in the console to see if it looks like the image
# Running the line below will load the csv that I made for you
# This will(/should) produce the same tibble as what you see in the slides
# mmdata <- read_csv("00_in-class-materials/data/mmdata-ND.csv")

# 6. Open `MM data.xlsx` in Excel (or similar) and examine it with your human eyeballs
    # What about this file creates problems when you read-in?
    # How would you fix the issue here in the Excel document? Don't do it in code yet, but write out your ideas in comments:
    # ...
    # ...

# 7. Review the ?read_excel documentation 
# What arguments can you pass to read_excel() to fix the issue on read-in?

# 8. Use the read_excel() function to assign the data to a tibble called mmdata that looks like the image

# 9. Use write_csv() to save mmdata to an intermediate dataset: `in-class-materials/data/mmdata.csv`




### magrittr: THE PIPE ###
## (BACK TO THE SLIDES) ##

# This assumes you have the mmdata tbl in your environment already
# Make sure you run whatever code is necessary to create/load that tbl first!

# Assign everything to follows to mmdata.long
# Using the mmdata tbl ...
mmdata.long <- mmdata %>%	
    # ...select three columns... 	
    select(Red, Green, Blue) %>% 	
    # ...then made that 3 col tbl long... 	
    pivot_longer(cols = c("Red", "Green","Blue"),              
                 names_to = "Color",              
                 values_to = "Number") %>% 	
    # ...then sort that tbl by new Number column...	
    arrange(Number) 
# Pipeline is over, so everything assigned to mmdata.long


### dplyr: TIDY TRANSFORMATION ###

## REFERENCE: Key functions from dplyr #
## functions listed for reference (won't run without arguments)

# library(dplyr) # Note: dplyr included in library(tidyverse) or can be loaded alone
# filter()
# select()
# arrange()
# mutate()
# summarize()
# group_by()
# ungroup() # This "resets" grouping, good practice to include at the end of pipelines that use group_by() but not usually necessary



### IN-CLASS (time permitting) dplyr DEMO ##

## dplyr functions

# filter()
filter(mmdata, Weight >= 50)
filter(mmdata, Green == 7, Blue == 7)

# select()
select(mmdata, Bag, Weight)
select(mmdata, Bag, Yellow, Red)
select(mmdata, Red, Yellow, Bag)
select(mmdata, -Bag)
select(mmdata, starts_with("B"))
select(mmdata, bag_id = Bag, Red, Blue, wght = Weight)
select(mmdata, red = Red, bag_id = Bag)
rename(mmdata, red = Red, bag_id = Bag)

# arrange() 
arrange(mmdata, Weight)
mmdata %>% 
    arrange(Blue, -Red)

# put it together: filter+select+rename+arrange
mmdata %>% 
    filter(Weight >= 50) %>% 
    select(-Bag) %>% 
    rename(Tangerine = Orange, Lemon = Yellow) %>% 
    arrange(Tangerine, -Lemon)

# mutate()
mutate(mmdata, Candy = "M&M") #very useful
mutate(mmdata, Christmas = Red + Green)
mutate(mmdata,
       Primary = Red + Blue + Yellow,
       Avg_Primary_Weight = Weight/Primary)
mutate(mmdata,
       Weight = Weight*.035, #convert grams to ounces, replacing values in Weight column
       Weight_Gr = Weight/.035, # convert back...just to be confusing I guess
       Total = Red + Green + Blue + Orange + Yellow + Brown,
       Avg_Weight_Oz = Weight/Total)

#####

# messing with the data a bit to use in next examples
mmdata2 <- mmdata %>%  
    mutate(Whose = case_when(Bag < 11 ~ "Natalie",
                             Bag < 21 ~ "Yuchen",
                             TRUE ~ "Mian"),
           Big_Green = ifelse(Green > 7, TRUE, FALSE))
#####

# group_by() & summarize()
summarize(mmdata2, mean_wgt = mean(Weight))
group_by(mmdata2, Whose) %>% summarize(whose_mean_wgt = mean(Weight))
mmdata2 %>% 
    group_by(Whose, Big_Green) %>% 
    summarize(count_big_green_bags = n()) # the n() function counts rows without taking any arguments
mmdata2 %>% 
    group_by(Whose, Big_Green) %>% 
    summarize(Sum_BG_Green = sum(Green), Sum_BG_Red = sum(Red), Sum_BG_Blue = sum(Blue)) %>% 
    # We can regroup by just one condition to then summarize on summary values
    # although why we would want the mean per person of the sum of each color Big Green and not Big Green is beyond me
    group_by(Whose) %>% 
    summarize(why_green = mean(Sum_BG_Green), why_red = mean(Sum_BG_Red), why_blue = (mean(Sum_BG_Blue)))

# ONE GIANT PIPE!
# Does Natalie or Yuchen have the Citrus-y-est, Primary-est, and Christmas-y collections of M&Ms?
# This is an absurd pipe for example purposes only
# Please never actually construct something that does something so confusing and pointless

mmdata %>%  
    mutate(Whose = case_when(Bag < 11 ~ "Natalie",
                             Bag < 21 ~ "Yuchen",
                             TRUE ~ "Mian")) %>% 
    filter(Whose != "Mian") %>% 
    select(-Bag) %>% 
    rowwise() %>% # This makes it easier to sum up many columns in the next line, but you could skip it if you wrote out each column individually: Red + Yellow + Green + ...
    mutate(Total = sum(c_across(Red:Brown))) %>% 
    rename(Tangerine = Orange, Lemon = Yellow, Grapefruit = Red, Lime = Green) %>% 
    mutate(Christmas = Blue + Lime,
           Primary = Grapefruit + Blue + Lemon,
           Citrus = Tangerine + Lemon + Grapefruit + Lime) %>% 
    group_by(Whose) %>% 
    summarize(Total = sum(Total), sum_Christmas = sum(Christmas), sum_Primary = sum(Primary), sum_Citrus = sum(Citrus)) %>% 
    mutate(pct_Christmas = sum_Christmas/Total, pct_Primary = sum_Primary/Total, pct_Citrus = sum_Citrus/Total) %>% 
    select(1:2, Christmas = pct_Christmas, Primary = pct_Primary, Citrus = pct_Citrus) %>% 
    arrange(Citrus)



