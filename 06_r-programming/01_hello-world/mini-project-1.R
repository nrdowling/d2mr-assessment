#install.packages("stringr")
#install.packages("dplyr")

library(stringr)
library(dplyr)

# build a hello_world function: greeting each person 
hello_world <-function(name_list, time_of_day, is_midnight = FALSE, know_time_left = FALSE, bed_time = "22:00"){
  
  # name_list - a list of names
  # time_of_day - time of day in string format "xx:xx"
  # is_midnight - a Boolean value to check if it is midnight
  # know_time_left - whether people want to know how many time left until bed time
  # bed_time - time to go to bed; bedtime can be customized if know_time_left = TRUE, default is "22:00".
  
  for (name in name_list){
    # Convert the name in standard format - capitalize the first letter of each word
    name <- name %>%
      str_to_title() %>%
      str_trim()
    # say hello to each name in the name_list
    print(paste("Hello", name, ">(^~^)<"))
    
    if (is_midnight == TRUE){
    # if they consider now is the midnight, no greetings, directly print "GO TO SLEEP RIGHT NOW!!!"
      print(paste0("GO TO SLEEP RIGHT NOW!!! ", str_to_upper(name),"!"))
      next
    }
    
    # print greetings words based on the time of the day if not midnight
    time_period <- check_time(time_of_day) # call check_time function to categorize time_of_day
    words <- greeting_words(time_period) # call greeting_words function to randomly return greeting words based on time period
    print(paste0("Good ", time_period,"! ", words))

    # call time_left_for_bed function to tell people how many hours and minutes left for bedtime
    time_left <- time_left_for_bed(time_of_day, bed_time)
    if(know_time_left == TRUE){
      print(paste("You have", time_left[1], "hours and", time_left[2], "minutes left until bedtime."))
    } 
  }
}


# function to 1) check the format of time_of_day and 2) categorize it into morning, afternoon, evening, and night
check_time <- function(time_of_day){
  
  # transform the string to POSIXlt date-time object 
 time <- strptime(time_of_day, "%H:%M")
  
  # check if the time_of_day is in the correct format, if not, return an error message
  if (is.na(time)) {
    stop("Invalid time format. Please input time as 'xx:xx'in 24-hour format.")
  }
  
  # extract the hour only  
  hour <- as.integer(format(time, "%H"))
  
  # return the time period of the day based on the actual time
  time_period <- case_when(
    hour >= 5 & hour < 12  ~ "morning",
    hour >= 12 & hour < 17 ~ "afternoon",
    hour >= 17 & hour < 20 ~ "evening",
    TRUE                   ~ "night"  
  )
  
}

# function to return other greeting words based on the time of the day besides Good morning/afternoon/evening/night
# greeting sentences will be randomly chosen for each time period
# greeting sentences used:
# Morning: 
#    1. Hope you have a great day!
#    2. How are you?
#    3. Miss me?
# Afternoon:
#    1. Hope you have a great afternoon!
#    2. How is your day going so far?
#    3. Any plans for the evening?
# Evening:
#    1. What's your plan for dinner?
#    2. How was your day?
#    3. Any exciting news to share?
# Night:
#    1. Good night! 
#    2. Have a good night!
#    3. Bed time!
morning_words = c("Hope you have a great day!","How are you?","Miss me?")
afternoon_words = c("Hope you have a great afternoon!","How is your day going so far?","Any plans for the evening?")
evening_words = c("What's your plan for dinner?","How was your day?","Any exciting news to share?")
night_words = c("Good night!","Have a good night!","Bed time!")

# Function to randomly return a greeting sentence based on the time period
?sample
greeting_words <- function(time_period){
  if (time_period == "morning"){
    sample(morning_words, 1)
  } else if (time_period == "afternoon") {
    sample(afternoon_words, 1)
  } else if (time_period == "evening"){
    sample(evening_words, 1)
  } else{
    sample(night_words, 1)
  }
  
}

# function to tell people how many time left until the bed time
time_left_for_bed <- function(time, bed_time){
  
  # transform the time and bed_time into standard time format
  time <- strptime(time, "%H:%M") 
  bed_time <- strptime(bed_time, "%H:%M")
  
  # calculate the time difference between current time and bed time
  time_left = bed_time - time
  time_left_num <- abs(as.numeric(time_left)) # transform time_left to a single positive digit
  
  # calculate the time left in hours and minutes separately
  hours_left <- floor(time_left_num) 
  minutes_left = (time_left_num - hours_left)*60     
  
  return(c(hours_left, minutes_left))
}

?abs


# call the function
hello_world("Skye", "11:24", FALSE, TRUE)
hello_world(c("Skye","Molly","Victor"), "15:04", FALSE, TRUE, "23:00")
hello_world("skye", "21:50", FALSE)
hello_world(c("Skye","Christy"), "23:34", TRUE)
hello_world(c("  Skye  "," christy "), "23:34", TRUE) # Name has white space in the begining and at the end

# quarto notebook


# unassessed marked: 1 2 5 8

# 2.  find and understand documentation for R packages and functions ?
# 5.  follow a coding style guide ?
# 8.  parse and write complex `for` and `while` loops

