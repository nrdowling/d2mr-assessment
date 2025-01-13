## This script was written by Tino Ma, a D2MR student in 2024. 
## It has not been edited in any way, and consequently may not 
## behave as it did at the time of writing.


hello_world <- function(name, current_time = Sys.time()) {
    Sys.setlocale("LC_TIME", "English")
    # Convert the string to a POSIXct object
    date_time <- as.POSIXct(current_time, format="%Y-%m-%d %H:%M:%OS")
    # Format the POSIXct object to only include date and time up to minutes
    display_time <- format(date_time, "%Y-%m-%d %H:%M")
    current_hour <- as.numeric(format(current_time, "%H"))
    if (weekdays(Sys.Date()) == "Tuesday" | weekdays(Sys.Date()) == "Thursday") {
        course <- TRUE
    } else {
        course <- FALSE
        days_to_add <- 1
        while (course == FALSE) {
            if (weekdays(Sys.Date() + days_to_add) == "Tuesday" | weekdays(Sys.Date() + days_to_add) == "Thursday") {
                course <- TRUE
            } else {
                days_to_add <- days_to_add + 1
            }
        }
    }
    # Assign time of day based on current hour
    if (current_hour >= 6 && current_hour < 12) {
        time_of_day <- ("morning")
    } else if (current_hour >= 12 && current_hour < 18) {
        time_of_day <- ("afternoon")
    } else {
        time_of_day <-("night")
    }
    if (course == TRUE) {
        return(paste("Good", time_of_day, name, "It's", display_time, weekdays(Sys.Date()), "and we have class today", sep = " "))
    } else {
        if (days_to_add > 1) {
            days_to_add <- paste(days_to_add, "days", sep = " ")
        } else {
            days_to_add <- paste(days_to_add, "day", sep = " ")
        }
        return(paste("Good", time_of_day, name, "It's", display_time, weekdays(Sys.Date()), "we don't have class today but we will have class in", days_to_add, sep = " "))
    }
}

hello_world("Dr. Dowling")