
### hello_world() Example 1 ###

# Define a function that gives you as many positive greetings as you want; assign to object hello_world()

# hello_world() takes arguments name (character) and n_greet (number of greetings requested)
hello_world <- function(name, n_greet) {
    # set some possible greetings, character variables that may reference the name argument
    greetings <- c(paste0("Hello ", name, "!"), # paste0 will concatenate strings and string-formatted objects without a separator
                   "How's your day going?",
                   paste0("Wow, ", name, ", looking good today."),
                   paste0(name, ", that outfit looks great on you."),
                   paste0(name, "! I'm so glad you're here!"),
                   "OMG hey!", # if I don't want to reference a stored variable/object, I don't have to concatenate anything
                   paste0("sup ", name, "?"))
    # start the counter off at 1
    i <- 1
    # but before using the counter, check to see if we don't want any greetings at all
    if (n_greet == 0) {
        return(paste0("I have nothing to say to you, ", name, ".")) # return something other than a greeting, but still reference the name argument
    } else {
        while(i <= n_greet) { # is i less than or equal to the number of greetings I asked for?
            print(sample(greetings, 1)) # if yes, print out a random greeting from my list (if no, while loop ends)
            i <- i + 1 # add 1 to the counter because we delivered 1 greeting and restart the while loop
        }
    }
} # end hello_world function define
