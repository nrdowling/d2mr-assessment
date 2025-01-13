# Mini-project: R Programming | hello_world function

Primary skills/categories targeted: 

-   Assessed: 5, 6, 7, 8
-   Unassessed: 1, 2, 5, 8

## Description

In this project, you will build a function called `hello_world()`. This is a classic beginner programming exercise for learning any coding language. It can start very, very small and work up to complex functionality.

## Instructions

1.  In an .R script, define the `hello_world()` function meeting at least the minimal requirements below. 
2.  In the same script, after defining the function, call the function with at least 2 different sets of arguments.
3.  Before submission, make sure your function is thoroughly documented with comments!

Minimally, your `hello_world()` function should include at least 1 object assignment and at least 1 conditional statement. 

It should take at least 1 argument, for example:

-   `name` (string)
-   `time_of_day` (numeric or POSIX)
-   `is_morning` (boolean)
-   `return_n_greetings` (integer)

Your function should have at least 2 possible return values, such as:

-   hello, class
-   Good morning, Dr. Dowling!
-   Sup?
-   bonjour mes amis


You can choose to make your function as simple or as complex as you like. The more complex, the more you can demonstrate your R programming skills.

In addition to the primary r programming objectives listed above, this project is a good opportunity to demonstrate use of tidyverse functions. 

### Challenge suggestions

To make things trickier, show more skills, or earn engagement points, try one or more of the following:

1.  You can also choose to build your project in a Quarto notebook (vs .R script) to demonstrate additional markdown/quarto skills. If you do this, you can either define your function within the notebook or source it from an .R script.
2.  Write multiple functions, so that your final `hello_world()` function includes the other function(s). For example, you could write a function takes a precise time and returns a general time of day (morning, afternoon, evening, night), then have your `hello_world()` function could then use this function to determine the time of day and return a greeting accordingly.
3.  Use the `stringr` package to manipulate the input arguments in some way. For example, you could use `str_to_lower()` to make sure the name argument is all (or mostly) lowercase before returning a greeting, so that a name argument of "NaTaLiE" would result in something like "hello Natalie".
4.  Include one or more `for` or `while` loops in addition to the conditional statement(s).
5.  Make me (and you) laugh! This is a classic beginner exercise, which gives you a chance to get creative. You'll be able to show off more coding skills if you can make it entertaining enough for you to stick with it a little longer.


## Submission

1. Copy the `assessment.md` doc from your top-level directory into this project subdirectory.
2. Complete the assessment document.
3. On Canvas, go to the next available open mini-project assignment. In the text box, include:
    1. The name of the project and/or a brief description of the project
    2. A link to the subdirectory for your mini-project
        1. If you completed the project in a separate repository, your `assessment.md` file in the subdirectory should explain how to access your work.
    3. Any additional info you'd like your grader to know
4. Submit the Canvas assignment. Your grader will access the directory you've linked to, grade your work on the assessment doc, push the doc back to your repository, and mark your Canvas assignment as complete. 