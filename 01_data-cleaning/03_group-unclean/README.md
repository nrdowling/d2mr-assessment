# Mini-project: Data cleaning | Group “uncleaning”

Primary skills/categories targeted:

- Assessed: 5, 6, 8, 9, 10, 11, 12
- Unassessed: 1, 2, 5, 9, 14

Use notebooks to additionally demonstrate quarto or markdown skills.

## Description

In this project, you will work together with a classmate to each “unclean” a dataset for the other(s) to clean. This will work best in pairs so that you can swap datasets (I’ll refer to “partners” in these instructions), but you can adjust things to do in a small group if you prefer. 

Note that in these instructions I purposefully use “clean” rather than “tidy.” You should think about why.

## Instructions

1. Each partner should select a publicly available dataset. You can use the `data()` function to view a list of dataframes loaded with base R. Some packages contain additional example dataframes to explore. 
2. In an .R script or rmd/qmd notebook *in your own assessment repo*, transform your selected dataframe to make it messy. You will not share this file with your partner.
    1. Refer to the `example-mess-making.R` script for ideas on how to “unclean” data. This example is the code that builds the messy data for the data cleaning walkthrough.
    2. Use comments (.R files) and/or markdown text (.Rmd/.qmd notebooks) to explain the steps you’re taking.
3. Give the messy .csv file you wrote to your partner (who will give their messy data to you). Don’t share the mess-making file!
4. In an .R script or rmd/qmd notebook *in your own assessment repo*, clean your partner’s messy dataset. You can aim to return it to the state of the original dataset or continue transforming the data to make your own data cleaning choices.
5. Discuss the process with your partner after you have both finished cleaning. Did you miss anything? Did you more or less “invert” the mess-making process or use different approaches? Do you think the resulting datasets are sufficiently “clean”? (Bonus: are they “tidy”?)

Your mess-making can be in either a script or notebook and does not need to follow the same patterns as the example. However you decide to approach the task, your mess-making should minimally: 1) load required libraries, 2) set a random seed, 3) use tidyverse functions to transform a publicly available dataset, and 4) write the messy dataset to a .csv file to share with your group.

**Be sure to use frequent, informative comments and/or markdown text to explain decision-making and procedure for both the uncleaning and cleaning processes.**

### Challenge suggestions

To make things trickier, show more skills, or earn engagement points, try one or more of the following:

1. Incorporate relatively more complex transformations that create puzzles for your partner. For example, if the original data has variables height and weight, you could add a BMI variable and then delete the height column. Your partner would need to recreate the height column by deriving values based on weight and BMI.
2. Reflect on whether your cleaned dataset is not just clean, but “tidy.” Why or why not? If not, continue transforming the dataset until it is tidy. What are the advantages and disadvantages of each version? (You can include discussion of this in your reflection in the assessment file.)
3. Explore packages outside the tidyverse that were made for the express purpose of cleaning, like `janitor`. Try to accomplish the same tasks from your tidyverse approach with functions from the other packages. You can also explore packages designed to make a mess of data (for cleaning practice), like `messy`, but be cautious about incorporating them into what you unclean for your partner. They can inject errors that require skills outside the scope of this class to clean.
4. Use the `daff` package to keep track of your progress as you work. If you choose to do this, I recommend cleaning in a .qmd rather than a script so that you can include chunks for checkpoint comparisons. See the cleaning walkthrough and Level 1 cleaning mini-project for how to incorporate daff functions.

## Submission

1. Copy the `assessment.md` doc from your top-level directory into this project subdirectory.
2. Complete the assessment document.
    1. Thoroughly complete any sections about collaboration. It should be perfectly clear who you worked with and who contributed what.
    2. Complete the (usually optional, here required) reflection, discussing the experience of collaborating. You can additionally reflect on other aspects of the project if you like.
3. Your mini-project’s subdirectory should include (presented here in the order you will probably create each):
    1. this README
    2. an [assessment.md](http://assessment.md) file completed by you and only you
    3. .csv of the clean data you started with, if applicable (i.e., if it was not a built-in dataset)
    4. script or notebook that uncleans your data
    5. .csv of the messy data you created
    6. .csv of the messy data your partner gave you
    7. script or notebook that cleans your partner’s messy data
    8. .csv of the resulting, clean dataset
4. On Canvas, go to the next available open mini-project assignment. In the text box, include:
    1. The name of the project and/or a brief description of the project
    2. A link to the subdirectory for your mini-project
        1. If you completed the project in a separate repository, your `assessment.md` file in the subdirectory should explain how to access your work.
    3. Name of partner (or group members)
    4. Any additional info you'd like your grader to know
5. Submit the Canvas assignment. Your grader will access the directory you've linked to, grade your work on the assessment doc, push the doc back to your repository, and mark your Canvas assignment as complete.