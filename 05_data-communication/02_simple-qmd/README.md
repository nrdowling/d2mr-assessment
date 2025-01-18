# Mini-project: Data communication | Simple .qmd

Primary skills/categories targeted:

- Assessed: 22, 23, 26, 27, 28, 29
- Unassessed: 2, 9, 10

## Description

In this project, you will create a Quarto notebook that includes the three core components: a YAML header, code chunks, and pandoc markdown text.

## Instructions

1. Create a .qmd Quarto notebook.
2. Refer to the example files from apaquarto and to Quarto's documentation to add meta-info to the YAML header, minimally:
    1. Title
    2. Short title
    3. Author(s)
3. Create one or more code chunks that perform setup tasks, for example:
    1. source() an .R script
    2. Load or require packages
    3. Set a random seed
    4. Read in data
    5. Set default chunk options
4. Create one or more code chunks that execute non-setup R code, for example:
    1. Use R operators to assign values to objects
    2. Construct a data frame
    3. Define a function
5. Add/edit markdown to include at least (lorem ipsum is ok):
    1. 1st, 2nd, & 3rd headers (1 of each)
    2. 1 unordered list & 1 ordered list
    3. Bold & italicized text
    4. 1 linked URL
    5. 1 HTML-style comment
    
### Challenge suggestions

To make things trickier, show more skills, or earn engagement points, try one or more of the following:

1. Create both a Quarto notebook (.qmd) and R Markdown notebook (.Rmd). Identify points of difference and similarity. Try to find additions that work in the “next gen” Quarto but not in the older R Markdown.
2. Render your notebook in multiple output formats (pdf, docx, html). Set different options for each.
3. Use the apaquarto extension so that your qmd renders into APA style. You’ll have to think carefully about which installation method to use and how to adapt it to work within your assessment repo.
4. Show off some R skills in your code chunks! Use this mini-project to demonstrate as many skills as you like in R programming, tidyverse functions, visualization, analysis, or anything else.
5. Figure out how to use .css, .scss, and/or .yml files (like the ones created when you start a new Quarto project).
6. Integrate HTML into your markdown text using in-text spans (with `<span>` tags) or fenced div blocks (with `::: {.divlayername} [...div contents...] :::` ).

## Submission

1. Copy the `assessment.md` doc from your top-level directory into this project subdirectory.
2. Complete the assessment document.
2. Your mini-project directory should contain both the .qmd *and* any rendered documents, like .pdf or .docx. You may need to edit your .gitignore to make sure these are included (or more accurately, not excluded). 
3. On Canvas, go to the next available open mini-project assignment. In the text box, include:
    1. The name of the project and/or a brief description of the project
    2. A link to the subdirectory for your mini-project
        1. If you completed the project in a separate repository, your [assessment.md](assessment.md) file in the subdirectory should explain how to access your work.
    3. Any additional info you'd like your grader to know
4. Submit the Canvas assignment. Your grader will access the directory you've linked to, grade your work on the assessment doc, push the doc back to your repository, and mark your Canvas assignment as complete. 