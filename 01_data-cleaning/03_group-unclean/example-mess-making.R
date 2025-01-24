
### EXAMPLE UNCLEANING SCRIPT ###

# This script creates the "messy" version of the iris dataset for the data cleaning walkthrough.
# You can use this as an example of "uncleaning" data to swap with partners/group members.
# Your script does not have to look like this, but it should always:
# 1. Load required libraries
# 2. Set a seed (so others can reproduce your work)
# 3. Use tidyverse functions to create a "messy" version of a dataset
# 4. Write the messy dataset to a CSV file to share with your group


## Tasks ##

# 1.  Renaming Columns: Rename columns to match the original Iris dataset column names: Sepal.Length, Sepal.Width, Petal.Length, Petal.Width, Species.
# 2.  Reordering Columns: Ensure the columns are in the correct order.
# 3.  Sorting Rows: Sort the rows by Species and then by Sepal.Length.
# 4.  Changing Data Types: Ensure Species is a factor and other columns are numeric.
# 5.  Mutating Data: Create a new column Sepal.Area calculated as Sepal.Length \* Sepal.Width.
# 6.  String Manipulation: Use stringr to clean any unwanted spaces or characters in the Species column.
# 7.  Handling Factors: Use forcats to ensure the levels of Species are ordered as setosa, versicolor, virginica.

# Load necessary libraries
library(tidyverse)
library(here)

# Load the iris dataset
data(iris)

# Create a messy version of the iris dataset
set.seed(123)  # For reproducibility
messy.iris <- iris %>% 
    # Add random spaces to Species column
    mutate(
        Species = case_when(
            runif(n()) < 0.3 ~ paste0(Species, " "),
            TRUE ~ Species
            ),
        Species = case_when(
            runif(n()) < 0.4 ~ paste0(" ", Species),
                TRUE ~ Species
            )
    ) %>% 
    mutate(
        Species = case_when(
            runif(n()) < 0.1 ~ toupper(Species),
            TRUE ~ Species)
    ) %>%
    # Create some deliberate typos in Species column
    mutate(
        Species = case_when(
            runif(n()) < 0.1 ~ str_replace(Species, "setosa", "setsoa"),
            runif(n()) < 0.1 ~ str_replace(Species, "versicolor", "versicolour"),
            runif(n()) < 0.1 ~ str_replace(Species, "virginica", "Virginia"),
            runif(n()) < 0.1 ~ str_replace(Species, "o", "0"),
            TRUE ~ Species
        )
    ) %>%
    # Shuffle the rows randomly
    sample_frac(1) %>% 
    # Rename columns to incorrect names
    rename(SepalLength = Sepal.Length, 
           SepalWidth = Sepal.Width, 
           PetalLength = Petal.Length, 
           PetalWidth = Petal.Width, 
           FlowerSpecies = Species) %>% 
    # Reorder columns randomly
    select(PetalWidth, SepalLength, FlowerSpecies, SepalWidth, PetalLength) %>% 
    # Convert Species to character and add random noise to numeric columns
    mutate(
        FlowerSpecies = as.character(FlowerSpecies),
        SepalLength = SepalLength + runif(n(), min = -0.05, max = 0.04),
        SepalWidth = SepalWidth + runif(n(), min = -0.05, max = 0.04),
        PetalLength = PetalLength + runif(n(), min = -0.05, max = 0.04),
        PetalWidth = PetalWidth + runif(n(), min = -0.05, max = 0.04)
    ) %>% 
    # Replace decimals with commas
    mutate(
        across(where(is.numeric), ~str_replace_all(.x, fixed("."), ","))
    )

# Save the messy dataset to a CSV file
write.csv(messy.iris, "01_data-cleaning/cleaning-walkthrough/messy-iris.csv", row.names = FALSE)

