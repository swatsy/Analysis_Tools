vlookup in R
================
Cliff

2023-03-04

Load necessary packages

``` r
rm(list = ls())

## First specify the packages of interest
packages = c("tidyverse", "dplyr","ggplot2")

## Now load or install&load all
package.check <- lapply(
  packages,
  FUN = function(x) {
    if (!require(x, character.only = TRUE)) {
      install.packages(x, dependencies = TRUE)
      library(x, character.only = TRUE)
    }
  }
)
```

### create first data frame

``` r
df1 <- data.frame(Name=c('Ana',' Eva', 'Danish', 'Sony', 'Roop', 'Pasu', 'Yeti', 'Saranya', 'Tabu' ),
                  Codeteam=rep(c('Bolly', 'Holly', 'Tolly')),
                  Maths_Score=c(24,23,34,27,29,30,24,22,29))
df1
```

    ##      Name Codeteam Maths_Score
    ## 1     Ana    Bolly          24
    ## 2     Eva    Holly          23
    ## 3  Danish    Tolly          34
    ## 4    Sony    Bolly          27
    ## 5    Roop    Holly          29
    ## 6    Pasu    Tolly          30
    ## 7    Yeti    Bolly          24
    ## 8 Saranya    Holly          22
    ## 9    Tabu    Tolly          29

### create second data frame

``` r
df2 <- data.frame(Name=c('Ana',' Ella', 'Danish', 'Sush', 'Roop', 'Pasu', 'Yeti', 'Tabu' ),
                  points=c(14, 15, 15, 16, 8, 9, 16, 275),
                  likes=c("Grey","Yellow","Green","Pink","Blue","Black","Grey","Yellow"))
df2
```

    ##     Name points  likes
    ## 1    Ana     14   Grey
    ## 2   Ella     15 Yellow
    ## 3 Danish     15  Green
    ## 4   Sush     16   Pink
    ## 5   Roop      8   Blue
    ## 6   Pasu      9  Black
    ## 7   Yeti     16   Grey
    ## 8   Tabu    275 Yellow

### 1. Retains all values, all rows from both dataframe - keeping everything

``` r
full_join(df1,df2, by="Name")
```

    ##       Name Codeteam Maths_Score points  likes
    ## 1      Ana    Bolly          24     14   Grey
    ## 2      Eva    Holly          23     NA   <NA>
    ## 3   Danish    Tolly          34     15  Green
    ## 4     Sony    Bolly          27     NA   <NA>
    ## 5     Roop    Holly          29      8   Blue
    ## 6     Pasu    Tolly          30      9  Black
    ## 7     Yeti    Bolly          24     16   Grey
    ## 8  Saranya    Holly          22     NA   <NA>
    ## 9     Tabu    Tolly          29    275 Yellow
    ## 10    Ella     <NA>          NA     15 Yellow
    ## 11    Sush     <NA>          NA     16   Pink

### 2. Retains rows common to both sets

``` r
inner_join(df1, df2, by="Name")
```

    ##     Name Codeteam Maths_Score points  likes
    ## 1    Ana    Bolly          24     14   Grey
    ## 2 Danish    Tolly          34     15  Green
    ## 3   Roop    Holly          29      8   Blue
    ## 4   Pasu    Tolly          30      9  Black
    ## 5   Yeti    Bolly          24     16   Grey
    ## 6   Tabu    Tolly          29    275 Yellow

### OR using merge()

``` r
merge(df1, df2, by="Name") 
```

    ##     Name Codeteam Maths_Score points  likes
    ## 1    Ana    Bolly          24     14   Grey
    ## 2 Danish    Tolly          34     15  Green
    ## 3   Pasu    Tolly          30      9  Black
    ## 4   Roop    Holly          29      8   Blue
    ## 5   Tabu    Tolly          29    275 Yellow
    ## 6   Yeti    Bolly          24     16   Grey

### 3. Retains all rows from left dataframe

``` r
left_join(df1,df2, by="Name")
```

    ##      Name Codeteam Maths_Score points  likes
    ## 1     Ana    Bolly          24     14   Grey
    ## 2     Eva    Holly          23     NA   <NA>
    ## 3  Danish    Tolly          34     15  Green
    ## 4    Sony    Bolly          27     NA   <NA>
    ## 5    Roop    Holly          29      8   Blue
    ## 6    Pasu    Tolly          30      9  Black
    ## 7    Yeti    Bolly          24     16   Grey
    ## 8 Saranya    Holly          22     NA   <NA>
    ## 9    Tabu    Tolly          29    275 Yellow

### 4. Retains all rows from right dataframe

``` r
right_join(df1,df2, by="Name")
```

    ##     Name Codeteam Maths_Score points  likes
    ## 1    Ana    Bolly          24     14   Grey
    ## 2 Danish    Tolly          34     15  Green
    ## 3   Roop    Holly          29      8   Blue
    ## 4   Pasu    Tolly          30      9  Black
    ## 5   Yeti    Bolly          24     16   Grey
    ## 6   Tabu    Tolly          29    275 Yellow
    ## 7   Ella     <NA>          NA     15 Yellow
    ## 8   Sush     <NA>          NA     16   Pink
