---
title: "Cleaning Data Guided Projects"
output: html_document
---

# Data Cleaning and Preparation Guided Projects

## COVID Dataset Project

The purpose of this Guided Project is to build our skills and understanding of the data analysis workflow by evaluating the COVID-19 situation through this dataset. At the end of this project, feel free to download the updated version of the dataset and take the same steps to analyze it.

Our analysis tries to provide an answer to this question: Which countries have had the highest number of positive cases against the number of tests?

Spoiler: It was the UK!

This guided project focused on skills such as isolating rows/columns that we need using the select and/or filter functions, grouping, aggregating, and arranging data, and creating datasets, vectors, matrices, and lists.

Below is code from the guided project: 


```r
# Load library
library(readr)

# Load COVID Dataset
covid_df <- read.csv("covid19.csv")

# Finding dimensions of dataset
dim(covid_df)
```

```
## [1] 10903    14
```

```r
# Vector of column names
vector_cols <- colnames(covid_df)
print(vector_cols)
```

```
##  [1] "Date"                    "Continent_Name"          "Two_Letter_Country_Code" "Country_Region"         
##  [5] "Province_State"          "positive"                "hospitalized"            "recovered"              
##  [9] "death"                   "total_tested"            "active"                  "hospitalizedCurr"       
## [13] "daily_tested"            "daily_positive"
```

```r
# Glimpse provides data type and snapshot of each column
covid_df_all_states <- covid_df %>%
  filter(Province_State == "All States") %>%
  select(-Province_State)
glimpse(covid_df_all_states)
```

```
## Rows: 3,781
## Columns: 13
## $ Date                    <chr> "2020-01-20", "2020-01-22", "2020-01-23", "2020-01-24", "2020-01-24", "2020-01-25", "2020-…
## $ Continent_Name          <chr> "Asia", "North America", "North America", "Asia", "North America", "Oceania", "Europe", "N…
## $ Two_Letter_Country_Code <chr> "KR", "US", "US", "KR", "US", "AU", "GB", "US", "AU", "IL", "KR", "GB", "US", "AU", "CZ", …
## $ Country_Region          <chr> "South Korea", "United States", "United States", "South Korea", "United States", "Australi…
## $ positive                <int> 1, 1, 1, 2, 1, 4, 1, 1, 4, 0, 3, 1, 1, 5, 0, 0, 4, 2, 1, 5, 0, 0, 0, 1, 9, 0, 0, 0, 1, 9, …
## $ hospitalized            <int> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, …
## $ recovered               <int> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, …
## $ death                   <int> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, …
## $ total_tested            <int> 4, 1, 1, 27, 1, 0, 31, 1, 0, 3, 51, 52, 1, 0, 20, 4, 61, 73, 1, 0, 28, 4, 97, 1, 0, 33, 7,…
## $ active                  <int> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, …
## $ hospitalizedCurr        <int> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, …
## $ daily_tested            <int> 0, 0, 0, 5, 0, 0, 0, 0, 0, 0, 12, 21, 0, 0, 0, 1, 10, 21, 0, 0, 8, 0, 24, 0, 0, 5, 3, 33, …
## $ daily_positive          <int> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 1, 1, 0, 0, 0, 0, 0, 0, 4, 0, 0, 0, 0, 0, …
```

```r
# Filter allows us to specify the data in the column so only All States data will be kept once we remove the Province state data
covid_df_all_states_daily <- covid_df_all_states %>%
  select(Date, Country_Region, active, hospitalizedCurr, daily_tested, daily_positive)

# Creating a new dataset that summarizes the number of tested, positive, active, and hospitalized cases that is grouped by region
covid_df_all_states_daily_sum <- covid_df_all_states_daily %>%
  group_by(Country_Region) %>%
  summarise(tested = sum(daily_tested), positive = sum(daily_positive), active = sum(active), hospitalized = sum(hospitalizedCurr)) %>%
  arrange((-tested))

# Extracting top 10 rows of summarized dataset
covid_top_10 <- head(covid_df_all_states_daily_sum, 10)

# Creating vectors to find top three countries with positive cases
countries <- covid_top_10$Country_Region # vector with number of countries
tested_cases <- covid_top_10$tested # vector with number of tested cases
positive_cases <- covid_top_10$positive # vector with number of positive cases
active_cases <- covid_top_10$active # vector with number of active cases
hospitalized_cases <- covid_top_10$hospitalized # vector with number of hospitalized cases
# Adding names of vectors to countries dataset
names(tested_cases) <- countries
names(positive_cases) <- countries
names(active_cases) <- countries
names(hospitalized_cases) <- countries
# Finding the top 3 countries with positive cases
positive_tested <- round(positive_cases/tested_cases, 4)
positive_tested_top3 <- positive_tested %>% sort() %>% rev() %>% head(3)
positive_tested_top3 %>% print()
```

```
## United Kingdom  United States         Turkey 
##         0.1133         0.1086         0.0807
```

```r
# Creating a matrix with top 3 positive cases
united_kingdom <- c(0.11, 1473672, 166909, 0, 0)
united_states <- c(0.10, 17282363, 1877179, 0, 0)
turkey <- c(0.08, 2031192, 163941, 2980960, 0)
# Combining vectors above into a matrix using rbind function
covid_mat <- rbind(united_kingdom, united_states, turkey)
colnames(covid_mat) <- c("Ratio", "tested", "positive", "active", "hospitalized")
covid_mat
```

```
##                Ratio   tested positive  active hospitalized
## united_kingdom  0.11  1473672   166909       0            0
## united_states   0.10 17282363  1877179       0            0
## turkey          0.08  2031192   163941 2980960            0
```

```r
# Answering the question: Which countries have had the most positive cases against the number of tests?
question <- "Which countries have had the highest number of positive cases against the number of tests?"
answer <- c("Positive tested cases" = positive_tested_top3)

# Creating data structure list
data_structure_list <- list("dataframes" = c(covid_df, covid_df_all_states, covid_df_all_states_daily, covid_df_all_states_daily_sum, covid_top_10), 
                            "matrices" = c(covid_mat), 
                            "vectors" = c(active_cases, countries, hospitalized_cases, covid_df_all_states_daily_sum$positive, positive_cases, positive_tested, positive_tested_top3, tested_cases, vector_cols))

covid_analysis_list <- list(question, answer, data_structure_list)
print(covid_analysis_list[[2]])
```

```
## Positive tested cases.United Kingdom  Positive tested cases.United States         Positive tested cases.Turkey 
##                               0.1133                               0.1086                               0.0807
```

## Creating an Efficient Data Anaylist Workflow Projects

There were two projects relating to an efficient data analyst workflow. The first project's goal was data cleaning and analyzing where are acting as a data analyst at a company that sells books on programming. The second project was focused on answering a specific question using the map function and other lubridate and purrr package functions.

The projects handled topics from creating functions using for loops, the if_else function, the case_when function, handling missing data, and analyzing and summarizing data. 

The code for the first project is below: 


```r
# Load library
library(tidyverse)

# Load file
book_reviews <- read.csv("book_reviews.csv")

# How big is the dataset?
glimpse(book_reviews)
```

```
## Rows: 2,000
## Columns: 4
## $ book   <chr> "R Made Easy", "R For Dummies", "R Made Easy", "R Made Easy", "Secrets Of R For Advanced Students", "R Made…
## $ review <chr> "Excellent", "Fair", "Excellent", "Poor", "Great", NA, "Great", "Poor", "Fair", "Fair", "Great", "Fair", "G…
## $ state  <chr> "TX", "NY", "NY", "FL", "Texas", "California", "Florida", "CA", "CA", "Texas", "NY", "Texas", "Florida", "C…
## $ price  <dbl> 19.99, 15.99, 19.99, 19.99, 50.00, 19.99, 19.99, 19.99, 29.99, 50.00, 19.99, 29.99, 29.99, 50.00, 15.99, 29…
```

```r
# What are column names?
colnames(book_reviews)
```

```
## [1] "book"   "review" "state"  "price"
```

```r
# Finding the column with the missing data
column_names <- colnames(book_reviews)

for (col in column_names) {
  null_var <- book_reviews %>% pull(col) %>% is.na() %>% sum()
  paste("Number of null values in", col, "column :", null_var) %>% print()
}
```

```
## [1] "Number of null values in book column : 0"
## [1] "Number of null values in review column : 206"
## [1] "Number of null values in state column : 0"
## [1] "Number of null values in price column : 0"
```

```r
# Removing the column with missing data
clean_book_reviews <- book_reviews %>% filter(!(is.na(review)))

# Dimsensions of the new dataset
dim(clean_book_reviews)
```

```
## [1] 1794    4
```

```r
# Finding the unique states in the state column
clean_book_reviews %>% pull(state) %>% unique()
```

```
## [1] "TX"         "NY"         "FL"         "Texas"      "Florida"    "CA"         "California" "New York"
```

```r
# Creating a new state column to choosing a naming convention
clean_book_reviews <- clean_book_reviews %>% mutate(
  state_code = case_when(
    state == "TX" ~ "TX",
    state == "NY" ~ "NY",
    state == "New York" ~ "NY",
    state == "FL" ~ "FL",
    state == "Texas" ~ "TX",
    state == "Florida" ~ "FL",
    state == "CA" ~ "CA",
    state == "California" ~ "CA"
  )
)
clean_book_reviews <- clean_book_reviews %>% select(-state)

# Creating a new column for a numberical review
clean_book_reviews <- clean_book_reviews %>% mutate(
  review_num = case_when(
    review == "Poor" ~ 1,
    review == "Fair" ~ 2,
    review == "Good" ~ 3,
    review == "Great" ~ 4,
    review == "Excellent" ~ 5
  )
)

# Creating a logical column for high reviews
clean_book_reviews <- clean_book_reviews %>% mutate(
  is_high_review = if_else(review_num >= 4, TRUE, FALSE)
)

# Finding the most profitable book
revenue_summary <- clean_book_reviews %>% group_by(book) %>% summarise(
  sales = n(),
  revenue = sum(price),
  avg_review = round(mean(review_num), 2),
  price = revenue / sales,
  high_review_perc = round((sum(is_high_review) / sales), 2)
) %>% arrange(-revenue)

# Checking each book in general
revenue_summary_state <- clean_book_reviews %>% group_by(book, state_code) %>% summarise(
  sales = n(),
  revenue = sum(price),
  avg_review = round(mean(review_num), 2),
  price = revenue / sales,
  high_review_perc = round((sum(is_high_review) / sales), 2)
) %>% arrange(-revenue)
```

```
## `summarise()` has grouped output by 'book'. You can override using the `.groups` argument.
```

```r
# It seems that the "Secrets Of R For Advanced Students" book was the most profitable based on revenue. Additionally, the sales of the book matched other books even with a higher price.
```

Conclusions:

We set out to demonstrate a complete and comprehensive data analysis work flow by analyzing book review
data. We had a well set up approach on how to achieve our goal. We had to explore and get familiar with
the data, clean the data, before finally analyzing it. Our goal was to find out the most profitable book. Here
are some valuable insights from the data:

  - Secrets Of R For Advanced Students is the most valuable book, it generated the most revenue and the
sales matched those of the other books.

  - The reason for the higher revenue is because it was priced higher. In fact, since all of the books had the
same sales performance, the key factor for driving the revenue generated by each book was the pricing.

  - Even when we look at the book performances in each individual state, it is nearly identical to the
overall performance

The code for the second project is below:


```r
# Load libraries
library(tidyverse)
library(lubridate)

# Load data
book_sales <- read.csv("sales2019.csv")

# Dimensions of dataset
dim(book_sales)
```

```
## [1] 5000    5
```

```r
# Taking a look at the dataset
glimpse(book_sales)
```

```
## Rows: 5,000
## Columns: 5
## $ date                  <chr> "5/22/19", "11/16/19", "6/27/19", "11/6/19", "7/18/19", "1/28/19", "2/20/19", "12/17/19", "7…
## $ user_submitted_review <chr> "it was okay", "Awesome!", "Awesome!", "Awesome!", "Hated it", "Never read a better book", "…
## $ title                 <chr> "Secrets Of R For Advanced Students", "R For Dummies", "R For Dummies", "Fundamentals of R F…
## $ total_purchased       <int> 7, 3, 1, 3, NA, 1, 5, NA, 7, 1, 7, NA, 3, 2, 0, 6, 3, 2, NA, 4, 3, 4, 3, 6, 4, 3, 1, 4, 4, 5…
## $ customer_type         <chr> "Business", "Business", "Individual", "Individual", "Business", "Business", "Business", "Bus…
```

```r
# Eliminate missing data
for(col in colnames(book_sales)) {
  null_val <- book_sales %>% pull(col) %>% is.na %>% sum()
  paste(col, ":", null_val) %>% print()
}
```

```
## [1] "date : 0"
## [1] "user_submitted_review : 456"
## [1] "title : 0"
## [1] "total_purchased : 718"
## [1] "customer_type : 0"
```

```r
# Remove NA values from user_submitted_review column
book_sales <- book_sales %>%
  filter(!is.na(user_submitted_review))
dim(book_sales)
```

```
## [1] 4544    5
```

```r
# 855 have been removed

# Calculate the average number of books purchased
avg_book_purchased <- (book_sales %>% filter(!is.na(total_purchased)) %>%
  pull(total_purchased) %>% mean() %>% round()
  )

# Fill all of the missing values in total_purchased with the average value
book_sales <- book_sales %>%
  mutate(
    total_purchased_notnull = ifelse(is.na(total_purchased), 
                              avg_book_purchased, total_purchased)
  )

head(book_sales)
```

```
##       date    user_submitted_review                              title total_purchased customer_type
## 1  5/22/19              it was okay Secrets Of R For Advanced Students               7      Business
## 2 11/16/19                 Awesome!                      R For Dummies               3      Business
## 3  6/27/19                 Awesome!                      R For Dummies               1    Individual
## 4  11/6/19                 Awesome!    Fundamentals of R For Beginners               3    Individual
## 5  7/18/19                 Hated it    Fundamentals of R For Beginners              NA      Business
## 6  1/28/19 Never read a better book Secrets Of R For Advanced Students               1      Business
##   total_purchased_notnull
## 1                       7
## 2                       3
## 3                       1
## 4                       3
## 5                       4
## 6                       1
```

```r
# Examine unique reviews
unique(book_sales$user_submitted_review)
```

```
##  [1] "it was okay"                          "Awesome!"                             "Hated it"                            
##  [4] "Never read a better book"             "OK"                                   "The author's other books were better"
##  [7] "A lot of material was not needed"     ""                                     "Would not recommend"                 
## [10] "I learned a lot"
```

```r
# Create a function determines if a review is positive or not
is_pos_review <- function(review) {
  case_when(
    str_detect(review, "okay") ~ TRUE,
    str_detect(review, "Awesome") ~ TRUE,
    str_detect(review, "OK") ~ TRUE,
    str_detect(review, "learned") ~ TRUE,
    str_detect(review, "Never read a better book") ~ TRUE,
    TRUE ~ FALSE
  )
}

# Create a new column that indicates whether the review was positive or not
book_sales <- book_sales %>%
  mutate(
    positive_review = is_pos_review(user_submitted_review)
  )

head(book_sales)
```

```
##       date    user_submitted_review                              title total_purchased customer_type
## 1  5/22/19              it was okay Secrets Of R For Advanced Students               7      Business
## 2 11/16/19                 Awesome!                      R For Dummies               3      Business
## 3  6/27/19                 Awesome!                      R For Dummies               1    Individual
## 4  11/6/19                 Awesome!    Fundamentals of R For Beginners               3    Individual
## 5  7/18/19                 Hated it    Fundamentals of R For Beginners              NA      Business
## 6  1/28/19 Never read a better book Secrets Of R For Advanced Students               1      Business
##   total_purchased_notnull positive_review
## 1                       7            TRUE
## 2                       3            TRUE
## 3                       1            TRUE
## 4                       3            TRUE
## 5                       4           FALSE
## 6                       1            TRUE
```

```r
# Convert data into proper format
book_sales <- book_sales %>%
  mutate(
    date = mdy(date)
  )

# Create a new column that indicates whether the sale was after the program was implemented
program_period <- mdy("07/01/2019")

book_sales <- book_sales %>%
  mutate(
    program = case_when(
      date >= program_period ~ "Yes",
      date < program_period ~ "No"
    )
  )

# Create a summary table of  of books purchased before and after the program
program_summary <- book_sales %>%
  group_by(program) %>% summarise(
    total_purchases = sum(total_purchased_notnull)
  )
print(program_summary)
```

```
## # A tibble: 2 × 2
##   program total_purchases
##   <chr>             <dbl>
## 1 No                 9114
## 2 Yes                9072
```

```r
# Adding a customer type column
program_summary_new <- book_sales %>%
  group_by(program, customer_type) %>% summarise(
    total_purchases = sum(total_purchased_notnull)
  )
```

```
## `summarise()` has grouped output by 'program'. You can override using the `.groups` argument.
```

```r
print(program_summary_new)
```

```
## # A tibble: 4 × 3
## # Groups:   program [2]
##   program customer_type total_purchases
##   <chr>   <chr>                   <dbl>
## 1 No      Business                 6222
## 2 No      Individual               2892
## 3 Yes     Business                 6309
## 4 Yes     Individual               2763
```

```r
# Create positive review summary table
program_summary_pos <- book_sales %>%
  group_by(program) %>%
  summarise(
    pos_review = sum(positive_review)
  )
print(program_summary_pos)
```

```
## # A tibble: 2 × 2
##   program pos_review
##   <chr>        <int>
## 1 No            1134
## 2 Yes           1128
```

Conclusions: 

Based on the program summary table, the program was not really effective. The total sales before the program was larger than the total sales during the program. Additionally, based on the positive review summary table, it seemed like the positive review got a little worse with the program.

## NYC Dataset

The purpose of this guided project is to expand upon data analyst processes of:

  - Consulting metadata to learn about the contents of datasets and plan data cleaning tasks.

  - Using correlation analysis to quantify the strength of relationships between variables.

This guided used data on parent, student, and teacher perceptions of New York City schools collected using surveys.

The code for the data is below:


```r
# Load libraries
library(readr)
library(dplyr)
library(stringr)
library(purrr)
library(tidyr)
library(ggplot2)

# Import data
combined <- read_csv("combined.csv") 
```

```
## Rows: 479 Columns: 30
## ── Column specification ────────────────────────────────────────────────────────────────────────────────────────────────────
## Delimiter: ","
## chr  (3): DBN, school_name, boro
## dbl (27): Num of SAT Test Takers, SAT Critical Reading Avg. Score, SAT Math Avg. Score, SAT Writing Avg. Score, avg_sat_...
## 
## ℹ Use `spec()` to retrieve the full column specification for this data.
## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
```

```r
survey <- read_tsv("masterfile11_gened_final.txt")
```

```
## Rows: 1646 Columns: 1942
## ── Column specification ────────────────────────────────────────────────────────────────────────────────────────────────────
## Delimiter: "\t"
## chr    (5): dbn, bn, schoolname, studentssurveyed, schooltype
## dbl (1904): d75, highschool, rr_s, rr_t, rr_p, N_s, N_t, N_p, nr_s, nr_t, nr_p, saf_p_11, com_p_11, eng_p_11, aca_p_11, ...
## lgl   (33): p_q1, p_q3d, p_q9, p_q10, p_q12aa, p_q12ab, p_q12ac, p_q12ad, p_q12ba, p_q12bb, p_q12bc, p_q12bd, t_q6m, t_q...
## 
## ℹ Use `spec()` to retrieve the full column specification for this data.
## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
```

```r
survey_d75 <- read_tsv("masterfile11_d75_final.txt")
```

```
## Rows: 56 Columns: 1773
## ── Column specification ────────────────────────────────────────────────────────────────────────────────────────────────────
## Delimiter: "\t"
## chr    (5): dbn, bn, schoolname, studentssurveyed, schooltype
## dbl (1739): d75, highschool, rr_s, rr_t, rr_p, N_s, N_t, N_p, nr_s, nr_t, nr_p, saf_p_11, com_p_11, eng_p_11, aca_p_11, ...
## lgl   (29): p_q5, p_q9, p_q13a, p_q13b, p_q13c, p_q13d, p_q14a, p_q14b, p_q14c, p_q14d, t_q11a, t_q11b, t_q14, t_q15a, t...
## 
## ℹ Use `spec()` to retrieve the full column specification for this data.
## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
```

```r
# Filter `survey` data to include only high schools and select columns needed for analysis based on the data dictionary
survey_select <- survey %>%
  filter(schooltype == "High School") %>%
  select(dbn:aca_tot_11)

# Select columns needed for analysis from `survey_d75`
survey_d75_select <- survey_d75 %>%       
  select(dbn:aca_tot_11)

# Combine `survey` and `survey_d75` data frames
survey_total <- survey_select %>% 
  bind_rows(survey_d75_select)

# Rename `survey_total` variable `dbn` to `DBN` so can use as key to join with the `combined` data frame
survey_total <- survey_total %>%
  rename(DBN = dbn)

# Join the `combined` and `survey_total` data frames
combined_survey <- combined %>%
  left_join(survey_total, by = "DBN")

# Create a correlation matrix to look for interesting relationships between pairs of variables in `combined_survey` and convert it to a tibble
cor_mat <- combined_survey %>%    ## interesting relationshipsS
  select(avg_sat_score, saf_p_11:aca_tot_11) %>%
  cor(use = "pairwise.complete.obs")

cor_tib <- cor_mat %>%
  as_tibble(rownames = "variable")

# Look for correlations of other variables with `avg_sat_score` that are greater than 0.25 or less than -0.25
strong_cors <- cor_tib %>%
  select(variable, avg_sat_score) %>%
  filter(avg_sat_score > 0.25 | avg_sat_score < -0.25)

# Make scatter plots of those variables with `avg_sat_score` to examine relationships more closely
create_scatter <- function(x, y) {     
  ggplot(data = combined_survey) + 
    aes_string(x = x, y = y) +
    geom_point(alpha = 0.3) +
    theme(panel.background = element_rect(fill = "white"))
}

x_var <- strong_cors$variable[2:5]
y_var <- "avg_sat_score"
  
map2(x_var, y_var, create_scatter)
```

```
## [[1]]
```

```
## Warning: Removed 137 rows containing missing values (`geom_point()`).
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1.png)

```
## 
## [[2]]
```

```
## Warning: Removed 139 rows containing missing values (`geom_point()`).
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-2.png)

```
## 
## [[3]]
```

```
## Warning: Removed 139 rows containing missing values (`geom_point()`).
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-3.png)

```
## 
## [[4]]
```

```
## Warning: Removed 137 rows containing missing values (`geom_point()`).
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-4.png)

```r
# Reshape the data so that you can investigate differences in student, parent, and teacher responses to survey questions
combined_survey_gather <- combined_survey %>%
   gather(key = "survey_question", value = score, saf_p_11:aca_tot_11)

combined_survey_gather <- combined_survey %>%
  pivot_longer(cols = saf_p_11:aca_tot_11,
               names_to = "survey_question",
               values_to = "score")

# Use `str_sub()` to create new variables, `response_type` and `question`, from the `survey_question` variable
combined_survey_gather <- combined_survey_gather %>%
  mutate(response_type = str_sub(survey_question, 4, 6)) %>%   
  mutate(question = str_sub(survey_question, 1, 3))

# Replace `response_type` variable values with names "parent", "teacher", "student", "total" using `if_else()` function
combined_survey_gather <- combined_survey_gather %>%
  mutate(response_type = ifelse(response_type  == "_p_", "parent", 
                                ifelse(response_type == "_t_", "teacher",
                                       ifelse(response_type == "_s_", "student", 
                                              ifelse(response_type == "_to", "total", "NA")))))

# Make a boxplot to see if there appear to be differences in how the three groups of responders (parents, students, and teachers) answered the four questions
combined_survey_gather %>%
  filter(response_type != "total") %>%
  ggplot(aes(x = question, y = score, fill = response_type)) +
  geom_boxplot()
```

```
## Warning: Removed 1268 rows containing non-finite values (`stat_boxplot()`).
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-5.png)
