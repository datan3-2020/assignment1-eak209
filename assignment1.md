Statistical assignment 1
================
Emilia Korobowicz; 143801
02/02/2019

## Open data (10 points)

In this assignment you will work with the individual level data from
wave 8 of the Understanding Society survey. First, you need to open the
data set. Please complete the code
    below.

``` r
library(tidyverse)
```

    ## ── Attaching packages ─────────────────────────────────────── tidyverse 1.3.0 ──

    ## ✓ ggplot2 3.2.1     ✓ purrr   0.3.3
    ## ✓ tibble  2.1.3     ✓ dplyr   0.8.3
    ## ✓ tidyr   1.0.2     ✓ stringr 1.4.0
    ## ✓ readr   1.3.1     ✓ forcats 0.4.0

    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## x dplyr::filter() masks stats::filter()
    ## x dplyr::lag()    masks stats::lag()

``` r
Data <- read_tsv("/Users/emiliakorobowicz/Desktop/DataScience3/EmilysRepo/NewRepo/Data/UKDA-6614-tab/tab/ukhls_w8/h_indresp.tab")
```

    ## Parsed with column specification:
    ## cols(
    ##   .default = col_double()
    ## )

    ## See spec(...) for full column specifications.

Now you have got your data frame stored as Data.

## Select variables (10 points)

The data for Wave 8 of the Understanding Society were collected in
2016-18. Among other things, people were asked the following question:
“Should the United Kingdom remain a member of the European Union or
leave the European Union?” In this assignment, we will explore how
answers to this question depend on sex and age.

First, you need to select the variables for the analysis. You want to
keep the following variables: cross-wave individual identifier (*pidp*),
support for the UK remaining or leaving the EU (*h\_eumem*), sex
(*h\_sex\_dv*), age (*h\_age\_dv*), and sample origin (*h\_memorig*).

Complete the code below to select those variables from the data frame
and save the
result.

``` r
Data <- Data <- Data %>% select(c("h_eumem", "h_sex_dv", "h_age_dv", "h_memorig"))
```

## Filter observations (10 points)

To make nationally representative estimates from the Understanding
Society data we would need to use weight coefficients. There are many
different types of weight coefficients that can be used depending on the
question and the level of analysis (see the User Guide, pp. 65-71). We
will not do this in this assignment. However, what we want to do is to
keep data from the original Understanding Society sample only (ukhls gb
2009-10), dropping data for Northern Ireland, the BHPS cohort members
and ethnic minority boost samples. This will make data closer to be
representative for Great Britain. You need to choose the observations
where *h\_memorig* has the value of 1.

``` r
Data <- Data <- Data %>% filter(h_memorig == 1)
table(Data$h_memorig, exclude = NULL)
```

    ## 
    ##     1 
    ## 22957

## Recode data (20 points)

Let us tabulate the variables for EU support, sex, and age.

``` r
table(Data$h_eumem)
```

    ## 
    ##    -9    -8    -7    -2    -1     1     2 
    ##    33   482   879   354   753 11118  9338

``` r
table(Data$h_sex_dv)
```

    ## 
    ##     0     1     2 
    ##     1 10470 12486

``` r
table(Data$h_age_dv)
```

    ## 
    ##  16  17  18  19  20  21  22  23  24  25  26  27  28  29  30  31  32  33  34  35 
    ## 284 309 290 291 278 295 268 326 287 257 243 234 229 249 274 278 278 293 314 332 
    ##  36  37  38  39  40  41  42  43  44  45  46  47  48  49  50  51  52  53  54  55 
    ## 351 332 321 336 320 327 368 404 372 386 435 465 425 447 406 420 427 414 432 422 
    ##  56  57  58  59  60  61  62  63  64  65  66  67  68  69  70  71  72  73  74  75 
    ## 408 413 416 434 369 398 358 399 354 412 345 358 412 434 431 334 326 293 275 251 
    ##  76  77  78  79  80  81  82  83  84  85  86  87  88  89  90  91  92  93  94  95 
    ## 219 231 211 205 181 162 138 117 117 108  89  78  77  48  41  27  15  18  15   7 
    ##  96  97  98  99 101 102 
    ##   6   2   3   1   1   1

You will see that all these variables are numeric. You can learn what
the numeric codes mean by checking the codebook here:
<https://www.understandingsociety.ac.uk/documentation/mainstage/dataset-documentation/datafile/indresp/wave/8>
.

We want to do the following:

1)  Recode the variable for EU support as binary (1 for Remain, 0 for
    Leave), coding all types of missing values (including refusals and
    “don’t know”) as NA.
2)  Recode sex into a character vector with the values “male” or
    “female”.
3)  Recode age into a variable with the following categories: 16 to 25,
    26 to 40, 41 to 55, 56 to 70, over 70.

In all cases, we want to create new variables.

``` r
Data <- Data %>%
    mutate(EU = ifelse(h_eumem < 0, NA, (ifelse(h_eumem == 2, 0, h_eumem)))) %>%
    
    mutate(sex = ifelse(h_sex_dv == 2, "female", 
                        ifelse(h_sex_dv == 1, "male", 
                               ifelse(h_sex_dv == 0, NA, h_sex_dv)))
           
           
    ) %>%
    mutate(agegr = ifelse(h_age_dv %in% c(16:25), "16 to 25", 
                          ifelse(h_age_dv %in% c(26:40), "26 to 40", 
                                 ifelse(h_age_dv %in% c(41:55), "41 to 55", 
                                        ifelse(h_age_dv %in% c(56:70), "56 to 70", 
                                               ifelse(h_age_dv > 70, "over 70", h_age_dv)))))
    )
```

## Summarise data (20 points)

Let us **dplyr** to calculate how many people in the sample supported
Remain and Leave, both as absolute numbers and percentages.

``` r
Data  %>%
    summarise(N_Remain = sum(na.omit(EU) == 1), 
              P_Remain = 100*(sum(na.omit(EU) == 1)/length(na.omit(EU))), 
              N_Leave = sum(na.omit(EU) == 0), 
              P_Leave = 100*(sum(na.omit(EU) == 0)/length(na.omit(EU)))
    )
```

    ## # A tibble: 1 x 4
    ##   N_Remain P_Remain N_Leave P_Leave
    ##      <int>    <dbl>   <int>   <dbl>
    ## 1    11118     54.4    9338    45.6

Write a couple of sentences with the interpretation of this result. How
this compares with the result of the 2016 referendum? Why?

The result I’ve obtained differs from that of the 2016 referendum when
the UK has voted to leave the EU by 52% to 48%. Here, on the other hand,
the number of Remainers is greater. There are several reasons why my
outcome doesn’t match what happened in reality. Firstly, to make good
estimates, we need a dataset that is nationally representative. The
ideal way to do this is to use weight coefficients and correct, for
example, for minor ethnic groups etc. However, in this case, we have
been asked to not do this. There are several other issues with the
survey itself, which could have had an impact on the obtained result.
Most notably, many responses were invalid, recoded to NA and not
counted. Additionally, the Understanding Society Survey interviews are
carried out face-to-face in respondents’ homes by trained interviewers,
a method prone to the social desirability bias.

## Summarise data by sex and age (30 points)

Now let us look at the support for Leave and Remain by sex and age. Use
your newly created variables.

``` r
Data %>%
  subset(!is.na(sex)) %>%

      group_by(sex, agegr) %>%
    summarise(N_Remain = sum(na.omit(EU) == 1), 
              P_Remain = 100*(sum(na.omit(EU) == 1)/length(na.omit(EU))), 
              N_Leave = sum(na.omit(EU) == 0), 
              P_Leave = 100*(sum(na.omit(EU) == 0)/length(na.omit(EU))),
    )
```

    ## # A tibble: 10 x 6
    ## # Groups:   sex [2]
    ##    sex    agegr    N_Remain P_Remain N_Leave P_Leave
    ##    <chr>  <chr>       <int>    <dbl>   <int>   <dbl>
    ##  1 female 16 to 25      985     72.6     371    27.4
    ##  2 female 26 to 40     1455     64.0     817    36.0
    ##  3 female 41 to 55     1733     56.9    1313    43.1
    ##  4 female 56 to 70     1476     50.5    1448    49.5
    ##  5 female over 70       722     44.2     910    55.8
    ##  6 male   16 to 25      763     66.1     392    33.9
    ##  7 male   26 to 40      985     58.8     691    41.2
    ##  8 male   41 to 55     1280     51.7    1196    48.3
    ##  9 male   56 to 70     1170     47.6    1289    52.4
    ## 10 male   over 70       548     37.6     911    62.4

``` r
Data %>%
  subset(!is.na(sex)) %>%

      group_by(sex) %>%
    summarise(N_Remain = sum(na.omit(EU) == 1), 
              P_Remain = 100*(sum(na.omit(EU) == 1)/length(na.omit(EU))), 
              N_Leave = sum(na.omit(EU) == 0), 
              P_Leave = 100*(sum(na.omit(EU) == 0)/length(na.omit(EU))),
    )
```

    ## # A tibble: 2 x 5
    ##   sex    N_Remain P_Remain N_Leave P_Leave
    ##   <chr>     <int>    <dbl>   <int>   <dbl>
    ## 1 female     6371     56.7    4859    43.3
    ## 2 male       4746     51.4    4479    48.6

Write a couple of sentences interpreting your results

For both males and females, the percentages of Remainers decreases with
age. The highest percentage of Remainers is found in the youngest, 16 to
25, age group. Consequently, the oldest respondents (over 70) display
the lowest percentage of supporters of the UK’s membership in the EU.
Similar patterns have been observed in the actual 2016 EU referendum.
There has been a wide debate about the generational divide over the
issue of Brexit in the mainstream media.

In a separate line of code, I’ve grouped only by sex to see how the two
genders compare, regardless of the age group they belong to. Females are
more supportive of the Remain camp, with 56,7% declaring themselves
pro-EU, compared to only 51,4% of male respondents doing so.
