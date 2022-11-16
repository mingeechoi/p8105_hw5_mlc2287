Homework 5
================
Mingee Choi
11/16/2022

# Problem 2

Description of raw data:

``` r
homicide_data=
  read_csv("./data/homicide-data.csv")%>%
  janitor::clean_names()%>%
  mutate(reported_date = lubridate::ymd(reported_date))%>%
  drop_na(reported_date)
```

    ## Rows: 52179 Columns: 12
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr (9): uid, victim_last, victim_first, victim_race, victim_age, victim_sex...
    ## dbl (3): reported_date, lat, lon
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

The `homicide_data` has 52177 observations and 12 variables and tells us
about homicides in 50 large U.S. cities a given year from years
2007-01-01 to 2017-12-31. The variables include victim’s first name,
victim’s second name, victim’s age, victim’s sex, city, state, latitude,
longitude, and disposition.

# Problem 3
