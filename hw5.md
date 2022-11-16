Homework 5
================
Mingee Choi
11/16/2022

# Problem 2

\###Description of raw data:

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

\###Create variable and summarize Create city_state variable and
summarize within cities to obtain the total number of homicides and the
number of unsolved homicides (those for which disposition is “closed
without arrest” or “open/no arrest”)

``` r
homicide_df=
homicide_data%>%
  mutate(city_state= str_c(city, state, sep=",")
  )%>%
  select(uid,city_state,everything())%>%
  group_by(city_state)%>%
  summarize(total_homicides = n(),  unsolved_homicides = sum(disposition%in%c("Closed without arrest", "Open/No arrest")))%>%
  arrange(desc(total_homicides))
```

\###Use prop.test for city of Baltimore, MD

``` r
baltimore_test=
  homicide_df%>%
  filter(city_state == "Baltimore,MD")%>%
  mutate(prop_test = map2(unsolved_homicides, total_homicides, ~prop.test(.x, .y)%>%
  broom::tidy()))%>%
  unnest()

save(baltimore_test, file="baltimore_test.RData")

  baltimore_test%>%
  select(city_state, estimate, "CI_lower" = conf.low, "CI_upper" = conf.high)
```

    ## # A tibble: 1 × 4
    ##   city_state   estimate CI_lower CI_upper
    ##   <chr>           <dbl>    <dbl>    <dbl>
    ## 1 Baltimore,MD    0.646    0.628    0.663

# Problem 3
