Homework 5
================
Mingee Choi
11/16/2022

# Problem 2

\####Description of raw data:

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

\####Create variable and summarize Create city_state variable and
summarize within cities to obtain the total number of homicides and the
number of unsolved homicides (those for which disposition is “closed
without arrest” or “open/no arrest”)

``` r
homicide_data%>%
  mutate(city_state=str_c(city,state, sep="_")
  )%>%
  group_by(city_state)%>%
  summarize(total_homicides = n(),  unsolved_homicides = sum(disposition%in%c("Closed without arrest", "Open/No arrest")))%>%
  arrange(desc(total_homicides))%>%
knitr::kable()
```

| city_state        | total_homicides | unsolved_homicides |
|:------------------|----------------:|-------------------:|
| Chicago_IL        |            5535 |               4073 |
| Philadelphia_PA   |            3037 |               1360 |
| Houston_TX        |            2942 |               1493 |
| Baltimore_MD      |            2827 |               1825 |
| Detroit_MI        |            2519 |               1482 |
| Los Angeles_CA    |            2257 |               1106 |
| St. Louis_MO      |            1677 |                905 |
| Dallas_TX         |            1567 |                754 |
| Memphis_TN        |            1514 |                483 |
| New Orleans_LA    |            1434 |                930 |
| Las Vegas_NV      |            1381 |                572 |
| Washington_DC     |            1345 |                589 |
| Indianapolis_IN   |            1322 |                594 |
| Kansas City_MO    |            1190 |                486 |
| Jacksonville_FL   |            1168 |                597 |
| Milwaukee_wI      |            1115 |                403 |
| Columbus_OH       |            1084 |                575 |
| Atlanta_GA        |             973 |                373 |
| Oakland_CA        |             947 |                508 |
| Phoenix_AZ        |             914 |                504 |
| San Antonio_TX    |             833 |                357 |
| Birmingham_AL     |             800 |                347 |
| Nashville_TN      |             767 |                278 |
| Miami_FL          |             742 |                449 |
| Cincinnati_OH     |             694 |                309 |
| Charlotte_NC      |             687 |                206 |
| Oklahoma City_OK  |             672 |                326 |
| San Francisco_CA  |             663 |                336 |
| Pittsburgh_PA     |             631 |                337 |
| New York_NY       |             627 |                243 |
| Boston_MA         |             614 |                310 |
| Tulsa_OK          |             583 |                193 |
| Louisville_KY     |             576 |                261 |
| Fort Worth_TX     |             549 |                255 |
| Buffalo_NY        |             521 |                319 |
| Fresno_CA         |             487 |                169 |
| San Diego_CA      |             461 |                175 |
| Stockton_CA       |             444 |                266 |
| Richmond_VA       |             429 |                113 |
| Baton Rouge_LA    |             424 |                196 |
| Omaha_NE          |             409 |                169 |
| Albuquerque_NM    |             378 |                146 |
| Long Beach_CA     |             378 |                156 |
| Sacramento_CA     |             376 |                139 |
| Minneapolis_MN    |             366 |                187 |
| Denver_CO         |             312 |                169 |
| Durham_NC         |             276 |                101 |
| San Bernardino_CA |             275 |                170 |
| Savannah_GA       |             246 |                115 |
| Tampa_FL          |             208 |                 95 |
| Tulsa_AL          |               1 |                  0 |

# Problem 3
