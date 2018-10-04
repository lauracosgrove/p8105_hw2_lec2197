Homework 2
================
Laura Cosgrove
9/29/2018

``` r
library(tidyverse)
options(tibble.print_min = 5)
```

Problem 1
---------

*Import data. Retain line, station, name, station latitude / longitude, routes served, entry, vending, entrance type, and ADA compliance.*

``` r
subway_data <- read_csv(file = "./data/NYC_Transit_Subway_Entrance_And_Exit_Data.csv") %>% 
  janitor::clean_names() %>% 
  select(line:entry, vending, starts_with("ada")) %>% 
  mutate(entry = ifelse(entry == "YES", TRUE, FALSE))
```

*Write a short paragraph about this dataset – explain briefly what variables the dataset contains, describe your data cleaning steps so far, and give the dimension (rows x columns) of the resulting dataset. Are these data tidy?*

I imported data, cleaned up the names, and selected some specific variables of interest: line, station, name, station latitude / longitude, routes served, entry, vending, entrance type, and ADA compliance. I kept the `ada_notes` variable, as from a quick viewing it looks to contain some important information for some entrances/exits. I also recoded the character vector indicating the status of the `entry` variable -- i.e., whether there is entry allowed in that entrance/exit -- to a logical vector.

The dataset describes the entry and exit data for the NYC subway; it has at minimum one row per entrance/exit. For each row, the dataset contains identifier information for the particular station the entry or exit gives access to: its name, and geographic location data (latitude, longitude). It also gives information about the particular line and route(s) a rider will find at the station the entry or exit gives access to: the line column in combination with the route1 through route11 columns. The dimensions of the subway data are 1868, 20. Eleven columns, so far, are dedicated to reserving space for presence of up to 11 routes for a particular entry or exit.

This data is not tidy; the columns route1 through route11 have variable information in their titles, i.e., the count of how many routes are served by that particular line.

### Count of distinct stations

*Note that stations are identified both by name and by line (e.g. 125th St A/B/C/D; 125st 1; 125st 4/5); the distinct function may be useful here.*

This count is complicated by two features of the MTA: redundant use of station names (e.g., 7th Ave in Midtown and 7th Ave in Park Slope), and the existence of station complexes (e.g., 42nd St Times Square or 14th St Union Square).

``` r
#First count by distinct station names x lines
dim(line_count <- subway_data %>% 
  distinct(station_name, line))
```

    ## [1] 465   2

``` r
# 465 observations

# What if we compare with counting by distinct geographic data?
dim(geo_count <- subway_data %>% 
  distinct(station_latitude, station_longitude))
```

    ## [1] 472   2

``` r
# 472 observations

# What if we compare with counting by the station name in combination with route1, as proxy for line?
dim(subway_data %>% 
  distinct(station_name, route1))
```

    ## [1] 450   2

``` r
# 450 observations
```

Using three different methods of counting, we get three different answers. First, looking at the data, we find using route1 as a proxy for line likely gives an underestimation, because it looks like there's some misattribution of routes to particular lines, as below:

``` r
ex_route_mistake <- filter(subway_data, station_name == "14th St", route1 == "F") %>% 
    select(line, station_name, route1)
knitr::kable(head(ex_route_mistake, 1))
```

| line     | station\_name | route1 |
|:---------|:--------------|:-------|
| 6 Avenue | 14th St       | F      |

``` r
knitr::kable(tail(ex_route_mistake, 1))
```

| line             | station\_name | route1 |
|:-----------------|:--------------|:-------|
| Broadway-7th Ave | 14th St       | F      |

Checking the wikipedia entry for the [14th st/ 6th ave station complex](https://en.wikipedia.org/wiki/14th_Street/Sixth_Avenue_(New_York_City_Subway)) shows us that the F shouldn't be a route associated with the Broadway-7th Ave line. So, we can tentatively discard that estimation.

To understand which of the two remaining counts to trust, let's use some anti-joins on our tibbles created by filtering by distinct observations.

``` r
line_count <- subway_data %>% 
  distinct(station_name, line, .keep_all = TRUE)
# 465 observations

geo_count <- subway_data %>% 
  distinct(station_latitude, station_longitude, .keep_all = TRUE)
# 472 observations

anti_join(geo_count, line_count) %>% 
  select(line:station_longitude) %>% 
  knitr::kable()
```

    ## Joining, by = c("line", "station_name", "station_latitude", "station_longitude", "route1", "route2", "route3", "route4", "route5", "route6", "route7", "route8", "route9", "route10", "route11", "entrance_type", "entry", "vending", "ada", "ada_notes")

| line      | station\_name               |  station\_latitude|  station\_longitude|
|:----------|:----------------------------|------------------:|-------------------:|
| Canarsie  | Canarsie - Rockaway Parkway |           40.64576|           -73.90250|
| Canarsie  | Canarsie - Rockaway Parkway |           40.64596|           -73.90174|
| Lexington | Brooklyn Bridge-City Hall   |           40.71272|           -74.00497|
| Lexington | Brooklyn Bridge-City Hall   |           40.71233|           -74.00439|
| Lexington | Brooklyn Bridge-City Hall   |           40.71186|           -74.00511|
| Lexington | Brooklyn Bridge-City Hall   |           40.71182|           -74.00506|
| Lexington | Brooklyn Bridge-City Hall   |           40.71381|           -74.00391|
| Lexington | Brooklyn Bridge-City Hall   |           40.71389|           -74.00365|
| Lexington | Brooklyn Bridge-City Hall   |           40.71307|           -74.00413|
| Nassau    | Chambers St                 |           40.71419|           -74.00320|

``` r
# 10 observations not in line_count, or counting by distinct name x line, but present in geo_count, or counting by distinct geographical coordinates.
```

Viewing this anti-join, I note that there are some stations with distinct geographical coordinates that are identical stations. This is likely because the data are row-listed by entrance and exit observations. This means that if a station is large enough, there would be different geographical coordinates for the same station. So, the `distinct` operation for `line_count` correctly threw out these observations when counting unique stations.

``` r
anti_join(line_count, geo_count) %>% 
  select(line:station_longitude) %>% 
  knitr::kable()
```

    ## Joining, by = c("line", "station_name", "station_latitude", "station_longitude", "route1", "route2", "route3", "route4", "route5", "route6", "route7", "route8", "route9", "route10", "route11", "entrance_type", "entry", "vending", "ada", "ada_notes")

| line         | station\_name |  station\_latitude|  station\_longitude|
|:-------------|:--------------|------------------:|-------------------:|
| 4 Avenue     | Pacific St    |           40.68367|           -73.97881|
| Coney Island | Stillwell Av  |           40.57742|           -73.98123|
| Coney Island | West 8th St   |           40.57613|           -73.97594|

``` r
# 3 observations not in geo_count but present in line_count
```

Viewing the three observations in this second anti-join, I note that "Pacific St" is the old name for Atlantic Avenue-Barclays Center. This is the same station as the renamed station at the same location, so the `distinct` operation for `geo_count` correctly threw out that observation when counting unique stations.

The two Coney Island observations that were thrown out in `geo_count` are very interesting! The Stillwell Av and West 8th St stops in Coney Island are unusually tall stations that serve two lines - Coney Island and Brighton Beach lines. For each respective stop, there's identical geographic data for the distinct lines. That this is only true for these stops might be a function of their being so tall: the platforms are stacked right on top of one another. I'm not quite sure whether this means that they are each functionally a single station, but I'm going to go with the principle of unique lines meaning unique stations at a given stop and decide that `geo_count` incorrectly threw out these observation when counting unique stations.

In conclusion, after identifying these inaccuracies, the most accurate unique station count for this dataset would be the dimension of `line_count` minus 1, or the dimension of `geo_count` minus 10 and plus 2. These are both the same value: **464** unique stations.

### ADA compliant station counts

Now that we've gotten an accurate station count, we're curious about how many of those stations are accessible by an ADA-compliant entrance/exit. (This picture will probably be dim.)

We saw earlier that counting distinct stations by distinct `line` and `station_name` is an imperfect count due to the Pacific Av hiccup. My first step will be to simply modify that observation, given that it seems like an honest mistake in the data. After this step, we should be able to count distinct stations by `line` and `station_name` without worrying.

``` r
subway_data <- subway_data %>% 
  mutate(
    station_name = replace(station_name, station_name == "Pacific St", "Atlantic Av-Barclays Ctr")
  )
```

Since we're concerned with ADA accessibility by station rather than by entrance/exit, since many stations have an ADA compliant entrance in addition to non-ADA compliance entrances, and since we have no control over which value of the ADA variable our `distinct` operation will keep when throwing out observations, we will need to create an interim dataset with a column asking whether there is any ADA compliant entrance at that station.

``` r
# First look at what the sum is over all observations for a reference, and figure out if there's any missing values
sum(subway_data$ada, na.rm = TRUE)
```

    ## [1] 468

``` r
sum(subway_data$ada)
```

    ## [1] 468

``` r
# There are no NAs in the ada variable, so we don't worry about it. 

# Group by station name and line to add a new ada_any variable by unique station, then make a new dataframe filtering on distinct stations to get the number by unique stations.

subway_data <- subway_data %>% 
  group_by(station_name, line) %>% 
  mutate(ada_count = sum(ada)) %>% 
  mutate(ada_any = ifelse(ada_count > 0, TRUE, FALSE))

subway_data_by_station <- subway_data %>% 
  distinct(line, station_name, .keep_all = TRUE)

table(subway_data_by_station$ada_any)
```

    ## 
    ## FALSE  TRUE 
    ##   381    83

There are **83** stations that are ADA-accessible. That's only 18%.

### No vending, yes entrance

Now, we're interested in calculating the proportion of entry/exits with no vending that allow entrance. At first I thought the `vending` variable meant vending machines for food, probably because I was hungry, but then I remembered reading [this](https://jalopnik.com/the-nyc-subway-metrocard-vending-machines-apparently-re-1827450266) story and realized it meant stations where you can buy a MetroCard.

We won't filter the data by unique stations this time, because either all or almost all stations have MTA vending machines. We're interested in the worrying situation when you've already rushed down the stairs to catch a rapidly-departing train and suddenly realize you have to go find another entrance to fill your already-empty MetroCard, which you swear you just put $40 on. We love NYC!

``` r
subway_data <- subway_data %>% 
  mutate(vending = ifelse(vending == "YES", TRUE, FALSE)) %>% 
  mutate(entry_novending = ifelse(entry == TRUE, 
                                  ifelse(vending == FALSE, TRUE, FALSE),
                                  FALSE)
         )
table(subway_data$entry_novending)
```

    ## 
    ## FALSE  TRUE 
    ##  1799    69

Only 69 entrance/exits, or **3.8%**, have no vending. Good job, MTA.

### Reformatting route data

To make this dataset tidy, we reformat data so that route number and route name are distinct variables.

``` r
#Create route index to preserve the order in which routes were originally listed

subway_data <- subway_data %>% 
  gather(key = "route_index", value = "route_name", starts_with("route")) %>% 
  filter(!is.na(route_name)) %>% 
  separate(route_index, c("empty", "route_index"), "route") %>% 
  select(-empty)
```

### Distinct stations that serve the A train

``` r
dim(subway_data %>% 
  distinct(station_name, line, .keep_all = TRUE) %>% 
  select(station_name, line, route_name) %>% 
  filter(route_name == "A")
  )
```

    ## [1] 60  3

**60** stations serve the A train.

### ADA compliance by A train

``` r
subway_data <- subway_data %>% 
  select(station_name, line, route_name, ada) %>% 
  filter(route_name == "A") %>% 
  group_by(station_name, line) %>% 
  mutate(ada_count = sum(ada)) %>% 
  mutate(ada_any = ifelse(ada_count > 0, TRUE, FALSE)) %>% 
  distinct(line, station_name, .keep_all = TRUE)

table(subway_data$ada_any)
```

    ## 
    ## FALSE  TRUE 
    ##    43    17

Seventeen out of 60 stations, or **28.3%**, of stations that serve the A train have an ADA-compliant entrance.

Problem 2
---------

*Read and clean the Mr. Trash Wheel sheet:*

-   *specify the sheet in the Excel file and to omit columns containing notes (using the range argument and cell\_cols() function)*

-   *use reasonable variable names*

-   *omit rows that do not include dumpster-specific data*

-   *rounds the number of sports balls to the nearest integer and converts the result to an integer variable (using as.integer)*

``` r
mrtrash_data <- readxl::read_excel(path = "./data/HealthyHarborWaterWheelTotals2017-9-26.xlsx",
                                   sheet = 1, range = "A2:N256") %>%
  janitor::clean_names() %>% 
  rename(weight = weight_tons, volume = volume_cubic_yards) %>% 
  drop_na(dumpster) %>% 
  mutate(sports_balls = as.integer(round(sports_balls)))
```

*Read and clean precipitation data for 2016 and 2017. For each, omit rows without precipitation data and add a variable year. Next, combine datasets and convert month to a character variable (the variable month.name is built into R and should be useful).*

``` r
precip_data_2017 <- readxl::read_excel(path = "./data/HealthyHarborWaterWheelTotals2017-9-26.xlsx",
                                   sheet = 3, range = "A2:B14") %>% 
  janitor::clean_names()

precip_data_2016 <- readxl::read_excel(path = "./data/HealthyHarborWaterWheelTotals2017-9-26.xlsx",
                                   sheet = 4, range = "A2:B14") %>% 
  janitor::clean_names() 

precip_data <- left_join(precip_data_2017, precip_data_2016, by = "month") %>% 
  rename("2017" = total.x, "2016" = total.y) %>% 
  gather(key = "year", value = precipitation, "2017","2016") %>%
  mutate(month = recode(month, !!!month.name)) %>% 
  filter(!is.na(precipitation))
```

*Write a paragraph about these data; you are encouraged to use inline R. Be sure to note the number of observations in both resulting datasets, and give examples of key variables. For available data, what was the total precipitation in 2017? What was the median number of sports balls in a dumpster in 2016?*

### Data Interpretation

The Mr. Trash dataset contains data about 215 dumpsters, the dates they were surveyed, and their contents. Each dumpster surveyed collected, taking the meadian as average, about 34000 cigarette butts, 2240 plastic bottles, 2140 (empty) bags of chips, and 13 lost sports balls! The median number of sports balls in a dumpster in 2016 was 26 balls -- a tough year for the athletes of Baltimore.

The precipitation dataset contains data about the precipitation in Baltimore over the years 2016 and 2017 (up to August 2017); 20 months' worth of data is represented. The mean value of inches of precipitation from January - August in 2017 was 3.74 inches, and for the whole of 2016 was 3.33 inches.

The total precipitation in 2017, for the months available in the dataset, was 29.93 inches.

Problem 3
---------

*For this question:*

*\* format the data to use appropriate variable names;*

*\* focus on the “Overall Health” topic*

*\* exclude variables for class, topic, question, sample size, and everything from lower confidence limit to GeoLocation*

*\* structure data so that responses (excellent to poor) are variables taking the value of Data\_value*

*\* create a new variable showing the proportion of responses that were “Excellent” or “Very Good"*

``` r
library(p8105.datasets)
data(brfss_smart2010)

brfss_smart2010 <- brfss_smart2010 %>% 
  janitor::clean_names() %>% 
  rename(state = locationabbr, state_and_county = locationdesc) %>% 
  filter(topic == "Overall Health") %>% 
  select(year:state_and_county, response, data_value) %>% 
  spread(key = response, value = data_value) %>% 
  janitor::clean_names() %>% 
  mutate(excellent_or_very_good = excellent + very_good)
```

*How many unique locations are included in the dataset? Is every state represented? What state is observed the most?*

``` r
dim(brfss_smart2010 %>% 
  distinct(state_and_county))
```

    ## [1] 404   1

``` r
#404 unique locations

dim(brfss_smart2010 %>% 
  distinct(state))
```

    ## [1] 51  1

``` r
#50 unique states (plus the District of Columbia)

# Quick view of what state is observed the most
brfss_smart2010 %>% 
  arrange(state) %>% 
  ggplot(., aes(x = state)) +
    geom_bar() + 
    theme(axis.text.x = element_text(angle = 90, hjust = 1))
```

![](homework2_files/figure-markdown_github/unique%20locations-1.png)

``` r
# It's New Jersey.
```

404 unique locations are represented in the dataset -- these are, roughly, counties, though dependent on the state there are some municipalities, parishes, and other similar mid-sized geographic categories. All 50 states and the District of Columbia are represented. New Jersey is represented most prominently, which means that from 2002-2009, the counties reporting these data in New Jersey were high in number, consistent, or a combination of the two in comparison to other states.

*In 2002, what is the median of the “Excellent” response value?*

``` r
brfss_smart2010 %>% 
  filter(year == 2002) %>% 
  summarise(N = length(excellent),
            count_NA = sum(is.na(excellent)),
            median = median(excellent, na.rm = TRUE)
            ) %>% 
  knitr::kable()
```

|    N|  count\_NA|  median|
|----:|----------:|-------:|
|  157|          2|    23.6|

The median of the "Excellent" response variable in 2002 is **23.6**.

*Make a histogram of “Excellent” response values in the year 2002.*

``` r
library(ggthemes)
brfss_smart2010 %>% 
  filter(year == 2002) %>% 
  ggplot(aes(x = excellent)) +
        geom_histogram() +
        labs(
          title = "Histogram of 'Excellent' Responses, 2002",
          x = "Proportion of 'Excellent' Responses for One Location",
          y = "Number of Responses for Given Proportion"
        ) + 
        theme_bw()
```

    ## Warning: Removed 2 rows containing non-finite values (stat_bin).

![](homework2_files/figure-markdown_github/2002%20excellent%20histogram-1.png)

*Make a scatterplot showing the proportion of “Excellent” response values in New York County and Queens County (both in NY State) in each year from 2002 to 2010.*

``` r
brfss_smart2010 %>% 
  filter(state_and_county == "NY - New York County" | state_and_county == "NY - Queens County") %>%
  ggplot(aes(x = year, y = excellent, color = state_and_county)) +
          geom_point() + 
          geom_smooth(se = FALSE) +
          labs(
            title = "Queens and New York County Performance, Overall Health",
            x = "Year",
            y = "Proportion of 'Excellent' Responses"
          ) + 
          theme_bw()
```

    ## `geom_smooth()` using method = 'loess' and formula 'y ~ x'

![](homework2_files/figure-markdown_github/scatter%20proportion%20excellent-1.png)
