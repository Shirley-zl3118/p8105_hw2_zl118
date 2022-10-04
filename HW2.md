HW2
================
Shirley Liang
2022-09-26

Setup for HW2

### Problem 1

``` r
transit <- read_csv("NYC_Transit_Subway_Entrance_And_Exit_Data.csv", col_types = cols(Route8 = "c", Route9 = "c", Route10 = "c", Route11 = "c")) %>% 
  janitor::clean_names() %>% 
  select(line, station_name, station_latitude, station_longitude, starts_with("route"), entry, exit_only, vending, entrance_type, ada) %>% 
  mutate(entry = ifelse(entry == "YES", TRUE, FALSE))

transit %>% 
  select(station_name, line) %>% 
  distinct()
```

    ## # A tibble: 465 × 2
    ##    station_name             line    
    ##    <chr>                    <chr>   
    ##  1 25th St                  4 Avenue
    ##  2 36th St                  4 Avenue
    ##  3 45th St                  4 Avenue
    ##  4 53rd St                  4 Avenue
    ##  5 59th St                  4 Avenue
    ##  6 77th St                  4 Avenue
    ##  7 86th St                  4 Avenue
    ##  8 95th St                  4 Avenue
    ##  9 9th St                   4 Avenue
    ## 10 Atlantic Av-Barclays Ctr 4 Avenue
    ## # … with 455 more rows

``` r
transit %>% 
  filter(ada == TRUE) %>% 
  select(station_name, line) %>% 
  distinct()
```

    ## # A tibble: 84 × 2
    ##    station_name                   line           
    ##    <chr>                          <chr>          
    ##  1 Atlantic Av-Barclays Ctr       4 Avenue       
    ##  2 DeKalb Av                      4 Avenue       
    ##  3 Pacific St                     4 Avenue       
    ##  4 Grand Central                  42nd St Shuttle
    ##  5 34th St                        6 Avenue       
    ##  6 47-50th Sts Rockefeller Center 6 Avenue       
    ##  7 Church Av                      6 Avenue       
    ##  8 21st St                        63rd Street    
    ##  9 Lexington Av                   63rd Street    
    ## 10 Roosevelt Island               63rd Street    
    ## # … with 74 more rows

``` r
transit %>% 
  filter(vending == "NO") %>% 
  pull(entry) %>% 
  mean
```

    ## [1] 0.3770492

``` r
transit %>% 
  pivot_longer(route1:route11, names_to = "route_num", values_to = "route") %>% 
  filter(route == "A") %>% 
  select(station_name, line) %>% 
  distinct()
```

    ## # A tibble: 60 × 2
    ##    station_name                  line           
    ##    <chr>                         <chr>          
    ##  1 Times Square                  42nd St Shuttle
    ##  2 125th St                      8 Avenue       
    ##  3 145th St                      8 Avenue       
    ##  4 14th St                       8 Avenue       
    ##  5 168th St - Washington Heights 8 Avenue       
    ##  6 175th St                      8 Avenue       
    ##  7 181st St                      8 Avenue       
    ##  8 190th St                      8 Avenue       
    ##  9 34th St                       8 Avenue       
    ## 10 42nd St                       8 Avenue       
    ## # … with 50 more rows

``` r
transit %>% 
  pivot_longer(route1:route11, names_to = "route_num", values_to = "route") %>% 
  filter(route == "A", ada == TRUE) %>% 
  select(station_name, line) %>% 
  distinct()
```

    ## # A tibble: 17 × 2
    ##    station_name                  line            
    ##    <chr>                         <chr>           
    ##  1 14th St                       8 Avenue        
    ##  2 168th St - Washington Heights 8 Avenue        
    ##  3 175th St                      8 Avenue        
    ##  4 34th St                       8 Avenue        
    ##  5 42nd St                       8 Avenue        
    ##  6 59th St                       8 Avenue        
    ##  7 Inwood - 207th St             8 Avenue        
    ##  8 West 4th St                   8 Avenue        
    ##  9 World Trade Center            8 Avenue        
    ## 10 Times Square-42nd St          Broadway        
    ## 11 59th St-Columbus Circle       Broadway-7th Ave
    ## 12 Times Square                  Broadway-7th Ave
    ## 13 8th Av                        Canarsie        
    ## 14 Franklin Av                   Franklin        
    ## 15 Euclid Av                     Fulton          
    ## 16 Franklin Av                   Fulton          
    ## 17 Howard Beach                  Rockaway

------------------------------------------------------------------------

### Problem 2

##### For both “Mr. Trash Wheel” and “Professor Trash Wheel” sheet: Read file and omit non-data entries, omit rows that do not include dumpster-specific data, round the number of sports balls to the nearest integer and converts the result to an integer variable

``` r
importMtrashwheel <- read_excel("Trash Wheel Collection Data.xlsx", sheet = "Mr. Trash Wheel", range = "A2:N535") %>% 
  janitor::clean_names()

mtrashwheel <- mutate(importMtrashwheel, dumpster = as.numeric(dumpster)) %>%
  mutate(year = as.numeric(year))

mtrashwheel <- drop_na(mtrashwheel, dumpster) %>%
  mutate(sports_balls = round(sports_balls)) %>%
  mutate(sports_balls = as.integer(sports_balls)) 

ptrashwheel <- read_excel("Trash Wheel Collection Data.xlsx", sheet = "Professor Trash Wheel", range = "A2:N117") %>% 
  janitor::clean_names()
```

    ## New names:
    ## • `` -> `...14`

------------------------------------------------------------------------

##### Merge “Mr. Trash Wheel” and “Professor Trash Wheel” into “Trash Wheel” dataset.

``` r
mtrashwheel <- mutate(mtrashwheel, ID = row_number(dumpster))

ptrashwheel <- mutate(ptrashwheel, ID = row_number(dumpster)+453)

trashwheel <- bind_rows(mtrashwheel, ptrashwheel)
```

##### There are 533 dumpster observations in the “Mr. Trash Wheel” sheet and 115 dumpster observations in the “Professor Trash Wheel” sheet. After merging the two sheets, there are 648 dumpster observations in the recording sheet “Trash Wheel”. Each obseration include information about dumpster, month, year, date, weight_tons, volume_cubic_yards, plastic_bottles, polystyrene, cigarette_butts, glass_bottles, grocery_bags, chip_bags, sports_balls, homes_powered, ID, x14. The total weight of trash collected is 2087.49. The total number of sports balls in 2020 is NA.

### Problem 3

#### Clean pols-month.csv dataset, seprate dates, change months into text, create “president” vairable, and remove unwanted variables.

``` r
polsmonth <- read.csv("pols-month.csv") 

polsmonth <- separate(polsmonth, col = mon, into = c('Year','Month','Day'), sep = '-') %>% mutate(polsmonth, Month = as.numeric(Month)) %>% mutate(polsmonth, Year = as.numeric(Year))

polsmonth <- polsmonth %>% mutate(Month = case_when(
  Month == 01 ~ "January", 
  Month == 02 ~ "February",
  Month == 03 ~ "March",
  Month == 04 ~ "April",
  Month == 05 ~ "May",
  Month == 06 ~ "June",
  Month == 07 ~ "July",
  Month == 08 ~ "August",
  Month == 09 ~ "Sepember",
  Month == 10 ~ "October",
  Month == 11 ~ "November",
  Month == 12 ~ "December"
  )) %>%
mutate(president = prez_dem + prez_gop) %>%
subset(select = -c(prez_dem, prez_gop, Day, mon))
```

#### Clean snp.csv dataset, seprate dates, change months into text,, and remove unwanted variables.

``` r
snp <- read.csv("snp.csv") 

snp <- separate(snp, col = date, into = c('Month','Day','Year'), sep = '/') %>% mutate(snp, Month = as.numeric(Month)) %>% mutate(snp, Year = as.numeric(Year))

snp <- snp %>% mutate(Month = case_when(
  Month == 01 ~ "January", 
  Month == 02 ~ "February",
  Month == 03 ~ "March",
  Month == 04 ~ "April",
  Month == 05 ~ "May",
  Month == 06 ~ "June",
  Month == 07 ~ "July",
  Month == 08 ~ "August",
  Month == 09 ~ "Sepember",
  Month == 10 ~ "October",
  Month == 11 ~ "November",
  Month == 12 ~ "December"
  )) %>%
subset(select = -c(Day, date))

snp <- snp %>% mutate(addyear = case_when(
  Year <= 22 ~ 2000,
  Year > 22 ~ 1900 )) %>% 
mutate(Year = Year + addyear) %>%
subset(select = -c(addyear))
```

#### Clean unemployment.csv dataset, seprate dates, change months into text,, and remove unwanted variables.

``` r
unemp <- read.csv("unemployment.csv") 

unemp <- pivot_longer(unemp, Jan:Dec, names_to = "Month", values_to = "Percentage")

unemp <- unemp %>% mutate(Month = case_when(
  Month == "Jan" ~ "January", 
  Month == "Feb" ~ "February",
  Month == "Mar" ~ "March",
  Month == "Apr" ~ "April",
  Month == "May" ~ "May",
  Month == "Jun" ~ "June",
  Month == "Jul" ~ "July",
  Month == "Aug" ~ "August",
  Month == "Sep" ~ "Sepember",
  Month == "Oct" ~ "October",
  Month == "Nov" ~ "November",
  Month == "Dec" ~ "December"
  )) 
```

#### Merge polsmonth, snp , and unemp datasets.

``` r
mergeset <- left_join(polsmonth, snp, by = c("Year", "Month")) %>%
            left_join(unemp, by = c("Year", "Month"))
```

The pols-month dataset includes data related to the number of national
politicians who are democratic or republican at a given time. After
cleaning, it includes 822 observations and the following variables:
Year, Month, gov_gop, sen_gop, rep_gop, gov_dem, sen_dem, rep_dem,
president.

The snp dataset includes Standard & Poor’s stock market index that
represent stock market as whole. After cleaning, it includes 787
observations and the following variables: Month, Year, close.

The unemployment dataset includes percentage of unemployment in each
month of the associated year. After cleaning, it includes 816
observations and the following variables: Year, Month, Percentage.

The merged dataste contains 822 observations and the following
variables: Year, Month, gov_gop, sen_gop, rep_gop, gov_dem, sen_dem,
rep_dem, president, close, Percentage (11 variables). The dataset
includes data range from year between 1947 and 2015.
