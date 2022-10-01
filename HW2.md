HW2
================
Shirley Liang
2022-09-26

------------------------------------------------------------------------

### Problem 2

##### For both “Mr. Trash Wheel” and “Professor Trash Wheel” sheet: Read file and omit non-data entries, omit rows that do not include dumpster-specific data, round the number of sports balls to the nearest integer and converts the result to an integer variable

``` r
importMtrashwheel <- read_excel("Trash-Wheel-Collection-Totals-7-2020-2.xlsx", sheet = "Mr. Trash Wheel", range = "A2:N535") %>% 
  janitor::clean_names()

mtrashwheel <- mutate(importMtrashwheel, dumpster = as.numeric(dumpster))
```

    ## Warning in mask$eval_all_mutate(quo): NAs introduced by coercion

``` r
mtrashwheel <- drop_na(mtrashwheel, dumpster) %>%
  mutate(sports_balls = round(sports_balls)) %>%
  mutate(sports_balls = as.integer(sports_balls)) 

importPtrashwheel <- read_excel("Trash-Wheel-Collection-Totals-7-2020-2.xlsx", sheet = "Professor Trash Wheel", range = "A2:N117") %>% 
  janitor::clean_names()

ptrashwheel <- importPtrashwheel %>% 
  filter(!is.na(dumpster)) %>%
  mutate(sports_balls = round(sports_balls), as.integer(sports_balls)) 
```

------------------------------------------------------------------------

##### Merge “Mr. Trash Wheel” and “Professor Trash Wheel” into “Trash Wheel” dataset.

``` r
mtrashwheel <- mutate(mtrashwheel, ID = row_number(dumpster))

ptrashwheel <- mutate(ptrashwheel, ID = row_number(dumpster)+453)

trashwheel = bind_rows(mtrashwheel, ptrashwheel)
```

##### There are 453 dumpster observations in the “Mr. Trash Wheel” sheet and 71 dumpster observations in the “Professor Trash Wheel” sheet. After merging the two sheets, there are 524 dumpster observations in the recording sheet “Trash Wheel”. Each obseration include information about dumpster, month, year, date, weight_tons, volume_cubic_yards, plastic_bottles, polystyrene, cigarette_butts, glass_bottles, grocery_bags, chip_bags, sports_balls, homes_powered, ID, as.integer(sports_balls). The total weight of trash collected is 1585.2. The total number of sports balls in 2020 is 856.

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
mergeset <- inner_join(polsmonth, snp, by = c("Year", "Month")) %>%
            inner_join(unemp, by = c("Year", "Month"))
```
