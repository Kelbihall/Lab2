---
title: "Lab 2"
author: "Kelbi Hall"
format: html
execute:
  echo: true
  cache: false
  options:
    repos: "https://cran.r-project.org"
---

```{r}

library(dplyr)


options(repos = c(CRAN = "https://cran.r-project.org"))


tree_dat <- read.csv("data/tree_data.csv")  


glimpse(tree_dat)

```


options(repos = c(CRAN = "https://cran.r-project.org"))


tree_dat <- read.csv("data/tree_data.csv")

glimpse(tree_dat)

options(repos = c(CRAN = "https://cran.r-project.org"))


#1
```{r}
tree_dat %>% 
filter(standID == 1) %>%
  tally()

```
#2
```{r}
tree_count1 <- tree_dat %>%
  filter(standID == 1)

print(tree_count1)
```
#3
```{r}
tree_dat %>% 
  filter(species %in% c("ABBA", "PIST")) %>%
  count(species)
```
#4
```{r}

last_year <- max(tree_dat$year , na.rm = TRUE)

tree_dat %>% 
  filter(year == last_year, age > 200)  %>% 
  tally()
```
#5
```{r}

tree_dat %>%
  slice_max(order_by = age, n =1)
oldest_tree <- tree_dat %>%
  slice_max(order_by = age, n=1)
head(oldest_tree)


```

#6
```{r}

tree_dat %>% 
  filter(year == 2001) %>% 
  slice_max(order_by = age, n = 5)

oldest_tree_2001 <- tree_dat %>%
  filter(year == 2001) %>% 
  slice_max(order_by = age, n=5) 
print(oldest_tree_2001)


```
#7 
```{r}

sampled_tree <- tree_dat %>% 
  slice_sample(prop = 0.3)
sample_size <- nrow(sampled_tree)

```
#8
```{r}

top_trees <- tree_dat %>%
  filter(standID == 5, year == 2007) %>%  
  arrange(desc(rad_ib)) %>%  
  slice_head(n = 3) %>%  
  select(treeID, rad_ib)  

top_trees



```
#9
```{r}
 
smallest_trees_alternate_stand <- tree_dat %>%
  select(treeID, stand, year, rad_ib) %>% 
  filter(stand == "A1", year == 2007) %>%  
  slice_min(order_by = rad_ib, n = 3)  

smallest_trees_alternate_stand



```
#10
```{r}

tree_dat_no_stand <- tree_dat %>%
  select(-stand)  

glimpse(tree_dat_no_stand)

```
#11
```{r}

tree_dat_id_columns <- tree_dat %>%
  select(contains("ID")) 

glimpse(tree_dat_id_columns)

```
#12
```{r}

tree_dat_id_stand_columns <- tree_dat %>%
  select(matches("ID|stand"))  

glimpse(tree_dat_id_stand_columns)

```
#13
```{r}
tree_dat <- tree_dat %>%
  rename(
    rad_ib_cm = rad_ib,  
  )
```
#14
```{r}

tree_dat <- tree_dat %>%
  mutate(
    DBH_cm = rad_ib_cm * 2, 
    BA_m2 = 0.00007854 * (DBH_cm^2)  
  )


glimpse(tree_dat)

```
```{r}

mean_ba_potr_2007 <- tree_dat %>%
  filter(species == "POTR", year == 2007) %>%  
  summarize(mean_BA_m2 = mean(BA_m2, na.rm = TRUE)) 


mean_ba_potr_2007

```
#15
```{r}

tree_dat <- tree_dat %>%
  mutate(
    established = if_else(age > 5, TRUE, FALSE)  
  )

glimpse(tree_dat)


```

```{r}

established_count <- tree_dat %>%
  count(established)


established_count


```
#16
```{r}
# Add DBH_class column using case_when
tree_dat <- tree_dat %>%
  mutate(
    DBH_class = case_when(
      DBH_cm < 10 ~ "Small",
      DBH_cm >= 10 & DBH_cm < 30 ~ "Medium",
      DBH_cm >= 30 & DBH_cm < 50 ~ "Large",
      DBH_cm >= 50 ~ "Extra Large"
    )
  )


glimpse(tree_dat)

```
```{r}

tree_dat <- tree_dat %>%
  mutate(
    DBH_class = case_when(
      DBH_cm < 100 ~ "Small",
      DBH_cm >= 100 & DBH_cm < 200 ~ "Medium",
      DBH_cm >= 200 & DBH_cm < 300 ~ "Large",
      DBH_cm >= 300 ~ "Extra Large"
    )
  )

dbh_class_count_2007 <- tree_dat %>%
  filter(year == 2007) %>%
  count(DBH_class)

dbh_class_count_2007


```
#17 
```{r}

dbh_stats_2007 <- tree_dat %>%
  filter(year == 2007) %>%  
  summarize(
    mean_DBH_cm = mean(DBH_cm, na.rm = TRUE),  
    sd_DBH_cm = sd(DBH_cm, na.rm = TRUE)       
  )


dbh_stats_2007

```
#18
```{r}

species_mean_age_2003 <- tree_dat %>%
  filter(year == 2003) %>%  
  group_by(species) %>%  
  summarize(
    mean_age = mean(age, na.rm = TRUE)   
   
  ) %>%
  arrange(desc(mean_age)) %>%  
  slice_head(n = 3)  


species_mean_age_2003

```
#19
```{r}

year_summary <- tree_dat %>%
  summarize(
    unique_years = n_distinct(year),  
    first_year = min(year, na.rm = TRUE),  
    last_year = max(year, na.rm = TRUE)  
  )


year_summary

```
#20 
```{r}
 
stands_with_most_years <- tree_dat %>%
  group_by(stand) %>%  
  summarize(
    unique_years = n_distinct(year)  
    
  ) %>%
  filter(unique_years == max(unique_years))  


stands_with_most_years

```
#Final
```{r}

annual_growth <- tree_dat %>%
  arrange(treeID, year) %>% 
  group_by(treeID) %>%  
  mutate(
    annual_growth = DBH_cm - lag(DBH_cm)  
    
  ) %>%
  filter(!is.na(annual_growth))


trees_with_10_years <- annual_growth %>%
  group_by(treeID) %>%
  filter(n() >= 10)  


species_growth_summary <- trees_with_10_years %>%
  group_by(species) %>%
  summarize(
    mean_growth = mean(annual_growth, na.rm = TRUE),
    sd_growth = sd(annual_growth, na.rm = TRUE),
    total_trees = n_distinct(treeID)  
    
  ) %>%
  arrange(desc(mean_growth)) 



fastest_species <- species_growth_summary %>% slice_head(n = 3)
slowest_species <- species_growth_summary %>% slice_tail(n = 3)


fastest_species
slowest_species

```
![image](https://github.com/user-attachments/assets/2e6d344c-10a8-400b-86c6-e35fd39e7bd8)
