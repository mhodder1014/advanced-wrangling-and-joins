##########################################
# Maya Hodder
# Dr. Jeremiah Blondeau
# Rstudio and GIS
# 11 Sept 2025 

library(tidyverse)

density <- read_csv("PSU_Fish_Density.csv")
taxa    <- read_csv("taxonomic_data.csv")

#1. Add the common name (only common name) of each species in the density data set as a new field. Reorder the columns to be more readable.

density <- density %>%
  left_join(taxa %>% select(SPECIES_CD, COMNAME),
            by = "SPECIES_CD")


#2. Add a new field to the density data set named “fishery_target” at set all to TRUE

density <- density %>%
  mutate(fishery_target = TRUE)



#3. Add a new field named “group” at set the value based on fish species where…


density <- density %>%
  mutate(group = case_when(
    COMNAME %in% c("Coney", "Red hind") ~ "grouper",
    COMNAME %in% c("Mutton snapper", "Gray snapper", "Yellowtail snapper") ~ "snapper",
    COMNAME %in% c("Stoplight parrotfish") ~ "parrotfish",
    COMNAME %in% c("Triggerfish", "Hogfish") ~ "other",
    TRUE ~ "other"
  ))



#4. Using the group_by function, how many unique PRIMARY_SAMPLE_UNITS were sampled in each YEAR?

density %>%
  group_by(YEAR) %>%
  summarize(unique_psu = n_distinct(PRIMARY_SAMPLE_UNIT))

#In 2017, there were 237 samples. In 2019, there were 322 samples. In 2021, there were 165 samples. 


  
#4. How many unique PRIMARY_SAMPLE_UNITS were sampled in each YEAR and PROT combination?

density %>%
  group_by(YEAR, PROT) %>%
  summarize(unique_psu = n_distinct(PRIMARY_SAMPLE_UNIT))

#In 2017, 144 open sites (PROT = 0) and 93 protected sites (PROT = 1). In 2019, 215 open sites and 107 protected sites. In 2021, 215 open sites and 41 protected sites. 

  
#5. How many unique PRIMARY_SAMPLE_UNITS were sampled in each YEAR, PROT and STRAT combination?

density %>%
  group_by(YEAR, PROT, STRAT) %>%
  summarize(unique_psu = n_distinct(PRIMARY_SAMPLE_UNIT))



  
#6. What is the difference between the following two expressions? The summarize and mutate calls after the group_by do very different things. In what situations would you use them?
  
  data %>% # data here is the “PSU_Fish_Density” data set
  group_by(YEAR) %>%
  summarize( n = n_distinct(PRIMARY_SAMPLE_UNIT))
  
#

data %>%
  group_by(YEAR) %>%
  mutate( n = n_distinct(PRIMARY_SAMPLE_UNIT))

Create a new dataframe that shows mean density of each species per year…hint

YEAR	SPECIES_CD	meanDensity
2017	BAL VETU	0.224
2019	CEP FULV	0.685
Create a new dataframe that shows mean density of each species in each PROT per year…hint

YEAR	SPECIES_CD	PROT	meanDensity
2017	BAL VETU	0	0.229
2019	CEP FULV	1	0.308
In the “PSU_Fish_Density.csv” data set, the PROT field refers to sites that are inside the VI National Park (PROT = 1) and sites that are outside the Park (PROT = 0). How many of the 4 groups had higher densities inside the National Park?