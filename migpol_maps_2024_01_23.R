#Load libraries

library("tidyverse") # load dplyr, ggplot2, stringr
library("sf") # working with geographic simple features in R
library("rnaturalearth") # world map data from Natural Earth
library("countrycode") # get ISO code from country names
library("readxl") # load excel files
library("tools") # compile tex files
library("rnaturalearth") # world map data from Natural Earth
library("stringr") # edit strings of dataframe cells
library(ggplot2)
library(grid)
library(dplyr)
library(haven)
library(readxl)
library(writexl)

rm(list = ls())

######## Below you will find the code used to calculate 
######## the percentage of covered years for each variables. 
######## Please note that we use the data as available in DEMSCORE, not the original data



#### DEMIG ####
#Note: DEMIG does not have a country year format but codes policy changes when they occur
#Maps are thus based on which variables are available within each country.
db_demig <- read_xlsx ("MIGPOL_DEMIG.xlsx")
# Replace 999 with NA in your data frame
db_demig[db_demig == 999] <- NA
#Remove variables not included in maps and those missing iso3 values
db_demig <- db_demig %>%
  select(-country_full_name, -iso2, -year) %>% filter(!is.na(iso3))
#Remove all demig_ additions in front of names 
db_demig <- db_demig %>% rename_all(~gsub("^demig_", "", .))

db_demig <- db_demig %>%
  group_by(iso3) %>%
  summarise(across(everything(), ~sum(!is.na(.)))) %>%
  mutate(across(-iso3, ~./summary * 100))

DEMIG_Policy <- db_demig %>% pivot_longer(!iso3, names_to = "Code", values_to = "percentages")



#### IMPIC RAW ####
db_impic_raw <- read_xlsx ("MIGPOL_IMPIC_RawData.xlsx")
# Replace missing values with NA 
db_impic_raw[db_impic_raw == -8] <- NA
db_impic_raw[db_impic_raw == -9] <- NA
#Remove variables not included in maps
db_impic_raw <- db_impic_raw %>%
  select(-country_full_name, -iso2) %>% 
  filter(!is.na(iso3))
#Remove all impic_ additions in front of names 
db_impic_raw <- db_impic_raw %>% rename_all(~gsub("^impic_", "", .))
#Group by year and iso3, sum everyting that is not missing
db_impic_raw <- db_impic_raw %>% group_by(iso3, year) %>%
  summarise(across(everything(), ~sum(!is.na(.)))) %>%
  ungroup()
# List of columns (other than iso3 and year) to be transformed
cols_to_transform <- setdiff(names(db_impic_raw), c("iso3", "year"))
# Apply the transformation to selected columns; replace 0 with NA and non-zero with 1. 
# This way we can see if variables have an observation in a certain year.
db_impic_raw <- db_impic_raw %>%
  mutate_at(vars(cols_to_transform), ~ifelse(. == 0, NA, 1))
# Sum every variable exlcuding missings grouped by iso3 except for year and divide by .31
# This code calculates the total number of observations of a variable by country over 31 years. 
# By dividing by .31, we get the percentage of years in which data was avaialabe. 
db_impic_raw <- db_impic_raw %>% select(-year) %>% group_by(iso3) %>%
  summarise(across(everything(), ~sum(!is.na(.)))) %>% mutate(across(-iso3, ~./0.31))
#Changes format to have iso3, Code and percentages as columns
IMPIC_RawData <- db_impic_raw %>% pivot_longer(!iso3, names_to = "Code", values_to = "percentages")



#### IMPIC 2016 ####
db_impic_2016 <- read_xlsx ("MIGPOL_IMPIC_2016.xlsx")
#All missing values are already coded as NA
#Remove variables not included in maps and those missing iso3 values
db_impic_2016 <- db_impic_2016 %>%
  select(-country_full_name, -iso2, -year) %>% filter(!is.na(iso3))
#Remove all impic_ additions in front of names 
db_impic_2016 <- db_impic_2016 %>% rename_all(~gsub("^impic_", "", .))
#Group by iso3, sum everything that is not missing and divide by .31.
# This code calculates the total number of observations of a variable by country over 31 years. 
# By dividing by .31, we get the percentage of years in which data was available. 
db_impic_2016 <- db_impic_2016 %>% group_by(iso3) %>%
  summarise(across(everything(), ~sum(!is.na(.))))
db_impic_2016 <- db_impic_2016 %>% mutate(across(-iso3, ~./0.31))
#Changes format to have iso3, Code and percentages as columns
IMPIC_2016 <- db_impic_2016 %>% pivot_longer(!iso3, names_to = "Code", values_to = "percentages")



#### IMPIC Political Rights ####
db_impic_pr <- read_xlsx ("MIGPOL_IMPIC_Political_Rights.xlsx")
# Replace missing values with NA 
db_impic_pr[db_impic_pr == -8] <- NA
db_impic_pr[db_impic_pr == -9] <- NA
#Remove variables not included in maps and those missing iso3 values
db_impic_pr <- db_impic_pr %>%
  select(-country_full_name, -iso2, -year) %>% filter(!is.na(iso3))
#Remove all impic_ additions in front of names 
db_impic_pr <- db_impic_pr %>% rename_all(~gsub("^impic_", "", .))
#Group by iso3, sum everything that is not missing and divide by .31.
# This code calculates the total number of observations of a variable by country over 31 years. 
# By dividing by .31, we get the percentage of years in which data was available. 
db_impic_pr <- db_impic_pr %>% group_by(iso3) %>%
  summarise(across(everything(), ~sum(!is.na(.))))
db_impic_pr <- db_impic_pr %>% mutate(across(-iso3, ~./0.31))
#Changes format to have iso3, Code and percentages as columns
IMPIC_Political_Rights <- db_impic_pr %>% pivot_longer(!iso3, names_to = "Code", values_to = "percentages")



#### Globalcit Country Years ####
db_globalcit <- read_xlsx ("MIGPOL_GLOBALCIT_country_year.xlsx")
#All missing values are already coded as NA
#Remove variables not included in maps and those missing iso3 values
db_globalcit <- db_globalcit %>%
  select(-country_full_name, -iso2, -year) %>% filter(!is.na(iso3))
# Group by iso3, sum everything that is not missing and divide by .63 or .03.
# This code calculates the total number of observations of a variable by country over 63 years for some vairable and 3 years for other variables. 
# By dividing by the total number of years during which a variable was measured, we get the percentage of years in which data was available. 
db_globalcit <- db_globalcit %>% group_by(iso3) %>%
  summarise(across(everything(), ~sum(!is.na(.))))

db_globalcit <- db_globalcit %>%
  mutate(across(
    c(globalcit_A06b_bin, globalcit_A06b_cat, globalcit_L01_bin,
      globalcit_L01_cat, globalcit_L05_bin, globalcit_L05_cat, 
      globalcit_dualcit_comb, globalcit_region), ~ . / 0.63)) %>%
  mutate(across(c(-globalcit_A06b_bin, -globalcit_A06b_cat, -globalcit_L01_bin,
                  -globalcit_L01_cat, -globalcit_L05_bin, -globalcit_L05_cat, 
                  -globalcit_dualcit_comb, -globalcit_region, -iso3), ~./ 0.03))
#Remove all globalcit_ additions in front of names 
db_globalcit <- db_globalcit %>% rename_all(~gsub("^globalcit_", "", .))
db_globalcit$iso3 <- toupper(db_globalcit$iso3)
#Changes format to have iso3, Code and percentages as columns
GLOBALCIT_Country_Year <- db_globalcit %>% pivot_longer(!iso3, names_to = "Code", values_to = "percentages")



#### IMISEM ####
db_imisem <- read_xlsx ("MIGPOL_imisem.xlsx")
#All missing values are already coded as NA
#Remove variables not included in maps and those missing iso3 values
db_imisem <- db_imisem %>% select(-country_full_name, -iso2, -year) %>% filter(!is.na(iso3))
#Remove all imisem_ additions in front of names 
db_imisem <- db_imisem %>% rename_all(~gsub("^imisem_", "", .))
# Group by iso3, sum everything that is not missing and divide by .01. 
db_imisem <- db_imisem %>% group_by(iso3) %>%
  summarise(across(everything(), ~sum(!is.na(.))))

db_imisem <- db_imisem %>% mutate(across(-iso3, ~ . / 0.01)) 
#Changes format to have iso3, Code and percentages as columns
IMISEM <- db_imisem %>% pivot_longer(!iso3, names_to = "Code", values_to = "percentages")


#### MIPEX ####
db_mipex <- read_xlsx ("MIGPOL_MIPEX.xlsx")
# Note: The MIPEX codebook states that h variables should not be measured between 2015-2018.
# However, some countries still have values in these years.
# We did not remove these variables in the datasets, as we wish to stay as close to the original data as possible
# For the codebook, we follow the MIPEX codebook, hence we remove these years for all h variables.
# List of years for which the values should be coded as missing
years_to_code_as_missing <- c(2015, 2016, 2017, 2018)
# List of variables to be coded as missing
variables_to_code_as_missing <- c(
  "mipex_h", "mipex_h145a", "mipex_h148", "mipex_h146a", "mipex_h149",
  "mipex_h147a", "mipex_h150", "mipex_h152c", "mipex_h153c", "mipex_h156a", 
  "mipex_h159", "mipex_h163", "mipex_h165")
# Apply the transformation
db_mipex_transformed <- db_mipex %>%
  mutate(across(all_of(variables_to_code_as_missing), ~ifelse(year %in% years_to_code_as_missing, NA, .)))
#Remove variables not included in maps and those missing iso3 values
db_mipex_transformed <- db_mipex_transformed %>%
  select(-country_full_name, -iso2, -year) %>% filter(!is.na(iso3))
#Group by iso3, sum everything that is not missing and divide by .10, .13 or .03.
# This code calculates the total number of observations of a variable by country over 13, 10 and 3 years for some variable. 
# By dividing by the total number of years during which a variable was measured, we get the percentage of years in which data was available. 
db_mipex_transformed <- db_mipex_transformed %>% group_by(iso3) %>%
  summarise(across(everything(), ~sum(!is.na(.))))

db_mipex_transformed <- db_mipex_transformed %>% 
  mutate(across(
    c(mipex_overall, mipex_h, mipex_h145a, mipex_h148, mipex_h146a, mipex_h149,
      mipex_h147a, mipex_h150, mipex_h152c, mipex_h153c, mipex_h156a, mipex_h159, mipex_h163, mipex_h165), ~ . / 0.03)) %>%
  mutate(across(
    c(mipex_overall_wo_health, mipex_c, mipex_ca45ca47, mipex_ca49, mipex_cb50, 
      mipex_cb51, mipex_cb51a, mipex_cb51b, mipex_cb51c, mipex_cb53, mipex_cd60,
      mipex_cc59cd64, mipex_cc59, mipex_cd64), ~ . / 0.10)) %>%
  mutate(across(
    c(-mipex_overall, -mipex_h, -mipex_h145a, -mipex_h148, -mipex_h146a, -mipex_h149, 
      -mipex_h147a, -mipex_h150, -mipex_h152c, -mipex_h153c, -mipex_h156a, 
      -mipex_h159, -mipex_h163, -mipex_h165, -mipex_overall_wo_health,
      -mipex_c, -mipex_ca45ca47, -mipex_ca49, -mipex_cb50, -mipex_cb51, -mipex_cb51a,
      -mipex_cb51b, -mipex_cb51c, -mipex_cb53, -mipex_cd60, -mipex_cc59cd64, -mipex_cc59,
      -mipex_cd64, -iso3),
    ~ . / 0.13
  ))
#Remove mipex_ addition in front of name
db_mipex <- db_mipex_transformed %>% rename_all(~gsub("^mipex_", "", .))
#Changes format to have iso3, Code and percentages as columns
MIPEX <- db_mipex %>% pivot_longer(!iso3, names_to = "Code", values_to = "percentages")



#### MAP MAKING ####
# Define the create_maps function with an additional argument for dataframe name
  create_maps <- function(dataframe, df_name) {
    # Load world map data from the rnaturalearth package
    world <- rnaturalearth::ne_countries(scale = "small", returnclass = "sf")
    
    # Exclude Antarctica by filtering it out
    world <- world[world$continent != "Antarctica", ]
    
    # Merge the world map with your dataframe using iso3
    merged_data <- dplyr::left_join(world, dataframe, by = c("iso_a3_eh" = "iso3"))
    
    # Create a factor variable for percentages with the specified breaks
    merged_data$percentages_factor <- cut(merged_data$percentages, 
                                          breaks = c(0, 25, 50, 75, 101), 
                                          labels = c("0-25%", "26-50%", "51-75%", "76-100%"))
    
    # Define the color scale
    color_scale <- scale_fill_manual(
      values = c("#95d5ff", "#2facff", "#0078c8", "#005064", "lavender"),
      labels = c("0-25%", "26-50%", "51-75%", "76-100%", "Unavailable"),
      drop = FALSE  # Prevent unused levels from causing errors
    )
    
    # Create the map
    p <- ggplot() +
      geom_sf(data = merged_data, aes(fill = percentages_factor)) +
      color_scale +
      labs(fill = "Years covered") +
      ggtitle("Share of years covered") +  # Add the title here
      theme_void() +
      theme(
        text = element_text(family = "mono"),
        legend.position = "bottom",
        plot.title = element_text(hjust = 0.5, face = "bold", size = 18),  # Adjust the title font size
        legend.text = element_text(size = 12),  # Adjust the legend font size
        plot.margin = margin(t = 1, r = 1, b = 1, l = 1)  # Add margin to accommodate larger title
      )
    
    return(p)
  }
  


  # List of data frames
  data_frames_list <- list(DEMIG_Policy, GLOBALCIT_Country_Year, IMISEM, IMPIC_2016, 
                           IMPIC_Political_Rights, IMPIC_RawData,
                           MIPEX)  # Add your data frames here
  
  # Names of the data frames
  data_frames_names <- c("DEMIG_Policy", "GLOBALCIT_Country_Year", "IMISEM", "IMPIC_2016", "IMPIC_Political_Rights", 
                         "IMPIC_RawData", "MIPEX")  # Add your data frame names here
  
  # Create the "MAPS" folder if it doesn't exist
  if (!file.exists("MAPS")) {
    dir.create("MAPS")
  }
  
  # Loop through each data frame and create and save individual maps
  for (i in seq_along(data_frames_list)) {
    df <- data_frames_list[[i]]
    df_name <- data_frames_names[i]
    
    unique_codes <- unique(df$Code)
    
    for (code in unique_codes) {
      subset_data <- df[df$Code == code, ]
      map <- create_maps(subset_data, df_name)
      
      # Save the map as a PNG file within the "MAPS" folder
      map_name <- paste0("MAPS/", df_name, "_", code, ".png")
      ggsave(map_name, map, width = 7, height = 4)  # Adjust width and height as needed
    }
  }
  
  
