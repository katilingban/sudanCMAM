## Load libraries ##############################################################
library(openxlsx)
library(readxl)
library(stringr)
library(magrittr)
library(dplyr)
library(tidyr)


## Read three versions of data - work with most recent (set1) ##################
set1 <- read_xlsx(path = "data/set1.xlsx", sheet = "CMAM")
set2 <- read_xlsx(path = "data/set2.xlsx", sheet = "CMAM")
set3 <- read_xlsx(path = "data/set3.xlsx", sheet = "CMAM")


## Clean up data ###############################################################

## Correct state spellings
set1$State <- set1$State %>%
  str_replace_all(pattern = "kassala", replacement = "Kassala") %>%
  str_replace_all(pattern = "Northren", replacement = "Northern") %>%
  str_replace_all(pattern = "Centeral Darfur", replacement = "Central Darfur")

## Remove rows of data with State == "(blank)"
set1 <- set1[set1$State != "(blank)", ]

## Clean up locality names
set1$Locality <- set1$Locality %>%
  str_to_title() %>%
  str_replace_all(pattern = "\\(", replacement = " (")

## Add columns for month and year
set1 <- set1 %>%
  mutate(month = str_extract(string = set1$Month, pattern = "[a-zA-z]+"),
         year = paste(20, str_extract(string = set1$Month, pattern = "[0-9]+"), sep = "")) %>%
  select(-Month)

## Clean up variable names
names(set1) <- str_to_title(names(set1))
sudan_cmam <- set1

## Save data as rda ############################################################
save(sudan_cmam, file = "data/sudan_cmam.rda", compress = "xz")


## Clean up workspace ##########################################################
rm(list = ls())

