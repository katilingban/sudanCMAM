## Libraries
if(!require(remotes)) install.packages("remotes")
if(!require(magrittr)) install.packages("magrittr")
if(!require(dplyr)) install.packages("dplyr")
if(!require(ggplot2)) install.packages("ggplot2")

remotes::install_github("rapidsurveys/squeacr")

library(magrittr)
library(dplyr)
library(squeacr)
library(ggplot2)

## Plot admissions over time by state with seasonal calendar

aggregate(cbind(`New Admissions`, Default) ~ State + Month + Year, 
          data = monitoring, FUN = sum) %>%
  filter(State == "Aljazira") %>%
  mutate(MonthYear = as.Date(paste(Year, Month, "01", sep = "-"), format = "%Y-%b-%d")) %>%
  arrange(MonthYear) %>%
  mutate(`Admissions (smoothed)` = smooth_m3a3(x = `New Admissions`),
         `Defaulters (smoothed)` = smooth_m3a3(x = Default)) %>%
  ggplot() +
  geom_line(mapping = aes(x = MonthYear, y = `New Admissions`, group = 1), 
            colour = "blue", linetype = 1, size = 2)


