## Libraries
if(!require(remotes)) install.packages("remotes")
if(!require(magrittr)) install.packages("magrittr")
if(!require(dplyr)) install.packages("dplyr")
if(!require(tidyr)) install.packages("tidyr")
if(!require(ggplot2)) install.packages("ggplot2")

remotes::install_github("rapidsurveys/squeacr")

library(magrittr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(squeacr)

## Load UNICEF palette
unicef_blue      <- "#1CABE2"
unicef_darkgreen <- "#00833D"
unicef_green     <- "#80BD41"
unicef_yellow    <- "#FFC20E"
unicef_orange    <- "#F26A21"
unicef_red       <- "#E2231A"
unicef_darkred   <- "#961A49" 
unicef_purple    <- "#6A1E74"
unicef_warmgrey  <- "#D8D1C9"
unicef_coolgrey  <- "#777779"
unicef_black     <- "#2D2926"
unicef_darkblue  <- "#374EA2"

## UNICEF ggplot theme settings
unicef_theme <- theme_bw() +
  theme(plot.title = element_text(size = 16, colour = unicef_darkblue),
        plot.subtitle = element_text(size = 14, colour = unicef_blue),
        panel.border = element_blank(),
        panel.grid.major.y = element_line(linetype = 1, size = 0.3),
        panel.grid.minor.y = element_line(linetype = 2, size = 0.15),
        panel.grid.major.x = element_line(linetype = 0),
        panel.grid.minor.x = element_line(linetype = 0),
        strip.background = element_rect(colour = unicef_darkblue,
                                        fill = unicef_darkblue),
        strip.text = element_text(face = "bold", colour = "white", size = 10, vjust = 1),
        legend.background = element_rect(colour = NA, fill = NA),
        legend.key = element_rect(linetype = 0),
        legend.key.size = unit(x = 8, units = "points"),
        legend.spacing = unit(x = 4, units = "points"),
        legend.text = element_text(size =  8),
        legend.position = "top",
        axis.line.x = element_line(colour = unicef_darkblue, size = 1),
        axis.text.x = element_text(size = 10, colour = unicef_darkblue),
        axis.title.x = element_text(size = 12, colour = unicef_blue),
        axis.text.y = element_text(size = 10, colour = unicef_darkblue),
        axis.title.y = element_text(size = 12, colour = unicef_blue),
        axis.ticks = element_line(colour = unicef_darkblue, size = 0.25))


## Plot admissions over time by state with seasonal calendar

x <- aggregate(cbind(`New Admissions`, Default) ~ State + Month + Year, 
          data = monitoring, FUN = sum) %>%
  filter(State %in% c("Gazera", "Kassala", "North Darfur")) %>%
  mutate(MonthYear = as.Date(paste(Year, Month, "01", sep = "-"), 
                             format = "%Y-%b-%d")) %>%
  arrange(MonthYear) %>%
  mutate(`Admissions (smoothed)` = smooth_m3a3(x = `New Admissions`),
         `Defaulters (smoothed)` = smooth_m3a3(x = Default)) %>%
  pivot_longer(cols = c(`New Admissions`:Default, 
                        `Admissions (smoothed)`:`Defaulters (smoothed)`),
               names_to = "indicator",
               values_to = "value")
  
  x %>%
    ggplot(aes(x = MonthYear, y = indicator)) +
    geom_line(mapping = aes(x = MonthYear, y = `New Admissions`, group = 1), 
              colour = "blue", linetype = 1, size = 2)
    scale_y_continuous(limits = c(0, max(x[["New Admissions"]]))) +
    scale_x_date(date_breaks = "3 months", date_labels = "%b %Y",
                 sec.axis = sec_axis(~ ., breaks = NULL)) +
    labs(title = paste("Admissions over time - 2016 to 2019 - ", 
                       "Al Gazera", " State", sep = ""), 
         x = "") +
    unicef_theme


