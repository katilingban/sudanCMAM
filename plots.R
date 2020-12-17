## Libraries
if(!require(remotes)) install.packages("remotes")
if(!require(magrittr)) install.packages("magrittr")
if(!require(dplyr)) install.packages("dplyr")
if(!require(tidyr)) install.packages("tidyr")
if(!require(ggplot2)) install.packages("ggplot2")
if(!require(lubridate)) install.packages("lubridate")

remotes::install_github("rapidsurveys/squeacr")

library(magrittr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(lubridate)
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
        panel.grid.major.y = element_line(linetype = 1, size = 0.2),
        panel.grid.minor.y = element_line(linetype = 2, size = 0.1),
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
  group_by(State) %>%
  mutate(`Admissions (smoothed)` = smooth_m3a3(x = `New Admissions`),
         `Defaulters (smoothed)` = smooth_m3a3(x = Default))
  
season <- seasonal_calendar %>%
  mutate(startmy = paste(month.abb[month(as.Date(start))], year(as.Date(start))),
         endmy = paste(month.abb[month(as.Date(end))], year(as.Date(end))))

  x %>%
    ggplot() +
    geom_line(mapping = aes(x = MonthYear, 
                            y = `New Admissions`,
                            colour = "Admissions"), size = 1) +
    geom_line(mapping = aes(x = MonthYear, 
                            y = `Admissions (smoothed)`,
                            colour = "Admissions (smoothed)"),  
              alpha = 0.5, size = 0.5) +
    scale_colour_manual(name = "", 
                        values = c("Admissions" = "blue", 
                                   "Admissions (smoothed)" = "gray50")) +
    scale_x_date(date_breaks = "3 months", date_labels = "%b %Y",
                 sec.axis = sec_axis(~ ., breaks = NULL)) +
    labs(title = "Admissions over time from 2016 to 2019", 
         x = "", y = "Admissions") +
    facet_wrap(. ~ State, nrow = 3) +
    geom_rect(mapping = aes(xmin = as.Date(start), xmax = as.Date(end),
                            ymin = 0, ymax = 200, fill = group),
              data = season %>% filter(group == "Lean season"),
              alpha = 0.25) +
    geom_rect(mapping = aes(xmin = as.Date(start), xmax = as.Date(end),
                            ymin = 250, ymax = 450, fill = group),
              data = season %>% filter(group == "Harvest"),
              alpha = 0.25) +
    geom_rect(mapping = aes(xmin = as.Date(start), xmax = as.Date(end),
                            ymin = 500, ymax = 700, fill = group),
              data = season %>% filter(group == "Planting"),
              alpha = 0.25) +
    geom_rect(mapping = aes(xmin = as.Date(start), xmax = as.Date(end),
                            ymin = 750, ymax = 950, fill = group),
              data = season %>% filter(group == "Rainfall"),
              alpha = 0.25) +
    unicef_theme +
    theme(legend.title = element_blank(),
          axis.text.x = element_text(size = 6),
          axis.text.y = element_text(size = 8)) 

ggsave(filename = "admissionsOverTime.png", device = "png", path = "figures", 
       width = 12, height = 8, units = "in")


## Plot admissions over time by state with seasonal calendar

x %>%
  ggplot() +
  geom_line(mapping = aes(x = MonthYear, 
                          y = Default,
                          colour = "Defaulters"), size = 1) +
  geom_line(mapping = aes(x = MonthYear, 
                          y = `Defaulters (smoothed)`,
                          colour = "Defaulters (smoothed)"),  
            alpha = 0.5, size = 0.5) +
  scale_colour_manual(name = "", 
                      values = c("Defaulters" = "red", 
                                 "Defaulters (smoothed)" = "gray50")) +
  scale_x_date(date_breaks = "3 months", date_labels = "%b %Y",
               sec.axis = sec_axis(~ ., breaks = NULL)) +
  labs(title = "Defaulters over time from 2016 to 2019", 
       x = "", y = "Defaulters") +
  facet_wrap(. ~ State, nrow = 3) +
  geom_rect(mapping = aes(xmin = as.Date(start), xmax = as.Date(end),
                          ymin = 0, ymax = 50, fill = group),
            data = season %>% filter(group == "Lean season"),
            alpha = 0.25) +
  geom_rect(mapping = aes(xmin = as.Date(start), xmax = as.Date(end),
                          ymin = 55, ymax = 105, fill = group),
            data = season %>% filter(group == "Harvest"),
            alpha = 0.25) +
  geom_rect(mapping = aes(xmin = as.Date(start), xmax = as.Date(end),
                          ymin = 110, ymax = 160, fill = group),
            data = season %>% filter(group == "Planting"),
            alpha = 0.25) +
  geom_rect(mapping = aes(xmin = as.Date(start), xmax = as.Date(end),
                          ymin = 165, ymax = 215, fill = group),
            data = season %>% filter(group == "Rainfall"),
            alpha = 0.25) +
  unicef_theme +
  theme(legend.title = element_blank(),
        axis.text.x = element_text(size = 6),
        axis.text.y = element_text(size = 8)) 

ggsave(filename = "defaultersOverTime.png", device = "png", path = "figures", 
       width = 12, height = 8, units = "in")