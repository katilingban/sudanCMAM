## Load libraries ##############################################################
library(openxlsx)
library(readxl)
library(stringr)
library(magrittr)
library(dplyr)
library(tidyr)
library(rgdal)
library(rgeos)
library(raster)


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
monitoring <- set1


## Read and process spatial data ###############################################

## Clean up states names
monitoring$State <- monitoring$State %>%
  str_replace_all(pattern = "Algazira", replacement = "Gazera") %>%
  str_replace_all(pattern = "Sinnar", replacement = "Sennar")

## Clean up localities names - Gazera
unique(monitoring$Locality[monitoring$State == "Gazera"])

monitoring$Locality[monitoring$State == "Gazera"] <- monitoring$Locality[monitoring$State == "Gazera"] %>%
  str_replace_all(pattern = "Algurashi", replacement = "El Qurashi") %>%
  str_replace_all(pattern = "Alkamlin", replacement = "El Kamlin") %>%
  str_replace_all(pattern = "Almanagil|Almanagel", replacement = "El Manaqil") %>%
  str_replace_all(pattern = "Ganop Algezira|South Gazira", replacement = "Ganub Elgazira") %>%
  str_replace_all(pattern = "Medani Alkupra", replacement = "Greater Medani") %>%
  str_replace_all(pattern = "Sharg Algezira", replacement = "Sharg El Gezira") %>%
  str_replace_all(pattern = "Umalgura|Umalgora", replacement = "Um Elqura") %>%
  str_replace_all(pattern = "Alhasahisa", replacement = "El Hassahisa") %>%
  str_replace_all(pattern = "Almanagil|Almanagel", replacement = "El Manaqil") %>%
  
  ## Clean up localities names - Blue Nile
  unique(monitoring$Locality[monitoring$State == "Blue Nile"])

monitoring$Locality[monitoring$State == "Blue Nile"] <- monitoring$Locality[monitoring$State == "Blue Nile"] %>%
  str_replace_all(pattern = "Al-Damazain|Aldamazain", replacement = "Ed Damazine") %>%
  str_replace_all(pattern = "Al-Kurmouk|Alkurmouk", replacement = "El Kurmuk") %>%
  str_replace_all(pattern = "Al-Rosaires|Alrosaires", replacement = "El Roseiris") %>%
  str_replace_all(pattern = "Altadamon_bn", replacement = "El Tadamon") %>%
  str_replace_all(pattern = "Gaissan", replacement = "Geisan") %>%
  str_replace_all(pattern = "Wad Almahi", replacement = "Wad El Mahi")

## Clean up localities names - Central Darfur
unique(monitoring$Locality[monitoring$State == "Central Darfur"])

monitoring$Locality[monitoring$State == "Central Darfur"] <- monitoring$Locality[monitoring$State == "Central Darfur"] %>%
  str_replace_all(pattern = "Azoum|Azuom", replacement = "Azum") %>%
  str_replace_all(pattern = "Bendisi", replacement = "Bendasi") %>%
  str_replace_all(pattern = "North Jm", replacement = "North Jebel Mara") %>%
  str_replace_all(pattern = "Zalingei", replacement = "Zalingi") %>%
  str_replace_all(pattern = "Central Jabal Mara", replacement = "Central Jebel Mara") %>%
  str_replace_all(pattern = "North Jabal Mara", replacement = "North Jebel Mara") %>%
  str_replace_all(pattern = "Bendise", replacement = "Bendasi") %>%
  str_replace_all(pattern = "West Jabal Mara", replacement = "Nertiti")

## Clean up localities names - East Darfur
unique(monitoring$Locality[monitoring$State == "East Darfur"])

monitoring$Locality[monitoring$State == "East Darfur"] <- monitoring$Locality[monitoring$State == "East Darfur"] %>%
  str_replace_all(pattern = "Alfirdos|El Firdos", replacement = "El Firdous") %>%
  str_replace_all(pattern = "Assalayia", replacement = "Assalaya") %>%
  str_replace_all(pattern = "Bahar Alarab|Bahr Alarab", replacement = "Bahr El Arab") %>%
  str_replace_all(pattern = "Sharia|Shearia", replacement = "Shia'ria") %>%
  str_replace_all(pattern = "El Daien|Ed Daein|Ed Daien", replacement = "Ed Dain") %>%
  str_replace_all(pattern = "Adilla", replacement = "Adila")

## Clean up localities names - Gedaref
unique(monitoring$Locality[monitoring$State == "Gedaref"])

monitoring$Locality[monitoring$State == "Gedaref"] <- monitoring$Locality[monitoring$State == "Gedaref"] %>%
  str_replace_all(pattern = "Al Faw", replacement = "El Fao") %>%
  str_replace_all(pattern = "Algirasha", replacement = "El Qureisha") %>%
  str_replace_all(pattern = "East Galabat", replacement = "Eastern Galabat") %>%
  str_replace_all(pattern = "Elbotana", replacement = "Elbutana") %>%
  str_replace_all(pattern = "Elfashaga", replacement = "El Fashaga") %>%
  str_replace_all(pattern = "Elmafaza", replacement = "El Mafaza") %>%
  str_replace_all(pattern = "Elrahad", replacement = "El Rahad") %>%
  str_replace_all(pattern = "Gala Anahal", replacement = "Gala'a El Nahal") %>%
  str_replace_all(pattern = "Gedaref", replacement = "Gedaref Town") %>%
  str_replace_all(pattern = "West Galabat", replacement = "Western Galabat") %>%
  str_replace_all(pattern = "Mid Gedaref", replacement = "Middle Geddaref")

## Clean up localities names - Kassala
unique(monitoring$Locality[monitoring$State == "Kassala"])

monitoring$Locality[monitoring$State == "Kassala"] <- monitoring$Locality[monitoring$State == "Kassala"] %>%
  str_replace_all(pattern = "Algerba", replacement = "Rural Khashm Elgirba") %>%
  str_replace_all(pattern = "Aroma", replacement = "Rural Aroma") %>%
  str_replace_all(pattern = "Halfa", replacement = "New Halfa") %>%
  str_replace_all(pattern = "Hamshkorep|Hamshkoreeb", replacement = "Refi Hamashkureib") %>%
  str_replace_all(pattern = "Naher Atbra", replacement = "Refi Nahr Atbara") %>%
  str_replace_all(pattern = "North Dalta|North Delta", replacement = "Refi North Delta") %>%
  str_replace_all(pattern = "Rural Kassala", replacement = "Refi Kassla") %>%
  str_replace_all(pattern = "Talkok", replacement = "Rural Telkok") %>%
  str_replace_all(pattern = "Wad Alhilw", replacement = "Rural Wad Elhilaiw") %>%
  str_replace_all(pattern = "West Kassala", replacement = "Rural Western Kassala")

monitoring$Locality[monitoring$State == "Kassala"][monitoring$Locality[monitoring$State == "Kassala"] == "Kassala"] <- "Kassala Town"

## Clean up localities names - Khartoum
unique(monitoring$Locality[monitoring$State == "Khartoum"])

monitoring$Locality[monitoring$State == "Khartoum"] <- monitoring$Locality[monitoring$State == "Khartoum"] %>%
  str_replace_all(pattern = "Bahari", replacement = "Bahri") %>%
  str_replace_all(pattern = "Jabalawlia", replacement = "Jebel Awlia") %>%
  str_replace_all(pattern = "Karari", replacement = "Karrari") %>%
  str_replace_all(pattern = "Shargelnil", replacement = "Sharg Elneel") %>%
  str_replace_all(pattern = "Umbada", replacement = "Um Bada") %>%
  str_replace_all(pattern = "Umdurman", replacement = "Um Durman")

## Clean up localities names - North Darfur
unique(monitoring$Locality[monitoring$State == "North Darfur"])

monitoring$Locality[monitoring$State == "North Darfur"] <- monitoring$Locality[monitoring$State == "North Darfur"] %>%
  str_replace_all(pattern = "Al Kuma", replacement = "ELkuma") %>%
  str_replace_all(pattern = "Al Lait", replacement = "El Lait") %>%
  str_replace_all(pattern = "Al Malha|Alwaha", replacement = "El Mahalha") %>%
  str_replace_all(pattern = "Dareslam", replacement = "Dar Elsalam") %>%
  str_replace_all(pattern = "El Sereif", replacement = "El Serief") %>%
  str_replace_all(pattern = "Kornowi|Kornoy", replacement = "Kernoi") %>%
  str_replace_all(pattern = "Mallet|Mellit", replacement = "Melit") %>%
  str_replace_all(pattern = "Tawilla", replacement = "Tawila") %>%
  str_replace_all(pattern = "Tawisha|Twasha", replacement = "El Tawisha") %>%
  str_replace_all(pattern = "Umbaru|Um Baro", replacement = "Um Baru") %>%
  str_replace_all(pattern = "Umkedada|Um Kaddada", replacement = "Um Kadada") %>%
  str_replace_all(pattern = "Kalamendo", replacement = "Kelemando") %>%
  str_replace_all(pattern = "El Laiet", replacement = "El Lait") %>%
  str_replace_all(pattern = "Eltina", replacement = "El Tina") %>%
  str_replace_all(pattern = "Kebkabya|Kabkabya", replacement = "Kebkabiya")

## Clean up localities names - North Kordofan
unique(monitoring$Locality[monitoring$State == "North Kordofan"])

monitoring$Locality[monitoring$State == "North Kordofan"] <- monitoring$Locality[monitoring$State == "North Kordofan"] %>%
  str_replace_all(pattern = "Alrahad", replacement = "El Rahad") %>%
  str_replace_all(pattern = "Jabra Al Shiehk", replacement = "Gebrat El Sheikh") %>%
  str_replace_all(pattern = "Shiekan", replacement = "Sheikan") %>%
  str_replace_all(pattern = "Sodary", replacement = "Soudari") %>%
  str_replace_all(pattern = "Um Dam", replacement = "Um Dam Haj Ahmed") %>%
  str_replace_all(pattern = "Um Rowaba", replacement = "Um Rawaba") %>%
  str_replace_all(pattern = "West Bara", replacement = "Gharb Bara")

## Clean up localities names - Northern
unique(monitoring$Locality[monitoring$State == "Northern"])

monitoring$Locality[monitoring$State == "Northern"] <- monitoring$Locality[monitoring$State == "Northern"] %>%
  str_replace_all(pattern = "Alborgeg", replacement = "El Burgaig") %>%
  str_replace_all(pattern = "Dalgo", replacement = "Delgo") %>%
  str_replace_all(pattern = "Merawi|Marawi", replacement = "Merwoe")

## Clean up localities names - Red Sea
unique(monitoring$Locality[monitoring$State == "Red Sea"])

monitoring$Locality[monitoring$State == "Red Sea"] <- monitoring$Locality[monitoring$State == "Red Sea"] %>%
  str_replace_all(pattern = "Dordaib", replacement = "Dordieb") %>%
  str_replace_all(pattern = "Gabiet Almadeen", replacement = "Gabaot Elma'aadin") %>%
  str_replace_all(pattern = "Halaib", replacement = "Halaeeb") %>%
  str_replace_all(pattern = "Portsudan", replacement = "Port Sudan") %>%
  str_replace_all(pattern = "Suakin", replacement = "Swakin") %>%
  str_replace_all(pattern = "Gunb Waleeb", replacement = "El Ganab Elawlait")

## Clean up localities names - Sennar
unique(monitoring$Locality[monitoring$State == "Sennar"])

monitoring$Locality[monitoring$State == "Sennar"] <- monitoring$Locality[monitoring$State == "Sennar"] %>%
  str_replace_all(pattern = "Al Dali", replacement = "El Dali") %>%
  str_replace_all(pattern = "Abo Hgar", replacement = "Abu Hujar") %>%
  str_replace_all(pattern = "Eldinder", replacement = "Ed Dinder") %>%
  str_replace_all(pattern = "Singa", replacement = "Sinja") %>%
  str_replace_all(pattern = "Sinnar", replacement = "Sennar") %>%
  str_replace_all(pattern = "Alsoki|Elsoki", replacement = "Es Suki") %>%
  str_replace_all(pattern = "East Sinnar", replacement = "Sharg Sennar") %>%
  str_replace_all(pattern = "Aldali&Almazmom", replacement = "El Dali")

## Clean up localities names - South Darfur
unique(monitoring$Locality[monitoring$State == "South Darfur"])

monitoring$Locality[monitoring$State == "South Darfur"] <- monitoring$Locality[monitoring$State == "South Darfur"] %>%
  str_replace_all(pattern = "Alsalam", replacement = "Es Salam") %>%
  str_replace_all(pattern = "Belil", replacement = "Beliel") %>%
  str_replace_all(pattern = "Dimso", replacement = "Damso") %>%
  str_replace_all(pattern = "Ed Elfersan", replacement = "Ed Elfursan") %>%
  str_replace_all(pattern = "Kass", replacement = "Kas") %>%
  str_replace_all(pattern = "Kateela|Katela", replacement = "Kateila") %>%
  str_replace_all(pattern = "Niteaga", replacement = "Nitega") %>%
  str_replace_all(pattern = "North Nyala", replacement = "Nyala North") %>%
  str_replace_all(pattern = "Kobum", replacement = "Kubum") %>%
  str_replace_all(pattern = "Rehaid Elberdi|Rehaid Alberdi", replacement = "Rehaid Elbirdi") %>%
  str_replace_all(pattern = "Alwehda", replacement = "El Wihda") %>%
  str_replace_all(pattern = "Alsunta", replacement = "El Sunta") %>%
  str_replace_all(pattern = "Um Dafog", replacement = "Um Dafoug") %>%
  str_replace_all(pattern = "Alradom", replacement = "El Radoum") %>%
  str_replace_all(pattern = "Sharg Algabal", replacement = "Sharg El Jabal") %>%
  str_replace_all(pattern = "Shataia", replacement = "Shattaya") %>%
  str_replace_all(pattern = "East Jabal Mara", replacement = "Sharg El Jabal")

monitoring$Locality[monitoring$State == "South Darfur"][monitoring$Locality[monitoring$State == "South Darfur"] == "Nyala"] <- "Nyala North"

## Clean up localities names - South Kordofan
unique(monitoring$Locality[monitoring$State == "South Kordofan"])

monitoring$Locality[monitoring$State == "South Kordofan"] <- monitoring$Locality[monitoring$State == "South Kordofan"] %>%
  str_replace_all(pattern = "Abugebiha", replacement = "Abu Jubaiha") %>%
  str_replace_all(pattern = "Abukarshola", replacement = "Abu Karshola") %>%
  str_replace_all(pattern = "Alabassia", replacement = "Abassiya") %>%
  str_replace_all(pattern = "Algooz", replacement = "El Quoz") %>%
  str_replace_all(pattern = "Alliry", replacement = "El Leri") %>%
  str_replace_all(pattern = "Altadamon", replacement = "El Tadamon") %>%
  str_replace_all(pattern = "Elreef Alshargi", replacement = "Reif Shargi") %>%
  str_replace_all(pattern = "Gadeer", replacement = "Ghadeer") %>%
  str_replace_all(pattern = "Habilla", replacement = "Habila") %>%
  str_replace_all(pattern = "Rashad", replacement = "El Rashad") %>%
  str_replace_all(pattern = "Dalami", replacement = "Delami")

## Clean up localities names - West Darfur
unique(monitoring$Locality[monitoring$State == "West Darfur"])

monitoring$Locality[monitoring$State == "West Darfur"] <- monitoring$Locality[monitoring$State == "West Darfur"] %>%
  str_replace_all(pattern = "Furbaranga", replacement = "Foro Baranga") %>%
  str_replace_all(pattern = "Kerenik", replacement = "Kereneik") %>%
  str_replace_all(pattern = "El-Genina", replacement = "El Geneina") %>%
  str_replace_all(pattern = "Habilla", replacement = "Habila")

## Clean up localities names - West Kordofan
unique(monitoring$Locality[monitoring$State == "West Kordofan"])

monitoring$Locality[monitoring$State == "West Kordofan"] <- monitoring$Locality[monitoring$State == "West Kordofan"] %>%
  str_replace_all(pattern = "Abo Zabad", replacement = "Abu Zabad") %>%
  str_replace_all(pattern = "Al Khowai", replacement = "ElKhiwai") %>%
  str_replace_all(pattern = "Al Meram", replacement = "El Meiram") %>%
  str_replace_all(pattern = "Al Nihood", replacement = "El Nuhud") %>%
  str_replace_all(pattern = "Al Odaya", replacement = "El Idia") %>%
  str_replace_all(pattern = "Al Salam \\(Elfula\\)", replacement = "Es Salam") %>%
  str_replace_all(pattern = "Aldebab", replacement = "El Dibab") %>%
  str_replace_all(pattern = "Alsunuit", replacement = "Elsunut") %>%
  str_replace_all(pattern = "Babanousa", replacement = "Babanusa") %>%
  str_replace_all(pattern = "Keilak", replacement = "Keilik") %>%
  str_replace_all(pattern = "Lagwa", replacement = "Lagawa") %>%
  str_replace_all(pattern = "Wad Banda", replacement = "Wad Benda") %>%
  str_replace_all(pattern = "Ghebiesh", replacement = "Ghubaish") %>%
  str_replace_all(pattern = "Elfula", replacement = "Es Salam")

## Clean up localities names - White Nile
unique(monitoring$Locality[monitoring$State == "White Nile"])

monitoring$Locality[monitoring$State == "White Nile"] <- monitoring$Locality[monitoring$State == "White Nile"] %>%
  str_replace_all(pattern = "Aljabalin", replacement = "El Jabalain") %>%
  str_replace_all(pattern = "Aldewaim", replacement = "El Diwaim") %>%
  str_replace_all(pattern = "Alsalam", replacement = "Es Salam") %>%
  str_replace_all(pattern = "Tandalti", replacement = "Tendalti") %>%
  str_replace_all(pattern = "Goole", replacement = "Guli") %>%
  str_replace_all(pattern = "Algetaina", replacement = "El Gitaina")

## Clean up localities names - River Nile
unique(monitoring$Locality[monitoring$State == "River Nile"])

monitoring$Locality[monitoring$State == "River Nile"] <- monitoring$Locality[monitoring$State == "River Nile"] %>%
  str_replace_all(pattern = "Al Damer|Aldamer", replacement = "El Damar") %>%
  str_replace_all(pattern = "Al Buhaira|Albuhira", replacement = "El Buhaira") %>%
  str_replace_all(pattern = "El Matammah|Almatama", replacement = "El Matama") %>%
  str_replace_all(pattern = "Shandi", replacement = "Shendi") %>%
  str_replace_all(pattern = "Abuhamad", replacement = "Abu Hamad") %>%
  str_replace_all(pattern = "Atbra", replacement = "Atbara")

## Save data as rda ############################################################
save(monitoring, file = "data/sudan_cmam.rda", compress = "xz")


## Clean up workspace ##########################################################
rm(list = ls())

