# LA_Analytical_Detective
# https://data.lacity.org/Public-Safety/Crime-Data-from-2020-to-Present/2nrs-mtv8/data_preview
LA_Analytical_Detective_URL <- "https://data.lacity.org/Public-Safety/Crime-Data-from-2020-to-Present/2nrs-mtv8/data_preview"
cat(LA_Analytical_Detective_URL)

setwd("D:/Coding_Exercises/R/The_Analytics_Edge/Assignment_1/LA_Analytical_Detective/2024")
getwd()
wrkdir <- getwd()
wrkdir
list.files()

# P 1.1 Loading the Data
# reading the file
LA_2024_02_Feb <- read.csv("2024_02_Feb.csv", stringsAsFactors = FALSE)
LA_2024_02_Feb
str(LA_2024_02_Feb) 
head(LA_2024_02_Feb)
tail(LA_2024_02_Feb)
summary(LA_2024_02_Feb)

# e.1 How many rows of data (observations) are in this dataset?
num_obs <- nrow(LA_2024_02_Feb) #1.1 to see number of row
num_obs
cat("There are", num_obs, "rows of data (observations) in the dataset.")


# 1.2 how many variables are there in this dataset?
num_var <- ncol(LA_2024_02_Feb)
num_var
cat("There are", num_var, "variables in this dataset.")

#1.2a ow many observations are there in this dataset?
num_obs <-nrow(LA_2024_02_Feb)
num_obs
cat("There are", num_obs, "observations in this dataset.")

# list all the variables (columns)
cat(names(LA_2024_02_Feb), sep = "\n")

# list all the AREA names
unique(LA_2024_02_Feb$AREA.NAME)
length(unique(LA_2024_02_Feb$AREA.NAME))
table(LA_2024_02_Feb$AREA.NAME)
AREA <- table(LA_2024_02_Feb$AREA.NAME)
AREA

# Max AREA
max_AREA <- names(which.max(AREA))
max_AREA
max_count_AREA <- max(AREA)
max_count_AREA

# Min AREA
min_AREA <- names(which.min(AREA))
min_AREA
min_count_AREA <- min(AREA)
min_count_AREA

cat("AREA Most incidents:\n", max_AREA, "with", max_count_AREA, "incidents\n\n")
cat("AREA Fewest incidents:\n", min_AREA, "with", min_count_AREA, "incidents\n")

# Create a string for AREA_Names
AREA_NAMES_string <- '
# list all the AREA names
unique(LA_2024_02_Feb$AREA.NAME)
length(unique(LA_2024_02_Feb$AREA.NAME))
table(LA_2024_02_Feb$AREA.NAME)
AREA <- table(LA_2024_02_Feb$AREA.NAME)
AREA

# Max AREA
max_AREA <- names(which.max(AREA))
max_AREA
max_count_AREA <- max(AREA)
max_count_AREA

# Min AREA
min_AREA <- names(which.min(AREA))
min_AREA
min_count_AREA <- min(AREA)
min_count_AREA

cat("AREA Most incidents:\n", max_AREA, "with", max_count_AREA, "incidents\n\n")
cat("AREA Fewest incidents:\n", min_AREA, "with", min_count_AREA, "incidents\n")
'
cat(AREA_NAMES_string)

# 1. Switch AREA.NAME to Crm.Cd.Desc
Crm_Cd_Desc_string_1 <- gsub("AREA\\.NAME", "Crm.Cd.Desc", AREA_NAMES_string)
cat(Crm_Cd_Desc_string_1)
# 2. Switch AREA (as a word) to Crm_Cd_Desc (variable prefix)
Crm_Cd_Desc_string_2 <- gsub("AREA", "Crm_Cd_Desc", Crm_Cd_Desc_string_1)
cat(Crm_Cd_Desc_string_2)
# Paste output
# list all the Crm_Cd_Desc names
unique(LA_2024_02_Feb$Crm.Cd.Desc)
length(unique(LA_2024_02_Feb$Crm.Cd.Desc))
table(LA_2024_02_Feb$Crm.Cd.Desc)
Crm_Cd_Desc <- table(LA_2024_02_Feb$Crm.Cd.Desc)
Crm_Cd_Desc

# Max Crm_Cd_Desc
max_Crm_Cd_Desc <- names(which.max(Crm_Cd_Desc))
max_Crm_Cd_Desc
max_count_Crm_Cd_Desc <- max(Crm_Cd_Desc)
max_count_Crm_Cd_Desc

# Min Crm_Cd_Desc
min_Crm_Cd_Desc <- names(which.min(Crm_Cd_Desc))
min_Crm_Cd_Desc
min_count_Crm_Cd_Desc <- min(Crm_Cd_Desc)
min_count_Crm_Cd_Desc

cat("Crm_Cd_Desc Most incidents: ", max_Crm_Cd_Desc, "with", max_count_Crm_Cd_Desc, "incidents ")
cat("Crm_Cd_Desc Fewest incidents: ", min_Crm_Cd_Desc, "with", min_count_Crm_Cd_Desc, "incidents ")


# 1. Switch AREA.NAME to Premis.Desc
Premis_Desc_string_1 <- gsub("AREA\\.NAME", "Premis.Desc", AREA_NAMES_string)
cat(Premis_Desc_string_1)
# 2. Switch AREA (as a word) to Premis_Desc (variable prefix)
Premis_Desc_string_2 <- gsub("AREA", "Premis_Desc", Premis_Desc_string_1)
cat(Premis_Desc_string_2)
# Paste output
# list all the Premis_Desc names
unique(LA_2024_02_Feb$Premis.Desc)
length(unique(LA_2024_02_Feb$Premis.Desc))
table(LA_2024_02_Feb$Premis.Desc)
Premis_Desc <- table(LA_2024_02_Feb$Premis.Desc)
Premis_Desc

# Max Premis_Desc
max_Premis_Desc <- names(which.max(Premis_Desc))
max_Premis_Desc
max_count_Premis_Desc <- max(Premis_Desc)
max_count_Premis_Desc

# Min Premis_Desc
min_Premis_Desc <- names(which.min(Premis_Desc))
min_Premis_Desc
min_count_Premis_Desc <- min(Premis_Desc)
min_count_Premis_Desc

cat("Premis_Desc Most incidents: ", max_Premis_Desc, "with", max_count_Premis_Desc, "incidents")
cat("Premis_Desc Fewest incidents: ", min_Premis_Desc, "with", min_count_Premis_Desc, "incidents")

# create separate table for MTA
# Trim whitespace, just in case
names(Premis_Desc) <- trimws(names(Premis_Desc))
# Extract rows that contain "MTA" (case-insensitive for safety)
MTA_Premis_Desc <- Premis_Desc[grep("MTA", names(Premis_Desc), ignore.case = TRUE)]
print(MTA_Premis_Desc)


# 1. Switch AREA.NAME to LOCATION
Premis_Desc_string_1 <- gsub("AREA\\.NAME", "LOCATION", AREA_NAMES_string)
cat(Premis_Desc_string_1)
# 2. Switch AREA (as a word) to Premis_Desc (variable prefix)
Premis_Desc_string_2 <- gsub("AREA", "LOCATION", Premis_Desc_string_1)
cat(Premis_Desc_string_2)
# Paste output
# list all the LOCATION names
unique(LA_2024_02_Feb$LOCATION)
length(unique(LA_2024_02_Feb$LOCATION))
table(LA_2024_02_Feb$LOCATION)
LOCATION <- table(LA_2024_02_Feb$LOCATION)
LOCATION

# Max LOCATION
max_LOCATION <- names(which.max(LOCATION))
max_LOCATION
max_count_LOCATION <- max(LOCATION)
max_count_LOCATION

# Min LOCATION
min_LOCATION <- names(which.min(LOCATION))
min_LOCATION
min_count_LOCATION <- min(LOCATION)
min_count_LOCATION

cat("LOCATION Most incidents:", max_LOCATION, "with", max_count_LOCATION, "incidents")
cat("LOCATION Fewest incidents:", min_LOCATION, "with", min_count_LOCATION, "incidents")


# 1. Switch AREA.NAME to Cross.Street
Cross_Street_String_1 <- gsub("AREA\\.NAME", "Cross.Street", AREA_NAMES_string)
cat(Cross_Street_String_1)
# 2. Switch AREA (as a word) to Premis_Desc (variable prefix)
Cross_Street_string_2 <- gsub("AREA", "Cross.Street", Cross_Street_String_1)
cat(Cross_Street_string_2)

# past Cross_Street_String_2 below
# list all the Cross.Street names
unique(LA_2024_02_Feb$Cross.Street)
length(unique(LA_2024_02_Feb$Cross.Street))
table(LA_2024_02_Feb$Cross.Street)
Cross.Street <- table(LA_2024_02_Feb$Cross.Street)
Cross.Street <- Cross.Street[names(Cross.Street) != ""]
Cross.Street

# Max Cross.Street
max_Cross.Street <- names(which.max(Cross.Street))
max_Cross.Street
max_count_Cross.Street <- max(Cross.Street)
max_count_Cross.Street

# Min Cross.Street
min_Cross.Street <- names(which.min(Cross.Street))
min_Cross.Street
min_count_Cross.Street <- min(Cross.Street)
min_count_Cross.Street

head(Cross.Street)
tail(Cross.Street)

cat("Cross.Street Most incidents: ", max_Cross.Street, "with", max_count_Cross.Street, "incidents")
cat("Cross.Street Fewest incidents: ", min_Cross.Street, "with", min_count_Cross.Street, "incidents")


### remove all memory from R console
rm(list = ls())
ls()
list.files()
dir()

    