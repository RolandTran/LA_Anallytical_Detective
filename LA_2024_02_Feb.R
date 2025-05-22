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

# list all the AREA.NAME names
unique(LA_2024_02_Feb$AREA.NAME)
length(unique(LA_2024_02_Feb$AREA.NAME))
table(LA_2024_02_Feb$AREA.NAME)
AREA.NAME <- table(LA_2024_02_Feb$AREA.NAME)
AREA.NAME

# Max AREA_NAME
max_AREA.NAME <- names(which.max(AREA.NAME))
max_AREA.NAME
max_count_AREA.NAME <- max(AREA.NAME)
max_count_AREA.NAME

# Min AREA_NAME
min_AREA.NAME <- names(which.min(AREA.NAME))
min_AREA.NAME
min_count_AREA.NAME <- min(AREA.NAME)
min_count_AREA.NAME

cat("AREA Most incidents:\n", max_AREA.NAME, "with", max_count_AREA.NAME, "incidents\n\n")
cat("AREA Fewest incidents:\n", min_AREA.NAME, "with", min_count_AREA.NAME, "incidents\n")

# Sort AREA.name in decreasing order
Sorted_AREA.NAME <- sort(AREA.NAME, decreasing = TRUE)
head(Sorted_AREA.NAME)
tail(Sorted_AREA.NAME)

# Get top 3 most frequent AREA_NAME
top_3_Sorted_AREA.NAME <- head(Sorted_AREA.NAME, 3)
top_3_Sorted_AREA.NAME

# Extract SOrted AREA_NAME, names and counts
top_3_Sorted_AREA.NAME_names <- names(top_3_Sorted_AREA.NAME)
top_3_Sorted_AREA.NAME_names
top_3_Sorted_AREA.NAME_counts <- as.numeric(top_3_Sorted_AREA.NAME)
top_3_Sorted_AREA.NAME_counts

# Print summary sentence
cat(paste0(
  top_3_Sorted_AREA.NAME_names[1], " had the most incidents with ", top_3_Sorted_AREA.NAME_counts[1], " cases; ",
  top_3_Sorted_AREA.NAME_names[2], " followed with ", top_3_Sorted_AREA.NAME_counts[2], " cases; and ",
  top_3_Sorted_AREA.NAME_names[3], " had ", top_3_Sorted_AREA.NAME_counts[3], " cases."
))


# list all the variables (columns)
cat(names(LA_2024_02_Feb), sep = "\n")
# onto next variable

# Create a string for AREA_Names
AREA.NAMES_string <- '

# list all the AREA.NAME names
unique(LA_2024_02_Feb$AREA.NAME)
length(unique(LA_2024_02_Feb$AREA.NAME))
table(LA_2024_02_Feb$AREA.NAME)
AREA.NAME <- table(LA_2024_02_Feb$AREA.NAME)
AREA.NAME

# Max AREA_NAME
max_AREA.NAME <- names(which.max(AREA.NAME))
max_AREA.NAME
max_count_AREA.NAME <- max(AREA.NAME)
max_count_AREA.NAME

# Min AREA_NAME
min_AREA.NAME <- names(which.min(AREA.NAME))
min_AREA.NAME
min_count_AREA.NAME <- min(AREA.NAME)
min_count_AREA.NAME

cat("AREA Most incidents:\n", max_AREA.NAME, "with", max_count_AREA.NAME, "incidents\n\n")
cat("AREA Fewest incidents:\n", min_AREA.NAME, "with", min_count_AREA.NAME, "incidents\n")

# Sort AREA.name in decreasing order
Sorted_AREA.NAME <- sort(AREA.NAME, decreasing = TRUE)
head(Sorted_AREA.NAME)
tail(Sorted_AREA.NAME)

# Get top 3 most frequent AREA_NAME
top_3_Sorted_AREA.NAME <- head(Sorted_AREA.NAME, 3)
top_3_Sorted_AREA.NAME

# Extract SOrted AREA_NAME, names and counts
top_3_Sorted_AREA.NAME_names <- names(top_3_Sorted_AREA.NAME)
top_3_Sorted_AREA.NAME_names
top_3_Sorted_AREA.NAME_counts <- as.numeric(top_3_Sorted_AREA.NAME)
top_3_Sorted_AREA.NAME_counts

# Print summary sentence
cat(paste0(
  top_3_Sorted_AREA.NAME_names[1], " had the most incidents with ", top_3_Sorted_AREA.NAME_counts[1], " cases; ",
  top_3_Sorted_AREA.NAME_names[2], " followed with ", top_3_Sorted_AREA.NAME_counts[2], " cases; and ",
  top_3_Sorted_AREA.NAME_names[3], " had ", top_3_Sorted_AREA.NAME_counts[3], " cases."
))


'
# list all the variables (columns)
cat(names(LA_2024_02_Feb), sep = "\n")
# onto next variable


# 1. Switch AREA.NAME to Crm.Cd.Desc
Crm_Cd_Desc_string <- gsub("AREA.NAME", "Crm.Cd.Desc", AREA.NAMES_string)
cat(Crm_Cd_Desc_string)
# 2. Switch AREA (as a word) to Crm_Cd_Desc (variable prefix)
Crm_Cd_Desc_string <- gsub("AREA", "Crm.Cd.Desc", Crm_Cd_Desc_string)
cat(Crm_Cd_Desc_string)
# Paste output
# list all the Crm.Cd.Desc names
unique(LA_2024_02_Feb$Crm.Cd.Desc)
length(unique(LA_2024_02_Feb$Crm.Cd.Desc))
table(LA_2024_02_Feb$Crm.Cd.Desc)
Crm.Cd.Desc <- table(LA_2024_02_Feb$Crm.Cd.Desc)
Crm.Cd.Desc

# Max Crm.Cd.Desc
max_Crm.Cd.Desc <- names(which.max(Crm.Cd.Desc))
max_Crm.Cd.Desc
max_count_Crm.Cd.Desc <- max(Crm.Cd.Desc)
max_count_Crm.Cd.Desc

# Min Crm.Cd.Desc
min_Crm.Cd.Desc <- names(which.min(Crm.Cd.Desc))
min_Crm.Cd.Desc
min_count_Crm.Cd.Desc <- min(Crm.Cd.Desc)
min_count_Crm.Cd.Desc

cat("Crm.Cd.Desc Most incidents:
", max_Crm.Cd.Desc, "with", max_count_Crm.Cd.Desc, "incidents

")
cat("Crm.Cd.Desc Fewest incidents:
", min_Crm.Cd.Desc, "with", min_count_Crm.Cd.Desc, "incidents
")

# Sort Crm.Cd.Desc.name in decreasing order
Sorted_Crm.Cd.Desc <- sort(Crm.Cd.Desc, decreasing = TRUE)
head(Sorted_Crm.Cd.Desc)
tail(Sorted_Crm.Cd.Desc)

# Get top 3 most frequent Crm.Cd.Desc
top_3_Sorted_Crm.Cd.Desc <- head(Sorted_Crm.Cd.Desc, 3)
top_3_Sorted_Crm.Cd.Desc

# Extract SOrted Crm.Cd.Desc, names and counts
top_3_Sorted_Crm.Cd.Desc_names <- names(top_3_Sorted_Crm.Cd.Desc)
top_3_Sorted_Crm.Cd.Desc_names
top_3_Sorted_Crm.Cd.Desc_counts <- as.numeric(top_3_Sorted_Crm.Cd.Desc)
top_3_Sorted_Crm.Cd.Desc_counts

# Print summary sentence
cat(paste0(
  top_3_Sorted_Crm.Cd.Desc_names[1], " had the most incidents with ", top_3_Sorted_Crm.Cd.Desc_counts[1], " cases; ",
  top_3_Sorted_Crm.Cd.Desc_names[2], " followed with ", top_3_Sorted_Crm.Cd.Desc_counts[2], " cases; and ",
  top_3_Sorted_Crm.Cd.Desc_names[3], " had ", top_3_Sorted_Crm.Cd.Desc_counts[3], " cases."
))




###########################
# Filter dataset for "VEHICLE - STOLEN"
VEHICLE_STOLEN_table <- subset(LA_2024_02_Feb, Crm.Cd.Desc == "VEHICLE - STOLEN", select = c("AREA.NAME", "LOCATION", "Cross.Street", "Premis.Desc", "TIME.OCC", "DATE.OCC"))
head(VEHICLE_STOLEN_table)
tail(VEHICLE_STOLEN_table)


# list all the AREA.NAME for Vehicle - Stolen
unique(VEHICLE_STOLEN_tablet$AREA.NAME)
length(unique(VEHICLE_STOLEN_table$AREA.NAME))
table(VEHICLE_STOLEN_table$AREA.NAME)
VEHICLE_STOLEN_AREA_NAME_table <- table(VEHICLE_STOLEN_table$AREA.NAME)
head(VEHICLE_STOLEN_AREA_NAME_table)

# Max VEHICLE_sTOLEN_AREA_NAME
max_VEHICLE_STOLEN_AREA_NAME <- names(which.max(VEHICLE_STOLEN_AREA_NAME_table))
max_VEHICLE_STOLEN_AREA_NAME
max_VEHICLE_STOLEN_AREA_NAME_count <- max(VEHICLE_STOLEN_AREA_NAME_table)
max_VEHICLE_STOLEN_AREA_NAME_count

# Min VEHICLE_sTOLEN_AREA_NAME
min_VEHICLE_STOLEN_AREA_NAME <- names(which.min(VEHICLE_STOLEN_AREA_NAME_table))
min_VEHICLE_STOLEN_AREA_NAME
min_VEHICLE_STOLEN_AREA_NAME_count <- min(VEHICLE_STOLEN_AREA_NAME_table)
min_VEHICLE_STOLEN_AREA_NAME_count

cat("VEHICLE STOLEN Most incidents: ", max_VEHICLE_STOLEN_AREA_NAME, "with", max_VEHICLE_STOLEN_AREA_NAME_count, "incidents ")
cat("VEHICLE STOLEN Fewest incidents: ", min_VEHICLE_STOLEN_AREA_NAME, "with", min_VEHICLE_STOLEN_AREA_NAME_count, "incidents ")


### top 3 sorted for AREA.NAME
# Sort the AREA.NAME frequency table in descending order
Sorted_VEHICLE_STOLEN_AREA_NAME <- sort(VEHICLE_STOLEN_AREA_NAME_table, decreasing = TRUE)
Sorted_VEHICLE_STOLEN_AREA_NAME
# Get top 3 areas
top_3_VEHICLE_STOLEN_AREA_NAME <- head(Sorted_VEHICLE_STOLEN_AREA_NAME, 3)
top_3_VEHICLE_STOLEN_AREA_NAME
# Extract names and counts
top_3_VEHICLE_STOLEN_AREA_NAME_names <- names(top_3_VEHICLE_STOLEN_AREA_NAME)
top_3_VEHICLE_STOLEN_AREA_NAME_names 
top_3_VEHICLE_STOLEN_AREA_NAME_counts <- as.numeric(top_3_VEHICLE_STOLEN_AREA_NAME)
top_3_VEHICLE_STOLEN_AREA_NAME_counts
# Print summary sentence
cat(paste0(
  top_3_VEHICLE_STOLEN_AREA_NAME_names[1], " had the most vehicle thefts with ", top_3_VEHICLE_STOLEN_AREA_NAME_counts[1], " cases; ",
  top_3_VEHICLE_STOLEN_AREA_NAME_names[2], " followed with ", top_3_VEHICLE_STOLEN_AREA_NAME_counts[2], " cases; and ",
  top_3_VEHICLE_STOLEN_AREA_NAME_names[3], " had ", top_3_VEHICLE_STOLEN_AREA_NAME_counts[3], " cases."
))

#######
# Create subset for Newton area only for Vehicle stolen
VEHICLE_STOLEN_Newton_table <- subset(VEHICLE_STOLEN_table, AREA.NAME == "Newton")
VEHICLE_STOLEN_Newton_table
# View the first few rows
head(VEHICLE_STOLEN_Newton_table)
tail(VEHICLE_STOLEN_Newton_table) 

# Max/Min LOCATION for subset NEwton area for vehicle stolen
VEHICLE_STOLEN_Newton_LOCATION_table <- table(VEHICLE_STOLEN_Newton_table$LOCATION)
head(VEHICLE_STOLEN_Newton_LOCATION_table)

max_VEHICLE_STOLEN_Newton_LOCATION_name <- names(which.max(VEHICLE_STOLEN_Newton_LOCATION_table))
max_VEHICLE_STOLEN_Newton_LOCATION_name
max_VEHICLE_STOLEN_Newton_LOCATION_count <- max(VEHICLE_STOLEN_Newton_LOCATION_table)
max_VEHICLE_STOLEN_Newton_LOCATION_count

VEHICLE_STOLEN_Newton_Cross.Street_table <- table(VEHICLE_STOLEN_Newton_table$Cross.Street)
VEHICLE_STOLEN_Newton_Cross.Street_table
head(VEHICLE_STOLEN_Newton_Cross.Street_table )
# Max/Min Cross.Street for subset Newton area for vehicle stolen
# Filter out rows with blank Cross.Street values
VEHICLE_STOLEN_Newton_Cross.Street_table <- VEHICLE_STOLEN_Newton_table$Cross.Street[VEHICLE_STOLEN_Newton_table$Cross.Street != ""]
VEHICLE_STOLEN_Newton_Cross.Street_table

# Create table without blank entries
VEHICLE_STOLEN_Newton_Cross.Street_table1 <- table(VEHICLE_STOLEN_Newton_Cross.Street_table)
VEHICLE_STOLEN_Newton_Cross.Street_table1

# Get max/min(1)
max_VEHICLE_STOLEN_Newton_Cross.Street_name <- names(which.max(VEHICLE_STOLEN_Newton_Cross.Street_table1))
max_VEHICLE_STOLEN_Newton_Cross.Street_name
max_VEHICLE_STOLEN_Newton_Cross.Street_count <- max(VEHICLE_STOLEN_Newton_Cross.Street_table1)
max_VEHICLE_STOLEN_Newton_Cross.Street_count
# Output result
cat("Most frequent Cross.Street (non-empty):", max_VEHICLE_STOLEN_Newton_Cross.Street_name, "with", max_VEHICLE_STOLEN_Newton_Cross.Street_count, "incidents\n")

# Max/Min(1) TIME.OCC
VEHICLE_STOLEN_Newton_TIME.OCC_table <- table(VEHICLE_STOLEN_Newton_table$TIME.OCC)
head(VEHICLE_STOLEN_Newton_TIME.OCC_table)
max_VEHICLE_STOLEN_Newton_TIME.OCC_name <- names(which.max(VEHICLE_STOLEN_Newton_TIME.OCC_table))
max_VEHICLE_STOLEN_Newton_TIME.OCC_name 
max_VEHICLE_STOLEN_Newton_TIME.OCC_count <- max(VEHICLE_STOLEN_Newton_TIME.OCC_table)
max_VEHICLE_STOLEN_Newton_TIME.OCC_count

# Create frequency table
VEHICLE_STOLEN_Newton_TIME.OCC_table <- table(VEHICLE_STOLEN_Newton_table$TIME.OCC)
VEHICLE_STOLEN_Newton_TIME.OCC_table 
# Sort in descending order
sorted_VEHICLE_STOLEN_Newton_TIME.OCC <- sort(VEHICLE_STOLEN_Newton_TIME.OCC_table, decreasing = TRUE)
sorted_VEHICLE_STOLEN_Newton_TIME.OCC 
# Extract top 3 TIME.OCC names and counts
top_3_sorted_VEHICLE_STOLEN_Newton_TIME.OCC_names <- names(sorted_VEHICLE_STOLEN_Newton_TIME.OCC[1:3])
top_3_sorted_VEHICLE_STOLEN_Newton_TIME.OCC_names
top_3_sorted_VEHICLE_STOLEN_Newton_TIME.OCC_counts <- as.numeric(sorted_VEHICLE_STOLEN_Newton_TIME.OCC[1:3])
top_3_sorted_VEHICLE_STOLEN_Newton_TIME.OCC_counts
# Print results
top_3_sorted_VEHICLE_STOLEN_Newton_TIME.OCC <- data.frame(TIME.OCC = top_3_sorted_VEHICLE_STOLEN_Newton_TIME.OCC_names, Count = top_3_sorted_VEHICLE_STOLEN_Newton_TIME.OCC_counts)
print(top_3_sorted_VEHICLE_STOLEN_Newton_TIME.OCC)
                      
# Max/Min DATE.OCC
VEHICLE_STOLEN_Newton_DATE.OCC_table <- table(VEHICLE_STOLEN_Newton_table$DATE.OCC)
head(VEHICLE_STOLEN_Newton_DATE.OCC_table)
max_VEHICLE_STOLEN_Newton_DATE.OCC_name <- names(which.max(VEHICLE_STOLEN_Newton_DATE.OCC_table))
max_VEHICLE_STOLEN_Newton_DATE.OCC_name
max_VEHICLE_STOLEN_Newton_DATE.OCC_count <- max(VEHICLE_STOLEN_Newton_DATE.OCC_table)
max_VEHICLE_STOLEN_Newton_DATE.OCC_count 

# Create frequency table
VEHICLE_STOLEN_Newton_DATE.OCC_table <- table(VEHICLE_STOLEN_Newton_table$DATE.OCC)
VEHICLE_STOLEN_Newton_DATE.OCC_table
head(VEHICLE_STOLEN_Newton_DATE.OCC_table)
# Sort in descending order
sorted_DATE_VEHICLE_STOLEN_Newton_DATE.OCC <- sort(VEHICLE_STOLEN_Newton_DATE.OCC_table, decreasing = TRUE)
sorted_DATE_VEHICLE_STOLEN_Newton_DATE.OCC

# Extract top 3 DATE.OCC names and counts
top_3_sorted_DATE_VEHICLE_STOLEN_Newton_DATE.OCC_names <- names(sorted_DATE_VEHICLE_STOLEN_Newton_DATE.OCC[1:3])
top_3_sorted_DATE_VEHICLE_STOLEN_Newton_DATE.OCC_names
top_3_sorted_DATE_VEHICLE_STOLEN_Newton_DATE.OCC_counts <- as.numeric(sorted_DATE_VEHICLE_STOLEN_Newton_DATE.OCC[1:3])
top_3_sorted_DATE_VEHICLE_STOLEN_Newton_DATE.OCC_counts

# Print results
top_3_sorted_DATE_VEHICLE_STOLEN_Newton_DATE.OCC <- data.frame(DATE.OCC = top_3_sorted_DATE_VEHICLE_STOLEN_Newton_DATE.OCC_names, Counts = top_3_sorted_DATE_VEHICLE_STOLEN_Newton_DATE.OCC_counts)
top_3_sorted_DATE_VEHICLE_STOLEN_Newton_DATE.OCC

# Convert DATE.OCC to Date format (if not already)
top_3_sorted_DATE_VEHICLE_STOLEN_Newton_DATE.OCC$DATE.OCC <- as.Date(top_3_sorted_DATE_VEHICLE_STOLEN_Newton_DATE.OCC$DATE.OCC, format = "%m/%d/%Y")
top_3_sorted_DATE_VEHICLE_STOLEN_Newton_DATE.OCC$DATE.OC
# Add a new column for the day of the week
top_3_sorted_DATE_VEHICLE_STOLEN_Newton_DATE.OCC$Day.of.Week <- weekdays(top_3_sorted_DATE_VEHICLE_STOLEN_Newton_DATE.OCC$DATE.OCC)
top_3_sorted_DATE_VEHICLE_STOLEN_Newton_DATE.OCC$Day.of.Week
# Print updated results
print(top_3_sorted_DATE_VEHICLE_STOLEN_Newton_DATE.OCC)















##################

# classify each crime as violent vs non violent
# Define pattern of keywords that signify violent crimes
violent_keywords <- c("ASSAULT", "BATTERY", "HOMICIDE", "ROBBERY", "RAPE", "KIDNAPPING", "STALKING", 
                      "THREAT", "SEX", "SODOMY", "LEWD", "INTIMATE", "CHILD ABUSE", "WEAPON", 
                      "SHOTS FIRED", "CRIMINAL THREATS")
violent_keywords
# Combine keywords into a single regex pattern (case insensitive)
violent_pattern <- paste(violent_keywords, collapse = "|")
violent_pattern

# Get unique crime descriptions
crime_types <- unique(LA_2024_02_Feb$Crm.Cd.Desc)
crime_types

# Classify each crime as Violent or Non-Violent using grepl
crime_class <- ifelse(grepl(violent_pattern, crime_types, ignore.case = TRUE), "Violent", "Non-Violent")
crime_class

# Create a named vector or data frame for classification
crime_map <- setNames(crime_class, crime_types)
crime_map

# Add classification back to your data
LA_2024_02_Feb$Crime.Type <- crime_map[LA_2024_02_Feb$Crm.Cd.Desc]
LA_2024_02_Feb$Crime.Type
# Optional summary
table(LA_2024_02_Feb$Crime.Type)

# list all the variables (columns)
cat(names(LA_2024_02_Feb), sep = "\n")
# onto next variable




# 1. Switch AREA.NAME to Premis.Desc
Premis.Desc_string <- gsub("AREA.NAME", "Premis.Desc", AREA.NAMES_string)
cat(Premis.Desc_string)
# 2. Switch AREA (as a word) to Premis_Desc (variable prefix)
Premis.Desc_string_1 <- gsub("AREA", "Premis_Desc", Premis.Desc_string)
cat(Premis.Desc_string_1)
# Paste output
# list all the Premis.Desc names
unique(LA_2024_02_Feb$Premis.Desc)
length(unique(LA_2024_02_Feb$Premis.Desc))
table(LA_2024_02_Feb$Premis.Desc)
Premis.Desc <- table(LA_2024_02_Feb$Premis.Desc)
Premis.Desc

# Max Premis.Desc
max_Premis.Desc <- names(which.max(Premis.Desc))
max_Premis.Desc
max_count_Premis.Desc <- max(Premis.Desc)
max_count_Premis.Desc

# Min Premis.Desc
min_Premis.Desc <- names(which.min(Premis.Desc))
min_Premis.Desc
min_count_Premis.Desc <- min(Premis.Desc)
min_count_Premis.Desc

cat("Premis_Desc Most incidents:
", max_Premis.Desc, "with", max_count_Premis.Desc, "incidents

")
cat("Premis_Desc Fewest incidents:
", min_Premis.Desc, "with", min_count_Premis.Desc, "incidents
")

# Sort Premis_Desc.name in decreasing order
Sorted_Premis.Desc <- sort(Premis.Desc, decreasing = TRUE)
head(Sorted_Premis.Desc)
tail(Sorted_Premis.Desc)

# Get top 3 most frequent Premis.Desc
top_3_Sorted_Premis.Desc <- head(Sorted_Premis.Desc, 3)
top_3_Sorted_Premis.Desc

# Extract SOrted Premis.Desc, names and counts
top_3_Sorted_Premis.Desc_names <- names(top_3_Sorted_Premis.Desc)
top_3_Sorted_Premis.Desc_names
top_3_Sorted_Premis.Desc_counts <- as.numeric(top_3_Sorted_Premis.Desc)
top_3_Sorted_Premis.Desc_counts

# Print summary sentence
cat(paste0(
  top_3_Sorted_Premis.Desc_names[1], " had the most incidents with ", top_3_Sorted_Premis.Desc_counts[1], " cases; ",
  top_3_Sorted_Premis.Desc_names[2], " followed with ", top_3_Sorted_Premis.Desc_counts[2], " cases; and ",
  top_3_Sorted_Premis.Desc_names[3], " had ", top_3_Sorted_Premis.Desc_counts[3], " cases."
))


# create separate table for MTA
# Trim whitespace, just in case
names(Premis.Desc) <- trimws(names(Premis.Desc))
# Extract rows that contain "MTA" (case-insensitive for safety)
MTA_Premis.Desc <- Premis.Desc[grep("MTA", names(Premis.Desc), ignore.case = TRUE)]
print(MTA_Premis.Desc)
# Sum all rows in MTA_Premis.Desc that contain "MTA"
total_MTA <- sum(MTA_Premis.Desc[grep("MTA", names(MTA_Premis.Desc))])
# Print the result
cat("Total incidents involving MTA locations:", total_MTA, "\n")
# Max MTA_Premis.Desc
max_MTA_Premis.Desc <- names(which.max(MTA_Premis.Desc))
max_MTA_Premis.Desc
max_count_MTA_Premis.Desc <- max(MTA_Premis.Desc)
max_count_MTA_Premis.Desc
cat("Most frequent MTA location:", max_MTA_Premis.Desc, "with", max_count_MTA_Premis.Desc, "incidents\n")



# list all the variables (columns)
cat(names(LA_2024_02_Feb), sep = "\n")
# onto next variable


# 1. Switch AREA.NAME to LOCATION
LOCATION_string_1 <- gsub("AREA.NAME", "LOCATION", AREA.NAMES_string)
cat(LOCATION_string_1)
# 2. Switch AREA (as a word) to Premis_Desc (variable prefix)
LOCATION_string_2 <- gsub("AREA", "LOCATION", LOCATION_string_1)
cat(LOCATION_string_2)
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

cat("LOCATION Most incidents: ", max_LOCATION, "with", max_count_LOCATION, "incidents")
cat("LOCATION Fewest incidents: ", min_LOCATION, "with", min_count_LOCATION, "incidents")

# Sort LOCATION.name in decreasing order
Sorted_LOCATION <- sort(LOCATION, decreasing = TRUE)
head(Sorted_LOCATION)
tail(Sorted_LOCATION)

# Get top 3 most frequent LOCATION
top_3_Sorted_LOCATION <- head(Sorted_LOCATION, 3)
top_3_Sorted_LOCATION

# Extract SOrted LOCATION, names and counts
top_3_Sorted_LOCATION_names <- names(top_3_Sorted_LOCATION)
top_3_Sorted_LOCATION_names
top_3_Sorted_LOCATION_counts <- as.numeric(top_3_Sorted_LOCATION)
top_3_Sorted_LOCATION_counts

# Print summary sentence
cat(paste0(
  top_3_Sorted_LOCATION_names[1], " had the most incidents with ", top_3_Sorted_LOCATION_counts[1], " cases; ",
  top_3_Sorted_LOCATION_names[2], " followed with ", top_3_Sorted_LOCATION_counts[2], " cases; and ",
  top_3_Sorted_LOCATION_names[3], " had ", top_3_Sorted_LOCATION_counts[3], " cases."
))


# 1. Switch AREA.NAME to Cross.Street
Cross_Street_String_1 <- gsub("AREA.NAME", "Cross.Street", AREA.NAMES_string)
cat(Cross_Street_String_1)
# 2. Switch AREA (as a word) to Premis_Desc (variable prefix)
Cross_Street_string_2 <- gsub("AREA", "Cross.Street", Cross_Street_String_1)
cat(Cross_Street_string_2)
# paste below, add code to remove empty strings
# List all the Cross.Street names
unique(LA_2024_02_Feb$Cross.Street)
length(unique(LA_2024_02_Feb$Cross.Street))
table(LA_2024_02_Feb$Cross.Street)
Cross.Street <- table(LA_2024_02_Feb$Cross.Street)
# Remove entries with empty string names
Cross.Street <- Cross.Street[names(Cross.Street) != ""]
head(Cross.Street)
tail(Cross.Street)

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

cat("Cross.Street Most incidents:\n", max_Cross.Street, "with", max_count_Cross.Street, "incidents\n\n")
cat("Cross.Street Fewest incidents:\n", min_Cross.Street, "with", min_count_Cross.Street, "incidents\n")

# Sort Cross.Street names in decreasing order
Sorted_Cross.Street <- sort(Cross.Street, decreasing = TRUE)
head(Sorted_Cross.Street)
tail(Sorted_Cross.Street)

# Get top 3 most frequent Cross.Street
top_3_Sorted_Cross.Street <- head(Sorted_Cross.Street, 3)
# Extract names and counts
top_3_Sorted_Cross.Street_names <- names(top_3_Sorted_Cross.Street)
top_3_Sorted_Cross.Street_names 
top_3_Sorted_Cross.Street_counts <- as.numeric(top_3_Sorted_Cross.Street)
top_3_Sorted_Cross.Street_counts
# Print summary sentence
cat(paste0(
  top_3_Sorted_Cross.Street_names[1], " had the most incidents with ", top_3_Sorted_Cross.Street_counts[1], " cases; ",
  top_3_Sorted_Cross.Street_names[2], " followed with ", top_3_Sorted_Cross.Street_counts[2], " cases; and ",
  top_3_Sorted_Cross.Street_names[3], " had ", top_3_Sorted_Cross.Street_counts[3], " cases."
))



# list all the variables (columns)
cat(names(LA_2024_02_Feb), sep = "\n")
# onto next variable


# 1. Switch AREA.NAME to TIME.OCC
TIME.OCC_string1 <- gsub("AREA.NAME", "TIME.OCC", AREA.NAMES_string)
cat(TIME.OCC_string1)
# 2. Switch AREA (as a word) to TIME.OCC (variable prefix)
TIME.OCC_string2 <- gsub("AREA", "TIME.OCC", TIME.OCC_string1)
cat(TIME.OCC_string2)
# paste below
# list all the TIME.OCC names
unique(LA_2024_02_Feb$TIME.OCC)
length(unique(LA_2024_02_Feb$TIME.OCC))
table(LA_2024_02_Feb$TIME.OCC)
TIME.OCC <- table(LA_2024_02_Feb$TIME.OCC)
TIME.OCC

# Max TIME.OCC
max_TIME.OCC <- names(which.max(TIME.OCC))
max_TIME.OCC
max_count_TIME.OCC <- max(TIME.OCC)
max_count_TIME.OCC

# Min TIME.OCC
min_TIME.OCC <- names(which.min(TIME.OCC))
min_TIME.OCC
min_count_TIME.OCC <- min(TIME.OCC)
min_count_TIME.OCC

cat("TIME.OCC Most incidents: ", max_TIME.OCC, "with", max_count_TIME.OCC, "incidents")
cat("TIME.OCC Fewest incidents: ", min_TIME.OCC, "with", min_count_TIME.OCC, "incidents")

# Sort TIME.OCC.name in decreasing order
Sorted_TIME.OCC <- sort(TIME.OCC, decreasing = TRUE)
head(Sorted_TIME.OCC)
tail(Sorted_TIME.OCC)

# Get top 3 most frequent TIME.OCC
top_3_Sorted_TIME.OCC <- head(Sorted_TIME.OCC, 3)
top_3_Sorted_TIME.OCC

# Extract SOrted TIME.OCC, names and counts
top_3_Sorted_TIME.OCC_names <- names(top_3_Sorted_TIME.OCC)
top_3_Sorted_TIME.OCC_names
top_3_Sorted_TIME.OCC_counts <- as.numeric(top_3_Sorted_TIME.OCC)
top_3_Sorted_TIME.OCC_counts

# Print summary sentence
cat(paste0(
  top_3_Sorted_TIME.OCC_names[1], " had the most incidents with ", top_3_Sorted_TIME.OCC_counts[1], " cases; ",
  top_3_Sorted_TIME.OCC_names[2], " followed with ", top_3_Sorted_TIME.OCC_counts[2], " cases; and ",
  top_3_Sorted_TIME.OCC_names[3], " had ", top_3_Sorted_TIME.OCC_counts[3], " cases."
))


# list all the variables (columns)
cat(names(LA_2024_02_Feb), sep = "\n")
# onto next variable


# 1. Switch AREA.NAME to DATE.OCC
DATE.OCC_string1 <- gsub("AREA.NAME", "DATE.OCC", AREA.NAMES_string)
cat(DATE.OCC_string1)
# 2. Switch AREA (as a word) to DATE.OCC (variable prefix)
DATE.OCC_string2 <- gsub("AREA", "DATE.OCC", DATE.OCC_string1)
cat(DATE.OCC_string2)
# paste below
# list all the DATE.OCC names
unique(LA_2024_02_Feb$DATE.OCC)
length(unique(LA_2024_02_Feb$DATE.OCC))
table(LA_2024_02_Feb$DATE.OCC)
DATE.OCC <- table(LA_2024_02_Feb$DATE.OCC)
DATE.OCC

# Max DATE.OCC
max_DATE.OCC <- names(which.max(DATE.OCC))
max_DATE.OCC
# get the day of the week
max_DATE.OCC_date <- as.Date("02/16/2024", format = "%m/%d/%Y")
max_day_DATE.OCC <-weekdays(max_DATE.OCC_date)
max_day_DATE.OCC                         
# get teh count               
max_count_DATE.OCC <- max(DATE.OCC)
max_count_DATE.OCC


# Min DATE.OCC
min_DATE.OCC <- names(which.min(DATE.OCC))
min_DATE.OCC
# get the day of the week
min_DATE.OCC_date <- as.Date("02/16/2024", format = "%m/%d/%Y")
min_day_DATE.OCC <-weekdays(min_DATE.OCC_date)
min_day_DATE.OCC  
# get the count
min_count_DATE.OCC <- min(DATE.OCC)
min_count_DATE.OCC

cat("DATE.OCC Most incidents: ", max_DATE.OCC, "with", max_count_DATE.OCC, "incidents on a ", max_day_DATE.OCC )
cat("DATE.OCC Fewest incidents: ", min_DATE.OCC, "with", min_count_DATE.OCC, "incidents on a ", min_day_DATE.OCC)

# Sort DATE.OCC.name in decreasing order
Sorted_DATE.OCC <- sort(DATE.OCC, decreasing = TRUE)
head(Sorted_DATE.OCC)
tail(Sorted_DATE.OCC)

# Get top 3 most frequent DATE.OCC
top_3_Sorted_DATE.OCC <- head(Sorted_DATE.OCC, 3)
top_3_Sorted_DATE.OCC

# Extract SOrted DATE.OCC, names and counts
top_3_Sorted_DATE.OCC_names <- names(top_3_Sorted_DATE.OCC)
top_3_Sorted_DATE.OCC_names
top_3_Sorted_DATE.OCC_counts <- as.numeric(top_3_Sorted_DATE.OCC)
top_3_Sorted_DATE.OCC_counts

# Convert the top 3 date strings to Date objects
top_3_Sorted_DATE.OCC_dates <- as.Date(top_3_Sorted_DATE.OCC_names, format = "%m/%d/%Y %I:%M:%S %p")

# Get the day of the week for each date
top_3_Sorted_DATE.OCC_days <- weekdays(top_3_Sorted_DATE.OCC_dates)

# Print summary with day of the week
cat(paste0(
  top_3_Sorted_DATE.OCC_names[1], " (", top_3_Sorted_DATE.OCC_days[1], ") had the most incidents with ", top_3_Sorted_DATE.OCC_counts[1], " cases; ",
  top_3_Sorted_DATE.OCC_names[2], " (", top_3_Sorted_DATE.OCC_days[2], ") followed with ", top_3_Sorted_DATE.OCC_counts[2], " cases; and ",
  top_3_Sorted_DATE.OCC_names[3], " (", top_3_Sorted_DATE.OCC_days[3], ") had ", top_3_Sorted_DATE.OCC_counts[3], " cases."
))




# list all the variables (columns)
cat(names(LA_2024_02_Feb), sep = "\n")




### remove all memory from R console
rm(list = ls())
ls()
list.files()
dir()
