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
t_AREA <- table(LA_2024_02_Feb$AREA.NAME)
t_AREA

# Max AREA
max_AREA <- names(which.max(t_AREA))
max_AREA
max_count_AREA <- max(t_AREA)
max_count_AREA

# Min AREA
min_AREA <- names(which.min(t_AREA))
min_AREA
min_count_AREA <- min(t_AREA)
min_count_AREA

cat("Most incidents:\n", max_AREA, "with", max_count_AREA, "incidents\n\n")
cat("Fewest incidents:\n", min_AREA, "with", min_count_AREA, "incidents\n")





unique(LA_2024_02_Feb$Crm.Cd.Desc)

length(unique(LA_2024_02_Feb$Crm.Cd.Desc))
sum(LA_2024_02_Feb$Crm.Cd.Desc == "VIOLATION OF TEMPORARY RESTRAINING ORDER")




unique(LA_2024_02_Feb$Premis.Desc)
length(unique(LA_2024_02_Feb$Premis.Desc))
max((LA_2024_02_Feb$Premis.Desc))
sum(LA_2024_02_Feb$Premis.Desc == "YARD (RESIDENTIAL/BUSINESS)")


unique(LA_2024_02_Feb$LOCATION)
length(unique(LA_2024_02_Feb$LOCATION))
max((LA_2024_02_Feb$LOCATION))
sum(LA_2024_02_Feb$LOCATION ==  "ZOO DR")
sum(grepl("ZOO\\s*DR", LA_2024_02_Feb$LOCATION))

### remove all memory from R console
rm(list = ls())
ls()
list.files()
dir()

    