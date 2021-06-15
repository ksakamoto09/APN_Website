## set root, data directory, and shapefile directory
root <- rprojroot::find_rstudio_root_file()
dataDir <- file.path(root, 'data')
shapeDir <- file.path(root, 'shape')

## Read in data
mainDF <- read_sheet("https://docs.google.com/spreadsheets/d/1OhUJk2CZ2dn0_37nf98lZQaDyviFr0jf6oShuy6oaiI/edit?usp=sharing",sheet = 1)

# subset researchers
researchersDF <- mainDF %>% 
    filter(Role == "Researcher")

# get distinct cities
cities <- researchersDF %>% 
    distinct(City) %>% pull(City)