## set root, data directory, and shapefile directory
root <- rprojroot::find_rstudio_root_file()
dataDir <- file.path(root, 'data')
shapeDir <- file.path(root, 'shape')

## Read in data
mainDF <- read_xlsx("ISC_APN/mainData.xlsx",sheet = 1)

# subset researchers
researchersDF <- mainDF %>% 
    filter(Role == "Researcher")

# get distinct cities
cities <- researchersDF %>% 
    distinct(City) %>% pull(City)
