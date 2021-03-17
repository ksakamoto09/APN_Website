library(googlesheets4)
library(dplyr)
library(sf)
library(stringr)
library(lubridate)
library(leaflet)
library(htmltools)
library(purrr)

cityBio <- function(city, ...){
    if(length(list(...))>4){
        stop("too many members, has to be less than 4")
    }
    div(h2(city),
    div(class="row",
        ...
        )
    )
}

memberBio <- function(first, last, bioImage, program, university, location, email, bio){
    name <- paste(first, last)
    div(class="col-md-4",
        a(href="#", `data-featherlight`=paste0("#bio-",last),
          img(src=bioImage, width="200", height="200")),
        p(tyle="text-align:left; color:#727272;", name),
        div(style="display:none",
            div(id=paste0("bio-", last),
                h3(name),
                p(style="font-family:Lato; font-weight:500; font-style:normal; 
                  font-size:12px; letter-spacing:.4px; line-height:2.18em; color:#999999;",
                  program,
                  br(),
                  university,
                  br(),
                  location,
                  br(),
                  email,
                  p(bio)))))
}


mainDF <- read_sheet("https://docs.google.com/spreadsheets/d/1kcZ6gZW1_okGLb1QscCwcjCAi4W3osO18D-uceZcJME/edit?usp=sharing",sheet = 1)

researchersDF <- mainDF %>% 
    filter(Role == "Researcher")
cities <- mainDF %>% 
    filter(Role == "Researcher") %>% 
    distinct(City) %>% pull(City)

cityEach <- function(city, df){
    emails <- df %>% 
        filter(City == city) %>% 
        pull(Email)
    
    cityBio(city, emails %>%  map(~memberEach(.x, df)))
}

## map through cities
#cities %>% map(~cityEach(.x, researchersDF))

## map through researchers per city
memberEach <- function(emails, df){
    subset <- df %>% 
        filter(Email == emails)
    memberBio(paste(subset$Title, subset$First), 
              subset$Last,
              subset$`image link`,
              subset$program,
              subset$`Institution Name`,
              paste0(subset$City, ", ", subset$Country),
              subset$Email,
              "Add in")
}

# researchersDF$Email %>% map(~memberEach(.x, researchersDF))
# 
# 
# cities %>% map(~cityEach(.x, researchersDF)) %>%htmltools::tagList()
    