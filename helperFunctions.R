library(googlesheets4)
library(dplyr)
library(sf)
library(stringr)
library(lubridate)
library(leaflet)
library(htmltools)
library(purrr)

###########################
##### Members Section #####
###########################

# this creates a section for each city
cityBio <- function(city, ...){
    if(length(list(...))>4){
        stop("too many members, has to be less than 4")
    }
    div(h2(city),
    div(class="row",style = "margin-left:12.5%",
        ...
        ),
    hr()
    )
}

# this creates a bio for each member
memberBio <- function(first, last, authorURL, bioImage, program, programURl, university,universityURL, city, country, email, bio){
    name <- paste(first, last)
    siteURL <- paste0(stringr::str_to_lower(city),".html")
    location <- paste0(city, ", ", country)
    div(class="col-md-4",
        a(href="#", `data-featherlight`=paste0("#bio-",last),
          img(src=bioImage, width="200", height="200")),
        p(tyle="text-align:left; color:#727272;", name),
        div(
            div(class = "lightbox", 
                id=paste0("bio-", last),
                if(!is.na(authorURL)) a(href=authorURL, h3(name))
                    else h3(name),
                p(style="font-family:Lato; font-weight:500; font-style:normal; 
                  font-size:12px; letter-spacing:.4px; line-height:2.18em; color:#999999;",
                  a(href = universityURL, university),
                  br(),
                  if(!is.na(programURl)) a(href=programURl, program)
                  else program,
                  br(),
                  location,
                  br(),
                  a(href = email, "email"),
                  br(),
                  " - ",
                  a(href = siteURL, "APN City Page"),
                  br(),
                  p(bio)))))
}

## map through researchers per city
memberEach <- function(emails, df){
    subset <- df %>% 
        filter(Email == emails)
    memberBio(paste(subset$Title, subset$First), 
              subset$Last,
              subset$`Author Homepage`,
              subset$`image link`,
              subset$program,
              subset$programURL,
              subset$`Institution Name`,
              subset$`Institution Homepage`,
              subset$City, 
              subset$Country,
              subset$Email,
              subset$bio)
}

# for each city map each member in each city 
cityEach <- function(city, df){
    emails <- df %>% 
        filter(City == city) %>% 
        pull(Email)
    
    cityBio(city, emails %>%  map(~memberEach(.x, df)))
}

## Read in data
mainDF <- read_sheet("https://docs.google.com/spreadsheets/d/1OhUJk2CZ2dn0_37nf98lZQaDyviFr0jf6oShuy6oaiI/edit?usp=sharing",sheet = 1)

# subset researchers
researchersDF <- mainDF %>% 
    filter(Role == "Researcher")

# get distinct cities
cities <- researchersDF %>% 
    distinct(City) %>% pull(City)


## map through cities
#cities %>% map(~cityEach(.x, researchersDF))
# researchersDF$Email %>% map(~memberEach(.x, researchersDF))
# 
# 
# cities %>% map(~cityEach(.x, researchersDF)) %>%htmltools::tagList()
    
    
################################
##### Publications Section #####
################################

# this creates a section for each publication Type
pubType <- function(pub, pubDescription, ...){
    if(length(list(...))>4){
        stop("too many papers, has to be less than 4")
    }

    div(class="bandContent gallerySection",
        div(class="gallerySectionTitle", pub),
        div(class="galleryIntro", pubDescription),
        div(class="galleryItems",
            ...
        )
    )
}

# this creates a icon for each paper
paperInfo <- function(link, paperImage, title, authors, summary){
    div(class="galleryItem",
        a(href=link,
          img(class="galleryItemImage", src=paperImage)),
        div(class="galleryItemDescription",
            a(href=link,
              p(title)),
            strong(authors),
            em(summary))
        )
}
