

###########################
##### Members Section #####
###########################

# this creates a section for each city
# takes in a city and creates it's own row
# needs to be less than 4
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

## this creates a bio div for each member
## takes variables such as first name, last name, etc..
## returns html of a bio section
memberBio <- function(first, last, authorURL, bioImage, program, programURl, 
                      university,universityURL, city, country, email, bio){
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

## function to map through each researcher per city
## searches for email in the main data frame from google sheets
## and uses the `memberBio` function return html
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

# function to map each city map and it's members
# takes in city and  main df, returns html of city and members
cityEach <- function(city, df){
    emails <- df %>% 
        filter(City == city) %>% 
        pull(Email)
    
    cityBio(city, emails %>%  map(~memberEach(.x, df)))
}

    
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
# with a description
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
