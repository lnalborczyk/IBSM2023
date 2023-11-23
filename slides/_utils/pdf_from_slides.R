######################################################################
# Generating pdf and extracting R code from quarto (revealjs) slides #
# ------------------------------------------------------------------ #
# Written by Ladislas Nalborczyk                                     #
# E-mail: ladislas.nalborczyk@gmail.com                              #
# Last updated on November 23, 2023                                   #
######################################################################

library(renderthis)
library(pagedown)
library(stringr)

# listing extant .html slides in the /html/ folder
slides <- list.files(
    path = "html", pattern = "course",
    recursive = TRUE, full.names = TRUE
    )

# keeping only the first course (for testing)
slides <- slides[1]

for (input in slides) { # for each course

    # printing progress
    print(paste0("Printing '", input, "'...") )
    
    # defining the output
    course <- str_extract_all(string = input, pattern = "(?<=html/).+(?=.html)")[[1]]
    output <- paste0("pdf/", course, ".pdf")
    
    # if (course == "course01") {
    # 
    #     # printing it using renderthis
    #     renderthis::to_pdf(
    #         # from = input,
    #         from = paste0("_", course, "/", course, ".qmd"),
    #         # to = output
    #         complex_slides = FALSE
    #         )
    # 
    # } else {
    # 
    #     # printing it using pagedown
    #     pagedown::chrome_print(
    #         input = input,
    #         output = output,
    #         format = "pdf",
    #         timeout = 60
    #         )
    # 
    # }
    
    # printing it using pagedown
    pagedown::chrome_print(
        input = input,
        output = output,
        format = "pdf",
        timeout = 120
        )
    
    # extracting the R code from slides
    knitr::purl(
        input = paste0("_", course, "/", course, ".qmd"),
        output = paste0("code/", course, ".R"),
        documentation = 1
        )
    
}
