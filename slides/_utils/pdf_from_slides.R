##############################################################################
# Generating pdf and extracting R code from quarto (revealjs) slides
# -----------------------------------------------------------------------
# Written by Ladislas Nalborczyk
# E-mail: ladislas.nalborczyk@gmail.com
# Last updated on November 16, 2022
#######################################################################

library(renderthis)
library(pagedown)
library(stringr)

# listing extant .html slides in the /html/ folder
slides <- list.files(
    path = "html", pattern = ".html",
    recursive = TRUE, full.names = TRUE
    )

# some example
# input <- slides[10]

for (input in slides) { # for each course

    # printing progress
    print(paste0("Printing '", input, "'...") )
    
    # defining the output
    course <- str_extract_all(string = input, pattern = "(?<=html/).+(?=.html)")[[1]]
    output <- paste0("pdf/", course, ".pdf")
    
    if (course == "cours01") {
        
        # printing it using renderthis
        renderthis::to_pdf(
            # from = input,
            from = paste0("_", course, "/", course, ".qmd"),
            # to = output
            complex_slides = FALSE
            )
        
    } else {
        
        # printing it using pagedown
        pagedown::chrome_print(
            input = input,
            output = output,
            format = "pdf",
            timeout = 60
            )
        
    }
    
    # extracting the R code from slides
    knitr::purl(
        input = paste0("_", course, "/", course, ".qmd"),
        output = paste0("code/", course, ".R"),
        documentation = 1
        )
    
}
