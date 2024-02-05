system("quarto render")
quarto::quarto_render("index.qmd")

library(stringr)

# Read the HTML file
html_as_text <- readLines("index.html", warn = FALSE)
system(paste("rm", shQuote("index.html")))
# Sys.sleep(2)


  
old_content <- "<title> | Byeong-Hak Choe</title>"
new_content <- "<title>B.H. Choe</title>"
html_as_text <- str_replace_all(html_as_text,
                                old_content,
                                new_content)

writeLines(html_as_text, 
           "index.html")



# file.copy("tmp/index.html", "index.html")

# Use system() to execute the rm command
# system(paste("rm", shQuote("index.html")))
# Sys.sleep(2)
# 
# file.rename("index2.html", "index.html")
