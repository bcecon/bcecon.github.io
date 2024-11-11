# system("quarto render")
quarto::quarto_render("index.qmd")

library(stringr)

# Read the HTML file
html_as_text <- readLines("index.html", warn = FALSE)
system(paste("rm", shQuote("index.html")))
# Sys.sleep(2)

# Next lecture number & dates 
lec_dates <- c('0826', '0828', '0830', 
               '0904', '0906', 
               '0909', '0911', '0913', 
               '0916', '0918', '0920', 
               '0925', '0927',
               '0930', '1002', '1004',
               '1016', '1018', 
               '1021', '1023', '1025',
               '1028', '1030', 
               '1104', '1106', '1108',
               '1111')
lec_n <- length(lec_dates) 

lec_nos <- 1:lec_n 
lec_nos <- sprintf("%02d", lec_nos)

for (i in 1:length(lec_nos)){
  lec_no <- lec_nos[i]
  lec_date <- lec_dates[i]
  
  old_content <- str_c('./econ-lec/econ-341-lec-', lec_no, '-2024-', lec_date, '.html')
  new_content <- str_c('#', 
                       '" onclick="', 
                       "window.open(", 
                       "'", 
                       old_content, 
                       "'); return false;", '"') 

  html_as_text <- str_replace_all(html_as_text,
                                  old_content,
                                  new_content)
}

writeLines(html_as_text, 
           "index.html")

# <a href="./econ-lec/econ-341-lec-01-2024-0826.html" class="title listing-title">Lecture 1</a>
# <a href="#" onclick="window.open('./econ-lec/econ-341-lec-01-2024-0826.html'); return false;" class="title listing-title">Lecture 1</a>
  