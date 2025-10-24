# system("quarto render")
quarto::quarto_render("index.qmd")

library(stringr)

# Read the HTML file
html_as_text <- readLines("index.html", warn = FALSE)
system(paste("rm", shQuote("index.html")))
# Sys.sleep(2)

# Next lecture number & dates
lec_dates <- c('0825', '0827', '0829', '0905',
               '0915', '0926', '1003', '1017',
               '1024')
lec_n <- length(lec_dates)

lec_nos <- 1:lec_n
lec_nos <- sprintf("%02d", lec_nos)

for (i in 1:length(lec_nos)){
  lec_no <- lec_nos[i]
  lec_date <- lec_dates[i]
  
  old_content <- str_c('./econ-lec/econ-340-lec-', lec_no, '-2025-', lec_date, '.html')
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

