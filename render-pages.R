fnames <- list.files(path=".", 
                     pattern=".qmd", 
                     all.files=TRUE,
                     full.names=TRUE)

f_render <- function(fname) {
  quarto::quarto_render( paste(fname) )
}

for (fname in fnames) {
  f_render(fname)
}
