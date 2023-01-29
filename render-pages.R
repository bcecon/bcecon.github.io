fnames <- list.files(path=".", 
                     pattern=".Rmd", 
                     all.files=TRUE,
                     full.names=TRUE)

f_render <- function(fname) {
  rmarkdown::render_site( paste(fname) )
}

for (fname in fnames) {
  f_render(fname)
}
