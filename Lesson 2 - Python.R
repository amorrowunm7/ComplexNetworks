require(knitr)
require(markdown)

# Create .md, .html, and .pdf files
knit("Python.Rmd")
markdownToHTML('Python.md', 'Python.html', options=c("use_xhml"))