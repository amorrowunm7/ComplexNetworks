require(knitr)
require(markdown)

# Create .md, .html, and .pdf files
knit("NetworkSimpleStatistics.Rmd")
markdownToHTML('NetworkSimpleStatistics.md', 'NetworkSimpleStatistics.html', options=c("use_xhml"))