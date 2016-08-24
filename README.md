##go_gadget

This is a shiny app/gadget for R to find the gene ontology terms of a given gene name. This app requires GO.db and biomart and can easily be downloaded through the following links:

source("https://bioconductor.org/biocLite.R")
biocLite("GO.db")
biocLite("biomaRt")
