##
## Shiny app to find the gene ontology of a gene
##

library(shiny)
library(biomaRt)
library(GO.db)

#Get the mapping from ensembl gene id to external
ensembl = useMart("ENSEMBL_MART_ENSEMBL")
ensembl = useDataset("mmusculus_gene_ensembl", ensembl)

# Define server logic required to create output of the kruskal/anova test
server = (function(input, output) {
  #Get the go ids, terms, as well as the ontology
  go_ids = reactive({
    if(input$geneName != ""){
      go_ids = getBM(attributes = "go_id",
                     filters = "external_gene_name", values = input$geneName,
                     mart = ensembl)
      
      if(nrow(go_ids) != 0){
        for(i in 1:nrow(go_ids)){
          go_ids$Term[i] = GOTERM[[go_ids[i,1]]]@Term
          go_ids$Ontology[i] = GOTERM[[go_ids[i,1]]]@Ontology
        }
        
        return(go_ids)
      }
    }
  })
  
  #Renders the table of biological processes
  output$bp = renderTable({
    if(input$geneName != ""){
      mf = subset(go_ids(), Ontology == "BP")
      return(mf)
    }
  })
  
  #Renders the table of cellular components
  output$cc = renderTable({
    if(input$geneName != ""){
      mf = subset(go_ids(), Ontology == "CC")
      return(mf)
    }
  })
  
  #Renders the table of molecular function
  output$mf = renderTable({
    if(input$geneName != ""){
      mf = subset(go_ids(), Ontology == "MF")
      return(mf)
    }
  })
})

# Define UI for application that renders genename to anova/kruskal test
ui = (fluidPage(
  titlePanel("Gene Ontology", windowTitle = "Gene Ontology"),
  mainPanel(
    #Input name of gene
    textInput("geneName", "Gene Name"),
    submitButton("Submit"),
    #Show off the breakdown in ontology
    h3("Molecular Function"),
    tableOutput("mf"),
    h3("Cellular Components"),
    tableOutput("cc"),
    h3("Biological Processes"),
    tableOutput("bp")
  )

  
))

#Load single file shiny app
shinyApp(ui = ui, server = server)