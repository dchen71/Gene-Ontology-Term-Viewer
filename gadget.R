##
## Shiny gadget to find the gene ontology of a gene
##

library(shiny)
library(miniUI)
library(biomaRt)

#Gadget function to find gene ontology by gene name
get_ontology = function(){
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
    
    #Renders # of hits found
    output$found = renderText({
      if(input$geneName != ""){
        return(paste("Found", nrow(go_ids()), "entries"))
      }
    })
    
    #Renders table of types of ontologies found
    output$go_breakdown = renderTable({
      if(input$geneName != ""){
        return(table(go_ids()$Ontology))
      }
    })
  })
  
  # Define mini UI for application that renders genename to anova/kruskal test
  ui = (miniPage(
    gadgetTitleBar("Gene Ontology"),
    miniTabstripPanel(
      miniTabPanel("Search", icon = icon("search"),
                   miniContentPanel(
                     textInput("geneName", "Gene Name"),
                     textOutput("found"),
                     tableOutput("go_breakdown")
                   )
      ),
      miniTabPanel("Molecular Function", icon = icon("sliders"),
                   miniContentPanel(
                     h3("Molecular Function"),
                     tableOutput("mf")
                   )
      ),
      miniTabPanel("Cellular Components", icon = icon("dropbox"),
                   miniContentPanel(
                     h3("Cellular Components"),
                     tableOutput("cc")
                   )
      ),
      miniTabPanel("Biological Processes", icon = icon("forumbee"),
                   miniContentPanel(
                     h3("Biological Processes"),
                     tableOutput("bp")
                   )
      )
    )
    
    
    
  ))
  
  #Run gadget file
  runGadget(ui, server)
}

