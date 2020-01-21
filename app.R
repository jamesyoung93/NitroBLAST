




files51142 <- list.files(path = "output51142", full.names = TRUE)
f51142.Data <- lapply(files51142, function(x) {read.csv(x, header = F)})
f51142.Data <- lapply(f51142.Data, function(x) {x[order(x$V4),]}) #11 is evalue
#Below gets rid of duplicates
f51142.Data <- lapply(f51142.Data, function(x) {x[!duplicated(x$V2), ]}) #11 is evalue
f51142.Data <- lapply(f51142.Data, function(x){na.omit(x)})
f51142.Data <- lapply(f51142.Data, function(x){x[,3] = as.character(x[,3])
x[,1] = as.character(x[,1])
x[,2] = as.character(x[,2])
x[,6] = as.character(x[,6])
x[,3] = as.double(x[,3])
return(x)})


files7120 <- list.files(path = "output7120", full.names = TRUE)
f7120.Data <- lapply(files7120, function(x) {read.csv(x, header = F)})
f7120.Data <- lapply(f7120.Data, function(x) {x[order(x$V4),]}) #11 is evalue
#Below gets rid of duplicates
f7120.Data <- lapply(f7120.Data, function(x) {x[!duplicated(x$V2), ]}) #11 is evalue
f7120.Data <- lapply(f7120.Data, function(x){na.omit(x)})
f7120.Data <- lapply(f7120.Data, function(x){x[,3] = as.character(x[,3])
x[,1] = as.character(x[,1])
x[,2] = as.character(x[,2])
x[,6] = as.character(x[,6])
x[,3] = as.double(x[,3])
return(x)})

filesTrichodesmium <- list.files(path = "outputTrichodesmium", full.names = TRUE)
fTrichodesmium.Data <- lapply(filesTrichodesmium, function(x) {read.csv(x, header = F)})
fTrichodesmium.Data <- lapply(fTrichodesmium.Data, function(x) {x[order(x$V4),]}) #11 is evalue
#Below gets rid of duplicates
fTrichodesmium.Data <- lapply(fTrichodesmium.Data, function(x) {x[!duplicated(x$V2), ]}) #11 is evalue
fTrichodesmium.Data <- lapply(fTrichodesmium.Data, function(x){na.omit(x)})
fTrichodesmium.Data <- lapply(fTrichodesmium.Data, function(x){x[,3] = as.character(x[,3])
x[,1] = as.character(x[,1])
x[,2] = as.character(x[,2])
x[,6] = as.character(x[,6])
x[,3] = as.double(x[,3])
return(x)})




###################################################################################################
library(mergeutils)
library(DT)
library(tidyverse)
library(shiny)
library(shinyjs)
library(markdown)
library(limma)
# Define UI for application that draws a histogram
#plotting theme for ggplot2



# UI for app
ui <- fluidPage(
  
  # App title ----
  navbarPage("NitroBLAST",
             
             tabPanel("Homepage",
                      includeMarkdown("test.rmd")),
             tabPanel("Instructions",
                      includeMarkdown("Instructions.rmd")),
             tabPanel("Data",
                      
                      # Sidebar layout with input and output definitions ----
                      sidebarLayout(
                        
                        # Sidebar panel for inputs ----
                        sidebarPanel(
                          useShinyjs(),
                          
                          
                          selectInput('db', 'Query against', choices = list(
                            Filamentous_Non_Heterocystous = c(`Trichodesmium erythraeum` = 'fTrichodesmium.data'),
                            Unicellular = c(`Cyanothece sp. ATCC 51142` = 'f51142.data'),
                            Filamentous_Heterocystous = c(`Nostoc sp. PCC 7120` = 'f7120.data')
                          )),
                          
                          selectInput('ingroup', 'Out group', choices = list(
                            Non_Diazotrophic = c(`Non-Diazotrophic` = 'Non-Diazotrophic'),
                            Diazotrophic = c( `Filamentous Heterocystous` = 'Filamentous Heterocystous', 
                                              `Filamentous Non-Heterocystous` = 'Fil. Non-Heterocystous', 
                                              `Unicellular` = 'Unicellular',
                                              `Anaerobic Unicellular` = 'Anaerobic Unicellular',
                                              `Filamentous` = 'Filamentous', `Other Bacteria` = 'Other Bacteria',
                                              `All Diaz. Cyanobacteria` = 'All Diaz. Cyanobacteria')
                          )),
                          
                          selectInput('out', 'In group', choices = list(
                            Diazotrophic = c( `Filamentous Heterocystous` = 'Filamentous Heterocystous', 
                                              `Filamentous Non-Heterocystous` = 'Fil. Non-Heterocystous', `Unicellular` = 'Unicellular',
                                              `Filamentous` = 'Filamentous', `Other Bacteria` = 'Other Bacteria',
                                              `All Diaz. Cyanobacteria` = 'All Diaz. Cyanobacteria'),
                            Non_Diazotrophic = c(`Non-Diazotrophic` = 'Non-Diazotrophic')
                            
                          )),
                          sliderInput("identity", "% Identity",
                                      min = 0, max = 100,
                                      value = 50),
                          br(),
                          numericInput("evalue", "E-value Cutoff", 1e-50, min = 0, max = 1e-50),
                          br(),
                          submitButton(text = "Submit"),
                          br(),
                          downloadButton("downloadData", "Download")
                          # Input: Select a file ----
                          
                          #downloadButton("report", "Generate report"),
                          
                          
                        ),
                        
                        
                        
                        # output
                        mainPanel(
                          DT::dataTableOutput("table")))),
             navbarMenu("Visualize",
             
             tabPanel("VennDiagram",
                      plotOutput(VennDiagram)))
  )
)



# shiny server side code for each call
server<-(function(input, output, session){
  
  
  datasetInput <- reactive({
    switch(input$db,
           "fTrichodesmium.data" = fTrichodesmium.Data,
           "f51142.data" = f51142.Data,
           "f7120.data" = f7120.Data)
  })
  
  datasetInput2 <- reactive({
    
    
    switch(input$ingroup,
           "Non-Diazotrophic" = df1, df2, df3, df4, df5,
           "Filamentous Heterocystous" = df6, df7, df8,
           "Fil. Non-Heterocystous" = df9, df10,
           "Unicellular" = df11, df12, df13,
           "Anaerobic Unicellular" = df14, df15,
           "Filamentous" = df6, df7, df8, df9, df10,
           "Other Bacteria" = df16, df17, df18, df19, df20,
           "All Diaz. Cyanobacteria" = df6, df7, df8, df9, df10, df11, df12, df13, df14, df15
    )
  })
  
  datasetInput3 <- reactive({
    
    
    switch(input$ingroup,
           "Non-Diazotrophic" = as.list(df1, df2, df3, df4, df5),
           "Filamentous Heterocystous" = list(df6, df7, df8),
           "Fil. Non-Heterocystous" = list(df9, df10),
           "Unicellular" = list(df11, df12, df13),
           "Anaerobic Unicellular" = list(df14, df15),
           "Filamentous" = list(df6, df7, df8, df9, df10),
           "Other Bacteria" = list(df16, df17, df18, df19, df20),
           "All Diaz. Cyanobacteria" = list(df6, df7, df8, df9, df10, df11, df12, df13, df14, df15)
    )
  })
  
  sliderValues <- reactive({input$identity})
  
  
  output$table <-DT::renderDataTable(DT::datatable({ #output$table
    
    
    data <- datasetInput()
    for (i in seq(data))
      assign(paste0("df", i), data[[i]])
    
    if(input$ingroup =="Non-Diazotrophic"){
      indata <- list(df1, df2, df3, df4, df5)
      
      
    }
    
    
    else if(input$ingroup =="Filamentous Heterocystous")	{	
      indata <- list(df6, df7, df8)
      
      
    }
    
    else if(input$ingroup =="Fil. Non-Heterocystous")	{	
      indata <- list(df9, df10)
      
      
    }
    
    
    else if(input$ingroup =="Unicellular")	{	
      indata <- list(df14, df15)
      
      
    }
    
    else if(input$ingroup =="Filamentous")	{	
      indata <- list(df6, df7, df8, df9, df10)
      
      
    }
    
    else if(input$ingroup =="Other Bacteria")	{	
      indata <- list(df16, df17, df18, df19, df20)
      
      
    }
    
    else {	
      indata <- list(df6, df7, df8, df9, df10, df11, df12, df13, df14, df15)}
    
    
    
    
    
    if(input$out =="Non-Diazotrophic"){
      outdata <- list(df1, df2, df3, df4, df5)
      
      
    }
    
    
    else if(input$out =="Filamentous Heterocystous")	{	
      outdata <- list(df6, df7, df8)
      
      
    }
    
    else if(input$out =="Fil. Non-Heterocystous")	{	
      outdata <- list(df9, df10)
      
      
    }
    
    else if(input$out =="Unicellular")	{	
      outdata <- list(df11, df12, df13)
      
      
    }
    
    
    else if(input$out =="Filamentous")	{	
      outdata <- list(df6, df7, df8, df9, df10)
      
      
    }
    
    else if(input$out =="Other Bacteria")	{	
      outdata <- list(df16, df17, df18, df19, df20)
      
      
    }
    
    else {	
      outdata <- list(df6, df7, df8, df9, df10, df11, df12, df13, df14, df15)} 
    
    
    
    
    
    
    
    
    #indata <- lapply(indata, function(x) {indata <- subset(x, x[[3]] > 30)
    #return(indata)})
    #indata <- lapply(indata, function(x) {select(filter(x, V3 >40),c(V1, V2, V3, V4, V5, V6))})
    indata <- lapply(indata, function(indata) {indata <- subset(indata, V3 > input$identity)
    return(indata)})
    indata <- lapply(indata, function(indata) {indata <- subset(indata, V4 < input$evalue)
    return(indata)})
    indata <- indata %>% reduce(left_join, by = "V2")
    indata <- na.omit(indata)
    indata <- indata[,1:6]
    indata <- indata[!duplicated(indata$V2), ]
    #indata
    
    
    
    
    
    
    
    
    outdata <- lapply(outdata, function(outdata) {outdata <- subset(outdata, V3 > input$identity)
    return(outdata)})
    outdata <- lapply(outdata, function(outdata) {outdata <- subset(outdata, V4 < input$evalue)
    return(outdata)})
    
    
    
    #outdata <- lapply(outdata, function(x) {outdata <- subset(x, x[[3]] > 30)
    #return(outdata)})
    #outdata <- lapply(outdata, subset, V3 > 20)
    #outdata <- lapply(outdata, function(x) {select(filter(x, V3 >40),c(V1, V2, V3, V4, V5, V6))})
    outdata <- outdata %>% reduce(left_join, by = "V2")
    outdata <- na.omit(outdata)
    outdata <- outdata[,1:6]
    
    
    indata$group <- "A"
    outdata$group <- "B"
    
    
    
    df <- rbind(indata, outdata)
    df <- cbind(df$group, as.character(df$V2), df$V6)
    
    
    df <- as.data.frame(df)
    colnames(df) <- c("Group", "Gene", "Role")
    dupRows <- dupsBetweenGroups(df, "Group")
    df <- cbind(df, unique= !dupRows)
    df <- subset(df, Group == "A" & unique == "TRUE")
    df <- cbind(as.character(df$Gene), as.character(df$Role))
    df <- as.data.frame(df)
    colnames(df) <- c("BLAST Accesion Name", "Role")
    df
    
    
    
    
    
    
  }))
  
  
  output$VennDiagram <-DT::renderDataTable(DT::datatable({ #output$table
    
    
    data <- datasetInput()
    for (i in seq(data))
      assign(paste0("df", i), data[[i]])
    
    if(input$ingroup =="Non-Diazotrophic"){
      indata <- list(df1, df2, df3, df4, df5)
      
      
    }
    
    
    else if(input$ingroup =="Filamentous Heterocystous")	{	
      indata <- list(df6, df7, df8)
      
      
    }
    
    else if(input$ingroup =="Fil. Non-Heterocystous")	{	
      indata <- list(df9, df10)
      
      
    }
    
    
    else if(input$ingroup =="Unicellular")	{	
      indata <- list(df14, df15)
      
      
    }
    
    else if(input$ingroup =="Filamentous")	{	
      indata <- list(df6, df7, df8, df9, df10)
      
      
    }
    
    else if(input$ingroup =="Other Bacteria")	{	
      indata <- list(df16, df17, df18, df19, df20)
      
      
    }
    
    else {	
      indata <- list(df6, df7, df8, df9, df10, df11, df12, df13, df14, df15)}
    
    
    
    
    
    if(input$out =="Non-Diazotrophic"){
      outdata <- list(df1, df2, df3, df4, df5)
      
      
    }
    
    
    else if(input$out =="Filamentous Heterocystous")	{	
      outdata <- list(df6, df7, df8)
      
      
    }
    
    else if(input$out =="Fil. Non-Heterocystous")	{	
      outdata <- list(df9, df10)
      
      
    }
    
    else if(input$out =="Unicellular")	{	
      outdata <- list(df11, df12, df13)
      
      
    }
    
    
    else if(input$out =="Filamentous")	{	
      outdata <- list(df6, df7, df8, df9, df10)
      
      
    }
    
    else if(input$out =="Other Bacteria")	{	
      outdata <- list(df16, df17, df18, df19, df20)
      
      
    }
    
    else {	
      outdata <- list(df6, df7, df8, df9, df10, df11, df12, df13, df14, df15)} 
    
    
    
    
    
    

    indata <- lapply(indata, function(indata) {indata <- subset(indata, V3 > input$identity)
    return(indata)})
    indata <- lapply(indata, function(indata) {indata <- subset(indata, V4 < input$evalue)
    return(indata)})
    indata <- indata %>% reduce(left_join, by = "V2")
    indata <- na.omit(indata)
    indata <- indata[,1:6]
    indata <- indata[!duplicated(indata$V2), ]

    
    
    
    
    
    
    
    
    outdata <- lapply(outdata, function(outdata) {outdata <- subset(outdata, V3 > input$identity)
    return(outdata)})
    outdata <- lapply(outdata, function(outdata) {outdata <- subset(outdata, V4 < input$evalue)
    return(outdata)})

    outdata <- outdata %>% reduce(left_join, by = "V2")
    outdata <- na.omit(outdata)
    outdata <- outdata[,1:6]
    
    
    df <- cbind(as.character(indata$V2), as.character(outdata$V2))
   # df <- cbind(df$group, as.character(df$V2), df$V6)
    
    d <- vennCounts(df)
    vennDiagram(d)
    
    
   
    
    
    
    
  })
  
  
  
  
  
  )
  
  datasetInput4 <- reactive({
    data <- datasetInput()
    for (i in seq(data))
      assign(paste0("df", i), data[[i]])
    
    if(input$ingroup =="Non-Diazotrophic"){
      indata <- list(df1, df2, df3, df4, df5)
      
      
    }
    
    
    else if(input$ingroup =="Filamentous Heterocystous")	{	
      indata <- list(df6, df7, df8)
      
      
    }
    
    else if(input$ingroup =="Fil. Non-Heterocystous")	{	
      indata <- list(df9, df10)
      
      
    }
    
    
    else if(input$ingroup =="Unicellular")	{	
      indata <- list(df14, df15)
      
      
    }
    
    else if(input$ingroup =="Filamentous")	{	
      indata <- list(df6, df7, df8, df9, df10)
      
      
    }
    
    else if(input$ingroup =="Other Bacteria")	{	
      indata <- list(df16, df17, df18, df19, df20)
      
      
    }
    
    else {	
      indata <- list(df6, df7, df8, df9, df10, df11, df12, df13, df14, df15)}
    
    
    
    
    
    if(input$out =="Non-Diazotrophic"){
      outdata <- list(df1, df2, df3, df4, df5)
      
      
    }
    
    
    else if(input$out =="Filamentous Heterocystous")	{	
      outdata <- list(df6, df7, df8)
      
      
    }
    
    else if(input$out =="Fil. Non-Heterocystous")	{	
      outdata <- list(df9, df10)
      
      
    }
    
    else if(input$out =="Unicellular")	{	
      outdata <- list(df11, df12, df13)
      
      
    }
    
    
    else if(input$out =="Filamentous")	{	
      outdata <- list(df6, df7, df8, df9, df10)
      
      
    }
    
    else if(input$out =="Other Bacteria")	{	
      outdata <- list(df16, df17, df18, df19, df20)
      
      
    }
    
    else {	
      outdata <- list(df6, df7, df8, df9, df10, df11, df12, df13, df14, df15)} 
    
    
    
    
    
    
    
    
    #indata <- lapply(indata, function(x) {indata <- subset(x, x[[3]] > 30)
    #return(indata)})
    #indata <- lapply(indata, function(x) {select(filter(x, V3 >40),c(V1, V2, V3, V4, V5, V6))})
    indata <- lapply(indata, function(indata) {indata <- subset(indata, V3 > input$identity)
    return(indata)})
    indata <- lapply(indata, function(indata) {indata <- subset(indata, V4 < input$evalue)
    return(indata)})
    indata <- indata %>% reduce(left_join, by = "V2")
    indata <- na.omit(indata)
    indata <- indata[,1:6]
    indata <- indata[!duplicated(indata$V2), ]
    #indata
    
    
    
    
    
    
    
    
    outdata <- lapply(outdata, function(outdata) {outdata <- subset(outdata, V3 > input$identity)
    return(outdata)})
    outdata <- lapply(outdata, function(outdata) {outdata <- subset(outdata, V4 < input$evalue)
    return(outdata)})
    
    
    
    #outdata <- lapply(outdata, function(x) {outdata <- subset(x, x[[3]] > 30)
    #return(outdata)})
    #outdata <- lapply(outdata, subset, V3 > 20)
    #outdata <- lapply(outdata, function(x) {select(filter(x, V3 >40),c(V1, V2, V3, V4, V5, V6))})
    outdata <- outdata %>% reduce(left_join, by = "V2")
    outdata <- na.omit(outdata)
    outdata <- outdata[,1:6]
    
    
    indata$group <- "A"
    outdata$group <- "B"
    
    
    
    df <- rbind(indata, outdata)
    df <- cbind(df$group, as.character(df$V2), df$V6)
    
    
    df <- as.data.frame(df)
    colnames(df) <- c("Group", "Gene", "Role")
    dupRows <- dupsBetweenGroups(df, "Group")
    df <- cbind(df, unique= !dupRows)
    df <- subset(df, Group == "A" & unique == "TRUE")
    df <- cbind(as.character(df$Gene), as.character(df$Role))
    df <- as.data.frame(df)
    colnames(df) <- c("BLAST Accesion Name", "Role")
    df
    
    
  })
  
  
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("check", ".csv", sep = "")
    },
    content = function(file) {
      write.csv(datasetInput4(), file, row.names = FALSE)
    }
  ) 
  
  
})




shinyApp(ui, server)
