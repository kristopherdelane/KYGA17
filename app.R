library(tidyverse)
library(broom)
library(forcats)
library(stringr)
library(scales)
library(rgdal)
library(R6)
library(gridExtra)
library(shiny)
library(reshape2)
library(pander)
library(markdown)
library(stringr)

votes <- read_csv('https://query.data.world/s/8ta9wcevg9crkp319k8knal2c')
legislators <- read_csv('https://query.data.world/s/9f49ewzedjslrh23g96v53ha5')

df<- votes
df<- dcast(df, legislator_id ~ BillName, value = "Vote")
df2 <- as.data.frame(sapply(df,gsub,pattern="YEA",replacement="YEA #bgyea"))
df2 <- as.data.frame(sapply(df2,gsub,pattern="NAY",replacement="NAY #bgnay"))
df2 <- as.data.frame(sapply(df2,gsub,pattern="NV",replacement="NV #bgnv"))
df2 <- as.data.frame(sapply(df2,gsub,pattern="PASS",replacement="PASS #bgpass"))
df2 <- as.data.frame(sapply(df2,gsub,pattern="ABS",replacement="ABS #bgabs"))
df2$legislator_id<- as.numeric(df2$legislator_id)
df2 <- left_join(legislators, df2)
df2<- df2[,c(4:6,13:275)]
df2<- df2[,c(1,3,2,4:264)]
df2$District<-substring(df2$District,1,1)

# function derived from the highlightHTMLcells() function of the highlightHTML package
colortable <- function(htmltab, css, style="table-condensed table-bordered"){
  tmp <- str_split(htmltab, "\n")[[1]] 
  CSSid <- gsub("\\{.+", "", css)
  CSSid <- gsub("^[\\s+]|\\s+$", "", CSSid)
  CSSidPaste <- gsub("#", "", CSSid)
  CSSid2 <- paste(" ", CSSid, sep = "")
  ids <- paste0("<td id='", CSSidPaste, "'")
  for (i in 1:length(CSSid)) {
    locations <- grep(CSSid[i], tmp)
    tmp[locations] <- gsub("<td", ids[i], tmp[locations])
    tmp[locations] <- gsub(CSSid2[i], "", tmp[locations], 
                           fixed = TRUE)
  }
  htmltab <- paste(tmp, collapse="\n")
  Encoding(htmltab) <- "UTF-8"
  list(
    tags$style(type="text/css", paste(css, collapse="\n")),
    tags$script(sprintf( 
      '$( "table" ).addClass( "table %s" );', style
    )),
    HTML(htmltab)
  )
}
###


##

ui <- fluidPage(
      headerPanel(""),
      sidebarPanel(
        selectInput(inputId = 'party_select', label = 'Select a Political Party', c('All', 'Republican', 'Democratic'), selected = 'All'),
        selectInput(inputId = 'chamber_select', label = 'Select a Chamber of the General Assembly', c('All', 'House'='H', 'Senate'="S"), selected = 'All'),
        
        a("For more work with this data, check out this work by Robert Kahne", href="https://rkahne.shinyapps.io/kyleg/"),
        br(),br(),
        h4('As a part of the first Data for Democracy hackathon, a group of members in Louisville, Kentucky gathered all the voting data from the Kentucky General Assembly 2017. Members of Congress are listed alphabetically down the side and the bills are listed alphabetically accross the top for you to explore how your representative voted.')
      ),
      mainPanel(
        h2("Kentucky General Assembly Voting Results"),
        uiOutput("htmltable")
      )
    )
    
    
    server <- function(input, output) {
      output$htmltable <- renderUI({
        
        if (input$party_select != "All") {
          df2 <- df2 %>% filter(Party %in% input$party_select)
        }
        
        if (input$chamber_select != "All") {
          df2 <- df2 %>% filter(District %in% input$chamber_select)
        }
        
        # define CSS tags
        css <- c("#bgyea {background-color: #2c7bb6;}",
                 "#bgnay {background-color: #d7191c;}",
                 "#bgnv {background-color: #ffffbf;}",
                 "#bgabs {background-color: #fdae61;}",
                 "#bgpass {background-color: #abd9e9;}")
        
        tab <- df2[, 3:264] 
        # generate html table with pander package and markdown package
        htmltab <- markdownToHTML(
          text=pandoc.table.return(
            tab, 
            style="rmarkdown", split.tables=Inf
          ), 
          fragment.only=TRUE
        ) 
        colortable(htmltab, css)
      })
    }

shinyApp(ui = ui, server = server)