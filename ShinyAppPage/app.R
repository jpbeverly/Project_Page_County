library(shiny)
library(shinydashboard)
library(ggplot2)
library(DT)
library(dplyr)

setwd("C:/Users/Admin/Documents/DSPG/Page/Project_Page_County")

gov_grants_only <-
    read.csv(
        "C:/Users/Admin/Documents/DSPG/Page/Project_Page_County/data/Grants/gov_grantsOnly.csv",
        stringsAsFactors = TRUE,
        header = T
    )
gov_all <-
    read.csv(
        "C:/Users/Admin/Documents/DSPG/Page/Project_Page_County/data/Grants/gov_all.csv",
        stringsAsFactors = TRUE,
        header = T
    )



sidebar <- dashboardSidebar(
    # selectInput(
    #     "list_type",
    #     "Select",
    #     choices = c("All", "Grants"),
    #     selected = "All"
    # )
)

header <- dashboardHeader(title = "Page County Dashboard")

body <- dashboardBody(
    DTOutput("mytable1")
)

ui <- dashboardPage(header,
                    sidebar = sidebar,
                    body)

server <- function(input, output, session) {
    output$mytable1 <- DT::renderDataTable({
        show_vars <- c("OPPORTUNITY.NUMBER", "AGENCY.CODE")
        DT::datatable(gov_all[, show_vars, drop = FALSE],
                      rownames = F,
                      colnames = c("Name", "Agency"),
                      class = "cell-border stripe")
    })
    
    # sorted columns are colored now because CSS are attached to them
    output$mytable2 <- DT::renderDataTable({
        DT::datatable(gov_grants_only, rownames = F)
    })
    
    # customize the length drop-down menu; display 5 rows per page by default
    # output$mytable3 <- DT::renderDataTable({
    # DT::datatable(iris, options = list(lengthMenu = c(5, 30, 50), pageLength = 5))
    # })
}

shinyApp(ui, server)
