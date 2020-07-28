library(shiny)
library(shinydashboard)
library(ggplot2)
library(DT)
library(dplyr)
library(networkD3)

#setwd("C:/Users/Admin/Documents/DSPG/Page/Project_Page_County")

gov_grants_only <-
    read.csv(
        "~/git/Project_Page_County/data/Grants/gov_grantsOnly.csv",
        stringsAsFactors = TRUE,
        header = T
    )
gov_all <-
    read.csv(
        "~/git/Project_Page_County/data/Grants/gov_all.csv",
        stringsAsFactors = TRUE,
        header = T
    )



sidebar <- dashboardSidebar(
    selectInput("select_var", "Funding Type", choices = c("All"="all",
                                                         "Grants"="grants"),
                selected = "grants"
                ),
    selectInput("level", "Eligibility", choices = c("County governments" ="county",
                                               "Small Business"="business",
                                               "Nonprofits"="nonprofits"),
                selected = "posted"
    ),
    selectInput("sort", "Sort by", choices = c("Agency" ="agency",
                                               "Status"="status"),
                selected = "posted"
    )
    
)

header <- dashboardHeader(title = "Page County Dashboard")

body <- dashboardBody(
    mainPanel(
        conditionalPanel(
            condition = "input.select_var == 'all'",
            DTOutput("mytable1")
        ),
        conditionalPanel(
            condition = "input.select_var == 'grants'",
            DTOutput('mytable2')
        )
    )
    # DTOutput("mytable1")
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
        show_vars <- c("OPPORTUNITY.NUMBER", "AGENCY.CODE")
        DT::datatable(gov_grants_only[, show_vars, drop = FALSE],
                      rownames = F,
                      colnames = c("Name", "Agency"),
                      class = "cell-border stripe")
    })
    
    
}

shinyApp(ui, server)
