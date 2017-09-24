##########
## ui.r ##
##########

library(data.table)
library(shiny)
library(shinydashboard)
library(plotly)
addResourcePath('data', 'data')

enableBookmarking(store = "url")

function(request) {

    dashboardPage(
        title = "Past party votes",
        header = dashboardHeader(
            title = 'Past election results',
                     titleWidth = 300
                 ),
        
        sidebar = dashboardSidebar(
            uiOutput('party'),
            uiOutput('electorate'),
            uiOutput('before_onday'),
            uiOutput('local_overseas'),
            uiOutput('ordinary_special')
        ),
            
        body = dashboardBody(
            tags$head(
                     tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
                 ),
            box(width = 12
                , h3('Explore past New Zealand election results since 2002')
                , p('Choose a subset of the data using the controls and see how the party votes changed over time')
                ),
            box(title = 'Past election results',
                width = 8,
                hr(),
                fluidRow(column(6, uiOutput('stat')),
                         column(6, uiOutput('group'))),
                plotlyOutput('chart', height = '600px')),
                            
            box(title = 'Info', width = 4,
                hr(),
                p('The data were sourced from electionresults.govt.nz'),
                hr(),
                p('Created by Yvan Richard with R, Shiny, and Plotly'),
                p('E-mail author: thefrenchstick<at>gmail.com'),
                p(),
                HTML('Source code <a href="https://github.com/yvanrichard/nz-elections">on GitHub</a>')
                
                )

           ## ,box(title = 'Test',
           ##      tableOutput('test')
           ##      )

        )
    )
}
