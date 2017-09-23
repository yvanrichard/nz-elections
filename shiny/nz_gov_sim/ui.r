##########
## ui.r ##
##########

library(data.table)
library(shiny)
## library(shinyjs)
library(shinydashboard)
library(plotly)
## library(DT)
## library(V8)
## library(rbokeh)
addResourcePath('data', 'data')

enableBookmarking(store = "url")

function(request) {

    dashboardPage(
        title = "NZ Government Simulator",
        header = dashboardHeader(
            title = 'NZ Government Simulator',
                     titleWidth = 300
                 ),
        
        sidebar = dashboardSidebar(width = '0%'
        ),
            
        body = dashboardBody(
            tags$head(
                     tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
                 ),
            box(width = 12,
                h3('Find out how many seats each party would have in the New Zealand government'),
                p('Change the importance sliders to get the desired percentages of party votes'),
                p('Tick the checkbox if the party won at least one electorate (a party with less than 5% of votes is assumed to win only one electorate)'),
                p('The numbers are initialised at the results of the 2014 election'),
                p('For fine tuning, click on a slider then use the arrow keys of the keyboard')
                ),
            box(title = 'Election results',
                width = 4,
                hr(),
            fluidRow(column(4, strong('Relative importance')),
                     column(3, h5('')),
                     column(1, align='center', strong('Won electo- rates')),
                     column(2, align='center', strong('Party votes %')),
                     column(2, align='center', strong('Seats'))),
            fluidRow(column(4, uiOutput('national_w')),
                     column(3, align='center', h4('National')),
                     column(1, align='center', uiOutput('national_e')),
                     column(2, textOutput('national_p')),
                     column(2, textOutput('national_s'))),
            fluidRow(column(4, uiOutput('labour_w')),
                     column(3, align='center', h4('Labour')),
                     column(1, align='center', uiOutput('labour_e')),
                     column(2, textOutput('labour_p')),
                     column(2, textOutput('labour_s'))),
            fluidRow(column(4, uiOutput('green_w')),
                     column(3, align='center', h4('Green')),
                     column(1, align='center', uiOutput('green_e')),
                     column(2, textOutput('green_p')),
                     column(2, textOutput('green_s'))),
            fluidRow(column(4, uiOutput('nzfirst_w')),
                     column(3, align='center', h4('NZ First')),
                     column(1, align='center', uiOutput('nzfirst_e')),
                     column(2, textOutput('nzfirst_p')),
                     column(2, textOutput('nzfirst_s'))),
            fluidRow(column(4, uiOutput('top_w')),
                     column(3, align='center', h4('TOP')),
                     column(1, align='center', uiOutput('top_e')),
                     column(2, textOutput('top_p')),
                     column(2, textOutput('top_s'))),
            fluidRow(column(4, uiOutput('maori_w')),
                     column(3, align='center', h4('Maori')),
                     column(1, align='center', uiOutput('maori_e')),
                     column(2, textOutput('maori_p')),
                     column(2, textOutput('maori_s'))),
            fluidRow(column(4, uiOutput('conserv_w')),
                     column(3, align='center', h4('Conservative')),
                     column(1, align='center', uiOutput('conserv_e')),
                     column(2, textOutput('conserv_p')),
                     column(2, textOutput('conserv_s'))),
            fluidRow(column(4, uiOutput('ufuture_w')),
                     column(3, align='center', h4('United Future')),
                     column(1, align='center', uiOutput('ufuture_e')),
                     column(2, textOutput('ufuture_p')),
                     column(2, textOutput('ufuture_s'))),
            fluidRow(column(4, uiOutput('act_w')),
                     column(3, align='center', h4('ACT')),
                     column(1, align='center', uiOutput('act_e')),
                     column(2, textOutput('act_p')),
                     column(2, textOutput('act_s'))),
            fluidRow(column(4, uiOutput('other_w')),
                     column(3, align='center', h4('Other')),
                     column(1, align='center', uiOutput('other_e')),
                     column(2, textOutput('other_p')),
                     column(2, textOutput('other_s')))
            ),
                
            box(title = 'Parliament', width = 4,
                hr(),
                div(plotlyOutput("pie", height = "100%"), align = "center"),
                textOutput('total_seats')
                ),
            
            box(title = 'Potential coalitions', width = 4,
                hr(),
                p('List of coalitions that could form a majority government'),
                p('Coalition Labour/National is assumed to be impossible'),
                hr(),
                HTML('<strong>Between two parties</strong>'),
                tableOutput('coalitions_2'),
                hr(),
                HTML('<strong>Between three parties</strong>'),
                tableOutput('coalitions_3')
                ),

            box(title = 'Info', width = 4,
                hr(),
                p('The relative importance is not a % of party votes. The % is calculated from the ratio of the importance to the sum of all importance values'),
                HTML('The calculation of the seats allocation is from Peter Ellis\'s <a href="https://github.com/ellisp/nzelect">nzelect</a> R package'),
                hr(),
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
