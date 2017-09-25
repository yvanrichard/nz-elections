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
        title = "Special votes impact",
        header = dashboardHeader(
            title = 'Special votes impact',
                     titleWidth = 300
                 ),
        
        sidebar = dashboardSidebar(width = '0%'
        ),
            
        body = dashboardBody(
            tags$head(
                     tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
                 ),
            tags$style(type = "text/css", "
      .irs-bar {width: 100%; height: 8px; background: #999; border-top: 1px solid #999; border-bottom: 1px solid #999; top: 15px}
      .irs-line {border: 1px solid #ddd; height: 0px; border-radius: 0px; top: 17px;}
      .irs-grid-text {display: none;}
      .irs-grid-pol {display: none;}
      .irs-min {display: none;}
      .irs-max {display: none;}
      .irs-single {display: none;}
      .irs-bar-edge {top: 15px; width: 5px; background: #888; border: #888; border-radios: 0px}
      .irs-slider {width: 10px; height: 20px; top: 9px;}
    "),
            box(width = 12,
                h3('Find out how many seats each party would have in the New Zealand government when considering the special votes'),
                p('Change the importance sliders to get the desired distribution of special votes'),
                p('The current votes are from the initial results of the 2017 elections and do not include special votes. The special votes are distributed initially from the results of the 2014 elections'),
                p('For fine tuning, click on a slider then use the arrow keys of the keyboard')
                ),
            box(title = 'Election results',
                width = 5,
                hr(),
                fluidRow(column(2, h5('No. special votes:'), offset = 4), column(2, uiOutput('special_votes'))),
                fixedRow(column(2, h5('')),
                         column(2, align='center', h4('Current'), hr()),
                         column(4, align='center', h4('Specials'), hr()),
                         column(4, align='center', h4('Total'), hr())),
                fixedRow(column(2, h5('')),
                         column(1, align='center', strong('Votes')),
                         column(1, align='center', strong('Seats')),
                         column(2, align='center', strong('Weight')),
                         column(1, align='center', strong('%')),
                         column(1, align='center', strong('Votes')),
                         column(1, align='center', strong('%')),
                         column(1, align='center', strong('Votes')),
                         column(1, align='center', strong('Seats')),
                         column(1, align='center', strong('Diff'))),
                
                fixedRow(column(2, align='right', h4('National')),
                         column(1, textOutput('national_v')),
                         column(1, textOutput('national_s')),
                         column(2, uiOutput('national_w')),
                         column(1, textOutput('national_ps')),
                         column(1, textOutput('national_vs')),
                         column(1, textOutput('national_pnew')),
                         column(1, textOutput('national_vnew')),
                         column(1, textOutput('national_snew')),
                         column(1, textOutput('national_sdiff'))),
                fixedRow(column(2, align='right', h4('Labour')),
                         column(1, textOutput('labour_v')),
                         column(1, textOutput('labour_s')),
                         column(2, uiOutput('labour_w')),
                         column(1, textOutput('labour_ps')),
                         column(1, textOutput('labour_vs')),
                         column(1, textOutput('labour_pnew')),
                         column(1, textOutput('labour_vnew')),
                         column(1, textOutput('labour_snew')),
                         column(1, textOutput('labour_sdiff'))),
                fixedRow(column(2, align='right', h4('Green')),
                         column(1, textOutput('green_v')),
                         column(1, textOutput('green_s')),
                         column(2, uiOutput('green_w')),
                         column(1, textOutput('green_ps')),
                         column(1, textOutput('green_vs')),
                         column(1, textOutput('green_pnew')),
                         column(1, textOutput('green_vnew')),
                         column(1, textOutput('green_snew')),
                         column(1, textOutput('green_sdiff'))),
                fixedRow(column(2, align='right', h4('NZ First')),
                         column(1, textOutput('nzfirst_v')),
                         column(1, textOutput('nzfirst_s')),
                         column(2, uiOutput('nzfirst_w')),
                         column(1, textOutput('nzfirst_ps')),
                         column(1, textOutput('nzfirst_vs')),
                         column(1, textOutput('nzfirst_pnew')),
                         column(1, textOutput('nzfirst_vnew')),
                         column(1, textOutput('nzfirst_snew')),
                         column(1, textOutput('nzfirst_sdiff'))),
                fixedRow(column(2, align='right', h4('TOP')),
                         column(1, textOutput('top_v')),
                         column(1, textOutput('top_s')),
                         column(2, uiOutput('top_w')),
                         column(1, textOutput('top_ps')),
                         column(1, textOutput('top_vs')),
                         column(1, textOutput('top_pnew')),
                         column(1, textOutput('top_vnew')),
                         column(1, textOutput('top_snew')),
                         column(1, textOutput('top_sdiff'))),
                fixedRow(column(2, align='right', h4('Maori')),
                         column(1, textOutput('maori_v')),
                         column(1, textOutput('maori_s')),
                         column(2, uiOutput('maori_w')),
                         column(1, textOutput('maori_ps')),
                         column(1, textOutput('maori_vs')),
                         column(1, textOutput('maori_pnew')),
                         column(1, textOutput('maori_vnew')),
                         column(1, textOutput('maori_snew')),
                         column(1, textOutput('maori_sdiff'))),
                fixedRow(column(2, align='right', h4('Conservative')),
                         column(1, textOutput('conserv_v')),
                         column(1, textOutput('conserv_s')),
                         column(2, uiOutput('conserv_w')),
                         column(1, textOutput('conserv_ps')),
                         column(1, textOutput('conserv_vs')),
                         column(1, textOutput('conserv_pnew')),
                         column(1, textOutput('conserv_vnew')),
                         column(1, textOutput('conserv_snew')),
                         column(1, textOutput('conserv_sdiff'))),
                fixedRow(column(2, align='right', h4('United Future')),
                         column(1, textOutput('ufuture_v')),
                         column(1, textOutput('ufuture_s')),
                         column(2, uiOutput('ufuture_w')),
                         column(1, textOutput('ufuture_ps')),
                         column(1, textOutput('ufuture_vs')),
                         column(1, textOutput('ufuture_pnew')),
                         column(1, textOutput('ufuture_vnew')),
                         column(1, textOutput('ufuture_snew')),
                         column(1, textOutput('ufuture_sdiff'))),
                fixedRow(column(2, align='right', h4('ACT')),
                         column(1, textOutput('act_v')),
                         column(1, textOutput('act_s')),
                         column(2, uiOutput('act_w')),
                         column(1, textOutput('act_ps')),
                         column(1, textOutput('act_vs')),
                         column(1, textOutput('act_pnew')),
                         column(1, textOutput('act_vnew')),
                         column(1, textOutput('act_snew')),
                         column(1, textOutput('act_sdiff'))),
                fixedRow(column(2, align='right', h4('Other')),
                         column(1, textOutput('other_v')),
                         column(1, textOutput('other_s')),
                         column(2, uiOutput('other_w')),
                         column(1, textOutput('other_ps')),
                         column(1, textOutput('other_vs')),
                         column(1, textOutput('other_pnew')),
                         column(1, textOutput('other_vnew')),
                         column(1, textOutput('other_snew')),
                         column(1, textOutput('other_sdiff')))
                ),
                     
            box(title = 'Parliament', width = 4,
                hr(),
                div(plotlyOutput("pie", height = "100%"), align = "center"),
                textOutput('total_seats')
                ),
            
            box(title = 'Potential coalitions', width = 3,
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

            box(title = 'Info', width = 3,
                hr(),
                p('The relative importance is not a percentage of special votes. The percentage is calculated from the ratio of the importance to the sum of all importance values'),
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
