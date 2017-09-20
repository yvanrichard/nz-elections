##########
## ui.r ##
##########

library(data.table)
library(shiny)
library(shinyjs)
library(shinydashboard)
library(DT)
library(V8)
## library(rbokeh)
addResourcePath('data', 'data')

enableBookmarking(store = "url")

function(request) {
    ## loadingLogo <- function(href, src, loadingsrc, height = NULL, width = NULL, alt = NULL) {
    ##     ## tagList(
    ##     ##     tags$head(
    ##     ##              tags$script(
    ##     ##                       "setInterval(function(){
    ##     ##              if ($('html').attr('class')=='shiny-busy') {
    ##     ##              $('div.busy').show();
    ##     ##              $('div.notbusy').hide();
    ##     ##              } else {
    ##     ##              $('div.busy').hide();
    ##     ##              $('div.notbusy').show();
    ##     ##    }
    ##     ##  },100)")
    ##     ##  ),
    ##     ##  div(class = "busy",  
    ##     ##      img(src=loadingsrc,height = height, width = width, alt = alt)),
    ##     ##  div(class = 'notbusy',
    ##     ##      div(class = 'logo', "Government simulator"))
    ##     ## )
    ## }
    dashboardPage(
        title = "Government simulator",
        header = dashboardHeader(
            title = 'Government simulator',
                     ## title = loadingLogo('http://www.google.co.nz',
                     ##                     'data/logo.png',
                     ##                     'data/ajax-loader.gif'),
                     titleWidth = 300
                 ),
        
        sidebar = dashboardSidebar(width = 400,
            tags$head(
                     tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
                 ),
            useShinyjs(),  # Set up shinyjs
            fluidRow(column(3, h5('')),
                     column(3, align = 'center', h4('Importance')),
                     column(3, uiOutput('Won electorates'))),
            fluidRow(column(3, h5('National')),
                     column(7, uiOutput('national_w')),
                     column(1, uiOutput('national_e'))),
            fluidRow(column(3, h5('Labour')),
                     column(7, uiOutput('labour_w')),
                     column(1, uiOutput('labour_e'))),
            fluidRow(column(3, h5('Green')),
                     column(7, uiOutput('green_w')),
                     column(1, uiOutput('green_e'))),
            fluidRow(column(3, h5('NZ First')),
                     column(7, uiOutput('nzfirst_w')),
                     column(1, uiOutput('nzfirst_e'))),
            fluidRow(column(3, h5('TOP')),
                     column(7, uiOutput('top_w')),
                     column(1, uiOutput('top_e'))),
            fluidRow(column(3, h5('Maori')),
                     column(7, uiOutput('maori_w')),
                     column(1, uiOutput('maori_e'))),
            fluidRow(column(3, h5('ACT')),
                     column(7, uiOutput('act_w')),
                     column(1, uiOutput('act_e'))),
            fluidRow(column(3, h5('Other')),
                     column(7, uiOutput('other_w')),
                     column(1, uiOutput('other_e'))),
            hr(),
            bookmarkButton()
        ),
        
            
        body = dashboardBody(
                   ## navbarPage('>>', id = 'navbar',
                   ##            tabPanel("Set up", value = 'tab1',
            ## useShinyjs(),  # Set up shinyjs
            ## extendShinyjs(text = jscode),
            ## inlineCSS(css),
            box(title = 'Parliament', width = 6, tableOutput('table')),
            box(title = 'Info', width = 6,
                h5('The importance sliders on the left hand side drive the results of party votes.'),
                h5('The check box on their right specifies whether the party won any electorate (assumed to be one electorate).'),
                h5('All calculations are made using the package nzelect from Peter Ellis.'))
             
        )
    )
}
