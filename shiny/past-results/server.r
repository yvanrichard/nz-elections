
##############
## server.r ##
##############

library(data.table)
library(nzelect)
library(shiny)
library(shinydashboard)
library(ggplot2)
library(plotly)
options(scipen = 100)
options(browser = 'google-chrome')


getmainparties <- function(parties) {
    return(ifelse(parties == 'National Party', 'National',
        ifelse(parties == 'Labour Party', 'Labour',
        ifelse(parties == 'Green Party', 'Green',
        ifelse(parties == 'NZ First', 'NZ First',
        ifelse(parties == 'Maori Party', 'Maori',
        ifelse(parties == 'TOP', 'TOP',
        ifelse(parties == 'ACT', 'ACT',
        ifelse(parties == 'Conservative', 'Conservative',
        ifelse(parties == 'United Future', 'United Future',
               'Other'))))))))))
}

cols <- list(party = c(Green           = '#008641',
                      Labour          = '#D82C20',
                      National        = '#00529E',
                      'NZ First'      = '#555555',
                      TOP             = '#0F175F',
                      Maori           = '#F6A78B',
                      ACT             = '#FFDA37',
                      Conservative    = '#F37133',
                      'United Future' = '#4B104E',
                      Other           = '#999999'),
            before_onday = c(normal   = "#E41A1C",
                             advanced = "#984EA3"),
            local_overseas = c(local    = "#E41A1C",
                               overseas = "#984EA3"),
            ordinary_special = c(ordinary = "#E41A1C",
                                 special  = "#984EA3"))

relabel <- function(x) {

    labels <- matrix(c(
        'electorate', 'Electorate',
        'before_onday', 'Advanced/Normal voting',
        'local_overseas', 'NZ / Overseas',
        'ordinary_special', 'Ordinary / Special',
        'party', 'Party',
        'ordinary', 'Ordinary votes',
        'special', 'Special votes',
        'normal', 'On-day votes',
        'advanced', 'Advance votes',
        'local', 'Local votes',
        'overseas', 'Overseas votes'), ncol=2, byrow=T)
    labs <- labels[,2]
    names(labs) <- labels[,1]

    x2 <- labs[x]
    x2[is.na(x2)] <- x[is.na(x2)]
    names(x2) <- NULL
    return(x2)
}



res <- fread('data/party-votes_long.csv')

electorates <- c('All electorates', sort(unique(res$electorate)))
parties <- c('All parties', sort(unique(res$party)))

before_onday_choices <- c('All', sort(unique(res$before_onday)))
names(before_onday_choices) <- relabel(before_onday_choices)

local_overseas_choices <- c('All', sort(unique(res$local_overseas)))
names(local_overseas_choices) <- relabel(local_overseas_choices)

ordinary_special_choices <- c('All', sort(unique(res$ordinary_special)))
names(ordinary_special_choices) <- relabel(ordinary_special_choices)

group_choices <- c('party', 'before_onday', 'local_overseas', 'ordinary_special')
names(group_choices) <- relabel(group_choices)

session <- NULL
debug <- FALSE

enableBookmarking(store = "url")

             
shinyServer(function(input, output, session) {
   

#################
## UI elements ##
#################

    output$electorate <- renderUI({
        selectInput("electorate", 'Electorate', choices = electorates, selected = 'All electorates')
    })

    output$before_onday <- renderUI({
        selectInput("before_onday", 'Advanced/Normal voting', choices = before_onday_choices, selected = 'All')
    })

    output$local_overseas <- renderUI({
        selectInput("local_overseas", 'NZ / Overseas', choices = local_overseas_choices, selected = 'All')
    })

    output$ordinary_special <- renderUI({
        selectInput("ordinary_special", 'Ordinary / Special', choices = ordinary_special_choices, selected = 'All')
    })
    
    output$party <- renderUI({
        selectInput("party", 'Party', choices = parties, selected = 'All parties')
    })

    output$stat <- renderUI({
        selectInput("stat", 'No. / %', choices = c('% votes', 'No. votes'), selected = '% votes')
    })

    output$group <- renderUI({
        selectInput("group", 'Comparison', choices = group_choices,
                    selected = 'party')
    })
    
    
    ## input <- list(electorate = 'All electorates', party = 'All parties', before_onday = 'All', local_overseas = 'All', ordinary_special = 'All', stat = '% votes', group = 'party')

    
##################
## Derived data ##
##################

    ## Election results
    data <- reactive({
        if (!is.null(input$electorate) & !is.null(input$before_onday) & !is.null(input$local_overseas) &
            !is.null(input$ordinary_special) & !is.null(input$party) & !is.null(input$stat)) {
            d <- copy(res)
            if (input$electorate != 'All electorates')
                d <- d[electorate == input$electorate]
            if (input$party != 'All parties')
                d <- d[party == input$party]
            if (input$ordinary_special != 'All')
                d <- d[ordinary_special == input$ordinary_special]
            if (input$before_onday != 'All')
                d <- d[before_onday == input$before_onday]
            if (input$local_overseas != 'All')
                d <- d[local_overseas == input$local_overseas]
            return(d)
        }
    })


    ## chart
    output$chart  <- renderPlotly({
        d <- copy(data())
        if (!is.null(d)) {

            if (nrow(d)) {

                d[, x := year]
                d[, z := get(input$group)]
                if (input$group == 'party' & input$party == 'All parties') {
                    d[, z := getmainparties(z)]
                }
                dd <- d[, .(votes = sum(votes)), .(x, z)]
                if (input$stat == '% votes') {
                    dd[, y := 100 * votes / sum(votes), x]
                } else dd[, y := votes]

                dd[, z := relabel(z)]
                
                clrs <- cols[[input$group]]
                names(clrs) <- relabel(names(clrs))

                dd[, col := clrs[z]]
                
                p <- plot_ly(dd, x = ~x, y = ~y, mode = 'lines+markers', color = ~z, colors = clrs
                          , hoverinfo = 'text'
                          , text = ~sprintf('%s\n%s', z, round(y,1))
                            ) %>%
                    layout(title = ''
                           , xaxis = list(title = 'Election year', showgrid = FALSE)
                           , yaxis = list(title = input$stat, showgrid = FALSE)
                           )

                return(p)
            } else return(NULL)
            } else return(NULL)
    })

    ## output$test <- renderTable({
    ##     data()
        
    ## })
    
})

