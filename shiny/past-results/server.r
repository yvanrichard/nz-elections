
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
                            ## , textposition = 'inside'
                            ## , textinfo = 'label+value'
                          , hoverinfo = 'text'
                            ## , insidetextfont = list(color = '#FFFFFF')
                          , text = ~sprintf('%s\n%s', z, round(y,1))
                          ## , marker = list(color = col)
                            ## , showlegend = F
                            ## , dislayModeBar = F
                            ) %>%
                    layout(title = ''
                           , xaxis = list(title = 'Election year', showgrid = FALSE)
                           , yaxis = list(title = input$stat, showgrid = FALSE)
                           ## , yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE)
                           ## , margin = list(l = 0, r = 0, t = 30, b = 0)
                           )
                ## p
                ## g
                
                ## p <- plot_ly(d, labels = ~party, values = ~seats, type = 'pie',
                ##             textposition = 'inside',
                ##             textinfo = 'label+value',
                ##             hoverinfo = 'text',
                ##             insidetextfont = list(color = '#FFFFFF'),
                ##             text = ~sprintf('%s\n%0.1f %% party votes\n%i seats', party, p, seats),
                ##             marker = list(colors = clrs),
                ##             showlegend = F,
                ##             dislayModeBar = F
                ##             ) %>%
                ##     layout(title = 'Seats in NZ Parliament',
                ##            xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                ##            yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                ##            margin = list(l = 0, r = 0, t = 30, b = 0))

                ## p %>% config(displayModeBar = F)

                return(p)
            } else return(NULL)
            } else return(NULL)
    })

    ## output$total_seats <- renderText({
    ##     d <- parties()
    ##     if (!is.null(d)) {
    ##         return(sprintf('%i seats in total', sum(d$seats)))
    ##     }
    ## })

    ## coalitions2 <- reactive({
    ##     d <- parties()
    ##     if (!is.null(d)) {
    ##         ord <- d[order(p), party]
    ##         minseats <- sum(d$seats) / 2

    ##         ## 2-ways
    ##         pairs <- CJ(party1 = d$party, party2 = d$party)
    ##         pairs <- pairs[party1 != party2]
    ##         pairs <- pairs[as.numeric(factor(party1, levels = ord)) > as.numeric(factor(party2, levels = ord))]
    ##         pairs[, seats1 := d[match(party1, party), seats]]
    ##         pairs[, seats2 := d[match(party2, party), seats]]
    ##         pairs[, tot := seats1 + seats2]
    ##         wins2 <- pairs[tot > minseats]
    ##         wins2 <- wins2[!(party1 %in% c('National', 'Labour') & party2 %in% c('National', 'Labour'))]
    ##         setorder(wins2, -tot)
            
    ##         if (nrow(wins2)) {
    ##             maxchar1 <- wins2[, max(nchar(party1))]
    ##             maxchar2 <- wins2[, max(nchar(party2))]
    ##             wins2[, lab := sprintf(sprintf('%%%is - %%%is', maxchar1, maxchar2), party1, party2)]
    ##             wins2[, seats := sprintf('%i seats', tot)]
    ##         }
    ##         return(wins2)
    ##     }
        
    ## })

    ## coalitions3 <- reactive({
    ##     d <- parties()
    ##     if (!is.null(d)) {
    ##         ord <- d[order(p), party]
    ##         minseats <- sum(d$seats) / 2

    ##         ## 3-ways
    ##         triples <- CJ(party1 = d$party, party2 = d$party, party3 = d$party)
    ##         triples <- triples[party1 != party2 & party2 != party3 & party1 != party3]
    ##         triples <- triples[(as.numeric(factor(party1, levels = ord)) > as.numeric(factor(party2, levels = ord))) &
    ##                           (as.numeric(factor(party2, levels = ord)) > as.numeric(factor(party3, levels = ord)))]
    ##         triples[, seats1 := d[match(party1, party), seats]]
    ##         triples[, seats2 := d[match(party2, party), seats]]
    ##         triples[, seats3 := d[match(party3, party), seats]]
    ##         triples[, tot := seats1 + seats2 + seats3]
    ##         wins3 <- triples[tot > minseats]
    ##         wins3 <- wins3[!apply(wins3, 1, function(x) 'National' %in% x & 'Labour' %in% x)]
    ##         setorder(wins3, -tot)

    ##         if (nrow(wins3)) {
    ##             maxchar1 <- wins3[, max(nchar(party1))]
    ##             maxchar2 <- wins3[, max(nchar(party2))]
    ##             maxchar3 <- wins3[, max(nchar(party3))]
    ##             wins3[, lab := sprintf(sprintf('%%%is - %%%is - %%%is', maxchar1, maxchar2, maxchar3), party1, party2, party3)]
    ##             wins3[, seats := sprintf('%i seats', tot)]
    ##         }
    ##         return(wins3)
    ##     }
    ## })
    
    ## ## Table of potential winning coalitions
    ## output$coalitions_2 <- renderTable({
    ##     wins <- coalitions2()
    ##     if (!is.null(wins)) {
    ##         if (nrow(wins)) {
    ##             return(wins[, .(lab, seats)])
    ##         } else return(data.frame('No possible coalitions'))
    ##     }
    ## }, colnames = F)
    
    ## output$coalitions_3 <- renderTable({
    ##     wins3 <- coalitions3()
    ##     if (!is.null(wins3)) {
    ##         if (nrow(wins3)) {
    ##             ## Remove combinations with already a possible 2-way coalition
    ##             wins2 <- coalitions2()
    ##             if (!is.null(wins2)) {
    ##                 if (nrow(wins2)) {
    ##                     for (i in 1:nrow(wins2)) {
    ##                         p1 <- wins2[i, party1]
    ##                         p2 <- wins2[i, party2]
    ##                         wins3 <- wins3[which(!apply(wins3[, .(party1, party2, party3)], 1, function(x) {p1 %in% x & p2 %in% x}))]
    ##                     }
    ##                 }
    ##             }
                
    ##         }
    ##         if (nrow(wins3)) {
    ##             return(wins3[, .(lab, seats)])
    ##         } else {
    ##             return(data.frame('No further coalitions (three-party coalitions are not shown when a coalition between two of the parties is sufficient)'))
    ##         }
    ##     }
    ## }, colnames = F)

    output$test <- renderTable({
        data()
        
    })
    
})

