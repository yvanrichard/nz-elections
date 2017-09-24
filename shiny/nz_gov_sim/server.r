
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


evote <- fread('data/electoral-votes_ordinary-special.csv')
pvote <- fread('data/party-votes_ordinary-special.csv')

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

evote14 <- evote[year == 2014,
                .(party = .SD[which.max(all), party]),
                .(year, electorate)][, .(elec_won=.N), .(year, party)]
pvote14 <- pvote[year == 2014,
                .(votes = sum(all)), party]
pvote14 <- pvote14[, .(votes = sum(votes)), .(party = getmainparties(party))]
pvote14 <- rbind(pvote14, data.table(party='TOP', votes=0))
pvote14[, p := votes / sum(votes)]
setkey(pvote14, party)
pvote14['National', w := 100.0]
pnat <- pvote14['National', p]
for (i in 1:nrow(pvote14)) {
    pvote14[i, w := 100.0 * p / pnat]
}

cols <- c(Green           = '#008641',
         Labour          = '#D82C20',
         National        = '#00529E',
         'NZ First'      = '#555555',
         TOP             = '#0F175F',
         Maori           = '#F6A78B',
         ACT             = '#FFDA37',
         Conservative    = '#F37133',
         'United Future' = '#4B104E',
         Other           = '#999999')

session <- NULL
debug <- FALSE

enableBookmarking(store = "url")

             
shinyServer(function(input, output, session) {
   

#################
## UI elements ##
#################

    output$national_w <- renderUI({
        sliderInput("national_w", NULL, label=NULL, min = 0, max = 100, step = 0.01, value = pvote14['National', w], ticks = F)
    })
    output$national_e <- renderUI({
        checkboxInput("national_e", NULL, value = T)
    })
    output$labour_w <- renderUI({
        sliderInput("labour_w", NULL, label=NULL, min = 0, max = 100, step = 0.01, value = pvote14['Labour', w], ticks = F)
    })
    output$labour_e <- renderUI({
        checkboxInput("labour_e", NULL, value = T)
    })
    output$green_w <- renderUI({
        sliderInput("green_w", NULL, label=NULL, min = 0, max = 100, step = 0.01, value = pvote14['Green', w], ticks = F)
    })
    output$green_e <- renderUI({
        checkboxInput("green_e", NULL, value = F)
    })
    output$nzfirst_w <- renderUI({
        sliderInput("nzfirst_w", NULL, label=NULL, min = 0, max = 100, step = 0.01, value = pvote14['NZ First', w], ticks = F)
    })
    output$nzfirst_e <- renderUI({
        checkboxInput("nzfirst_e", NULL, value = F)
    })
    output$top_w <- renderUI({
        sliderInput("top_w", NULL, label=NULL, min = 0, max = 100, step = 0.01, value = pvote14['TOP', w], ticks = F)
    })
    output$top_e <- renderUI({
        checkboxInput("top_e", NULL, value = F)
    })
    output$maori_w <- renderUI({
        sliderInput("maori_w", NULL, label=NULL, min = 0, max = 100, step = 0.01, value = pvote14['Maori', w], ticks = F)
    })
    output$maori_e <- renderUI({
        checkboxInput("maori_e", NULL, value = T)
    })
    output$act_w <- renderUI({
        sliderInput("act_w", NULL, label=NULL, min = 0, max = 100, step = 0.01, value = pvote14['ACT', w], ticks = F)
    })
    output$act_e <- renderUI({
        checkboxInput("act_e", NULL, value = T)
    })
    output$conserv_w <- renderUI({
        sliderInput("conserv_w", NULL, label=NULL, min = 0, max = 100, step = 0.01, value = pvote14['Conservative', w], ticks = F)
    })
    output$conserv_e <- renderUI({
        checkboxInput("conserv_e", NULL, value = F)
    })
    output$ufuture_w <- renderUI({
        sliderInput("ufuture_w", NULL, label=NULL, min = 0, max = 100, step = 0.01, value = pvote14['United Future', w], ticks = F)
    })
    output$ufuture_e <- renderUI({
        checkboxInput("ufuture_e", NULL, value = T)
    })
    output$other_w <- renderUI({
        sliderInput("other_w", NULL, label=NULL, min = 0, max = 100, step = 0.01, value = pvote14['Other', w], ticks = F)
    })
    output$other_e <- renderUI({
        checkboxInput("other_e", NULL, value = F)
    })

    
##################
## Derived data ##
##################

    ## Election results
    parties <- reactive({
        if (!is.null(input$other_w)) {
            d <- data.table(party = c('National', 'Labour', 'Green', 'NZ First', 'TOP',
                                     'Maori', 'ACT', 'Conservative', 'United Future', 'Other'),
                       weights = c(input$national_w, input$labour_w, input$green_w, input$nzfirst_w, input$top_w,
                                   input$maori_w, input$act_w, input$conserv_w, input$ufuture_w, input$other_w),
                       w_elec = c(input$national_e, input$labour_e, input$green_e, input$nzfirst_e, input$top_e,
                                  input$maori_e, input$act_e, input$conserv_e, input$ufuture_e, input$other_e))
        d[, p := 100 * weights / sum(weights)]
        d[, e := ifelse(w_elec == T, 1, 0)]
        d[, seats := allocate_seats(d$p, d$party, electorate=d$e)$seats_v]
        return(d)
        }
    })

    ## Report back on % of party votes and the number of seats next to controls
    output$national_p <- renderText({if (!is.null(parties())) {parties()[party == 'National', sprintf('%0.1f %%', p)]}})
    output$national_s <- renderText({if (!is.null(parties())) {parties()[party == 'National', sprintf('%i seat%s', seats, ifelse(seats>1, 's', ''))]}})
    output$labour_p <- renderText({if (!is.null(parties())) {parties()[party == 'Labour', sprintf('%0.1f %%', p)]}})
    output$labour_s <- renderText({if (!is.null(parties())) {parties()[party == 'Labour', sprintf('%i seat%s', seats, ifelse(seats>1, 's', ''))]}})
    output$green_p <- renderText({if (!is.null(parties())) {parties()[party == 'Green', sprintf('%0.1f %%', p)]}})
    output$green_s <- renderText({if (!is.null(parties())) {parties()[party == 'Green', sprintf('%i seat%s', seats, ifelse(seats>1, 's', ''))]}})
    output$nzfirst_p <- renderText({if (!is.null(parties())) {parties()[party == 'NZ First', sprintf('%0.1f %%', p)]}})
    output$nzfirst_s <- renderText({if (!is.null(parties())) {parties()[party == 'NZ First', sprintf('%i seat%s', seats, ifelse(seats>1, 's', ''))]}})
    output$top_p <- renderText({if (!is.null(parties())) {parties()[party == 'TOP', sprintf('%0.1f %%', p)]}})
    output$top_s <- renderText({if (!is.null(parties())) {parties()[party == 'TOP', sprintf('%i seat%s', seats, ifelse(seats>1, 's', ''))]}})
    output$maori_p <- renderText({if (!is.null(parties())) {parties()[party == 'Maori', sprintf('%0.1f %%', p)]}})
    output$maori_s <- renderText({if (!is.null(parties())) {parties()[party == 'Maori', sprintf('%i seat%s', seats, ifelse(seats>1, 's', ''))]}})
    output$act_p <- renderText({if (!is.null(parties())) {parties()[party == 'ACT', sprintf('%0.1f %%', p)]}})
    output$act_s <- renderText({if (!is.null(parties())) {parties()[party == 'ACT', sprintf('%i seat%s', seats, ifelse(seats>1, 's', ''))]}})
    output$conserv_p <- renderText({if (!is.null(parties())) {parties()[party == 'Conservative', sprintf('%0.1f %%', p)]}})
    output$conserv_s <- renderText({if (!is.null(parties())) {parties()[party == 'Conservative', sprintf('%i seat%s', seats, ifelse(seats>1, 's', ''))]}})
    output$ufuture_p <- renderText({if (!is.null(parties())) {parties()[party == 'United Future', sprintf('%0.1f %%', p)]}})
    output$ufuture_s <- renderText({if (!is.null(parties())) {parties()[party == 'United Future', sprintf('%i seat%s', seats, ifelse(seats>1, 's', ''))]}})
    output$other_p <- renderText({if (!is.null(parties())) {parties()[party == 'Other', sprintf('%0.1f %%', p)]}})
    output$other_s <- renderText({if (!is.null(parties())) {parties()[party == 'Other', sprintf('%i seat%s', seats, ifelse(seats>1, 's', ''))]}})


    ## Government pie
    output$pie  <- renderPlotly({
        d <- parties()
        if (!is.null(d)) {

                clrs <- cols[d$party]

                p <- plot_ly(d, labels = ~party, values = ~seats, type = 'pie',
                            textposition = 'inside',
                            textinfo = 'label+value',
                            hoverinfo = 'text',
                            insidetextfont = list(color = '#FFFFFF'),
                            text = ~sprintf('%s\n%0.1f %% party votes\n%i seats', party, p, seats),
                            marker = list(colors = clrs),
                            showlegend = F,
                            dislayModeBar = F
                            ) %>%
                    layout(title = 'Seats in NZ Parliament',
                           xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                           yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                           margin = list(l = 0, r = 0, t = 30, b = 0))

                p %>% config(displayModeBar = F)

            } else return(NULL)
    })

    output$total_seats <- renderText({
        d <- parties()
        if (!is.null(d)) {
            return(sprintf('%i seats in total', sum(d$seats)))
        }
    })

    coalitions2 <- reactive({
        d <- parties()
        if (!is.null(d)) {
            ord <- d[order(p), party]
            minseats <- sum(d$seats) / 2

            ## 2-ways
            pairs <- CJ(party1 = d$party, party2 = d$party)
            pairs <- pairs[party1 != party2]
            pairs <- pairs[as.numeric(factor(party1, levels = ord)) > as.numeric(factor(party2, levels = ord))]
            pairs[, seats1 := d[match(party1, party), seats]]
            pairs[, seats2 := d[match(party2, party), seats]]
            pairs[, tot := seats1 + seats2]
            wins2 <- pairs[tot > minseats]
            wins2 <- wins2[!(party1 %in% c('National', 'Labour') & party2 %in% c('National', 'Labour'))]
            setorder(wins2, -tot)
            
            if (nrow(wins2)) {
                maxchar1 <- wins2[, max(nchar(party1))]
                maxchar2 <- wins2[, max(nchar(party2))]
                wins2[, lab := sprintf(sprintf('%%%is - %%%is', maxchar1, maxchar2), party1, party2)]
                wins2[, seats := sprintf('%i seats', tot)]
            }
            return(wins2)
        }
        
    })

    coalitions3 <- reactive({
        d <- parties()
        if (!is.null(d)) {
            ord <- d[order(p), party]
            minseats <- sum(d$seats) / 2

            ## 3-ways
            triples <- CJ(party1 = d$party, party2 = d$party, party3 = d$party)
            triples <- triples[party1 != party2 & party2 != party3 & party1 != party3]
            triples <- triples[(as.numeric(factor(party1, levels = ord)) > as.numeric(factor(party2, levels = ord))) &
                              (as.numeric(factor(party2, levels = ord)) > as.numeric(factor(party3, levels = ord)))]
            triples[, seats1 := d[match(party1, party), seats]]
            triples[, seats2 := d[match(party2, party), seats]]
            triples[, seats3 := d[match(party3, party), seats]]
            triples[, tot := seats1 + seats2 + seats3]
            wins3 <- triples[tot > minseats]
            wins3 <- wins3[!apply(wins3, 1, function(x) 'National' %in% x & 'Labour' %in% x)]
            setorder(wins3, -tot)

            if (nrow(wins3)) {
                maxchar1 <- wins3[, max(nchar(party1))]
                maxchar2 <- wins3[, max(nchar(party2))]
                maxchar3 <- wins3[, max(nchar(party3))]
                wins3[, lab := sprintf(sprintf('%%%is - %%%is - %%%is', maxchar1, maxchar2, maxchar3), party1, party2, party3)]
                wins3[, seats := sprintf('%i seats', tot)]
            }
            return(wins3)
        }
    })
    
    ## Table of potential winning coalitions
    output$coalitions_2 <- renderTable({
        wins <- coalitions2()
        if (!is.null(wins)) {
            if (nrow(wins)) {
                return(wins[, .(lab, seats)])
            } else return(data.frame('No possible coalitions'))
        }
    }, colnames = F)
    
    output$coalitions_3 <- renderTable({
        wins3 <- coalitions3()
        if (!is.null(wins3)) {
            if (nrow(wins3)) {
                ## Remove combinations with already a possible 2-way coalition
                wins2 <- coalitions2()
                if (!is.null(wins2)) {
                    if (nrow(wins2)) {
                        for (i in 1:nrow(wins2)) {
                            p1 <- wins2[i, party1]
                            p2 <- wins2[i, party2]
                            wins3 <- wins3[which(!apply(wins3[, .(party1, party2, party3)], 1, function(x) {p1 %in% x & p2 %in% x}))]
                        }
                    }
                }
                
            }
            if (nrow(wins3)) {
                return(wins3[, .(lab, seats)])
            } else {
                return(data.frame('No further coalitions (three-party coalitions are not shown when a coalition between two of the parties is sufficient)'))
            }
        }
    }, colnames = F)

    ## output$test <- renderTable({
    ##     w <- coalitions3()
        
    ## })
    
})

