
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
        sliderInput("national_w", NULL, min = 0, max = 100, step = 0.01, value = pvote14['National', w], ticks = F)
    })
    output$national_e <- renderUI({
        checkboxInput("national_e", NULL, value = T)
    })
    output$labour_w <- renderUI({
        sliderInput("labour_w", NULL, min = 0, max = 100, step = 0.01, value = pvote14['Labour', w], ticks = F)
    })
    output$labour_e <- renderUI({
        checkboxInput("labour_e", NULL, value = T)
    })
    output$green_w <- renderUI({
        sliderInput("green_w", NULL, min = 0, max = 100, step = 0.01, value = pvote14['Green', w], ticks = F)
    })
    output$green_e <- renderUI({
        checkboxInput("green_e", NULL, value = F)
    })
    output$nzfirst_w <- renderUI({
        sliderInput("nzfirst_w", NULL, min = 0, max = 100, step = 0.01, value = pvote14['NZ First', w], ticks = F)
    })
    output$nzfirst_e <- renderUI({
        checkboxInput("nzfirst_e", NULL, value = F)
    })
    output$top_w <- renderUI({
        sliderInput("top_w", NULL, min = 0, max = 100, step = 0.01, value = pvote14['TOP', w], ticks = F)
    })
    output$top_e <- renderUI({
        checkboxInput("top_e", NULL, value = F)
    })
    output$maori_w <- renderUI({
        sliderInput("maori_w", NULL, min = 0, max = 100, step = 0.01, value = pvote14['Maori', w], ticks = F)
    })
    output$maori_e <- renderUI({
        checkboxInput("maori_e", NULL, value = T)
    })
    output$act_w <- renderUI({
        sliderInput("act_w", NULL, min = 0, max = 100, step = 0.01, value = pvote14['ACT', w], ticks = F)
    })
    output$act_e <- renderUI({
        checkboxInput("act_e", NULL, value = T)
    })
    output$conserv_w <- renderUI({
        sliderInput("conserv_w", NULL, min = 0, max = 100, step = 0.01, value = pvote14['Conservative', w], ticks = F)
    })
    output$conserv_e <- renderUI({
        checkboxInput("conserv_e", NULL, value = F)
    })
    output$ufuture_w <- renderUI({
        sliderInput("ufuture_w", NULL, min = 0, max = 100, step = 0.01, value = pvote14['United Future', w], ticks = F)
    })
    output$ufuture_e <- renderUI({
        checkboxInput("ufuture_e", NULL, value = T)
    })
    output$other_w <- renderUI({
        sliderInput("other_w", NULL, min = 0, max = 100, step = 0.01, value = pvote14['Other', w], ticks = F)
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
    output$national_s <- renderText({if (!is.null(parties())) {parties()[party == 'National', sprintf('%i seats', seats)]}})
    output$labour_p <- renderText({if (!is.null(parties())) {parties()[party == 'Labour', sprintf('%0.1f %%', p)]}})
    output$labour_s <- renderText({if (!is.null(parties())) {parties()[party == 'Labour', sprintf('%i seats', seats)]}})
    output$green_p <- renderText({if (!is.null(parties())) {parties()[party == 'Green', sprintf('%0.1f %%', p)]}})
    output$green_s <- renderText({if (!is.null(parties())) {parties()[party == 'Green', sprintf('%i seats', seats)]}})
    output$nzfirst_p <- renderText({if (!is.null(parties())) {parties()[party == 'NZ First', sprintf('%0.1f %%', p)]}})
    output$nzfirst_s <- renderText({if (!is.null(parties())) {parties()[party == 'NZ First', sprintf('%i seats', seats)]}})
    output$top_p <- renderText({if (!is.null(parties())) {parties()[party == 'TOP', sprintf('%0.1f %%', p)]}})
    output$top_s <- renderText({if (!is.null(parties())) {parties()[party == 'TOP', sprintf('%i seats', seats)]}})
    output$maori_p <- renderText({if (!is.null(parties())) {parties()[party == 'Maori', sprintf('%0.1f %%', p)]}})
    output$maori_s <- renderText({if (!is.null(parties())) {parties()[party == 'Maori', sprintf('%i seats', seats)]}})
    output$act_p <- renderText({if (!is.null(parties())) {parties()[party == 'ACT', sprintf('%0.1f %%', p)]}})
    output$act_s <- renderText({if (!is.null(parties())) {parties()[party == 'ACT', sprintf('%i seats', seats)]}})
    output$conserv_p <- renderText({if (!is.null(parties())) {parties()[party == 'Conservative', sprintf('%0.1f %%', p)]}})
    output$conserv_s <- renderText({if (!is.null(parties())) {parties()[party == 'Conservative', sprintf('%i seats', seats)]}})
    output$ufuture_p <- renderText({if (!is.null(parties())) {parties()[party == 'United Future', sprintf('%0.1f %%', p)]}})
    output$ufuture_s <- renderText({if (!is.null(parties())) {parties()[party == 'United Future', sprintf('%i seats', seats)]}})
    output$other_p <- renderText({if (!is.null(parties())) {parties()[party == 'Other', sprintf('%0.1f %%', p)]}})
    output$other_s <- renderText({if (!is.null(parties())) {parties()[party == 'Other', sprintf('%i seats', seats)]}})


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
                           yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))

                p %>% config(displayModeBar = F)

            } else return(NULL)
    })

    ## Table of potential winning coalitions
    output$coalitions <- renderTable({
        d <- parties()
        if (!is.null(d)) {
            ord <- d[order(p), party]
            minseats <- sum(d$seats) / 2
            pairs <- CJ(party1 = d$party, party2 = d$party)
            pairs <- pairs[party1 != party2]
            pairs <- pairs[as.numeric(factor(party1, levels = ord)) > as.numeric(factor(party2, levels = ord))]
            pairs[, seats1 := d[match(party1, party), seats]]
            pairs[, seats2 := d[match(party2, party), seats]]
            pairs[, tot := seats1 + seats2]
            wins <- pairs[tot >= minseats]
            wins <- wins[!(party1 %in% c('National', 'Labour') & party2 %in% c('National', 'Labour'))]
            setorder(wins, -tot)
            maxchar1 <- wins[, max(nchar(party1))]
            maxchar2 <- wins[, max(nchar(party2))]
            wins[, lab := sprintf(sprintf('%%%is - %%%is', maxchar1, maxchar2), party1, party2)]
            wins[, seats := sprintf('%i seats', tot)]
            if (nrow(wins)) {
                return(wins[, .(lab, seats)])
            }
        }
    }, colnames = F)
    

})

