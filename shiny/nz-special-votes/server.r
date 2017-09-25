
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


## getmainparties <- function(parties) {
##     return(ifelse(parties == 'National Party', 'National',
##         ifelse(parties == 'Labour Party', 'Labour',
##         ifelse(parties == 'Green Party', 'Green',
##         ifelse(parties == 'New Zealand First Party', 'NZ First',
##         ifelse(parties == 'Māori Party', 'Maori',
##         ifelse(parties == 'The Opportunities Party (TOP)', 'TOP',
##         ifelse(parties == 'ACT New Zealand', 'ACT',
##         ifelse(parties == 'Conservative', 'Conservative',
##         ifelse(parties == 'United Future', 'United Future',
##                paste0('Other_', parties)))))))))))
## }

getmainparties <- function(parties) {
    return(ifelse(parties == 'National Party', 'National',
        ifelse(parties == 'Labour Party', 'Labour',
        ifelse(parties == 'Green Party', 'Green',
        ifelse(parties == 'New Zealand First Party', 'NZ First',
        ifelse(parties == 'Māori Party', 'Maori',
        ifelse(parties == 'The Opportunities Party (TOP)', 'TOP',
        ifelse(parties == 'ACT New Zealand', 'ACT',
        ifelse(parties == 'Conservative', 'Conservative',
        ifelse(parties == 'United Future', 'United Future',
               'Other'))))))))))
}



res <- fread('data/results_ordinary_2017.csv')

bef <- fread('data/party-votes_long.csv')
spec14 <- bef[ordinary_special == 'special' & year == 2014]
spec14 <- spec14[, .(votes = sum(votes)), party]
spec14[, p := 100 * votes / sum(votes)]

ren <- c('NZ First' = 'New Zealand First Party',
        'ACT' = 'ACT New Zealand',
        'Maori Party' = 'Māori Party')
spec14[party == 'NZ First', party := 'New Zealand First Party']
spec14[party == 'ACT', party := 'ACT New Zealand']
spec14[party == 'Maori Party', party := 'Māori Party']

res[spec14, votes_spec_14 := i.votes, on = 'party']

res[party == 'MANA', votes_spec_14 := spec14[party == 'Internet MANA', votes/2]]
res[party == 'Internet Party', votes_spec_14 := spec14[party == 'Internet MANA', votes/2]]
res[is.na(votes_spec_14), votes_spec_14 := 0]
## res[, p_votes_spec_14 := 100 * votes_spec_14 / sum(votes_spec_14)]
## res[, party := getmainparties(party)]

simplify <- function(res) {
    s <- res[, .(votes = sum(votes), votes_spec_14 = sum(votes_spec_14), elec_seats = sum(elec_seats), tot_seats = sum(tot_seats)),
            .(party=getmainparties(party))]
    s[, p_spec := 100 * votes_spec_14 / sum(votes_spec_14)]
    setorder(s, -votes)
    return(s)
}

res <- simplify(res)

setkey(res, party)

res['National', w := 100.0]
pnat <- res['National', p_spec]
for (i in 1:nrow(res)) {
    res[i, w := 100.0 * p_spec / pnat]
}

## setorder(res, -votes)

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

# input <- list(national_w = 47, labour_w = 25.1, green_w = 10.7, nzfirst_w = 8.7, top_w = 0, maori_w = 1.3, act_w = 0.7, conserv_w = 0.4, ufuture_w = 0.4, other_w = 6.5, national_e = T, labour_e = T, green_e = F, nzfirst_e = F, top_e = F, maori_e = T, act_e = T, other_e = T, conserv_e = F, ufuture_e = F, special_votes = 384072)


shinyServer(function(input, output, session) {
   

#################
## UI elements ##
#################

    output$special_votes <- renderUI({
        numericInput("special_votes", label=NULL, value = 384072, step = 1000)
    })

    output$national_w <- renderUI({
        sliderInput("national_w", NULL, label=NULL, min = 0, max = 100, step = 0.01, value = res['National', w], ticks = F)
    })
    output$labour_w <- renderUI({
        sliderInput("labour_w", NULL, label=NULL, min = 0, max = 100, step = 0.01, value = res['Labour', w], ticks = F)
    })
    output$nzfirst_w <- renderUI({
        sliderInput("nzfirst_w", NULL, label=NULL, min = 0, max = 100, step = 0.01, value = res['NZ First', w], ticks = F)
    })
    output$green_w <- renderUI({
        sliderInput("green_w", NULL, label=NULL, min = 0, max = 100, step = 0.01, value = res['Green', w], ticks = F)
    })
    output$top_w <- renderUI({
        sliderInput("top_w", NULL, label=NULL, min = 0, max = 100, step = 0.01, value = res['TOP', w], ticks = F)
    })
    output$maori_w <- renderUI({
        sliderInput("maori_w", NULL, label=NULL, min = 0, max = 100, step = 0.01, value = res['Maori', w], ticks = F)
    })
    output$act_w <- renderUI({
        sliderInput("act_w", NULL, label=NULL, min = 0, max = 100, step = 0.01, value = res['ACT', w], ticks = F)
    })
    output$conserv_w <- renderUI({
        sliderInput("conserv_w", NULL, min = 0, max = 100, step = 0.01, value = res['Conservative', w], ticks = F)
    })
    output$ufuture_w <- renderUI({
        sliderInput("ufuture_w", NULL, min = 0, max = 100, step = 0.01, value = res['United Future', w], ticks = F)
    })
    output$other_w <- renderUI({
        sliderInput("other_w", NULL, label=NULL, min = 0, max = 100, step = 0.01, value = res['Other', w], ticks = F)
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
                                       input$maori_w, input$act_w, input$conserv_w, input$ufuture_w, input$other_w)
                           )
            r <- copy(res)
            ## d[, weights := 0]
            ## if (all(d$weights == 0)) d[, weights := 0.0001]
            r[d, w_s := i.weights, on = 'party']
            ## r[grepl('^Other', party), w_s := d[party == 'Other', weights]]
            if (!all(r$w_s == 0)) {
                r[, p_s := 100 * w_s / sum(w_s)]
                r[, v_s := input$special_votes * p_s / 100]
                r[, votes_upd := votes + v_s]
                r[, seats := allocate_seats(votes=round(r$votes_upd), parties=r$party, nseats=120, threshold=0.05,
                                            electorate=as.numeric(r$elec_seats))$seats_v]
                r[, seats_diff := seats - tot_seats]
                r[, p := 100 * votes_upd / sum(votes_upd)]
                d <- r
                return(d[, .(party, votes, tot_seats, p_s, v_s, votes_upd, seats, seats_diff, p)])
            } else {
                return(r[, .(party, votes, tot_seats, p_s = 0, v_s = 0, votes_upd = votes,
                             seats = tot_seats, seats_diff = 0, p = 100 * votes / sum(votes))])
            }
        }
    })

    ## Report back on % of party votes and the number of seats next to controls
    output$national_v <- renderText({if (!is.null(parties())) {parties()[party == 'National', format(votes, big.mark=',')]}})
    output$national_s <- renderText({if (!is.null(parties())) {parties()[party == 'National', sprintf('%0.0f', tot_seats)]}})
    output$national_ps <- renderText({if (!is.null(parties())) {parties()[party == 'National', sprintf('%0.1f %%', p_s)]}})
    output$national_vs <- renderText({if (!is.null(parties())) {parties()[party == 'National', format(round(v_s), big.mark=',')]}})
    output$national_pnew <- renderText({if (!is.null(parties())) {parties()[party == 'National', sprintf('%0.1f', p)]}})
    output$national_vnew <- renderText({if (!is.null(parties())) {parties()[party == 'National', format(round(votes_upd), big.mark=',')]}})
    output$national_snew <- renderText({if (!is.null(parties())) {parties()[party == 'National', sprintf('%i', seats)]}})
    output$national_sdiff <- renderText({if (!is.null(parties())) {parties()[party == 'National', sprintf('%s', ifelse(seats_diff==0, '', ifelse(seats_diff > 0, paste0('+', seats_diff), seats_diff)))]}})
    output$labour_v <- renderText({if (!is.null(parties())) {parties()[party == 'Labour', format(votes, big.mark=',')]}})
    output$labour_s <- renderText({if (!is.null(parties())) {parties()[party == 'Labour', sprintf('%0.0f', tot_seats)]}})
    output$labour_ps <- renderText({if (!is.null(parties())) {parties()[party == 'Labour', sprintf('%0.1f %%', p_s)]}})
    output$labour_vs <- renderText({if (!is.null(parties())) {parties()[party == 'Labour', format(round(v_s), big.mark=',')]}})
    output$labour_pnew <- renderText({if (!is.null(parties())) {parties()[party == 'Labour', sprintf('%0.1f', p)]}})
    output$labour_vnew <- renderText({if (!is.null(parties())) {parties()[party == 'Labour', format(round(votes_upd), big.mark=',')]}})
    output$labour_snew <- renderText({if (!is.null(parties())) {parties()[party == 'Labour', sprintf('%i', seats)]}})
    output$labour_sdiff <- renderText({if (!is.null(parties())) {parties()[party == 'Labour', sprintf('%s', ifelse(seats_diff==0, '', ifelse(seats_diff > 0, paste0('+', seats_diff), seats_diff)))]}})
    output$green_v <- renderText({if (!is.null(parties())) {parties()[party == 'Green', format(votes, big.mark=',')]}})
    output$green_s <- renderText({if (!is.null(parties())) {parties()[party == 'Green', sprintf('%0.0f', tot_seats)]}})
    output$green_ps <- renderText({if (!is.null(parties())) {parties()[party == 'Green', sprintf('%0.1f %%', p_s)]}})
    output$green_vs <- renderText({if (!is.null(parties())) {parties()[party == 'Green', format(round(v_s), big.mark=',')]}})
    output$green_pnew <- renderText({if (!is.null(parties())) {parties()[party == 'Green', sprintf('%0.1f', p)]}})
    output$green_vnew <- renderText({if (!is.null(parties())) {parties()[party == 'Green', format(round(votes_upd), big.mark=',')]}})
    output$green_snew <- renderText({if (!is.null(parties())) {parties()[party == 'Green', sprintf('%i', seats)]}})
    output$green_sdiff <- renderText({if (!is.null(parties())) {parties()[party == 'Green', sprintf('%s', ifelse(seats_diff==0, '', ifelse(seats_diff > 0, paste0('+', seats_diff), seats_diff)))]}})
    output$nzfirst_v <- renderText({if (!is.null(parties())) {parties()[party == 'NZ First', format(votes, big.mark=',')]}})
    output$nzfirst_s <- renderText({if (!is.null(parties())) {parties()[party == 'NZ First', sprintf('%0.0f', tot_seats)]}})
    output$nzfirst_ps <- renderText({if (!is.null(parties())) {parties()[party == 'NZ First', sprintf('%0.1f %%', p_s)]}})
    output$nzfirst_vs <- renderText({if (!is.null(parties())) {parties()[party == 'NZ First', format(round(v_s), big.mark=',')]}})
    output$nzfirst_pnew <- renderText({if (!is.null(parties())) {parties()[party == 'NZ First', sprintf('%0.1f', p)]}})
    output$nzfirst_vnew <- renderText({if (!is.null(parties())) {parties()[party == 'NZ First', format(round(votes_upd), big.mark=',')]}})
    output$nzfirst_snew <- renderText({if (!is.null(parties())) {parties()[party == 'NZ First', sprintf('%i', seats)]}})
    output$nzfirst_sdiff <- renderText({if (!is.null(parties())) {parties()[party == 'NZ First', sprintf('%s', ifelse(seats_diff==0, '', ifelse(seats_diff > 0, paste0('+', seats_diff), seats_diff)))]}})
    output$top_v <- renderText({if (!is.null(parties())) {parties()[party == 'TOP', format(votes, big.mark=',')]}})
    output$top_s <- renderText({if (!is.null(parties())) {parties()[party == 'TOP', sprintf('%0.0f', tot_seats)]}})
    output$top_ps <- renderText({if (!is.null(parties())) {parties()[party == 'TOP', sprintf('%0.1f %%', p_s)]}})
    output$top_vs <- renderText({if (!is.null(parties())) {parties()[party == 'TOP', format(round(v_s), big.mark=',')]}})
    output$top_pnew <- renderText({if (!is.null(parties())) {parties()[party == 'TOP', sprintf('%0.1f', p)]}})
    output$top_vnew <- renderText({if (!is.null(parties())) {parties()[party == 'TOP', format(round(votes_upd), big.mark=',')]}})
    output$top_snew <- renderText({if (!is.null(parties())) {parties()[party == 'TOP', sprintf('%i', seats)]}})
    output$top_sdiff <- renderText({if (!is.null(parties())) {parties()[party == 'TOP', sprintf('%s', ifelse(seats_diff==0, '', ifelse(seats_diff > 0, paste0('+', seats_diff), seats_diff)))]}})
    output$maori_v <- renderText({if (!is.null(parties())) {parties()[party == 'Maori', format(votes, big.mark=',')]}})
    output$maori_s <- renderText({if (!is.null(parties())) {parties()[party == 'Maori', sprintf('%0.0f', tot_seats)]}})
    output$maori_ps <- renderText({if (!is.null(parties())) {parties()[party == 'Maori', sprintf('%0.1f %%', p_s)]}})
    output$maori_vs <- renderText({if (!is.null(parties())) {parties()[party == 'Maori', format(round(v_s), big.mark=',')]}})
    output$maori_pnew <- renderText({if (!is.null(parties())) {parties()[party == 'Maori', sprintf('%0.1f', p)]}})
    output$maori_vnew <- renderText({if (!is.null(parties())) {parties()[party == 'Maori', format(round(votes_upd), big.mark=',')]}})
    output$maori_snew <- renderText({if (!is.null(parties())) {parties()[party == 'Maori', sprintf('%i', seats)]}})
    output$maori_sdiff <- renderText({if (!is.null(parties())) {parties()[party == 'Maori', sprintf('%s', ifelse(seats_diff==0, '', ifelse(seats_diff > 0, paste0('+', seats_diff), seats_diff)))]}})
    output$act_v <- renderText({if (!is.null(parties())) {parties()[party == 'ACT', format(votes, big.mark=',')]}})
    output$act_s <- renderText({if (!is.null(parties())) {parties()[party == 'ACT', sprintf('%0.0f', tot_seats)]}})
    output$act_ps <- renderText({if (!is.null(parties())) {parties()[party == 'ACT', sprintf('%0.1f %%', p_s)]}})
    output$act_vs <- renderText({if (!is.null(parties())) {parties()[party == 'ACT', format(round(v_s), big.mark=',')]}})
    output$act_pnew <- renderText({if (!is.null(parties())) {parties()[party == 'ACT', sprintf('%0.1f', p)]}})
    output$act_vnew <- renderText({if (!is.null(parties())) {parties()[party == 'ACT', format(round(votes_upd), big.mark=',')]}})
    output$act_snew <- renderText({if (!is.null(parties())) {parties()[party == 'ACT', sprintf('%i', seats)]}})
    output$act_sdiff <- renderText({if (!is.null(parties())) {parties()[party == 'ACT', sprintf('%s', ifelse(seats_diff==0, '', ifelse(seats_diff > 0, paste0('+', seats_diff), seats_diff)))]}})
    output$conserv_v <- renderText({if (!is.null(parties())) {parties()[party == 'Conservative', format(votes, big.mark=',')]}})
    output$conserv_s <- renderText({if (!is.null(parties())) {parties()[party == 'Conservative', sprintf('%0.0f', tot_seats)]}})
    output$conserv_ps <- renderText({if (!is.null(parties())) {parties()[party == 'Conservative', sprintf('%0.1f %%', p_s)]}})
    output$conserv_vs <- renderText({if (!is.null(parties())) {parties()[party == 'Conservative', format(round(v_s), big.mark=',')]}})
    output$conserv_pnew <- renderText({if (!is.null(parties())) {parties()[party == 'Conservative', sprintf('%0.0f', p)]}})
    output$conserv_vnew <- renderText({if (!is.null(parties())) {parties()[party == 'Conservative', format(round(votes_upd), big.mark=',')]}})
    output$conserv_snew <- renderText({if (!is.null(parties())) {parties()[party == 'Conservative', sprintf('%i', seats)]}})
    output$conserv_sdiff <- renderText({if (!is.null(parties())) {parties()[party == 'Conservative', sprintf('%s', ifelse(seats_diff==0, '', ifelse(seats_diff > 0, paste0('+', seats_diff), seats_diff)))]}})
    output$ufuture_v <- renderText({if (!is.null(parties())) {parties()[party == 'United Future', format(votes, big.mark=',')]}})
    output$ufuture_s <- renderText({if (!is.null(parties())) {parties()[party == 'United Future', sprintf('%0.0f', tot_seats)]}})
    output$ufuture_ps <- renderText({if (!is.null(parties())) {parties()[party == 'United Future', sprintf('%0.1f %%', p_s)]}})
    output$ufuture_vs <- renderText({if (!is.null(parties())) {parties()[party == 'United Future', format(round(v_s), big.mark=',')]}})
    output$ufuture_pnew <- renderText({if (!is.null(parties())) {parties()[party == 'United Future', sprintf('%0.0f', p)]}})
    output$ufuture_vnew <- renderText({if (!is.null(parties())) {parties()[party == 'United Future', format(round(votes_upd), big.mark=',')]}})
    output$ufuture_snew <- renderText({if (!is.null(parties())) {parties()[party == 'United Future', sprintf('%i', seats)]}})
    output$ufuture_sdiff <- renderText({if (!is.null(parties())) {parties()[party == 'United Future', sprintf('%s', ifelse(seats_diff==0, '', ifelse(seats_diff > 0, paste0('+', seats_diff), seats_diff)))]}})
    output$other_v <- renderText({if (!is.null(parties())) {parties()[party == 'Other', format(votes, big.mark=',')]}})
    output$other_s <- renderText({if (!is.null(parties())) {parties()[party == 'Other', sprintf('%0.0f', tot_seats)]}})
    output$other_ps <- renderText({if (!is.null(parties())) {parties()[party == 'Other', sprintf('%0.1f %%', p_s)]}})
    output$other_vs <- renderText({if (!is.null(parties())) {parties()[party == 'Other', format(round(v_s), big.mark=',')]}})
    output$other_pnew <- renderText({if (!is.null(parties())) {parties()[party == 'Other', sprintf('%0.1f', p)]}})
    output$other_vnew <- renderText({if (!is.null(parties())) {parties()[party == 'Other', format(round(votes_upd), big.mark=',')]}})
    output$other_snew <- renderText({if (!is.null(parties())) {parties()[party == 'Other', sprintf('%i', seats)]}})
    output$other_sdiff <- renderText({if (!is.null(parties())) {parties()[party == 'Other', sprintf('%s', ifelse(seats_diff==0, '', ifelse(seats_diff > 0, paste0('+', seats_diff), seats_diff)))]}})


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
            ord <- d[order(votes_upd), party]
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
            ord <- d[order(votes_upd), party]
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
    ##     parties()
    ## })
    
})

