
##############
## server.r ##
##############

library(data.table)
library(nzelect)
library(shiny)
library(shinydashboard)
library(shinyjs)
library(lubridate)
library(ggplot2)
## library(ggthemes)
library(RColorBrewer)
library(DT)
## library(V8)
## library(viridis)
options(scipen = 100)
options(browser = 'google-chrome')


## source('helpers.r')

evote <- fread('data/electoral-votes_ordinary-special.csv')
pvote <- fread('data/party-votes_ordinary-special.csv')

evote14 <- evote[year == 2014,
                .(party = .SD[which.max(all), party]),
                .(year, electorate)][, .(elec_won=.N), .(year, party)]

session <- NULL
debug <- FALSE

enableBookmarking(store = "url")

input <- list(national_w = 47, labour_w = 25.1, green_w = 10.7, nzfirst_w = 8.7, top_w = 0,
             maori_w = 1.3, act_w = 0.7, other_w = 6.5,
             national_e = T, labour_e = T, green_e = F, nzfirst_e = F, top_e = F,
             maori_e = T, act_e = T, other_e = T)
             
shinyServer(function(input, output, session) {
    

#################
## UI elements ##
#################

    output$national_w <- renderUI({
        sliderInput("national_w", "", min = 0, max = 60, step = 0.1, value = 47, ticks = F)
    })
    output$national_e <- renderUI({
        checkboxInput("national_e", "", value = T)
    })
    output$labour_w <- renderUI({
        sliderInput("labour_w", "", min = 0, max = 60, step = 0.1, value = 25.1, ticks = F)
    })
    output$labour_e <- renderUI({
        checkboxInput("labour_e", "", value = T)
    })
    output$green_w <- renderUI({
        sliderInput("green_w", "", min = 0, max = 60, step = 0.1, value = 10.7, ticks = F)
    })
    output$green_e <- renderUI({
        checkboxInput("green_e", "", value = F)
    })
    output$nzfirst_w <- renderUI({
        sliderInput("nzfirst_w", "", min = 0, max = 60, step = 0.1, value = 8.7, ticks = F)
    })
    output$nzfirst_e <- renderUI({
        checkboxInput("nzfirst_e", "", value = F)
    })
    output$top_w <- renderUI({
        sliderInput("top_w", "", min = 0, max = 60, step = 0.1, value = 0, ticks = F)
    })
    output$top_e <- renderUI({
        checkboxInput("top_e", "", value = F)
    })
    output$maori_w <- renderUI({
        sliderInput("maori_w", "", min = 0, max = 60, step = 0.1, value = 1.3, ticks = F)
    })
    output$maori_e <- renderUI({
        checkboxInput("maori_e", "", value = T)
    })
    output$act_w <- renderUI({
        sliderInput("act_w", "", min = 0, max = 60, step = 0.1, value = 0.7, ticks = F)
    })
    output$act_e <- renderUI({
        checkboxInput("act_e", "", value = T)
    })
    output$other_w <- renderUI({
        sliderInput("other_w", "", min = 0, max = 60, step = 0.1, value = 6.5, ticks = F)
    })
    output$other_e <- renderUI({
        checkboxInput("other_e", "", value = F)
    })

    
##################
## Derived data ##
##################

    ## Weights of party votes
    parties <- reactive({
        if (!is.null(input$other_w)) {
        d <- data.table(party = c('National', 'Labour', 'Green', 'NZ First', 'TOP', 'Maori', 'ACT', 'Other'),
                       weights = c(input$national_w, input$labour_w, input$green_w, input$nzfirst_w, input$top_w,
                                   input$maori_w, input$act_w, input$other_w),
                       w_elec = c(input$national_e, input$labour_e, input$green_e, input$nzfirst_e, input$top_e,
                                  input$maori_e, input$act_e, input$other_e))
        d[, p := 100 * weights / sum(weights)]
        d[, e := ifelse(w_elec == T, 1, 0)]
        d[, seats := allocate_seats(d$p, d$party, electorate=d$e)$seats_v]
        return(d)
        }
    })

    output$table <- renderTable({
        d <- parties()
        return(d)
    })

    
    
###########
## Plots ##
###########

    ## output$plot <- renderPlot({
    ## })
    
    ## output$zelog <- renderPrint({
    ##     print()
    ## })

    

})

