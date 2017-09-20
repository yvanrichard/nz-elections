#' ---
#' title: "Preliminary analysis of special votes"
#' author: "YR"
#' date: "2017-09-20"
#' output:
#'  pdf_document:
#'    number_sections: true
#'    fig_caption: true
#'    toc: true
#' geometry: margin = 0.8in
#' header-includes:
#' - \usepackage{placeins}
#' ---

#+ setup, echo=FALSE, results="hide", message=FALSE
suppressPackageStartupMessages({
    library(data.table)
    library(ggplot2)
    library(knitr)
    library(pander)
    library(nzelect)
})
opts_chunk$set(echo=FALSE)

partyvotes <- fread('../../output/party-votes_ordinary-special.csv')
elecvotes <- fread('../../output/electoral-votes_ordinary-special.csv')

cols <- c(Green      = '#008641',
         Labour     = '#D82C20',
         National   = '#00529E',
         'NZ First' = '#555555',
         Other      = '#999999')

partyvotes[, mainparty := factor(ifelse(party == 'National Party', 'National',
                          ifelse(party == 'Labour Party', 'Labour',
                          ifelse(party == 'Green Party', 'Green',
                          ifelse(party == 'NZ First', 'NZ First', 'Other')))),
                          levels = c('Green', 'Labour', 'National', 'NZ First', 'Other'))]


## * Proportion of special votes
#' # Proportion of special votes

summ <- partyvotes[,
           .(all = sum(all), ordinary = sum(ordinary), special = sum(special)),
           .(year, mainparty)]
setorder(summ, mainparty, year)

summ[, p_special := 100 * special / all]

max_p_special <- summ[mainparty == 'Green'][which.max(p_special)]

#' The Green party has a relatively high proportion of party votes that were cast as special votes,
#' up to `r max_p_special[, round(p_special,1)]`% in `r max_p_special[, year]`.
#' These results are not included on the night of the election day.
#'

#+ perc_special_votes, fig.width=9, fig.height=5, fig.cap="Proportion of special votes as % of party votes by main party and election year" 

g <- ggplot(summ, aes(x = year, y = p_special, fill = mainparty)) +
    geom_col() +
    geom_text(aes(label = paste0(round(p_special,1), '%')), nudge_y = 0.3, size = 2.5) +
    facet_wrap(~ mainparty, nrow=1, strip.position='bottom') +
    scale_fill_manual(values = cols, name = 'Party') +
    scale_y_continuous(labels = function(x) paste0(x, '%'), limits = c(0, 20)) +
    labs(x = NULL, y = 'Proportion of special votes (% of party votes)') +
    scale_x_continuous(breaks = sort(unique(summ$year))) +
    theme_minimal() +
    theme(legend.position = 'none',
          strip.placement= 'outside',
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          strip.text= element_text(face = 'bold', size = 13),
          axis.ticks.length=unit(c(-2, 2), 'mm'))
g


## * With or without special votes
#' # With or without special votes

summ <- partyvotes[,
           .(all = sum(all), ordinary = sum(ordinary), special = sum(special)),
           .(year, mainparty)]
summ[, all_parties_all := sum(all), year]
summ[, all_parties_ordinary := sum(ordinary), year]
summ[, all_parties_special := sum(special), year]
summ[, p_tot_all := 100 * all / all_parties_all]
summ[, p_ordinary_all := 100 * ordinary / all_parties_ordinary]
summ[, p_special_all := 100 * special / all_parties_special]

setorder(summ, mainparty, year)

d <- rbind(summ[, .(year, mainparty, type = 'Without special votes', perc_votes = p_ordinary_all)],
          summ[, .(year, mainparty, type = 'Final (with special votes)', perc_votes = p_tot_all)],
          summ[, .(year, mainparty, type = 'Only special votes', perc_votes = p_special_all)])
d[, type := factor(type, levels = c('Final (with special votes)', 'Only special votes', 'Without special votes'))]

#+ with_wo_special_votes, fig.width=9, fig.height=5, fig.cap="Proportion of special votes as % of party votes by main party and election year" 

g <- ggplot(d, aes(x = year, y = perc_votes, colour = mainparty, linetype = type)) +
    geom_line() +
    scale_colour_manual(name = 'Party',
                        values = cols) +
    scale_linetype_manual(name = 'With/without special votes',
                          values = c('Final (with special votes)' = 1,
                                     'Without special votes' = 2,
                                     'Only special votes' = 3)) +
    theme_minimal() +
    scale_x_continuous(breaks = sort(unique(d$year))) +
    labs(x = 'Election year', y = 'Percentage of all party votes') +
    scale_y_continuous(labels = function(x) paste0(x, '%'), limits = c(0, 50)) +
    theme(strip.placement= 'outside',
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          panel.grid.minor.y = element_blank(),
          strip.text= element_text(face = 'bold', size = 13),
          axis.ticks.length=unit(c(-2, 2), 'mm'))
g


## ## Code block
## #+ label1, echo=FALSE
## kable(tab)

## #+ results='asis'

## #+ results='markup'

## #+ annual_tows, fig.width=7, fig.height=4, fig.cap="Annual effort for each vessel, function of vessel length and target species" 
## ggplot()



## * Seats with or without special votes
#' # Seats with or without special votes
#'

years <- sort(unique(elecvotes$year))


seats <- rbindlist(lapply(c('all', 'ordinary', 'special'), function(t) {
    rbindlist(lapply(years, function(y) {
        elec <- elecvotes[year == y,
                         .(party = .SD[which.max(get(t)), party]),
                         .(year, electorate)][, .(elec_won=.N), .(year, party)]
        pvotes <- partyvotes[year == y][, .(votes = sum(get(t))), party][order(-votes)]
        pvotes[, perc_votes := votes / sum(votes)]
        pvotes[elec, elec_won := i.elec_won, on = 'party']
        pvotes[is.na(elec_won), elec_won := 0L]
        pvotes[, mainparty := factor(ifelse(party == 'National Party', 'National',
                                     ifelse(party == 'Labour Party', 'Labour',
                                     ifelse(party == 'Green Party', 'Green',
                                     ifelse(party == 'NZ First', 'NZ First', 'Other')))),
                                     levels = c('Green', 'Labour', 'National', 'NZ First', 'Other'))]
        pvotes[, .(votes = sum(votes), elec_won = sum(elec_won)), .(mainparty)]
        seats <- data.table(
            allocate_seats(as.numeric(pvotes$votes), parties = pvotes$party, electorate=as.numeric(pvotes$elec_won))$seats_df
        )
        seats[, list_mps := final - electorate_seats]
        seats <- seats[final > 0]
        seats[, `:=`(year = y, type = t)]
    }))
}))

seats[, mainparty := factor(ifelse(party == 'National Party', 'National',
                            ifelse(party == 'Labour Party', 'Labour',
                            ifelse(party == 'Green Party', 'Green',
                            ifelse(party == 'NZ First', 'NZ First', 'Other')))),
                            levels = c('Green', 'Labour', 'National', 'NZ First', 'Other'))]

seats <- seats[,
              .(proportionally_allocated = sum(proportionally_allocated),
                electorate_seats = sum(electorate_seats),
                final = sum(final),
                list_mps = sum(list_mps)),
              .(year, mainparty, type)]

seats[, type := c(all      = 'Final (with special votes)',
                  special  = 'Only special votes',
                  ordinary = 'Without special votes')[type]]
#+ seats_with_wo_special_votes, fig.width=9, fig.height=5, fig.cap="Number of MPs with or without special votes"

g <- ggplot(seats, aes(x = year, y = final, colour = mainparty, linetype = type)) +
    geom_line() +
    scale_colour_manual(name = 'Party',
                        values = cols) +
    scale_linetype_manual(name = 'With/without special votes',
                          values = c('Final (with special votes)' = 1,
                                     'Without special votes' = 2,
                                     'Only special votes' = 3)) +
    theme_minimal() +
    scale_x_continuous(breaks = sort(unique(seats$year))) +
    labs(x = 'Election year', y = 'Number of parliamentary seats') +
    ## scale_y_continuous(labels = function(x) paste0(x, '%'), limits = c(0, 50)) +
    theme(strip.placement= 'outside',
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          panel.grid.minor.y = element_blank(),
          strip.text= element_text(face = 'bold', size = 13),
          axis.ticks.length=unit(c(-2, 2), 'mm'))
g
