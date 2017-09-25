library(data.table)
library(rvest)

clean_txt <- function(x) gsub(',', '', gsub('^[[:blank:]]+|[[:blank:]]+$', '', gsub('-', '', x)))

## Get web page
htmlpage <- read_html("http://electionresults.govt.nz/")

## Extract values
frames <- html_nodes(htmlpage, '#overall-results-table')
votes <- html_text(html_nodes(frames, 'td'))
votes <- data.table(matrix(votes, ncol = 6, byrow=T))
setnames(votes, c('party', 'votes', 'perc', 'elec_seats', 'list_seats', 'tot_seats'))

votes[, party := clean_txt(party)]
for (v in names(votes)[2:ncol(votes)]) {
    votes[, eval(v) := as.numeric(clean_txt(get(v)))]
    votes[is.na(get(v)), eval(v) := 0]
}


votes <- votes[party != 'Total']

fwrite(votes, 'results_ordinary_2017.csv')

