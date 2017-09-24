library(data.table)

partyvotes <- readRDS('party-votes.rds')
partyvotes[, electorate_id := as.numeric(sub('.* ([0-9]+)$', '\\1', electorate))]
partyvotes[, electorate := sub(' [0-9]+$', '', electorate)]

## print(partyvotes[, .N, electorate][N < 630], 50)



## ** Simplify spelling
simplify_spelling <- function(x) {
    x <- gsub('ō', 'o', 
             gsub('ā', 'a', 
                  gsub('Ō', 'O', 
                       gsub('ī', 'i', 
                            gsub('ū', 'u',
                                 gsub('\xe2', 'a',
                                      gsub('\xe9', 'e', x)))))))
    return(x)
}
partyvotes[, electorate := simplify_spelling(electorate)]
partyvotes[, party := simplify_spelling(party)]

## ** Merge parties with different names
partyvotes[, party := gsub('New Zealand', 'NZ', party)]
partyvotes[party %in% c('NZ First', 'NZ First Party'), party := 'NZ First']
partyvotes[party %in% c('ACT NZ', 'ACT'), party := 'ACT']
partyvotes[party %in% c('Conservative Party', 'Conservative'), party := 'Conservative']
partyvotes[party %in% c('United Future', 'United Future NZ'), party := 'United Future']

## partyvotes[, .(votes = sum(votes)), party][order(-votes)]

## ** Add total of ordinary votes on polling day
partyvotes <- rbind(partyvotes,
                   partyvotes[, .(category = 'ordinary on polling day',
                                  votes = .SD[grepl('total', category, ignore.case=T), sum(votes)] -
                                      .SD[!grepl('total', category, ignore.case=T), sum(votes)]),
                              .(year, electorate, electorate_id, party)])

## partyvotes[year == 2011 & party == 'ACT New Zealand' & electorate == 'Auckland Central']

## ** Identify ordinary/special, advanced/on-day votes
partyvotes[, ordinary_special := ifelse(grepl('ordinary|^voting places|^polling places', category, ignore.case=T),
                                        'ordinary',
                                 ifelse(grepl('special|hospital|^votes allowed', category, ignore.case=T), 'special',
                                 ifelse(grepl('total', category, ignore.case=T), 'all', NA_character_)))]
partyvotes[, before_onday := ifelse(grepl('before polling', category, ignore.case=T), 'advanced',
                             ifelse(grepl('on polling', category, ignore.case=T), 'normal',
                             ifelse(grepl('total', category, ignore.case=T), 'all', NA_character_)))]
partyvotes[, local_overseas := ifelse(grepl('overseas', category, ignore.case=T), 'overseas', 'local')]
partyvotes[grepl('less than|party only|hospital', category, ignore.case=T), before_onday := 'normal']
partyvotes[grepl('overseas', category, ignore.case=T), before_onday := 'advanced']

partyvotes[grepl('total$', category, ignore.case=T), category := 'total']

## ** Remove extra totals from parties
partyvotes <- partyvotes[!grepl('party votes', party, ignore.case=T)]

## ** Check that all categories are assigned
if (sum(unlist(lapply(partyvotes, function(x) sum(is.na(x)))))>0)
    stop('Some NAs in data')

## ** Details
longform <- partyvotes[category != 'total',
                      .(votes = sum(votes)),
                      .(year, electorate, party, ordinary_special, before_onday, local_overseas)]

## ** Check that all categories have been assigned ordinary/special category
stopifnot(partyvotes[, !any(is.na(ordinary_special))])

## ** Totals special VS ordinary
partyvotes[ordinary_special %in% c('ordinary', 'special'),
           .(votes = sum(votes)),
           .(year, ordinary_special)]

special_votes <- partyvotes[, .(votes = sum(votes)), .(year, electorate, party, ordinary_special)]
special_votes <- dcast(special_votes, year + electorate + party ~ ordinary_special, value.var = 'votes')
fwrite(special_votes,
       'party-votes_ordinary-special.csv')

fwrite(longform,
       'party-votes_long.csv')

## ## ** Add missing combinations
## special_votes <- special_votes[special_votes[, CJ(year = unique(year), electorate=unique(electorate), party=unique(party))],
##                               on = c('year', 'electorate', 'party')]
## for (v in c('all', 'ordinary', 'special'))
##     special_votes[is.na(get(v)), eval(v) := 0L]




