library(data.table)

txt_clean <- function(x) gsub('[[:blank:]]{2,}', ' ', gsub('^[[:blank:]]+|[[:blank:]]+$', '', x))


electoralvotes <- readRDS('electoral-votes.rds')
electoralvotes[, electorate_id := as.numeric(sub('.* ([0-9]+)$', '\\1', electorate))]
electoralvotes[, electorate := sub(' [0-9]+$', '', electorate)]


## print(electoralvotes[, .N, electorate][N < 630], 50)

electoralvotes[, `:=`(electorate = txt_clean(electorate),
                      party      = txt_clean(party))]
levels(electoralvotes$candidate) <- txt_clean(levels(electoralvotes$candidate))



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
electoralvotes[, electorate := simplify_spelling(electorate)]
levels(electoralvotes$candidate) <-  simplify_spelling(levels(electoralvotes$candidate))
electoralvotes[, party := simplify_spelling(party)]


## ** Merge parties with different names
electoralvotes[, party := gsub('New Zealand', 'NZ', party)]
electoralvotes[party %in% c('NZ First', 'NZ First Party'), party := 'NZ First']
electoralvotes[party %in% c('ACT NZ', 'ACT'), party := 'ACT']
electoralvotes[party %in% c('Conservative Party', 'Conservative'), party := 'Conservative']
electoralvotes[party %in% c('United Future', 'United Future NZ'), party := 'United Future']

## electoralvotes[, .(votes = sum(votes)), party][order(-votes)]

## ** Add total of ordinary votes on polling day
electoralvotes <- rbind(electoralvotes,
                   electoralvotes[, .(category = 'ordinary on polling day',
                                  votes = .SD[grepl('total', category, ignore.case=T), sum(votes)] -
                                      .SD[!grepl('total', category, ignore.case=T), sum(votes)]),
                              .(year, electorate, electorate_id, candidate, party)])

## partyvotes[year == 2011 & party == 'ACT New Zealand' & electorate == 'Auckland Central']

## ** Identify ordinary/special, advanced/on-day votes
electoralvotes[, ordinary_special := ifelse(grepl('ordinary|^voting places|^polling places', category, ignore.case=T),
                                        'ordinary',
                                 ifelse(grepl('special|hospital|^votes allowed', category, ignore.case=T), 'special',
                                 ifelse(grepl('total', category, ignore.case=T), 'all', NA_character_)))]
electoralvotes[, before_onday := ifelse(grepl('before polling', category, ignore.case=T), 'advanced',
                             ifelse(grepl('on polling', category, ignore.case=T), 'normal',
                             ifelse(grepl('total', category, ignore.case=T), 'all', NA_character_)))]

electoralvotes[grepl('total$', category, ignore.case=T), category := 'total']

## ** Remove extra totals from parties
electoralvotes <- electoralvotes[!grepl('candidate votes', candidate, ignore.case=T)]


## ** Totals special VS ordinary
electoralvotes[ordinary_special %in% c('ordinary', 'special'),
           .(votes = sum(votes)),
           .(year, ordinary_special)]

special_votes <- electoralvotes[, .(votes = sum(votes)), .(year, electorate, candidate, party, ordinary_special)]
special_votes <- dcast(special_votes, year + electorate + party + candidate ~ ordinary_special, value.var = 'votes')

special_votes[, .(winning_party = .SD[which.max(all), party]), .(year, electorate)][, .N, .(year, winning_party)]


fwrite(special_votes,
       'electoral-votes_ordinary-special.csv')

## ## ** Add missing combinations
## special_votes <- special_votes[special_votes[, CJ(year = unique(year), electorate=unique(electorate), party=unique(party))],
##                               on = c('year', 'electorate', 'party')]
## for (v in c('all', 'ordinary', 'special'))
##     special_votes[is.na(get(v)), eval(v) := 0L]


## ** Check that all categories have been assigned ordinary/special category
stopifnot(electoralvotes[, !any(is.na(ordinary_special))])


