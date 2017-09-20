library(data.table, quietly = T)
library(stringr, quietly = T)

years <- c(2014, 2011, 2008, 2005, 2002, 1999, 1996)

## year=2014;i=1
electoralvotes <- rbindlist(lapply(years, function(year) {
    cat(year, '\n')
    rbindlist(lapply(1:75, function(i) {
        cat('\t', i)
        long <- NULL
        try({
            download.file(sprintf('http://archive.electionresults.govt.nz/electionresults_%i/e9/csv/e9_part8_cand_%i.csv',
                                  year, i),
                          'electoral-votes.txt', quiet=T)
            txt <- readLines('electoral-votes.txt', encoding='UTF-8')
            electorate <- read.table(text = txt[2], header=F, stringsAsFactors=F, encoding='UTF-8')[1,1]
            cat('-', electorate, '\n')
            parties <- fread(paste(txt[str_count(txt, ',')==4], collapse='\n'), encoding='UTF-8')
            setnames(parties, c('candidate', 'party', 'votes', 'perc'))
            
            txt <- txt[str_count(txt, ',') >= as.numeric(names(tail(sort(table(str_count(txt, ','))), 1)))-1]
            if (grepl('valid', tail(txt,1), ignore.case=T)) {
                txt <- txt[-length(txt)]
            }
            csv <- fread(paste(txt, collapse='\n'), encoding='UTF-8')
            csv <- tail(csv, 10)
            csv <- csv[grepl('votes|total', V2, ignore.case=T)]
            csv <- cbind(year, electorate, csv[, -1])
            setnames(csv, 'V2', 'category')
            long <- melt(csv, id.vars = c('year', 'electorate', 'category'), variable.name = 'candidate', value.name = 'votes')
            long[parties, party := i.party, on = 'candidate']
            return(long)
        }, TRUE)
        return(long)
    }))
}))

saveRDS(electoralvotes, 'electoral-votes.rds')

