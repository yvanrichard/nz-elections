# New Zealand election results

Results of past elections from electionresults.govt.nz and government
simulator. 

THIS IS A WORK IN PROGRESS.


## Data

Past election results are fetched from electionresults.govt.nz,
groomed, and formatted in a usable way.

The created files can be found in the output/ folder.

So far:

- *party-votes_ordinary-special.csv*: Results of party vote since
2002, showing the number of votes by year, electorate, and party, with
the total number of votes, the number of ordinary votes and the number
of special votes.

- *electoral-votes_ordinary-special.csv*: Results of electoral vote
since 2002, showing the number of votes by year, electorate, candidate
and party, with the total number of votes, the number of ordinary
votes and the number of special votes.


## Preliminary exploration

Characterisation of special votes and what to expect from the results
on election day (they generally do not include special votes).

PDF: analysis/special-votes/special-votes.pdf


## NZ government simulator

A shiny app is in the /shiny/nz_gov_sim folder to explore how election results
affect the number of seats in the New Zealand government.

A version of it is [available online](https://dragonfly-science.shinyapps.io/nz_gov_sim)


## Impact of special votes

Another shiny app is in the /shiny/nz_special_votes folder to explore
how the number of seats in the New Zealand government may change with
the inclusion of special votes given the current results of the 2017 elections.

A version of it is [available online](https://dragonfly-science.shinyapps.io/nz-special-votes)


## Past results exploration

Another shiny app is in the /shiny/past_results folder to explore in
detail the past election results in New Zealand.

A version of it is [available online](https://dragonfly-science.shinyapps.io/nz_past_elections)


## Dependencies

1- To prepare the data:

- [R](https://www.r-project.org/), with additional packages:
- [data.table](http://cran.stat.auckland.ac.nz/web/packages/data.table/index.html)
- [lubridate](http://cran.stat.auckland.ac.nz/web/packages/lubridate/index.html)
- [stringr](http://cran.stat.auckland.ac.nz/web/packages/stringr/index.html)


2- For the preliminary exploration:

- [R](https://www.r-project.org/)
- [data.table](http://cran.stat.auckland.ac.nz/web/packages/data.table/index.html)
- [ggplot2](http://cran.stat.auckland.ac.nz/web/packages/ggplot2/index.html)
- [knitr](http://cran.stat.auckland.ac.nz/web/packages/knitr/index.html)
- [nzelect](http://cran.stat.auckland.ac.nz/web/packages/nzelect/index.html)
- [pander](http://cran.stat.auckland.ac.nz/web/packages/pander/index.html)


To prepare the data then create the PDF report at once, you need [GNU
make](https://www.gnu.org/software/make/), and then just run `make` at
the root of the project.


3- For the shiny app:

- [R](https://www.r-project.org/)
- [data.table](http://cran.stat.auckland.ac.nz/web/packages/data.table/index.html)
- [nzelect](http://cran.stat.auckland.ac.nz/web/packages/nzelect/index.html)
- [shiny](http://cran.stat.auckland.ac.nz/web/packages/shiny/index.html)
- [shinydashboard](http://cran.stat.auckland.ac.nz/web/packages/shinydashboard/index.html)
- [plotly](http://cran.stat.auckland.ac.nz/web/packages/plotly/index.html)


