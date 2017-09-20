all: party-votes_ordinary-special.csv electoral-votes_ordinary-special.csv .output

electoral-votes.rds: electoral-votes-download.r
	Rscript electoral-votes-download.r

electoral-votes_ordinary-special.csv: electoral-votes.rds electoral-votes-groom.r
	Rscript electoral-votes-groom.r

party-votes.rds: party-votes-download.r
	Rscript party-votes-download.r

party-votes_ordinary-special.csv: party-votes.rds party-votes-groom.r
	Rscript party-votes-groom.r

.output: party-votes_ordinary-special.csv electoral-votes_ordinary-special.csv
	cp party-votes_ordinary-special.csv electoral-votes_ordinary-special.csv ../output/ && touch .output
