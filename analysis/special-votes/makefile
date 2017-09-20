all: special-votes.pdf

special-votes.pdf: special-votes.r ../../output/party-votes_ordinary-special.csv
	Rscript -e "rmarkdown::render('special-votes.r', 'pdf_document', clean=F)"
