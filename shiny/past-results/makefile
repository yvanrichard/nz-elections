all:
	Rscript -e "shiny::runApp('.', port=8000, launch.browser=F)"

huia:
	Rscript -e "shiny::runApp('.', port=5403, launch.browser=F, host='192.168.0.136')"

getdata:
	cp ../../output/party-votes_ordinary-special.csv ../../output/electoral-votes_ordinary-special.csv ../../output/party-votes_long.csv data/

deploy:
	Rscript -e "rsconnect::deployApp('.', appName='nz_past_elections', appTitle='NZ past elections')"
