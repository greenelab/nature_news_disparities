#### this file is used to run the initial "gold standard" scrapes

JSON_LINK_FILE="./links.json"
JSON_OUT_FILE="./links_crawled.json"

# get the random files by doing a precrawl of 2010, 2015, 2020
scrapy crawl precrawl -O ${JSON_LINK_FILE} && ( 
    # run the scrape on 10 random articles 
    scrapy crawl naturenews -a json_links=${JSON_LINK_FILE} -O ${JSON_OUT_FILE}
)
