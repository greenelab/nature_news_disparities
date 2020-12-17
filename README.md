# Analysis Pipeline for Nature News Articles

Note: This is currently a work in progress -- many things will change and may also be undocumented at this moment.

The code is made of 2 parts currently: 
1. Scrape Nature News Articles
2. Analyze the text of the articles to identify possible speaker biases

To download neccessary software and reference data, you must run [setup.sh](https://github.com/nrosed/nature_news_disparities/blob/main/nature_news_scraper/setup.sh)

## Scraping
  
The code relies upon [scrapy](https://docs.scrapy.org/en/latest/index.html) to crawl links and process the articles.
This code is found in [here](https://github.com/nrosed/nature_news_disparities/tree/main/nature_news_scraper), and most of which is automatically generated.
To run the scraper tool, you run the shell script found here: [run_scrape_gold_standard.sh](https://github.com/nrosed/nature_news_disparities/blob/main/ature_news_scraper/run_scrape_gold_standard.sh).
This runs an initial scrape to identify all articles in 2020, 2015, and 2010, then randomly chooses 10 from each year to write to a file. 
This will later be extended.
  
## Analysis
  
Currently, we look at the proportion of MALE:FEMALE quoted speakers in each article and which countries are mentioned in each article.
All analysis functions and markdown code that utilizes the functions can be found in [here](https://github.com/nrosed/nature_news_disparities/tree/main/analyze_benchmark_data/).

