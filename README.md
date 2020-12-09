# nature news disparities
<h1>Analysis Pipeline for Nature News Articles<h1>

Note: This is currently a work in progress -- many things will change and may also be undocumented at this moment.

The code is made of 2 parts currently: 
1. Scrape Nature News Articles
2. Analyze the text of the articles to identify possible speaker biases

<h2>Scraping<h2>
  
The code relies upon [scrapy](https://docs.scrapy.org/en/latest/index.html) to crawl links and process the articles.
  This code is found in [here](https://github.com/nrosed/nature_news_disparities/tree/main/nature_news_scraper), and most of which is automatically generated.
  To run the scraper tool, you run the shell script found here: [run_scrape_gold_standard.sh](https://github.com/nrosed/nature_news_disparities/blob/main/nature_news_scraper/run_scrape_gold_standard.sh).
  This runs an initial scrape to identify all articles in 2020, 2015, and 2010, then randomly chooses 10 from each year to write to a file. 
  This will later be extended.
  
<h2>Analysis<h2>
  
  TBD
