# https://www.nature.com/search?order=relevance&date_range=2010-2015

from collections import defaultdict
from random import sample, seed
import scrapy
from datetime import datetime
import json
import pandas as pd

seed(1000)
CRAWL_SINCE = 2001 # if none, grabs the front page of nature news
CRAWL_UNTIL = None # if none, uses the current year

class NewsSpider(scrapy.Spider):
    name = "benchmark"

    def start_requests(self):

        #read in the file needed for subsampling
        with open(self.json_links, "r") as read_file:
            json_list = json.load(read_file)

        json_df = pd.DataFrame(json_list)

        # now subset the data by year
        json_grouped = json_df.groupby('year')
        links_sampled = json_grouped.apply(lambda x: x.sample(10))

        # article_links has a full set of articles in it now
        for index, row in links_sampled.iterrows():
            print(row['link'], index[0])
            yield scrapy.Request(url=row['link'], callback=self.parse_article, cb_kwargs=dict(year=index[0]), dont_filter=True)

    def parse_article(self, response, year=None):

        article_body = response.css('div.article__body.cleared > p::text')

        ## 2015
        if not article_body:
            article_body = response.css('section#article-body div.main-content.content > p *::text')

        ## 2010
        if not article_body:
            article_body = response.css('div#content div.entry-content > p *::text')

        
        # format the articles
        article_body = " ".join(article_body.getall())
        article_body = article_body.strip("\xa0")

        yield {
           "file_id": response.url.split("/")[-1],
           "year": year,
           "body": article_body
        }
