# https://www.nature.com/search?order=relevance&date_range=2010-2015

from collections import defaultdict
from random import sample, seed
import scrapy
from datetime import datetime

CRAWL_SINCE = None # if none, grabs the front page of nature news
CRAWL_UNTIL = None # if none, uses the current year

class NewsSpider(scrapy.Spider):
    name = "precrawl"

    def start_requests(self):
 
        urls = [
            (year, 'https://www.nature.com/nature/articles?type=news&year=%d' % year)
            for year in (2020, 2015, 2010)
        ]

        for year, url in urls:
            yield scrapy.Request(url=url, callback=self.parse_article_list, cb_kwargs=dict(year=year), dont_filter=True)

    def parse_article_list(self, response, year=None):
        # produce a bunch of scrape tasks for each article on the page
        # and finally produce a task for the next page, if it exists

        # here (and all other places) we are using attribute selectors in CSS 
        articles = response.css('div[itemtype="http://schema.org/ScholarlyArticle"]')

        for article in articles:
            link = article.css('a[itemprop="url"]::attr(href)').get()

            if link is not None:
                full_link = response.urljoin(link)
                print("Article: %s" % full_link)
                yield {
                    "year": year,
                    "link": full_link
                }

        # also see if there's a next page and yield that, too
        next_page = response.css('ol.pagination > li[data-page="next"] > a::attr(href)').get()
        if next_page is not None:
            next_page = response.urljoin(next_page)
            print("next page: %s" % next_page)
            yield scrapy.Request(next_page, callback=self.parse_article_list, cb_kwargs=dict(year=year))


