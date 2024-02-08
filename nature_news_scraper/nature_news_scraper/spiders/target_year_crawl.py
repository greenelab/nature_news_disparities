
from collections import defaultdict
from random import sample, seed
import scrapy
from datetime import datetime
from nature_news_scraper.spiders import article_crawl

class NewsSpider(scrapy.Spider):
    name = "target_year_crawl"

    def start_requests(self):

        year = int(self.target_year)
        type_article = self.target_type
 
        url = 'https://www.nature.com/nature/articles?type=%s&year=%d' % (type_article, year)

        yield scrapy.Request(url=url, callback=self.parse_article_list, cb_kwargs=dict(year=year), dont_filter=True)

    def parse_article_list(self, response, year=None):
        # produce a bunch of scrape tasks for each article on the page
        # and finally produce a task for the next page, if it exists

        # here (and all other places) we are using attribute selectors in CSS 
        articles = response.css('article[itemtype="http://schema.org/ScholarlyArticle"]')

        for article in articles:
            link = article.css('a[itemprop="url"]::attr(href)').get()

            if link is not None:
                full_link = response.urljoin(link)
                print("Article: %s" % full_link)
                yield scrapy.Request(url=full_link, callback=self.parse_article, cb_kwargs=dict(year=year), dont_filter=True)


        # also see if there's a next page and yield that, too
        next_page = response.css('ul.c-pagination li[data-page="next"] > a::attr(href)').get()
        if next_page is not None:
            next_page = response.urljoin(next_page)
            print("next page: %s" % next_page)
            yield scrapy.Request(next_page, callback=self.parse_article_list, cb_kwargs=dict(year=year))


    def parse_article(self, response, year=None):

        article_body = response.css('div.article__body.cleared > p::text')

        ## beyond 2015
        if not article_body:
            # div.c-article-body.main-content > p:nth-child(1)
            article_body = response.css('div.c-article-body.main-content > p *::text')


        ## 2015
        if not article_body:
            # div.c-article-body.main-content > p:nth-child(1)
            article_body = response.css('section#article-body div.main-content.content > p *::text')

        ## 2010
        if not article_body:
            article_body = response.css('div#content div.entry-content > p *::text')

        
        ## 2006
        if not article_body:
            article_body = response.css('div#content > main > article > div.c-article-body div.c-article-section__content > p *::text')


        # format the articles
        article_body = " ".join(article_body.getall())
        article_body = article_body.strip("\xa0")

        yield {
           "file_id": response.url.split("/")[-1],
           "year": year,
           "body": article_body
        }

