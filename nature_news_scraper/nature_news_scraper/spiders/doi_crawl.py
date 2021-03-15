
from collections import defaultdict
from random import sample, seed
import scrapy
import re
from datetime import datetime
from nature_news_scraper.spiders import article_crawl

class NewsSpider(scrapy.Spider):
    name = "doi_crawl"

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
        next_page = response.css('ol.pagination > li[data-page="next"] > a::attr(href)').get()
        if next_page is not None:
            next_page = response.urljoin(next_page)
            print("next page: %s" % next_page)
            yield scrapy.Request(next_page, callback=self.parse_article_list, cb_kwargs=dict(year=year))


    def parse_article(self, response, year=None):

        import re

        # get doi's that are in an anchor tag
        doi_box = response.css('#article-refrences a')
        href_doi = [x.attrib.get('href') for x in doi_box if 'doi' in x.attrib.get('href', '')]

        # get doi's that are written in text
        refbox =  response.css('#article-refrences')
        doi_re = re.compile(r"doi:10\.[.0-9]+/[^\s<>]+")
        text_doi = doi_re.findall(" ".join(refbox.getall()))

        # starting in 2011, format changed
        if year > 2010:
            # doi box in anchor tag
            doi_box = response.css('#references a')
            href_doi = [x.attrib.get('href') for x in doi_box if 'doi' in x.attrib.get('href', '')]
            # doi in text, not hyperlinked
            refbox =  response.css('#references')
            doi_re_url = [re.sub(r"http[s]?://(dx[./])?doi\.org/", "doi:", x) for x in refbox.getall()]
            doi_re = re.compile(r"doi:10\.[.0-9]+/[^\s<>]+")
            text_doi = doi_re.findall(" ".join(doi_re_url))




        # format the doi's
        all_doi = set(re.sub(r"http[s]?://(dx[./])?doi\.org/", "doi:", x) for x in href_doi + text_doi)
        all_doi = set(re.sub(r"[%][2][F]", "/", x) for x in all_doi)
        doi_str = ", ".join(all_doi)
        doi_str

        yield {
           "file_id": response.url.split("/")[-1],
           "year": year,
           "dois": doi_str
        }

