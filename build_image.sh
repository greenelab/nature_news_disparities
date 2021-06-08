#!/usr/bin/env bash

# WHEN is the tag to the R library snapshot 
TAG_NAME="nature_news_disparities:1.0.0"
docker build --build-arg WHEN=2021-04-01 -t ${TAG_NAME} .
