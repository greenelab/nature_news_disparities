#!/usr/bin/env bash

TAG_NAME="nature_news_disparities:1.0.0"
docker build --build-arg WHEN=2021-04-01 -t ${TAG_NAME} .
