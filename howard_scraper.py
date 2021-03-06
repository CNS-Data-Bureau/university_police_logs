#!/usr/bin/env python
# coding: utf-8

import selenium
from selenium import webdriver
from selenium.webdriver.chrome.options import Options
from selenium.webdriver.support import expected_conditions as EC
from selenium.webdriver.support.ui import WebDriverWait
from selenium.webdriver.common.by import By
from selenium.webdriver.common.keys import Keys
# Reqeusts
import requests
# Other tools
from bs4 import BeautifulSoup
from datetime import datetime
from datetime import timedelta
from datetime import date
import csv
import pandas as pd
import time
import json
import fnmatch
import os
import tabula
# from tabula.io import read_pdf

def howard_links():

    # def howard_scraper():
    # get the linnks we want
    url = "https://publicsafety.howard.edu/about/crime-stats"
    page = requests.get(url)
    soup = BeautifulSoup(page.text, 'html.parser')
    # ls_id = soup.find(id="tab1631541161195_1")
    # ls_rows 

    item = soup.find_all('section', attrs={'class': 'hp_button_link hp_button_link--fancy_link'})
    item2 = item[2]

    final = item2.find_all("a")

    final_links = []
    var = 'https://publicsafety.howard.edu'

    for link in final:
        href = (link["href"])
        # print(href)
        if href.startswith('https://publicsafety.howard.edu/'):
            # print(href)
            final_links.append(href)
            # print(final_links)
        # else:
        #     print ('error')
    for link in final:
        href = (link["href"])
        if href.startswith('/sites/'):
            print(href)
            final_href = var + href
            final_links.append(final_href)
        # else:
        #     print ('error')
            # print(final_links)
            # print('---')
    # print(final_links)
    return final_links

            
def howard_pdfs(final_links):
    counter = 0 
    for i in final_links:
        counter = counter +1
    #     print(counter)
        g = requests.get(i, stream=True)
        with open(f'data/handmade/howard/pdfs/{counter}.pdf', 'wb') as sav:
            for chunk in g.iter_content(chunk_size=1000000):
                sav.write(chunk)
                # print('this')

def howard_convert(pdfs):
    directory = r'data/handmade/howard/pdfs/'
    directory_output = r'data/handmade/howard/csvs/'
    count = 0
    for file in os.listdir(directory):
    #     print(file)
        if file.endswith(".pdf"):
    #         print('done')
            count = count + 1
    #         print(count)
    #         print(file)
            tabula.convert_into(f'{directory}{file}', f'{directory_output}{count}.csv', output_format="csv", pages='all')
            # print('done')
# howard_links()
howard_convert(howard_pdfs(howard_links()))