# Selenium
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
from tabula.io import read_pdf


def requests_get_item(url, item):
    headers = {
        "User-Agent": "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/70.0.3538.77 Safari/537.36"}
    if item == "html":
        page = requests.get(url, headers=headers)
        soup = BeautifulSoup(page.text, 'html.parser')
        return(soup)

    elif item == "pdf":
        page = requests.get(url, headers=headers, stream=True)
        return(page)
    else:
        print("Valid Item Not Selected")


def download_pdfs(ls_pdf_urls, download_path, file_name):
    counter = 0
    for pdf_url in ls_pdf_urls:
        counter = counter + 1
        g = requests_get_item(pdf_url, "pdf")
        with open(f'{download_path}{file_name}_{counter}.pdf', 'wb') as sav:
            for chunk in g.iter_content(chunk_size=1000000):
                sav.write(chunk)
        print(f"download number: {counter}")


def convert_pdf_to_csv(pdf_directory, csv_directory):
    directory = fr'{pdf_directory}'
    directory_output = fr'{csv_directory}'
    count = 0
    for file in os.listdir(directory):
        print(f'{directory}{file}')
        if file.endswith(".pdf"):
            count = count + 1
            tabula.convert_into(f'{directory}{file}', f'{directory_output}{count}.csv',
                                output_format="csv", pages='all', lattice=True)


url = "https://police.gmu.edu/central-records/crime-log-test/"


def scrape_gmu(url):
    # Load html
    soup = requests_get_item(url, "html")

    # Isolate part of page where links are
    middle_page = soup.find("div", {"id": "content-right"})
    urls = middle_page.find_all("a")

    # Append only .pdf links to array
    ls_pdf_links = []
    for url in urls:
        if (".pdf" in url.get('href', [])):
            ls_pdf_links.append(url["href"])

    # Download pdfs from array
    download_pdfs(ls_pdf_links, "../data/handmade/gmu/pdfs2/", "gmu_scraped")

    # Convert downloaded pdfs to csvs
    convert_pdf_to_csv("../data/handmade/gmu/pdfs2/",
                       "../data/handmade/gmu/csvs2/")


scrape_gmu(url)
