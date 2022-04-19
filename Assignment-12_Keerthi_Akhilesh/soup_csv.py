#Assignment 12 - Web Scrapping using Python

#Akhilesh Keerthi
#GID - G01353729

import csv
import bs4 as bs
import urllib.request

"""1. Parse/Scrap the given URL and organize each book information on the webpage in 
following columns and save the organized information in csv file as an output """

aitbooks_repo = 'https://repo.vse.gmu.edu/ait/AIT580/580books.html'
repo_pagesource = urllib.request.urlopen(aitbooks_repo).read()
books_content = bs.BeautifulSoup(repo_pagesource, 'lxml')

table_of_books = books_content.table
#print(table_of_books)

with open('ait_listings.csv', 'w') as racks:
   stacker = csv.writer(racks)
   for tr in table_of_books.find_all('tr'):
       each_row = []
       for td in tr.find_all('td'):
           # print(td.get_text())
           what = td.get_text()
           if td.a != None:
               a_tag = td.a
               """
               #I have displayed the links for user-readability purpose only
               """
               what = what + ' --> ' + a_tag.attrs["href"]
           # print(td.find_all('a'))
           each_row.append(what)
       stacker.writerow(each_row)
   racks.close()


"""2.a Print the frequency count for publishers (that is, how many books for each publisher)"""

import xlrd
import pandas as pd
import matplotlib.pyplot as plt

df = pd.read_csv('ait_listings.csv')
howmany_publishers = df['Publisher\r\n'].to_frame()
print(howmany_publishers.value_counts())
print('\n')

"""2b. Visualize the output of (a) using a chart of your own choice"""
try:
    howmany_publishers.value_counts().plot(kind='barh')
    plt.show()
except(Exception):
    pass

"""2.c Print the frequency count for year of publication"""

dates = []
for date in df['PubDate\r\n'].dropna():
    date = '20' + date[-2:]
    dates.append(date)
data = pd.Series(dates, name = 'PubDate')
howmany_years = data.to_frame()
print(howmany_years.value_counts())

"""2.d Visualize the output of (b) using a chart of your own choice"""
try:
    howmany_years.value_counts().plot(kind='line')
    plt.show()
except(Exception):
    pass







