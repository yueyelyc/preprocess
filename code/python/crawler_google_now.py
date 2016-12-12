# import urllib2,cookielib
#
# site= "http://www.rutgers.edu/"
# hdr = {'User-Agent': 'Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.11 (KHTML, like Gecko) Chrome/23.0.1271.64 Safari/537.11',
#        'Accept': 'text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8',
#        'Accept-Charset': 'ISO-8859-1,utf-8;q=0.7,*;q=0.3',
#        'Accept-Encoding': 'none',
#        'Accept-Language': 'en-US,en;q=0.8',
#        'Connection': 'keep-alive'}
#
# req = urllib2.Request(site, headers=hdr)
#
# try:
#     page = urllib2.urlopen(req)
# except urllib2.HTTPError, e:
#     print e.fp.read()
#
# content = page.read()
# print content


# browser.open('http://example.com/form/')
# browser.select_form(name='the_form')
# browser['field1'] = 'value'
# browser['field2'] = 'value'
# browser['field3'] = 'value'
# browser.submit()


#!/usr/bin/env python

import mechanize

br = mechanize.Browser()
br.open('http://thayton.github.io')

response = br.response()

print response.geturl() # URL of the page we just opened
print response.info()   # headers
print response.read()   # body

# select form with name
# br.select_form('searchForm')

def select_form(form):
  return form.attrs.get('id', None) == 'searchboxinput'

br.select_form(predicate=select_form)
br.form.set_all_readonly(False)
br.form['q'] = 'Chase Bank, East Broad Street, Westfield, NJ'

br.submit()