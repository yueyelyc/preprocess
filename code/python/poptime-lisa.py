# produce popular time from google search

from HTMLParser import HTMLParser
import urllib2


class MyHTMLParser(HTMLParser):
    def handle_starttag(self, tag, attrs):
        if tag != 'div':
            return
        attr = dict(attrs)
        self.links.append(attr)
 
def extract(htmlname, category):

 
#    f = urllib2.urlopen(url)
#    f = open('magicfountain.html')
    f = open(htmlname)
    html = f.read()
    f.close()
 

    parser = MyHTMLParser()
    parser.links = []
    parser.feed(html)
    pophours = {}

    day = None
    for l in parser.links:
	if 'aria-label' in l and 'Histogram showing popular times' in l['aria-label']:
		day = l['aria-label'].split()[-1]
		pophours[day] = []
#		print day
		continue
	if day == None: continue

# beginning 6am, may not have any visits
	if l=={}: 
		pophours[day].append('0')
# extract number of visits
	elif 'style' in l:
		numvisits = l['style'].replace('height:', '').replace('px','').replace(';','')
		pophours[day].append(numvisits)
#		print numvisits
	if len(pophours[day]) == 18:
		day = None
		

    for day in pophours:
	print category+',', day+',', 
	for i in pophours[day]: print i+',',
    	print

print "businss type, day, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23" 
extract(r'../../output/Chase Bank - Google Maps.html','Chase Bank')
