import csv

poi3 = []
poi3name = {}
poi3lat = {}
poi3lon = {}

yanchi = []
yanchiname = {}
yanchilat = {}
yanchilon = {}

def main():



    with open('poi3_activitylabel.csv', 'rb') as csvfile:

        POIreader = csv.DictReader(csvfile, delimiter='\t')
        for line in POIreader:
            poi3.append(line['id'])
            
            poi3name[line['id']] = line['name']
            poi3lat[line['id']] = float(line['lat'])
            poi3lon[line['id']] = float(line['lon'])

    with open('yanchi_poi_small.csv', 'rb') as csvfile:

        POIreader = csv.DictReader(csvfile, delimiter='\t')
        for line in POIreader:
            yanchi.append(line['place_id'])
            
            yanchiname[line['place_id']] = line['name']
            yanchilat[line['place_id']] = float(line['lat'])
            yanchilon[line['place_id']] = float(line['lon'])



    for i in poi3:
	if poi3name[i] !="Double Dipper Cafe": continue 
#        print i, poi3name[i],
        B = []
        Bid = []
        lat = poi3lat[i]
        lon = poi3lon[i]
        eps = 0.001
        for j in yanchi:
            if yanchilat[j]<lat+eps and yanchilat[j]>lat-eps and yanchilon[j]>lon-eps and yanchilon[j]<lon+eps: 
                Bid.append(j)
                B.append(yanchiname[j])

#        print len(B),
#        print B
        ilower = poi3name[i].lower()
        words = ilower.split()

        maxcount = 0
        bestmatch = 'no match'

        for bid in Bid:
            b = yanchiname[bid]
#            print '&', b
            count = 0 

            for w in words:
                if w in b.lower(): count+=1

            if count > maxcount:
                maxcount = count
                bestmatch = b

                
        print "  ", bestmatch, bid, B
       
        if bestmatch == 'no match':
            print i, '\t', poi3name[i]#, '\t', bestmatch, '\t', bid

#        if bestmatch == 'no match':
#            print '      ', B


#main()
#


