# coding: utf-8

# In[15]:

import pandas as pd
import requests
import time
import json


# In[2]:

cell_data_orig = pd.read_csv('utrafdd-530-01_RNC901_971_belllabs.csv')


# Remove rows where lat, lon, or radius are null

# In[3]:

cell_data = cell_data_orig[ (cell_data_orig.lon != 'null') & (cell_data_orig.lat != 'null')  & (cell_data_orig.radius != 'null')]
cell_data.loc[:, ['lat','lon']] = cell_data[['lat','lon']].apply(pd.to_numeric)


# Construct query template

# In[4]:

query_categories = """eat-drink,going-out,sights-museums,transport,accommodation,shopping,leisure-outdoor,administrative-areas-buildings,natural-geographical,petrol-station,atm-bank-exchange,toilet-rest-area,hospital-health-care-facility"""
query_params = {'lat': False,  ## latitude
                    'lon': False,  ## longitude
                    'radius': 300, ## radius in meter
                    'size': 1000,   ## number of items to retrieve. This seems to be upper-bounded by 100
                    'cat': query_categories ## which categories to query. This is the latest complete list
                   }
query_template = ''.join([
    """https://places.cit.api.here.com/places/v1/discover/explore""", 
    """?in={lat},{lon};r={radius}""", ## circle model: location and radius
    """&app_id=eQ751KMtu5uaz8z5ZH9N""", ## secret
    """&app_code=iMzWgAZK46qzcb4Uwu9kgw""", ## secret
    """&size={size}""", 
    """&tf=plain""",
    """&pretty=true""",
    """&categories={cat}"""
])


# Get POIs near cell locations

# In[13]:

data_collector = []
error_data_collector = []

for index, row in cell_data.iterrows():
    if index % 100 == 0:
        print("Processing Row {}".format(index))
    time.sleep(0.1)
    query_params['lat'] = row.lat
    query_params['lon'] = row.lon
    query = query_template.format(**query_params)
    response = requests.get(query)
    if response.status_code != 200:
        print("Error fetcting cell lat={}, lon={}".format(row.lat, row.lon))
        error_data_collector.append(row)
        continue
    data_collector.append({'query_params': query_params,
                           'json': response.json()})
   


# Save the poi-cell pair data

# In[18]:

with open("poi_cell_data.json", 'w') as outfile:
    json.dump(data_collector, outfile, indent=4)


# In[ ]:



