#!/usr/bin/env python
# coding: utf-8

# In[30]:


import populartimes
import json
import csv


# In[2]:


2+2


# In[3]:


belfountain = populartimes.get_id(api_key="AIzaSyAKSzjYU6hrokqatr6HNaADbAjqGESL64I", place_id="ChIJH5i_lbQIK4gRJkH8gOJps_I")


# In[4]:


belfountain


# In[5]:


islandLake = populartimes.get_id(api_key="AIzaSyAKSzjYU6hrokqatr6HNaADbAjqGESL64I", place_id="ChIJd8UnymYAK4gRc6-GeCf43CQ")


# In[6]:


islandLake


# In[ ]:


limehouse = populartimes.get_id(api_key="AIzaSyAKSzjYU6hrokqatr6HNaADbAjqGESL64I", place_id="ChIJzSme5BhzK4gRTMBFac8Z4PU")


# In[14]:


## Saving output to json file
open('data.json', "x")
with open('data.json', 'w') as f:
    json.dump(islandLake, f)
    f.close()


# In[31]:


## Run loop to download all places
## first load sites
with open('ListOfPlaces.csv', 'r') as file:
    reader = csv.reader(file)
    for row in reader:
        print(row)


# In[34]:





# In[ ]:




