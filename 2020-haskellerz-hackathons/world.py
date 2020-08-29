#!/usr/bin/env python
import cartopy.crs as ccrs
import matplotlib.pyplot as plt
from geopy.geocoders import Nominatim
from collections import defaultdict
import sys
import yaml

with open(r'hackathons.yaml') as f:
    # The FullLoader parameter handles the conversion from YAML
    # scalar values to Python the dictionary format
    hackathons = yaml.load(f, Loader=yaml.FullLoader)['hackathons']

location_frequency = defaultdict(lambda: 0)
for hackathon in hackathons:
        location_frequency[hackathon['location']] += 1

geolocator = Nominatim(user_agent='world-hackathon-map')
longitudes = []
latitudes = []
frequencies = []
for location_name, freq in location_frequency.items():
    location = geolocator.geocode(location_name)
    print(f'Resolving {location_name}...', file=sys.stderr)
    longitudes += [location.longitude]
    latitudes += [location.latitude]
    frequencies += [freq * 20]

fig = plt.figure(figsize=(6, 4), dpi=300, frameon=False)
ax = fig.add_subplot(1, 1, 1, projection=ccrs.PlateCarree())
ax.stock_img()
ax.scatter(longitudes, latitudes, frequencies, alpha=0.7, c='#d57dbb')
fig.savefig('images/world.png')
