import pandas as pd
from geocoder import Geocoder
from shapely.geometry import Point
import geopandas

dat = pd.read_csv('NYPD_Station_Houses.csv')

valids = []
lats = []
lngs = []
cleaned_addys = []
responses = []
for index, row in dat.iterrows():
    addr = row['Address']

    gc = Geocoder(address=addr)

    valids.append(gc.valid)
    lat, lng = gc.get_lat_long()
    responses.append(gc.get_response())
    lats.append(lat)
    lngs.append(lng)
    cleaned_addys.append(gc.get_cleaned_address())

add_fr = pd.DataFrame(data={
    'Address.Valid': valids,
    'Clean.Address': cleaned_addys,
    'Lat': lats,
    'Long': lngs
})

geocoded_contacts = pd.concat([dat, add_fr], axis=1, sort=False)

geocoded_contacts.to_csv('NYPD_Station_Houses_Geo.csv', index=False)

# combine lat and lon column to a shapely Point() object
geocoded_contacts['geometry'] = geocoded_contacts.apply(lambda x: Point((float(x.Long), float(x.Lat))), axis=1)

geocoded_contacts = geopandas.GeoDataFrame(geocoded_contacts, geometry='geometry')

geocoded_contacts.to_file('NYPD_Station_Houses_Geo.shp'.format(type), driver='ESRI Shapefile')
