"""
Location finder functions
=====
Location finder functions is a module for locating location mentions in text.
"""

# Dependencies

#import geopandas
import geopy
import folium
import spacy
import pandas as pd
import numpy as np
#from geopy.geocoders import Nominatim
#from geopy.geocoders import Photon
#geopy.geocoders.options.default_user_agent = "myGeocoder2"



def vector_of_coords(series):
    locator = geopy.Nominatim(user_agent="myGeocoder")
    out = set(range(2))
    for i in (range(len(series)-1)):
        out[i] = locator.geocode(series[i])
    return(out)

def loc_to_coords(loc):
    
    #geolocator = Nominatim(user_agent="myGeocoder2")
    
    geolocator = geopy.Photon(user_agent="measurements")
    location = geolocator.geocode(query = loc)
    return(location)
    




def text_to_frame(text):
    locator = geopy.Nominatim(user_agent="myGeocoder")

    nlp = spacy.load('en_core_web_sm')
    doc = nlp(text)
    
    output_array = []
    for ent in range(len(doc.ents)):
        if doc.ents[ent].label_ == "GPE":
            location = locator.geocode(doc.ents[ent].text)
            output_array.append((doc.ents[ent].text, location.latitude, location.longitude))
    return output_array

    
    

def create_species_range_map(point_dataframe):
    """This method take the array from text_to_frame and
     2 column dataframe with latitude and longitude data and creates a
    map encompassing all points, centered on the centroid of all points."""
    
    centroid = (sum(point_dataframe["X"]) / point_dataframe.shape[0],
    sum(point_dataframe["Y"]) / point_dataframe.shape[0])

    centered_map = folium.Map(
        location = [centroid[0],centroid[1]],
        tiles = 'cartodbpositron',
        zoom_start = 12,
    )

    for r in range(point_dataframe.shape[0]):
        folium.Marker(location = [point_dataframe["X"][r], point_dataframe["Y"][r]], tooltip = point_dataframe["LOCATION"][r]).add_to(centered_map)


    
    centered_map.save("./map_test.html")
