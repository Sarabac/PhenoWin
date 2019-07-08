
import geopandas as gpd
import rasterio
from rasterio.mask import mask
# from rasterio.plot import show
import json
import pandas as pd
import os
import re
import sqlite3


def getFeatures(gdf, n):
    """Function to parse features from GeoDataFrame in such a manner
    that rasterio wants them"""
    return [json.loads(gdf.to_json())['features'][n]['geometry']]


def extract_n(dat, n):
    rule = re.compile("(?:(?<!\\d)\\d{"+n+"}(?!\\d))")
    return([rule.search(x).group(0) for x in dat])


def extract_tif_info(directory):
    is_geotif = re.compile(".*.tif$")
    tif = [os.path.join(directory, x)
           for x in os.listdir(directory) if is_geotif.match(x) is not None]
    return(pd.DataFrame({
        "dir": tif,
        "Year": extract_n(tif, "4"),
        "Crop": extract_n(tif, "3"),
        "P": extract_n(tif, "1,2")
    }))


TIF_DIR = "_DOY"
SHP_ID = "ID_1"
DB_DIR = "DOY.db"

tif_info = extract_tif_info(TIF_DIR)

lander = gpd.read_file("_Zones/gem2005_BKR.shp")
# reproject with the first raster
with rasterio.open(tif_info["dir"][0]) as src:
    lander = lander.to_crs(crs=src.crs.data)
geojsons = [getFeatures(lander, i) for i in range(len(lander))]

try:
    os.remove(DB_DIR)
except FileNotFoundError:
    pass

conn = sqlite3.connect(DB_DIR)

IDs = list(lander[SHP_ID])
for inf in range(len(tif_info)):
    geotif = rasterio.open(tif_info.iloc[inf]["dir"])
    for i in range(len(IDs)):
        out_image, tran = mask(geotif, geojsons[i], filled=False)
        day = out_image.compressed().round().astype(int)
        day_weight = pd.DataFrame({"DOY": day, "weight": 1/len(day)})
        day_weight = day_weight.groupby("DOY").sum().reset_index()
        day_weight["Area"] = IDs[i]
        day_weight["Crop"] = int(tif_info.iloc[inf]["Crop"])
        day_weight["P"] = int(tif_info.iloc[inf]["P"])
        day_weight["Year"] = int(tif_info.iloc[inf]["Year"])
        if i == 0:
            weighted_pixels = day_weight
        else:
            weighted_pixels = weighted_pixels.append(day_weight)
    geotif.close()  # close the raster
    weighted_pixels.to_sql("PIXEL", conn, if_exists="append")
    print("processing: {}/{}".format(inf, len(tif_info)))

cursor = conn.cursor()
with open("views.sql") as queryfile:
    cursor.executescript(queryfile.read())
conn.commit()
conn.close()

cursor.execute("""
select (select count() from START_END b where julianday(b.Start) <= julianday(s.start)
group by s.Area,s.Crop) from START_END s
""")
cursor.fetchall()
cursor.execute("""
select sqlite3_step()  from START_END
""")
