
import geopandas as gpd
import rasterio
from rasterio.mask import mask
# from rasterio.plot import show
import json
import pandas as pd
import os
import re
import sqlite3
import variables_pheno


def getFeatures(gdf):
    """Function to parse features from GeoDataFrame in such a manner
    that rasterio wants them"""
    to_json = json.loads(gdf.to_json())
    return [[to_json['features'][n]['geometry']] for n in range(len(gdf))]


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


tif_info = extract_tif_info(variables_pheno.RU_DIR)

lander = gpd.read_file(variables_pheno.SHP_FILE)
# reproject with the first raster
with rasterio.open(tif_info["dir"][0]) as src:
    lander = lander.to_crs(crs=src.crs.data)
#geojsons = [getFeatures(lander, i) for i in range(len(lander))]
geojsons = getFeatures(lander)
try:
    os.remove(variables_pheno.DB_DIR)
except FileNotFoundError:
    pass

conn = sqlite3.connect(variables_pheno.DB_DIR)

IDs = list(lander[variables_pheno.SHP_ID])
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
