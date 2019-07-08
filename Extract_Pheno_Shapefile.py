
import geopandas as gpd
import rasterio
from rasterio.mask import mask
# from rasterio.plot import show
import json
import pandas as pd
import os
import re
import sqlite3
from threading import Thread, Lock, Semaphore
from datetime import datetime

datetime.strptime("2018-200", "%Y-%j")


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


class Extractor(Thread):
    nb_thread = 0
    finished_thread = 0
    db_access = Lock()
    raster_access = Semaphore()

    def __init__(self, tif_info, polygons, IDs, conn):
        Thread.__init__(self)
        self.infos = tif_info
        self.polygons = polygons
        self.IDs = IDs
        self.conn = conn
        Extractor.nb_thread += 1

    def run(self):

        # try to open the geotif file
        flag = True
        while flag:
            try:
                geotif = rasterio.open(self.infos["dir"])
            except rasterio.RasterioIOError:
                # wait for another thread finishing it process
                Extractor.raster_access.acquire(True)
            else:
                # if no problem, aquiere the lock without blocking
                Extractor.raster_access.acquire(False)
                flag = False

        for i in range(len(self.IDs)):
            out_image, tran = mask(geotif, self.polygons[i], filled=False)
            day = out_image.compressed().round().astype(int)
            day_weight = pd.DataFrame({"DOY": day, "weight": 1/len(day)})
            day_weight = day_weight.groupby("DOY").sum().reset_index()
            day_weight["Area"] = self.IDs[i]
            day_weight["Crop"] = int(self.infos["Crop"])
            day_weight["P"] = int(self.infos["P"])
            day_weight["Year"] = int(self.infos["Year"])
            if i == 0:
                weighted_pixels = day_weight
            else:
                weighted_pixels = weighted_pixels.append(day_weight)
        geotif.close()  # close the raster
        Extractor.raster_access.release()
        with Extractor.db_access:
            weighted_pixels.to_sql("PIXEL", self.conn, index=False,
                                   if_exists="append")
            Extractor.finished_thread += 1
            msg = "processing: {}/{}"
            print(msg.format(Extractor.finished_thread, Extractor.nb_thread))


TIF_DIR = "_DOY"
SHP_ID = "ID_1"
DB_DIR = "DOY.db"

tif_info = extract_tif_info(TIF_DIR)

lander = gpd.read_file("Deu_admin/DEU_adm1.shp")
# reproject with the first raster
with rasterio.open(tif_info["dir"][0]) as src:
    lander = lander.to_crs(crs=src.crs.data)
geojsons = [getFeatures(lander, i) for i in range(len(lander))]

try:
    os.remove(DB_DIR)
except FileNotFoundError:
    pass

conn = sqlite3.connect(DB_DIR)
extras = [Extractor(tif_info.iloc[i], geojsons, list(lander[SHP_ID]), conn)
          for i in range(len(tif_info))]
for extra in extras:
    extra.run()

cursor = conn.cursor()
with open("views.sql") as queryfile:
    cursor.executescript(queryfile.read())
conn.commit()
conn.close()
