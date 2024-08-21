from ecmwfapi import *
import xarray as xr
import rioxarray
import pandas as pd
import numpy as np
from pathlib import Path
import os
import geopandas as gpd
server = ECMWFService("mars")

DATA_DIR = Path(os.getenv("AA_DATA_DIR"))

CODAB_FP = (
    DATA_DIR / "public" / "raw" / "eth" / "cod_ab" / "eth_adm_csa_bofedb_2021_shp"
)

OUT_DIR = DATA_DIR / "private" / "raw" / "eth" / "ecmwf_seasonal" / "mars"

shapefile = gpd.read_file(CODAB_FP)

buffer_aoi = 0.1
bounding_box = shapefile.total_bounds
bounding_box_str = "/".join(
    [
        str(round(coord, 1))
        for coord in [
            bounding_box[3] + buffer_aoi,
            bounding_box[0] - buffer_aoi,
            bounding_box[1] - buffer_aoi,
            bounding_box[2] + buffer_aoi,
        ]
    ]
)



# accordinging to documentation 1981-2016 have 25 ensemble members, afterwords 51
start_year = 1981
end_year = 2023


for year in range(start_year, end_year):
    print(f"downloading {year}")
    start_date = pd.to_datetime(f"{year}-01-01")
    end_date = pd.to_datetime(f"{year}-12-01")

    # Generate a sequence of monthly dates
    date_range = pd.date_range(start=start_date, end=end_date, freq="MS")

    # Convert the date range to a list of formatted strings
    date_strings = [date.strftime("%Y-%m-%d") for date in date_range]

    # Join the list of formatted strings into a single string with "/"
    dates_use = "/".join(date_strings)

    if year <= 2016:
        number_use = "/".join([str(i) for i in range(25)])
    else:
        number_use = "/".join([str(i) for i in range(51)])

    grid_setup = "0.4/0.4"
    server.execute(
        {
            "class": "od",
            "date": dates_use,
            "expver": "1",
            "fcmonth": "1/2/3/4/5/6/7",
            "levtype": "sfc",
            "method": "1",
            "area": bounding_box_str,
            "grid": grid_setup,
            "number": number_use,
            "origin": "ecmwf",
            "param": "228.172",
            "stream": "msmm",
            "system": "5",
            "time": "00:00:00",
            "type": "fcmean",
            "target": "output",
        },
        OUT_DIR / f"ecmwf_hres_seas5_{year}.grib",
    )
