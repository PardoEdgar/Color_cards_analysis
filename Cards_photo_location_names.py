import Metashape
import numpy as np
import pandas as pd
import os
import re
import random
import tkinter as tk
import tifffile
from pathlib import Path

doc = Metashape.app.document
chunks = doc.chunks


def extract_images_name():
    Photos_names = []
    for chunk in chunks:
        split = chunk.label.split("_")
        site_name, Month, Transect = split[0], split[2], split[4]
        markers = chunk.markers
        for marker in markers:
            if re.fullmatch(r"G\d", marker.label) is not None:
                for cam, proj in marker.projections.items():
                    if not cam.enabled:
                        continue
                    cam_path = Path(cam.photo.path)
                    Photos_names.append(
                        {
                            "Month": Month,
                            "Card_ID": marker.group.label,
                            "site_name": site_name,
                            "Transect": Transect,
                            "names": cam.label,
                            "marker": marker.label,
                            "X_location_pixel": proj.coord.x,
                            "Y_location_pixel": proj.coord.y,
                            "cam_path": cam_path,
                        }
                    )

    return pd.DataFrame(Photos_names), site_name, Transect


def pixel_extraction(Photos_names):
    New_pixels = []
    for _, row in Photos_names.iterrows():
        path = Path(row["cam_path"])
        path = path.parent / Path("GW_world_img") / f"{path.stem}.TIFF"
        Grey_world_img = tifffile.imread(str(path))
        arr = np.array(Grey_world_img)
        center_X = row["X_location_pixel"]
        center_Y = row["Y_location_pixel"]
        for _ in range(25):
            dx = np.random.randint(-25, 26)
            dy = np.random.randint(-25, 26)
            nx = round(center_X) + dx
            ny = round(center_Y) + dy
            R, G, B = arr[ny, nx, 0], arr[ny, nx, 1], arr[ny, nx, 2]
            New_pixels.append(
                {
                    **row.to_dict(),
                    "X_ran_location_pixel": nx,
                    "y_ran_location_pixel": ny,
                    "R": float(R),
                    "G": float(G),
                    "B": float(B),
                }
            )

    return pd.DataFrame(New_pixels)


def create_data(New_pixels, site_name, Transect):
    output_directory = Metashape.app.getExistingDirectory(
        "Select the output directory:", "C:/"
    )
    output_path_photo_name = os.path.join(
        output_directory, f"Photos_names_{site_name}_{Transect}.csv"
    )
    New_pixels.to_csv(output_path_photo_name, index=False)


def main():
    Photos_names, site_name, Transect = extract_images_name()
    New_pixels = pixel_extraction(Photos_names)
    create_data(New_pixels, site_name, Transect)


main()
