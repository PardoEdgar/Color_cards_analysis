import cv2
from numpy import shape
import Metashape
import numpy as np
import pandas as pd
import os
import re
import random
import tkinter as tk
import tifffile
from pathlib import Path
from datetime import datetime

doc = Metashape.app.document
chunks = doc.chunks


def extract_images_name():
    Photos_names = []
    for chunk in chunks:
        split = chunk.label.split("_")
        site_name, Month, Transect = split[0], split[2], split[4]
        markers = chunk.markers
        camera_times = []
        for camera in chunk.cameras:
            camera_meta = camera.photo.imageMeta()
            time_str = camera_meta["Exif/DateTimeOriginal"]
            camera_time = datetime.strptime(time_str, "%Y:%m:%d %H:%M:%S")
            if time_str:
                camera_times.append(datetime.strptime(time_str, "%Y:%m:%d %H:%M:%S"))
        first_photo_time = min(camera_times)
        for marker in markers:
            if re.fullmatch(r"G\d", marker.label) is not None:
                for cam, proj in marker.projections.items():
                    if cam.enabled:
                        cam_path = Path(cam.photo.path)
                        camera_meta = cam.photo.imageMeta()
                        time_str = camera_meta["Exif/DateTimeOriginal"]
                        camera_time = datetime.strptime(time_str, "%Y:%m:%d %H:%M:%S")
                        Hour = camera_time.hour
                        minutes_noncorrected = camera_time.minute
                        minutes = (camera_time - first_photo_time).total_seconds() / 60
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
                                "camera_time": camera_time,
                                "Hour": Hour,
                                "Minutes_noncorrected": minutes_noncorrected,
                                "minutes": minutes,
                            }
                        )

    return pd.DataFrame(Photos_names), site_name, Transect


def pixel_extraction(Photos_names):
    New_pixels = []
    for _, row in Photos_names.iterrows():
        path = Path(row["cam_path"])
        path = path.parent.parent / Path("GW") / f"{path.stem}.TIFF"
        Grey_world_img = tifffile.imread(str(path))
        arr = np.array(Grey_world_img)
        Height, Width = arr.shape[:2]
        center_X = row["X_location_pixel"]
        center_Y = row["Y_location_pixel"]
        if not (0 <= center_X < Width and 0 <= center_Y < Height):
            continue
        for _ in range(25):
            dx = np.random.randint(-25, 26)
            dy = np.random.randint(-25, 26)
            nx = round(center_X) + dx
            ny = round(center_Y) + dy
            # print(f"Image shape {arr.shape}")
            # print(f"nx range: {np.min(nx), np.max(nx)}, ny range: {np.min(ny), np.max(ny)}")
            if 0 <= nx < Width and 0 <= ny < Height:
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
