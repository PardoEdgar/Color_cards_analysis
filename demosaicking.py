import rawpy
from tkinter import filedialog
import numpy as np
import tkinter as tk
from pathlib import Path
import os
import tifffile
from concurrent.futures import ProcessPoolExecutor, as_completed
from PIL import Image
import exifread
import subprocess

EXIFTOOL_PATH = (
    r"C:\Users\jandr\Downloads\exiftool-13.59_64\exiftool-13.59_64\exiftool.exe"
)


def load_data():
    root = tk.Tk()
    root.withdraw()
    folder = filedialog.askdirectory(title="Please, selecte folder with DNG files")
    return Path(folder)


def copy_datetime_metadata(dng_path, jpg_path):
    cmd = [
        EXIFTOOL_PATH,
        "-overwrite_original",
        "-TagsFromFile",
        str(dng_path),
        "-all:all",
        "-unsafe",
        "-F",
        str(jpg_path),
    ]
    subprocess.run(cmd, check=True)


def copy_datetime_metadata_tiff(dng_path, tiff_path):
    cmd = [
        EXIFTOOL_PATH,
        "-overwrite_original",
        "-TagsFromFile",
        str(dng_path),
        "-all:all",
        "-unsafe",
        "-F",
        str(tiff_path),
    ]

    subprocess.run(cmd, check=True)


def demosaicking_linear(file, output_dir_tiff):
    img_raw = rawpy.imread(str(file))
    Max_saturation = img_raw.white_level - img_raw.black_level_per_channel[0]
    img = img_raw.postprocess(
        output_bps=16,
        four_color_rgb=True, 
        demosaic_algorithm=rawpy.DemosaicAlgorithm.AHD,
        output_color=rawpy.ColorSpace.raw,
        fbdd_noise_reduction=rawpy.FBDDNoiseReductionMode.Off,
        highlight_mode=rawpy.HighlightMode.Ignore, 
        no_auto_scale=True, no_auto_bright=True,
        use_camera_wb=False,  
        use_auto_wb=False,
        gamma=(1, 1),
        user_flip=0,
    )
    linear = img.astype(np.float32) / Max_saturation

    tiff_path = output_dir_tiff / (f"{file.stem}.TIFF")
    tifffile.imwrite(str(tiff_path), linear, photometric="rgb")
    copy_datetime_metadata_tiff(file, tiff_path)
    img_raw.close()


def process_folder_parallel_linear(input_dir, max_workers):

    output_dir_tiff = input_dir.parent / "linear"
    output_dir_tiff.mkdir(parents=True, exist_ok=True)
    dng_files = list(input_dir.glob("*.dng"))

    if not dng_files:
        print("No hay archivos DNG para procesar.")
        return

    if max_workers is None:
        max_workers = max(1, os.cpu_count() - 1)

    print(f"Processing {len(dng_files)} files with  {max_workers} units")

    with ProcessPoolExecutor(max_workers=max_workers) as executor:
        futures = {
            executor.submit(demosaicking_linear, raw_path, output_dir_tiff): raw_path
            for raw_path in dng_files
        }

        for future in as_completed(futures):
            raw_path = futures[future]
            try:
                future.result()
                print(f"{raw_path.name} processed and saved in {output_dir_tiff}")
            except Exception as e:
                print(f"{raw_path} Error {e}")


def demosaicking_gamma(file, output_dir_jpg):
    img_raw = rawpy.imread(str(file))
    img = img_raw.postprocess(
        output_bps=16,
        four_color_rgb=True,  
        demosaic_algorithm=rawpy.DemosaicAlgorithm.AHD,
        output_color=rawpy.ColorSpace.sRGB,
        fbdd_noise_reduction=rawpy.FBDDNoiseReductionMode.Off,
        highlight_mode=rawpy.HighlightMode.Ignore,  
        no_auto_scale=False,  
        no_auto_bright=True,
        use_camera_wb=False, 
        use_auto_wb=True,
        gamma=(1, 1),
    )

    low_val = np.percentile(img, 2)
    high_val = np.percentile(img, 98)
    img = np.clip(img, low_val, high_val)
    img = (img - low_val) / (high_val - low_val)

    img_gamma = np.power(img, 1.0 / 2.2)
    img_gamma = np.clip(img_gamma, 0, 1)

    jpg_path = output_dir_jpg / (f"{file.stem}.jpg")
    Image.fromarray((img_gamma * 255).astype(np.uint8), mode="RGB").save(
        jpg_path, format="JPEG", quality=95
    )
    copy_datetime_metadata(file, jpg_path)
    img_raw.close()

    return "ok", {"file_name": file.name}


def process_folder_parallel_gamma(input_dir, max_workers):
    output_dir_jpg = input_dir.parent / "gamma"
    output_dir_jpg.mkdir(parents=True, exist_ok=True)
    dng_files = list(input_dir.glob("*.dng"))
    if not dng_files:
        print("NO DNG files to process.")
        return
    print(f"Processing {len(dng_files)} files with {max_workers} units")

    with ProcessPoolExecutor(max_workers=max_workers) as executor:
        futures = {
            executor.submit(demosaicking_gamma, raw_path, output_dir_jpg): raw_path
            for raw_path in dng_files}
        for future in as_completed(futures):
            raw_path = futures[future]
            try:
                future.result()
                print(f"{raw_path.name}  processed and saved in {output_dir_jpg}")
            except Exception as e:
                print(f"{raw_path.name}  ERROR: {e}")

if __name__ == "__main__":
    folder = load_data()
    print(f"Directorio: {folder}")
    process_folder_parallel_linear(input_dir=folder, max_workers=10)
    process_folder_parallel_gamma(input_dir=folder, max_workers=10)
