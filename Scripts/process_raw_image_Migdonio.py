import multiprocessing
from concurrent.futures import ProcessPoolExecutor
from pathlib import Path
from tkinter import Tk, filedialog

import pandas as pd
import rawpy
import tifffile
from PIL import Image

WORKING_DIR = r'C:\Synced_and_Backups\Synced\updated_image_workflow'
WORKERS = 10
WRITE_TIFF = True
WRITE_JPG = True

def process_raw_image_linear(raw, white_factor, correction_factors, output_color = rawpy.ColorSpace.raw):
    rf = correction_factors

    linear_img = raw.postprocess(
        use_camera_wb=False,
        use_auto_wb=False,
        user_wb=[rf[0], rf[1], rf[2], rf[1]],
        user_sat=round(white_factor / min(rf)),
        adjust_maximum_thr=0,
        no_auto_bright=True,
        bright=1.0,
        highlight_mode=rawpy.HighlightMode.Clip,
        gamma=(1, 1),
        output_bps=16,
        user_flip=0,
        demosaic_algorithm=rawpy.DemosaicAlgorithm.LINEAR,
        output_color=output_color,
    )

    return linear_img

def process_raw_image_gamma(raw, white_factor, correction_factors):
    rf = correction_factors

    gamma_img = raw.postprocess(
                    use_camera_wb=False,
                    use_auto_wb=False,
                    user_wb=[rf[0], rf[1], rf[2], rf[1]],
                    user_sat=round(white_factor/min(rf)),
                    adjust_maximum_thr=0,
                    no_auto_bright=True,
                    bright=1.0,
                    highlight_mode=rawpy.HighlightMode.Clip,
                    gamma=(2.4, 12.92),
                    output_bps=8,
                    user_flip=0,
                    demosaic_algorithm=rawpy.DemosaicAlgorithm.AHD,
                    noise_thr=300,
                    output_color=rawpy.ColorSpace.sRGB,
    )

    return gamma_img

def process_photo(img_path, white_selected, f_R, f_G, f_B):
    rf = [f_R, f_G, f_B]
    site_dir = img_path.parent.parent

    # Each postprocess() needs its own rawpy.imread(): a used object keeps some settings.
    if WRITE_TIFF:
        with rawpy.imread(str(img_path)) as raw:
            linear_img = process_raw_image_linear(raw, white_selected, rf)
        tifffile.imwrite(site_dir / "TIFF" / f"{img_path.stem}.tif", linear_img)

    if WRITE_JPG:
        with rawpy.imread(str(img_path)) as raw:
            gamma_img = process_raw_image_gamma(raw, white_selected, rf)
        Image.fromarray(gamma_img).save(site_dir / "JPG" / f"{img_path.stem}.jpg", quality = 95)

    return img_path.name

if __name__ == "__main__":
    __spec__ = None
    Tk().withdraw()
    dng_dir = filedialog.askdirectory(initialdir = WORKING_DIR, title = "Choose the DNG folder")
    if not dng_dir:
        raise SystemExit("No folder chosen.")
    dng_dir = Path(dng_dir)
    site_dir = dng_dir.parent

    # Only the photos listed in the CSV are processed.
    factors = pd.read_csv(site_dir / "factors.csv")
    img_paths = [dng_dir / name for name in factors["img_name"]]

    if WRITE_TIFF:
        (site_dir / "TIFF").mkdir(exist_ok = True)
    if WRITE_JPG:
        (site_dir / "JPG").mkdir(exist_ok = True)

    # "spawn": rawpy's LibRaw uses OpenMP, which can deadlock in workers created by "fork".
    with ProcessPoolExecutor(WORKERS, mp_context = multiprocessing.get_context("spawn")) as pool:
        results = pool.map(process_photo, img_paths, factors["white_selected"], factors["f_R"], factors["f_G"], factors["f_B"])
        for name in results:
            print(f"Done: {name}")
