import multiprocessing
import subprocess
from concurrent.futures import ProcessPoolExecutor
from pathlib import Path
from tkinter import Tk, filedialog

import pandas as pd
import rawpy
import tifffile
from PIL import Image
from tqdm import tqdm

WORKING_DIR = r'C:\Synced_and_Backups\Synced\updated_image_workflow'
WORKERS = 10
WRITE_TIFF = True
WRITE_JPG = True

# exiftool copies the DNG metadata (EXIF, maker notes, XMP, ...) to every TIFF and JPG.
# 'exiftool' works if it is on the PATH; otherwise give the full path,
# for example r'F:/segmentationDataset/paralelizar_bayer/exiftool-13.52_64/exiftool.exe'.
EXIFTOOL_PATH = 'exiftool'

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

# clip_level: the white-balanced value (black subtracted) that becomes 1 in the JPG.
# rawpy saturates channel c where f_c * v_c >= user_sat * min(f), so user_sat = clip_level / min(f).
def process_raw_image_gamma(raw, clip_level, correction_factors):
    rf = correction_factors

    gamma_img = raw.postprocess(
                    use_camera_wb=False,
                    use_auto_wb=False,
                    user_wb=[rf[0], rf[1], rf[2], rf[1]],
                    user_sat=round(clip_level/min(rf)),
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

# Same exiftool call as the old apply_color_correction.py: all tags of the DNG, also the "unsafe" ones.
# A failed copy does not stop the run, but it is reported.
def copy_metadata(dng_path, output_path):
    cmd = [EXIFTOOL_PATH, '-overwrite_original', '-TagsFromFile', str(dng_path), '-all:all', '-unsafe', '-F', str(output_path)]
    result = subprocess.run(cmd, capture_output = True, text = True)
    if result.returncode != 0:
        print(f"WARNING: exiftool could not copy the metadata to {output_path.name}: {result.stderr.strip()}")

def process_photo(img_path, white_selected, high_val, f_R, f_G, f_B):
    rf = [f_R, f_G, f_B]
    site_dir = img_path.parent.parent

    # Each postprocess() needs its own rawpy.imread(): a used object keeps some settings.
    # TIFF: clipped at white_selected, as before.
    if WRITE_TIFF:
        with rawpy.imread(str(img_path)) as raw:
            linear_img = process_raw_image_linear(raw, white_selected, rf)
        tiff_path = site_dir / "TIFF" / f"{img_path.stem}.tif"
        tifffile.imwrite(tiff_path, linear_img)
        copy_metadata(img_path, tiff_path)

    # JPG: clipped at high_val (99th percentile, from compute_gw_factors.py), for a better look.
    # Never above white_selected: if more than 1% of the image is saturated, high_val can be above the
    # saturation level, and the clipped highlights would come out grey instead of white.
    # Exception: if a factor is below 1 (e.g. f_B = 0.8), that channel tops out at f * white_selected, so clipped
    # highlights keep a slight cast. The original script's TIFF does the same. See TECHNICAL_REPORT.md (review, item 5).
    if WRITE_JPG:
        jpg_clip = min(high_val, white_selected)
        with rawpy.imread(str(img_path)) as raw:
            gamma_img = process_raw_image_gamma(raw, jpg_clip, rf)
        jpg_path = site_dir / "JPG" / f"{img_path.stem}.jpg"
        Image.fromarray(gamma_img).save(jpg_path, quality = 95)
        copy_metadata(img_path, jpg_path)

    return img_path.name

if __name__ == "__main__":
    __spec__ = None
    Tk().withdraw()
    dng_dir = filedialog.askdirectory(initialdir = WORKING_DIR, title = "Choose the DNG folder")
    if not dng_dir:
        raise SystemExit("No folder chosen.")
    dng_dir = Path(dng_dir)
    site_dir = dng_dir.parent

    # Check exiftool before processing anything.
    try:
        subprocess.run([EXIFTOOL_PATH, '-ver'], capture_output = True, check = True)
    except (OSError, subprocess.CalledProcessError):
        raise SystemExit(f"exiftool not found at '{EXIFTOOL_PATH}'. Set EXIFTOOL_PATH at the top of this script.")

    # Only the photos listed in the CSV are processed.
    factors = pd.read_csv(site_dir / "factors.csv")
    # high_val is only needed for the JPGs, so an older factors.csv without it still works for TIFF-only runs.
    required = {"img_name", "white_selected", "f_R", "f_G", "f_B"}
    if WRITE_JPG:
        required.add("high_val")
    missing = required - set(factors.columns)
    if missing:
        raise SystemExit(f"factors.csv has no column {sorted(missing)}. high_val comes from compute_gw_factors.py.")
    img_paths = [dng_dir / name for name in factors["img_name"]]
    high_vals = factors["high_val"] if "high_val" in factors.columns else [None] * len(factors)

    if WRITE_TIFF:
        (site_dir / "TIFF").mkdir(exist_ok = True)
    if WRITE_JPG:
        (site_dir / "JPG").mkdir(exist_ok = True)

    # "spawn": rawpy's LibRaw uses OpenMP, which can deadlock in workers created by "fork".
    with ProcessPoolExecutor(WORKERS, mp_context = multiprocessing.get_context("spawn")) as pool:
        results = pool.map(process_photo, img_paths, factors["white_selected"], high_vals,
                           factors["f_R"], factors["f_G"], factors["f_B"])
        # Going through the results waits for each photo (one step of the bar) and raises any error from a worker.
        for _ in tqdm(results, total = len(img_paths), desc = f"Processing photos in {dng_dir.name}"):
            pass
