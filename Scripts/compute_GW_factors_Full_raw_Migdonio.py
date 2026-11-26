¿import multiprocessing
from concurrent.futures import ProcessPoolExecutor
from pathlib import Path
from tkinter import Tk, filedialog

import numpy as np
import pandas as pd
import rawpy
import tifffile
from tqdm import tqdm

WORKING_DIR = r'C:\Synced_and_Backups\Synced\updated_image_workflow'
WORKERS = 10 # Images processed at once. See TECHNICAL_REPORT.md for the memory each one needs.

# Full RAW (Bayer mosaic): which samples to keep for the grey-world means.
# A 2x2 block of the mosaic holds one R, two G and one B sample, i.e. one colour pixel.
# If ANY of the 4 samples is near saturation, the whole block is dropped, the same rule as
# the sRAW path (drop a pixel if any channel is near saturation) and as LibRaw.
# Dropping single samples instead biases the factors: where only G clips, the bright R and B
# samples stay in but their G neighbours go. See TECHNICAL_REPORT.md.
def keep_unsaturated_blocks(mosaic, cfa, white_level_bc):
    # Each 2x2 block must contain all four CFA colours (R, G, B, G2), starting at the top-left corner.
    if sorted(cfa[:2, :2].ravel()) != [0, 1, 2, 3]:
        raise ValueError(f'Unexpected CFA layout in the first 2x2 block: {cfa[:2, :2].tolist()}')

    saturated = mosaic >= 0.98 * white_level_bc

    # An odd last row or column has no complete block: leave it out.
    height = mosaic.shape[0] - mosaic.shape[0] % 2
    width = mosaic.shape[1] - mosaic.shape[1] % 2

    # The four samples of every block, each as an array with one entry per block.
    top_left = saturated[0:height:2, 0:width:2]
    top_right = saturated[0:height:2, 1:width:2]
    bottom_left = saturated[1:height:2, 0:width:2]
    bottom_right = saturated[1:height:2, 1:width:2]
    block_is_saturated = top_left | top_right | bottom_left | bottom_right

    # Copy each block's verdict back to its four samples.
    keep = np.zeros(mosaic.shape, dtype = bool)
    keep[0:height:2, 0:width:2] = ~block_is_saturated
    keep[0:height:2, 1:width:2] = ~block_is_saturated
    keep[1:height:2, 0:width:2] = ~block_is_saturated
    keep[1:height:2, 1:width:2] = ~block_is_saturated
    return keep

def compute_grey_world_factors(mosaic, cfa, white_level_bc):
    keep = keep_unsaturated_blocks(mosaic, cfa, white_level_bc)
    m_R = mosaic[keep & (cfa == 0)].mean(dtype = np.float64)
    m_G = mosaic[keep & ((cfa == 1) | (cfa == 3))].mean(dtype = np.float64)
    m_B = mosaic[keep & (cfa == 2)].mean(dtype = np.float64)

    return [m_R, m_G, m_B, m_G/m_R, np.float64(1.0), m_G/m_B]

# sRAW / linear DNG: an already demosaiced H x W x 3 array (R, G, B).
# A pixel is dropped if ANY channel is near saturation: in demosaiced data a clipped value
# also contaminates the other channels of that pixel.
def compute_grey_world_factors_linear(rgb, white_level_bc):
    keep = (rgb < 0.98 * white_level_bc).all(axis = 2)
    m_R, m_G, m_B = (rgb[..., c][keep].mean(dtype = np.float64) for c in range(3))

    return [m_R, m_G, m_B, m_G/m_R, np.float64(1.0), m_G/m_B]

# DNG AnalogBalance (tag 50727): the gain the converter already applied to the stored values,
# stored_c = AnalogBalance_c * camera_native_c. Adobe DNG Converter writes the as-shot camera WB
# here for sRAW (linear) DNGs, e.g. (2.267, 1, 1.468); for mosaic DNGs it is (1, 1, 1).
# CR2 files and DNGs without the tag: (1, 1, 1). See TECHNICAL_REPORT.md.
def read_analog_balance(img_path):
    if img_path.suffix.lower() == '.dng':
        with tifffile.TiffFile(str(img_path)) as tif:
            tag = tif.pages[0].tags.get(50727)
            if tag is not None:
                v = tag.value # Rationals, flattened: (num, den, num, den, ...).
                return np.array([v[i] / v[i + 1] for i in range(0, 6, 2)], dtype = np.float64)
    return np.ones(3, dtype = np.float64)

# When the photo was taken: EXIF DateTimeOriginal of the DNG, with dashes in the date.
# EXIF stores '2023:03:10 09:05:18'; this returns '2023-03-10 09:05:18'. An empty string when the tag is missing.
def read_exif_datetime(img_path):
    with tifffile.TiffFile(str(img_path)) as tif:
        exif_tag = tif.pages[0].tags.get('ExifTag')
        exif = exif_tag.value if exif_tag is not None else {}

    date_time = str(exif.get('DateTimeOriginal', '')).strip()
    if not date_time:
        return ''
    date, time = date_time.split(' ')
    return date.replace(':', '-') + ' ' + time

# Full RAW: one colour pixel per 2x2 block, (R, mean of the two Gs, B), as an H/2 x W/2 x 3 array.
# This gives every channel the same weight as in the sRAW path. Used for high_val only.
def bayer_blocks_to_rgb(mosaic, cfa):
    height = mosaic.shape[0] - mosaic.shape[0] % 2
    width = mosaic.shape[1] - mosaic.shape[1] % 2

    # The 4 positions of a block, by CFA colour (0 = R, 1 = G, 2 = B, 3 = G2).
    plane = {}
    for row in (0, 1):
        for col in (0, 1):
            plane[cfa[row, col]] = mosaic[row:height:2, col:width:2].astype(np.float64)

    green = (plane[1] + plane[3]) / 2
    return np.stack([plane[0], green, plane[2]], axis = 2)

# high_val: where the JPG clips. The 99th percentile of all white-balanced channel values f_c * v_c,
# with v_c black-subtracted, so it is on the same scale as white_ref and white_selected.
# Every pixel counts, saturated ones too: the point is how the image looks.
# process_raw_image.py gives it to rawpy as user_sat for the JPG. About 1% of channel values become 1.
def compute_high_val(rgb, factors):
    balanced = rgb * np.array(factors, dtype = np.float64)
    return np.percentile(balanced, 99)

def process_raw_image_metadata(img_path):
    img_name = img_path.name

    with rawpy.imread(str(img_path)) as raw:
        is_bayer = raw.raw_type == rawpy.RawType.Flat
        black_level = np.array(raw.black_level_per_channel)
        white_level = raw.white_level
        if is_bayer:
            mosaic = raw.raw_image_visible.copy()
            cfa = raw.raw_colors_visible.copy()
        else:
            # LibRaw gives H x W x 4 for linear DNGs; the 4th plane is unused (zeros).
            rgb = raw.raw_image_visible[..., :3].copy()

    if is_bayer:
        mosaic = mosaic.astype(np.int32) - black_level[cfa] # Black level subtraction.
        mosaic = np.maximum(mosaic, 0) # If a difference is negative, clip to 0.
        white_ref = white_level - black_level.min()
        rf_meta = compute_grey_world_factors(mosaic, cfa, white_ref)
        high_val = compute_high_val(bayer_blocks_to_rgb(mosaic, cfa), rf_meta[3:6])
    else:
        black_level = black_level[:3]
        rgb = rgb.astype(np.int32) - black_level # Per-channel black level subtraction.
        rgb = np.maximum(rgb, 0)
        white_ref = white_level - black_level.min()
        rf_meta = compute_grey_world_factors_linear(rgb, white_ref)
        high_val = compute_high_val(rgb, rf_meta[3:6])

    exif_datetime = read_exif_datetime(img_path)

    # Two sets of factors:
    # f_R, f_G, f_B:          for rawpy, user_wb = [f_R, f_G, f_B, f_G]. Computed on the values as stored
    #                         in the file, which is what postprocess() multiplies. process_raw_image.py uses these.
    # f_R_cam, f_G_cam, f_B_cam: camera-native factors, comparable between full RAW and sRAW.
    #                         NOT for user_wb on a DNG whose AnalogBalance is not 1 (double white balance).
    # Same pixels, other domain: f_c_cam = f_c * ab_c / ab_G. For full RAW and CR2, ab = 1 and both sets are equal.
    ab = read_analog_balance(img_path)
    f_cam = np.array(rf_meta[3:6]) * ab / ab[1]

    # image_type: what the photo was shot as. A mosaic DNG comes from a full RAW;
    # a linear (already demosaiced) DNG comes from an sRAW.
    return {'img_name':img_name, 'image_type':'RAW' if is_bayer else 'sRAW', 'exif_datetime':exif_datetime,
            'raw_kind':'bayer' if is_bayer else 'linear', 'white_ref':white_ref, 'high_val':high_val,
            'avg_R':rf_meta[0], 'avg_G':rf_meta[1], 'avg_B':rf_meta[2], 'f_R':rf_meta[3], 'f_G':rf_meta[4], 'f_B':rf_meta[5],
            'ab_R':ab[0], 'ab_G':ab[1], 'ab_B':ab[2], 'f_R_cam':f_cam[0], 'f_G_cam':f_cam[1], 'f_B_cam':f_cam[2]}

if __name__ == "__main__":
    # Choose the DNG folder, for example /photos_site1/DNG. The CSV goes to /photos_site1/factors.csv.
    Tk().withdraw()
    dng_dir = filedialog.askdirectory(initialdir = WORKING_DIR, title = "Choose the DNG folder")
    if not dng_dir:
        raise SystemExit("No folder chosen.")
    dng_dir = Path(dng_dir)

    # factors.csv may already hold white_selected and your other columns, so it is never overwritten.
    csv_path = dng_dir.parent / "factors.csv"
    if csv_path.exists():
        raise SystemExit(f"{csv_path} already exists. Move or rename it to compute the factors again.")

    img_paths = sorted(p for p in dng_dir.iterdir() if p.suffix.lower() == ".dng")
    if not img_paths:
        raise SystemExit(f"No DNG files in {dng_dir}.")

    # "spawn": rawpy's LibRaw uses OpenMP, which can deadlock in workers created by "fork".
    # pool.map returns the rows in the order of img_paths, so the CSV stays sorted by name.
    with ProcessPoolExecutor(WORKERS, mp_context = multiprocessing.get_context("spawn")) as pool:
        results = pool.map(process_raw_image_metadata, img_paths)
        rows = list(tqdm(results, total = len(img_paths), desc = f"Computing factors in {dng_dir.name}"))

    factors = pd.DataFrame(rows)
    factors.to_csv(csv_path, index = False)

    print(factors)
    print(factors["image_type"].value_counts())
    print(factors["white_ref"].value_counts())
    print(f"Saved {csv_path}. Add the white_selected column before running process_raw_image.py.")
    print("rawpy user_wb: use f_R, f_G, f_B. The *_cam columns are for comparing sRAW with full RAW only.")
