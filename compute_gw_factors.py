import multiprocessing
from concurrent.futures import ProcessPoolExecutor
from pathlib import Path
from tkinter import Tk, filedialog

import numpy as np
import pandas as pd
import rawpy

WORKING_DIR = r"C:\Synced_and_Backups\Synced\updated_image_workflow"
WORKERS = 10  # Images processed at once. See TECHNICAL_REPORT.md for the memory each one needs.


def compute_grey_world_factors(mosaic, cfa, white_level_bc):
    keep = mosaic < 0.98 * white_level_bc
    m_R = mosaic[keep & (cfa == 0)].mean(dtype=np.float64)
    m_G = mosaic[keep & ((cfa == 1) | (cfa == 3))].mean(dtype=np.float64)
    m_B = mosaic[keep & (cfa == 2)].mean(dtype=np.float64)

    return [m_R, m_G, m_B, m_G / m_R, np.float64(1.0), m_G / m_B]


def process_raw_image_metadata(img_path):
    img_name = img_path.name

    with rawpy.imread(str(img_path)) as raw:
        mosaic = raw.raw_image_visible.copy()
        cfa = raw.raw_colors_visible.copy()
        black_level = np.array(raw.black_level_per_channel)
        white_level = raw.white_level

    mosaic = mosaic.astype(np.int32) - black_level[cfa]  # Black level subtraction.
    mosaic = np.maximum(mosaic, 0)  # If a difference is negative, clip to 0.

    white_ref = white_level - black_level.min()

    # Computing white balance factors (stored in rf_meta).
    # This also includes channel averages.
    rf_meta = compute_grey_world_factors(mosaic, cfa, white_ref)

    return {
        "img_name": img_name,
        "white_ref": white_ref,
        "avg_R": rf_meta[0],
        "avg_G": rf_meta[1],
        "avg_B": rf_meta[2],
        "f_R": rf_meta[3],
        "f_G": rf_meta[4],
        "f_B": rf_meta[5],
    }


if __name__ == "__main__":
    # Choose the DNG folder, for example /photos_site1/DNG. The CSV goes to /photos_site1/factors.csv.
    Tk().withdraw()
    dng_dir = filedialog.askdirectory(
        initialdir=WORKING_DIR, title="Choose the DNG folder"
    )
    if not dng_dir:
        raise SystemExit("No folder chosen.")
    dng_dir = Path(dng_dir)

    # factors.csv may already hold white_selected and your other columns, so it is never overwritten.
    csv_path = dng_dir.parent / "factors.csv"
    if csv_path.exists():
        raise SystemExit(
            f"{csv_path} already exists. Move or rename it to compute the factors again."
        )

    img_paths = sorted(p for p in dng_dir.iterdir() if p.suffix.lower() == ".dng")
    if not img_paths:
        raise SystemExit(f"No DNG files in {dng_dir}.")

    # "spawn": rawpy's LibRaw uses OpenMP, which can deadlock in workers created by "fork".
    with ProcessPoolExecutor(
        WORKERS, mp_context=multiprocessing.get_context("spawn")
    ) as pool:
        rows = list(pool.map(process_raw_image_metadata, img_paths))

    factors = pd.DataFrame(rows)
    factors.to_csv(csv_path, index=False)

    print(factors)
    print(factors["white_ref"].value_counts())
    print(
        f"Saved {csv_path}. Add the white_selected column before running process_raw_image.py."
    )
