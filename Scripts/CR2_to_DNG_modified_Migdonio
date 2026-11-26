import subprocess
import os
from pathlib import Path
from concurrent.futures import ProcessPoolExecutor, as_completed
import tkinter as tk
from tkinter import filedialog
from tqdm import tqdm

# Copy of user_shared/CR2_to_DNG.py with a retry for failed conversions.
# What changed: a conversion only counts as OK if the DNG was written, failed CR2s are converted again
# (up to MAX_ATTEMPTS times), and the CR2s that still fail are listed at the end. See TECHNICAL_REPORT.md.

# Possible dimensions. Not used in this script actually. Will move to a different script later.
# CR2_DIMENSIONS = [3360, 6720]

# Path for DNG converter app
DNG_CONVERTER_PATH = (
    r"C:\Program Files\Adobe\Adobe DNG Converter\Adobe DNG Converter.exe"
)
INITIAL_DIR1 = "E:/Photos/RRR_project"
INITIAL_DIR2 = "E:/"
SECTION_NAMES = ["0to25", "25to50"]

# How many times each CR2 is tried in total: 1 = no retry, 3 = the first try and up to 2 retries.
MAX_ATTEMPTS = 3


# Custom exceptions.
class NoDirectorySelected(Exception):
    pass


class NoPhotoFiles(Exception):
    pass


class NamingConventionError(Exception):
    pass


class DNGMismatchError(Exception):
    pass


class FileOrganizationError(Exception):
    pass


class MissingFarawayName(Exception):
    pass


class OutputNotEmpty(Exception):
    pass


def choose_directory(title: str, initialdir: str):
    root = tk.Tk()

    root.withdraw()
    folder = filedialog.askdirectory(title=title, initialdir=initialdir)
    root.destroy()

    if not folder:
        raise NoDirectorySelected("No directory was selected.")
    return folder


# Input path includes the file name.
# Output directory is the directory where the file will be stored, no file name.
# Code by Edgar. Returns (status, input_path, message), status is 'OK' or 'ERROR'.
#
# A DNG is a TIFF file, so its first 4 bytes are 'II*\0' (little endian) or 'MM\0*' (big endian).
def is_DNG_file(dng_path: Path):
    if not dng_path.exists():
        return False
    with open(dng_path, "rb") as f:
        header = f.read(4)
    return header in (b"II*\x00", b"MM\x00*")


# The exit code alone is not enough: for a corrupt CR2 (truncated, or not a CR2 at all) the converter
# exits with 0, writes no DNG and prints "*** Error: Failed to convert '<name>' ***" on stderr.
# So a conversion is only OK if stderr has no "Failed to convert" and the DNG exists with a TIFF header
# (a leftover partial file is not enough). Tested with DNG Converter 19.0, see TECHNICAL_REPORT.md.
def convert_CR2_to_DNG(input_path: Path, output_dir: Path):
    dng_path = output_dir / f"{input_path.stem}.dng"
    cmd = [DNG_CONVERTER_PATH, "-p1", "-d", str(output_dir), str(input_path)]

    try:
        result = subprocess.run(
            cmd, stdout=subprocess.PIPE, stderr=subprocess.PIPE, timeout=300, text=True
        )
    except subprocess.TimeoutExpired:
        return "ERROR", input_path, "conversion timed out"
    except Exception as e:
        return "ERROR", input_path, str(e)

    if result.returncode != 0:
        return (
            "ERROR",
            input_path,
            f"exit code {result.returncode}: {result.stderr.strip()}",
        )

    if "Failed to convert" in result.stderr:
        return (
            "ERROR",
            input_path,
            f"the converter could not convert it: {result.stderr.strip()}",
        )

    if not is_DNG_file(dng_path):
        return "ERROR", input_path, f"no valid DNG was written: {result.stderr.strip()}"

    return "OK", input_path, ""


# Converts the CR2s in CR2_paths and returns the ones that failed, as {CR2 path: error message}.
def convert_batch(executor, CR2_paths, output_dir: Path, description: str):
    failed = {}
    futures = [
        executor.submit(convert_CR2_to_DNG, photo_path, output_dir)
        for photo_path in CR2_paths
    ]

    for future in tqdm(as_completed(futures), total=len(futures), desc=description):
        status, photo_path, message = future.result()

        if status == "ERROR":
            tqdm.write(f"ERROR {photo_path.name}: {message}")
            failed[photo_path] = message

    return failed


def convert_directory_CR2_to_DNG(input_dir: Path, output_dir: Path, max_workers=12):
    output_dir.mkdir(parents=True, exist_ok=True)

    # Only select CR2 files. This should've been quality checked at some point.
    CR2_paths = [
        photo_path
        for photo_path in input_dir.iterdir()
        if photo_path.suffix.lower() == ".cr2"
    ]

    if len(CR2_paths) == 0:
        raise NoPhotoFiles(f"No CR2 files found in {input_dir}")

    # The converter never overwrites: if 233A0364.dng exists, it writes 233A0364_1.dng next to it.
    # An old DNG would also make a failed conversion look like a success. So the output folder must have no DNGs.
    old_DNGs = [path for path in output_dir.iterdir() if path.suffix.lower() == ".dng"]
    if len(old_DNGs) > 0:
        raise OutputNotEmpty(
            f"{output_dir} already has {len(old_DNGs)} DNG files. Move them to convert again."
        )

    max_workers = max(1, min(os.cpu_count() - 2, max_workers))

    with ProcessPoolExecutor(max_workers=max_workers) as executor:
        failed = convert_batch(
            executor,
            CR2_paths,
            output_dir,
            f"Converting CR2 files in {input_dir.name} to DNG",
        )

        for attempt in range(2, MAX_ATTEMPTS + 1):
            if len(failed) == 0:
                break

            # A failed or timed-out conversion can leave a partial DNG. Remove it, otherwise the retry writes <name>_1.dng.
            for photo_path in failed:
                (output_dir / f"{photo_path.stem}.dng").unlink(missing_ok=True)

            failed = convert_batch(
                executor,
                list(failed),
                output_dir,
                f"Retry {attempt - 1} of {MAX_ATTEMPTS - 1}: {len(failed)} CR2 files in {input_dir.name}",
            )

    if len(failed) > 0:
        print(
            f"\n{len(failed)} CR2 files could not be converted after {MAX_ATTEMPTS} attempts:"
        )
        for photo_path, message in failed.items():
            print(f"  {photo_path.name}: {message}")

    print(f"DNGs saved to {output_dir}\n")


if __name__ == "__main__":
    # Check the converter before choosing folders: if it is missing, every conversion would fail.
    if not Path(DNG_CONVERTER_PATH).exists():
        raise SystemExit(
            f"DNG Converter not found at '{DNG_CONVERTER_PATH}'. Set DNG_CONVERTER_PATH at the top of this script."
        )

    # This assumes that the user chooses the folder with 0to25 and 25to50.
    main_dir = Path(
        choose_directory("Select folder with CR2 images to convert", INITIAL_DIR1)
    )

    for section in SECTION_NAMES:
        input_dir = main_dir / Path(main_dir.name + "_" + section) / "CR2"
        main_output_dir = main_dir / Path(main_dir.name + "_" + section) / "DNG"
        if not input_dir.exists():
            raise NamingConventionError(
                f"Directory {input_dir} should exist. Naming convention violated."
            )
        elif any(
            photo_path.suffix.lower() != ".cr2" for photo_path in input_dir.iterdir()
        ):
            raise FileOrganizationError(
                f"Directory {input_dir} should only contain CR2 files. File organization violated."
            )

        # By convention, there should be a Photos subdirectory somewhere in the input directory.
        if "CR2" not in input_dir.parts:
            raise NamingConventionError(
                "Naming convention error. Photos should be within a Photos subdirectory and a CR2 directory."
            )

        output_dir = main_output_dir
        output_dir.mkdir(parents=True, exist_ok=True)

        # Copying text file with faraway photo name.
        text_path = (
            main_dir / Path(main_dir.name + "_" + section) / Path("faraway_image.txt")
        )
        text_target_path = output_dir.parent / Path("faraway_image.txt")

        if not text_path.exists():
            raise MissingFarawayName(
                "The faraway name text file is missing. Make sure to generate the metadata for the original photo set first."
            )

        text_path.copy(text_target_path, preserve_metadata=True)

        # Main function calls.
        convert_directory_CR2_to_DNG(input_dir, output_dir)

        # Checking number of files.
        input_files = [photo_path for photo_path in input_dir.iterdir()]
        output_files = [photo_path for photo_path in output_dir.iterdir()]

        if len(input_files) != len(output_files):
            raise DNGMismatchError(
                f"The number of CR2 files is {len(input_files)} and the number of DNG files is {len(output_files)}. There is a size mismatch."
            )
