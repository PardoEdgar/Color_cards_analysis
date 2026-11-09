import Metashape
import numpy as np
import pandas as pd
import os
import re

doc = Metashape.app.document
chunks = doc.chunks


def extract_location():
    Cards_data = []
    for chunk in chunks:
        split = chunk.label.split("_")
        site_name = split[0]
        Transect = split[4]
        transform = chunk.transform.matrix
        markers = chunk.markers
        for marker in markers:
            if re.fullmatch(r"G\d", marker.label) is not None:
                name = marker.label
                position = np.array(transform.mulp(marker.position))
                Cards_data.append(
                    {
                        "Card_ID": marker.group.label,
                        "site_name": site_name,
                        "Transect": Transect,
                        "name": name,
                        "x_position": position[0],
                        "y_position": position[1],
                        "z_position": position[2],
                    }
                )
    print(Cards_data)
    return Cards_data, site_name, Transect


def extract_images_name():
    Photos_names = []
    for chunk in chunks:
        split = chunk.label.split("_")
        site_name = split[0]
        Transect = split[4]
        markers = chunk.markers
        for marker in markers:
            if re.fullmatch(r"G\d", marker.label) is not None:
                for cam, proj in marker.projections.items():
                    Photos_names.append(
                        {
                            "Card_ID": marker.group.label,
                            "site_name": site_name,
                            "Transect": Transect,
                            "names": cam.label,
                            "marker": marker.label,
                            "X_location_pixel": proj.coord.x,
                            "y_location_pixel": proj.coord.y,
                        }
                    )

    print(Photos_names)
    return Photos_names


def create_data(Cards_data, Photos_names, site_name, Transect):
    output_directory = Metashape.app.getExistingDirectory(
        "Select the output directory:", "C:/"
    )
    output_path_markers_position = os.path.join(
        output_directory, f"Cards_data_{site_name}_{Transect}.csv"
    )
    output_path_photo_name = os.path.join(
        output_directory, f"Photos_names_{site_name}_{Transect}.csv"
    )
    Cards_data = pd.DataFrame(Cards_data)
    Cards_data.to_csv(output_path_markers_position, index=False)
    Photos_names = pd.DataFrame(Photos_names)
    Photos_names.to_csv(output_path_photo_name, index=False)
    print(f"saved {Cards_data}")


def main():
    Cards_data, site_name, Transect = extract_location()
    Photos_names = extract_images_name()
    create_data(Cards_data, Photos_names, site_name, Transect)


main()
