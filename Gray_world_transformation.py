from numpy._core.defchararray import endswith
from numpy import float32
from cv2 import mean
import cv2
import numpy as np
import os

files = r"C:\Users\jandr\OneDrive - Universidad del rosario\Physiology_Photrogrammetry\Land_Color_cards_experiment_D_P\linear"
Transformed = r"C:\Users\jandr\OneDrive - Universidad del rosario\Physiology_Photrogrammetry\Underwater\Transformed"

os.makedirs(Transformed, exist_ok=True)


def extract_RGB(image):
    Image = image.astype(float32)
    R = Image[:, :, 0]
    G = Image[:, :, 1]
    B = Image[:, :, 2]
    Image_R = np.mean(R)
    Image_G = np.mean(G)
    Image_B = np.mean(B)
    All_average = np.mean([Image_R, Image_G, Image_B])
    Ratio_R = All_average / Image_R
    Ratio_G = All_average / Image_G
    Ratio_B = All_average / Image_B
    return Image, Ratio_R, Ratio_G, Ratio_B


def factor_pixel(Image, Ratio_R, Ratio_G, Ratio_B):
    Image[:, :, 0] *= Ratio_R
    Image[:, :, 1] *= Ratio_G
    Image[:, :, 2] *= Ratio_B
    return Image


def main(files):
    for file in os.listdir(files):
        if file.endswith(".TIFF"):
            path = os.path.join(files, file)
            image = cv2.imread(path, cv2.IMREAD_UNCHANGED)
            image = cv2.cvtColor(image, cv2.COLOR_BGR2RGB)
            Image, Ratio_R, Ratio_G, Ratio_B = extract_RGB(image)
            Transformed_image = factor_pixel(Image, Ratio_R, Ratio_G, Ratio_B)
            output_path = os.path.join(Transformed, file)
            Transformed_image = cv2.cvtColor(Transformed_image, cv2.COLOR_RGB2BGR)
            cv2.imwrite(output_path, Transformed_image)
            print(f"Processed {file}")


main(files)
