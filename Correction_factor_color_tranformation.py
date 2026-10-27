
import numpy as np
import os
import cv2
files = 
Transformed_2 = r"C:\Users\jandr\OneDrive - Universidad del rosario\Physiology_Photrogrammetry\Underwater\Transformed"
os.makedirs(Transformed_2, exist_ok=True)
Rf = 1.34
Bf =  1.34
Gf = 1

def main(files):
    for file in os.listdir(files):
        if file.endswith(".TIFF"):
            image,Image_R,Image_G,Image_B = open_file(file)
            Transformed = transform_images(image, Image_R,Image_G,Image_B)
            output_path = os.path.join(Transformed_2, file)
            cv2.imwrite(output_path, Transformed)
            print(f"processed {file}")

def open_file(file):
    image = cv2.imread(file).astype(np.float32)
    R = image[0:,0:,0] 
    G = image[0:,0:,1] 
    B = image[0:,0:,2] 
    Image_R = np.mean(R)
    Image_G =  np.mean(G)
    Image_B =  np.mean(B)
    return image, Image_R, Image_G, Image_B

def transform_images(image, Image_R,Image_G,Image_B):
    image[:, :, 0] *= Rf
    image[:, :, 1] *= Rf
    image[:, :, 2] *= Rf
    return image

main(files)
