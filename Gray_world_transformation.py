
import numpy as np
import os
import cv2
files = 
Transformed_2 = r"C:\Users\jandr\OneDrive - Universidad del rosario\Physiology_Photrogrammetry\Underwater\Transformed"
os.makedirs(Transformed_2, exist_ok=True)
Site = "Underwater"
UFR = 0.0833
UFG = 1
UFB = 0.121

LFR = 0.0363
LFG = 1
LFB = 0.0453

def main(files):
    for file in os.listdir(files):
        if file.endswith(".TIFF"):
            image,Ratio_R,Ratio_G,Ratio_B = open_file(file)
            Transformed = transform_images(image, Ratio_R,Ratio_G,Ratio_B)
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
    if Site == "Underwater":
        Ratio_R = UFR / Image_R
        Ratio_G = UFG / Image_G
        Ratio_B = UFB / Image_B
    else:
        Ratio_R = LFR / Image_R
        Ratio_G = LFG / Image_G
        Ratio_B = LFB / Image_B
    return image, Ratio_R, Ratio_G, Ratio_B

def transform_images(image, Ratio_R, Ratio_G, Ratio_B):
        image[:, :, 0] *= Ratio_R
        image[:, :, 1] *= Ratio_G
        image[:, :, 2] *= Ratio_B
    
        return image

main(files)
