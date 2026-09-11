from random import Random
import tkinter as tk
import cv2
from tkinter import filedialog, ttk
from PIL import Image, ImageTk
from pathlib import Path
import numpy as np
import pandas as pd

PANEL = "#02236f"
BG = "#000000"
TEXT = "#FFFFFF"
ACCENT = "#FFFFFF"


class Pixel_selector:
    def __init__(self, root):
        self.region_number = 0
        self.root = root
        self.root.title("RGB extraction")
        self.image = None
        self.tk_image = None
        self.coords = []
        self.image_path = None
        self.lineal_image = None
        self.lineal_path = None
        self.site = tk.StringVar(value="Lab")
        self.case = tk.StringVar(value="Uncased")
        self.status = tk.StringVar(value="Degraded")
        self.grey_patch = []
        self.id = 1
        self.click_count = 0
        self.zoom_factor = 1
        self.zoom_step = 1.1
        self.container = tk.Frame(root, bg=PANEL)
        self.container.pack(fill="both", expand=True)
        tk.Label(
            self.container,
            text="Pixel intensity extraction",
            bg=PANEL,
            fg=ACCENT,
        ).pack(padx=20, pady=20, anchor="w")

        self.build_tab_1()

    def build_tab_1(self):
        btn_frame_1 = tk.Frame(self.container, bg=BG)
        btn_frame_1.pack(side="right", fill="x", padx=5, pady=10)
        main_1 = tk.Frame(self.container, bg=BG)
        main_1.pack(fill="both", padx=10, pady=10, expand=True)
        self.h_scroll = tk.Scrollbar(main_1, orient="horizontal")
        self.v_scroll = tk.Scrollbar(main_1, orient="vertical")
        self.canvas_1 = tk.Canvas(
            main_1,
            bg=BG,
            cursor="crosshair",
            xscrollcommand=self.h_scroll.set,
            yscrollcommand=self.v_scroll.set,
        )

        self.v_scroll.config(command=self.canvas_1.yview)
        self.h_scroll.config(command=self.canvas_1.xview)
        self.v_scroll.pack(side="right", fill="y")
        self.h_scroll.pack(side="bottom", fill="x")

        self.canvas_1.pack(side="left", fill="both", expand=True)

        self.btn_1 = tk.Button(
            btn_frame_1,
            text="Load Image",
            bg=ACCENT,
            fg="black",
            relief="flat",
            padx=20,
            pady=5,
            command=self.load_image_Gamma_JPG,
        )
        self.btn_1.pack(side="bottom", pady=10, padx=10, fill="x")

        self.btn_2 = tk.Button(
            btn_frame_1,
            text="save data",
            bg=ACCENT,
            fg="black",
            relief="flat",
            padx=20,
            pady=5,
            command=self.extract_data,
        )
        self.btn_2.pack(side="bottom", pady=10, padx=10, fill="x")

        self.btn_3 = tk.Button(
            btn_frame_1,
            text="Reset counter",
            bg=ACCENT,
            fg="black",
            relief="flat",
            padx=20,
            pady=5,
            command=self.reset_counter,
        )
        self.btn_3.pack(side="bottom", pady=10, padx=10, fill="x")

        self.btn_zoom_in = tk.Button(btn_frame_1, text="+", command=self.zoom_in)
        self.btn_zoom_in.pack(side="bottom", pady=10, padx=10, fill="x")
        self.btn_zoom_out = tk.Button(btn_frame_1, text="-", command=self.zoom_out)
        self.btn_zoom_out.pack(side="bottom", pady=10, padx=10, fill="x")

        # Events
        self.canvas_1.bind("<ButtonPress-1>", self.on_click)

        self.pixel_label_1 = tk.Label(btn_frame_1, text="pixel ---", bg=BG, fg="white")
        self.pixel_label_1.pack(side="bottom", padx=10)

        self.grey_patch_1 = tk.Label(
            btn_frame_1, text="Grey_patch ---", bg=BG, fg="white"
        )
        self.grey_patch_1.pack(side="bottom", padx=10)

        self.id_1 = tk.Label(btn_frame_1, text="ID ---", bg=BG, fg="white")
        self.id_1.pack(side="bottom", padx=10)

        tk.Label(
            btn_frame_1,
            text="Site",
            bg=PANEL,
            fg=ACCENT,
        ).pack(side="right", anchor="c", pady=(10, 10))

        ttk.Combobox(
            btn_frame_1,
            textvariable=self.site,
            values=["Lab", "Archaeology", "SetUp", "Parking", "Amador", "Aquarium"],
            state="readonly",
            width=12,
        ).pack(side="right")

        tk.Label(
            btn_frame_1,
            text="Status",
            bg=PANEL,
            fg=ACCENT,
        ).pack(side="right", anchor="c", pady=(10, 10))

        ttk.Combobox(
            btn_frame_1,
            textvariable=self.status,
            values=["Degraded", "Preserved"],
            state="readonly",
            width=12,
        ).pack(side="right")

        tk.Label(
            btn_frame_1,
            text="Case",
            bg=PANEL,
            fg=ACCENT,
        ).pack(side="right", anchor="c", pady=(10, 10))

        ttk.Combobox(
            btn_frame_1,
            textvariable=self.case,
            values=["Cased", "Uncased"],
            state="readonly",
            width=12,
        ).pack(side="right")

    def load_image_Gamma_JPG(self):
        path = filedialog.askopenfilename()
        self.image_path = Path(path)
        img = cv2.imread(path)
        img = cv2.cvtColor(img, cv2.COLOR_BGR2RGB)
        self.image = img
        self.display_image(img)
        self.load_image_lineal_TIFF()
        self.path = path
        print(self.image_path)

    def load_image_lineal_TIFF(self):
        lineal_path = (
            self.image_path.parent.parent
            / Path("linear")
            / Path(self.image_path.stem + ".TIFF")
        )

        print(f"lineal_path: {lineal_path}")
        self.lineal_path = Path(lineal_path)
        lineal_img = cv2.imread(lineal_path)
        lineal_img = cv2.cvtColor(lineal_img, cv2.COLOR_BGR2RGB)
        self.lineal_image = lineal_img
        self.arr = np.array(lineal_img)
        self.hsv_arr = cv2.cvtColor(lineal_img, cv2.COLOR_RGB2HSV)
        self.lab_arr = cv2.cvtColor(lineal_img, cv2.COLOR_RGB2LAB)

    def display_image(self, img):
        self.canvas_1.update_idletasks()
        canvas_width = self.canvas_1.winfo_width()
        canvas_height = self.canvas_1.winfo_height()
        h, w = img.shape[:2]
        self.scale = min(canvas_width / w, canvas_height / h)
        self.display_scale = self.scale * self.zoom_factor
        new_w = int(w * self.display_scale)
        new_h = int(h * self.display_scale)
        resized = cv2.resize(img, (new_w, new_h), interpolation=cv2.INTER_AREA)
        pil_img = Image.fromarray(resized)
        self.tk_image = ImageTk.PhotoImage(pil_img)
        self.canvas_1.delete("all")
        self.img_canvas_x_position = max(0, (canvas_width - new_w) // 2)
        self.img_canvas_y_position = max(0, (canvas_height - new_h) // 2)
        self.canvas_1.create_image(
            self.img_canvas_x_position,
            self.img_canvas_y_position,
            anchor="nw",
            image=self.tk_image,
        )

        self.canvas_1.config(
            scrollregion=(
                0,
                0,
                max(canvas_width, new_w + self.img_canvas_x_position),
                max(canvas_height, new_h + self.img_canvas_y_position),
            )
        )

    def on_click(self, event):
        self.coords = []
        grey_patches = ["G0", "G1", "G2", "G3", "G4", "G5"]
        self.grey_patch = grey_patches[self.region_number % len(grey_patches)]
        self.region_number += 1
        if self.click_count == 6:
            self.id += 1
            self.click_count = 0
        self.click_count += 1

        if self.id == 7:
            self.id = 1

        canvas_x = self.canvas_1.canvasx(event.x)
        canvas_y = self.canvas_1.canvasy(event.y)

        relative_x_position = canvas_x - self.img_canvas_x_position
        relative_y_position = canvas_y - self.img_canvas_y_position

        Real_x = int(relative_x_position / self.display_scale)
        Real_y = int(relative_y_position / self.display_scale)

        for _ in range(25):
            dx = np.random.randint(-25, 25)
            dy = np.random.randint(-25, 25)
            nx = Real_x + dx
            ny = Real_y + dy
            self.coords.append((nx, ny))
        r = self.arr[Real_y, Real_x, 0]
        g = self.arr[Real_y, Real_x, 1]
        b = self.arr[Real_y, Real_x, 2]
        h = self.hsv_arr[Real_y, Real_x, 0]
        s = self.hsv_arr[Real_y, Real_x, 1]
        v = self.hsv_arr[Real_y, Real_x, 2]
        l_lab = self.lab_arr[Real_y, Real_x, 0]
        a_lab = self.lab_arr[Real_y, Real_x, 1]
        b_lab = self.lab_arr[Real_y, Real_x, 2]
        self.pixel_label_1.config(
            text=f"Pixel ({Real_x}, {Real_y}); R={r},G={g},B={b}; h={h},s={s},v={v}; l={l_lab},a={a_lab},b={b_lab}"
        )
        self.grey_patch_1.config(text=f"Grey patch {self.grey_patch}")
        self.draw_pixels()

        self.id_1.config(text=f"Id {self.id}")
        self.extract_data()

    def reset_counter(self):
        self.region_number = 0
        self.click_count = 0
        self.id = 1

    def extract_data(self, output_path=r"C:\Users\jandr\Downloads"):
        records = []
        for x, y in self.coords:
            r = self.arr[y, x, 0]
            g = self.arr[y, x, 1]
            b = self.arr[y, x, 2]

            h = self.hsv_arr[y, x, 0]
            s = self.hsv_arr[y, x, 1]
            v = self.hsv_arr[y, x, 2]

            l_lab = self.lab_arr[y, x, 0]
            a_lab = self.lab_arr[y, x, 1]
            b_lab = self.lab_arr[y, x, 2]

            records.append(
                {
                    "folder": self.lineal_path.parent.stem,
                    "Filename": self.lineal_path.stem,
                    "ID": self.id,
                    "Case": self.case.get(),
                    "Status": self.status.get(),
                    "Site": self.site.get(),
                    "Grey_patch": self.grey_patch,
                    "x": x,
                    "y": y,
                    "R": float(r),
                    "G": float(g),
                    "B": float(b),
                    "h": float(h),
                    "s": float(s),
                    "v": float(v),
                    "l_lab": float(l_lab),
                    "a_lab": float(a_lab),
                    "b_lab": float(b_lab),
                }
            )
        df_img = pd.DataFrame(records)
        csv_path = Path(output_path) / "Data_pixels.csv"

        file_exists = csv_path.exists()
        if file_exists:
            mode = "a"
        else:
            mode = "w"
        df_img.to_csv(
            csv_path,
            mode=mode,
            header=not file_exists,
            index=False,
        )

        print(
            f"{len(df_img):,} pixels from {self.image_path.name} were saved in {csv_path}"
        )

    def draw_pixels(self):
        for x, y in self.coords:
            screen_x = x * self.display_scale + self.img_canvas_x_position
            screen_y = y * self.display_scale + self.img_canvas_y_position
            self.canvas_1.create_oval(
                screen_x - 0.5,
                screen_y - 0.5,
                screen_x + 0.5,
                screen_y + 0.5,
                outline="red",
            )

    def zoom_in(self):
        self.zoom_factor = self.zoom_factor * self.zoom_step
        self.display_image(self.image)
        self.draw_pixels()

    def zoom_out(self):
        self.zoom_factor = self.zoom_factor / self.zoom_step
        self.display_image(self.image)
        self.draw_pixels()


if __name__ == "__main__":
    root = tk.Tk()
    root.geometry("1200x900")
    app = Pixel_selector(root)
    root.mainloop()
