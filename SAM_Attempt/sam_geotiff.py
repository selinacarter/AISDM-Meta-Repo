import os
import cv2
import torch
import rasterio
import numpy as np
import matplotlib.pyplot as plt
import traceback

from tqdm import tqdm
from segment_anything import sam_model_registry
from segment_anything import SamAutomaticMaskGenerator


############################################
# CONFIG
############################################

IMAGE_DIR = "./crasar_data/train/imagery/SATELLITE"

SAM_CHECKPOINT = "./sam_vit_h_4b8939.pth"
MODEL_TYPE = "vit_h"

OUTPUT_DIR = "./sam_outputs"

# Resize large images for testing
MAX_IMAGE_SIZE = 2048

os.makedirs(OUTPUT_DIR, exist_ok=True)

############################################
# LOAD MODEL
############################################

device = "cuda" if torch.cuda.is_available() else "cpu"

print(f"Using device: {device}")

sam = sam_model_registry[MODEL_TYPE](
    checkpoint=SAM_CHECKPOINT
)

sam.to(device)

mask_generator = SamAutomaticMaskGenerator(
    model=sam,
    points_per_side=32,
    pred_iou_thresh=0.86,
    stability_score_thresh=0.92,
    crop_n_layers=1,
)

############################################
# HELPERS
############################################

def read_geotiff(path):
    """
    Convert GeoTIFF to RGB uint8 image.
    """

    with rasterio.open(path) as src:
        img = src.read()

    print(f"Raw TIFF shape: {img.shape}")

    img = np.transpose(img, (1, 2, 0))

    if img.shape[2] > 3:
        img = img[:, :, :3]

    if img.shape[2] == 1:
        img = np.repeat(img, 3, axis=2)

    img = img.astype(np.float32)

    img -= img.min()

    if img.max() > 0:
        img /= img.max()

    img = (255 * img).astype(np.uint8)

    print(f"Processed image shape: {img.shape}")

    return img


def resize_if_needed(image):

    h, w = image.shape[:2]

    if max(h, w) <= MAX_IMAGE_SIZE:
        return image

    scale = MAX_IMAGE_SIZE / max(h, w)

    new_w = int(w * scale)
    new_h = int(h * scale)

    print(
        f"Resizing image from "
        f"{w}x{h} -> {new_w}x{new_h}"
    )

    return cv2.resize(
        image,
        (new_w, new_h),
        interpolation=cv2.INTER_AREA
    )


def save_mask_overlay(image, masks, output_path):

    overlay = image.copy()

    rng = np.random.default_rng(42)

    for mask in masks:

        color = rng.integers(
            0,
            255,
            size=(3,),
            dtype=np.uint8
        )

        seg = mask["segmentation"]

        overlay[seg] = (
            0.5 * overlay[seg]
            + 0.5 * color
        ).astype(np.uint8)

    plt.figure(figsize=(10, 10))
    plt.imshow(overlay)
    plt.axis("off")

    plt.savefig(
        output_path,
        bbox_inches="tight",
        pad_inches=0
    )

    plt.close("all")


############################################
# FIND TIFF FILES
############################################

tiffs = [
    os.path.join(root, f)
    for root, _, files in os.walk(IMAGE_DIR)
    for f in files
    if f.lower().endswith((".tif", ".tiff"))
]

print(f"Found {len(tiffs)} GeoTIFFs")

############################################
# PROCESS TIFFS
############################################

for idx, tif_path in enumerate(tqdm(tiffs), start=1):

    print("\n" + "=" * 80)
    print(f"Processing {idx}/{len(tiffs)}")
    print(f"File: {tif_path}")

    try:

        name = os.path.splitext(
            os.path.basename(tif_path)
        )[0]

        ####################################
        # READ IMAGE
        ####################################

        image = read_geotiff(tif_path)

        image = resize_if_needed(image)

        ####################################
        # EMBEDDING
        ####################################

        print("Generating embedding...")

        with torch.no_grad():

            transformed = sam.preprocess(
                torch.as_tensor(
                    image,
                    device=device
                ).permute(2, 0, 1)[None]
            )

            embedding = sam.image_encoder(
                transformed
            )

        embedding_path = os.path.join(
            OUTPUT_DIR,
            f"{name}_embedding.pt"
        )

        torch.save(
            embedding.cpu(),
            embedding_path
        )

        del embedding
        del transformed

        if torch.cuda.is_available():
            torch.cuda.empty_cache()

        print("Embedding saved.")

        ####################################
        # MASK GENERATION
        ####################################

        print("Generating masks...")

        masks = mask_generator.generate(
            image
        )

        print(
            f"Generated {len(masks)} masks"
        )

        ####################################
        # SAVE OVERLAY
        ####################################

        overlay_path = os.path.join(
            OUTPUT_DIR,
            f"{name}_masks.png"
        )

        save_mask_overlay(
            image,
            masks,
            overlay_path
        )

        print(
            f"Finished {name}"
        )

    except Exception as e:

        print("\nERROR PROCESSING FILE:")
        print(tif_path)

        traceback.print_exc()

        break

print("\nDone.")