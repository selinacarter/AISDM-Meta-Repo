import torch
import rasterio
import matplotlib.pyplot as plt
import numpy as np

# ---- Load embedding ----
embedding = torch.load(
    "sam_outputs/1002-Palm-Acers.2.geo.tif_10300100DB064000-visual.tif.geo_embedding.pt",
    map_location="cpu"
)

print(type(embedding))
print(embedding.shape)
print(embedding.dtype)
print(embedding.min())
print(embedding.max())

# ---- Load GeoTIFF ----
geotiff_path = "./crasar_data/train/imagery/SATELLITE/1002-Palm-Acers.2.geo.tif_10300100DB064000-visual.tif.geo.tif"

with rasterio.open(geotiff_path) as src:
    img = src.read()

# Convert TIFF to RGB for display
if img.shape[0] >= 3:
    rgb = np.moveaxis(img[:3], 0, -1).astype(np.float32)
    rgb -= rgb.min()
    rgb /= (rgb.max() + 1e-8)
else:
    rgb = img[0]

# ---- Plot 5x5 grid ----
fig, axes = plt.subplots(5, 5, figsize=(18, 18))
axes = axes.ravel()

# Panel 0: original GeoTIFF
axes[0].imshow(rgb)
axes[0].set_title("GeoTIFF")
axes[0].axis("off")

# Panels 1-24: embedding channels
for ch in range(24):
    ax = axes[ch + 1]

    im = ax.imshow(
        embedding[0, ch].numpy(),
        cmap="viridis"
    )
    ax.set_title(f"Emb Ch {ch}")
    ax.axis("off")

# One shared colorbar for embeddings
fig.colorbar(
    im,
    ax=axes[1:].tolist(),
    fraction=0.02,
    pad=0.01
)

plt.tight_layout()
plt.show()