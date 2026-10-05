# To Run SAM's Automatic Mask Generation mode

### Create environment
```bash
conda env create -f environment.yml
conda activate sam_satellite
```

### Download CRASAR Data
Note: I don't run this entire thing, just for a few seconds to get some data to experiment with.
```bash
python download_dataset.py
```

### Run SAM's Automatic Mask Generation mode
Note: generating masks takes the longest time (~20 min per image), so it's best to have a smaller dataset.
```bash
python sam_geotiff.py
```

### Visualize Embeddings
```bash
python embeddings_viz.py
```

### View Masks
Find the .png files under `./sam_outputs/`