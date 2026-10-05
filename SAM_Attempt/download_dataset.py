from huggingface_hub import snapshot_download

snapshot_download(
    repo_id="CRASAR/CRASAR-U-DROIDs",
    repo_type="dataset",
    local_dir="./crasar_data",
    allow_patterns=[
        "train/imagery/SATELLITE/*"
    ]
)