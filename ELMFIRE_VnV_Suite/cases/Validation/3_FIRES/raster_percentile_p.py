#!/usr/bin/env python3

import os
import glob
import numpy as np
import rasterio
from collections import defaultdict
from PIL import Image
import matplotlib.pyplot as plt
from concurrent.futures import ProcessPoolExecutor
from itertools import product
import argparse


"""
Example usage:
python raster_percentile_p.py \
  --root_dir /home/dwip/elmfire/firedx-main/firedx/outputs_NH_AOI_FLENGTH/Tubbs \
  --output_types time_of_arrival flame_length \
  --percentile_vals 50 90
"""


def extract_output_and_time_bucket(filename):
    basename = os.path.basename(filename)
    output = basename[:-20]
    timestamp = basename.split("_")[-1][:-4]
    time_bucket = round(int(timestamp), -2)
    return output, time_bucket


def apply_viridis(array):
    normed_data = (array - array.min()) / (array.max() - array.min())
    colored_data = plt.cm.viridis(normed_data)
    alpha_channel = np.ones(array.shape, dtype=np.uint8) * 255
    alpha_channel[array == 0] = 0
    colored_data_with_alpha = np.dstack((colored_data[:, :, :3] * 255, alpha_channel))
    return colored_data_with_alpha.astype("uint8")


def process_raster_files(dir, output_types, percentile_vals):
    post_processed_dir = os.path.join(dir, "post-processed")
    png_dir = os.path.join(post_processed_dir, "png-files")

    os.makedirs(post_processed_dir, exist_ok=True)
    os.makedirs(png_dir, exist_ok=True)

    files_by_output_and_time_bucket = defaultdict(lambda: defaultdict(list))

    for file in glob.glob(os.path.join(dir, "*.tif")):
        for ot in output_types:
            if ot in os.path.basename(file):
                _, time_bucket = extract_output_and_time_bucket(file)
                files_by_output_and_time_bucket[ot][time_bucket].append(file)

    for output, time_buckets in files_by_output_and_time_bucket.items():
        for time_bucket, raster_files in time_buckets.items():
            print(f"Processing {output} in time bucket {time_bucket}")

            masked_stacked_data = []

            for file in raster_files:
                with rasterio.open(file) as src:
                    if src.count != 1:
                        print(f"Skipping {file} as it has multiple bands.")
                        continue
                    band = src.read(1)
                    no_data_value = np.float32(-9999)
                    masked_band = np.ma.masked_array(band, mask=(band == no_data_value))
                    masked_stacked_data.append(masked_band)

            stacked_data = np.ma.stack(masked_stacked_data)

            percentile_arrays = {
                percentile_val: np.full(
                    stacked_data.shape[1:], no_data_value, dtype=np.float32
                )
                for percentile_val in percentile_vals
            }

            for i in range(stacked_data.shape[1]):
                for j in range(stacked_data.shape[2]):
                    pixel_values = stacked_data[
                        :, i, j
                    ].compressed()  # Compress to remove nodata values
                    if pixel_values.size > 0:
                        for percentile_val in percentile_vals:
                            percentile_arrays[percentile_val][i, j] = np.percentile(
                                np.asarray(pixel_values), percentile_val
                            )

            for percentile_val, percentile_array in percentile_arrays.items():
                with rasterio.open(raster_files[0]) as src:
                    meta = src.meta
                    meta.update({"nodata": -9999})
                    output_file = os.path.join(
                        post_processed_dir,
                        f"{output}_pct_{percentile_val}_time_{time_bucket}.tif",
                    )
                    with rasterio.open(output_file, "w", **meta) as dest:
                        dest.write(percentile_array.astype(rasterio.float32), 1)

                valid_data_png = percentile_array[percentile_array != no_data_value]
                if valid_data_png.size > 0 and np.nanmax(valid_data_png) != np.nanmin(
                    valid_data_png
                ):
                    normalized_data = (
                        (percentile_array - np.nanmin(valid_data_png))
                        / (np.nanmax(valid_data_png) - np.nanmin(valid_data_png))
                        * 255
                    ).astype("uint8")
                else:
                    normalized_data = np.zeros_like(
                        percentile_array, dtype="uint8"
                    )  # Fill with zeros if no valid range

                png_file_path = os.path.join(
                    png_dir, f"{output}_pct_{percentile_val}_time_{time_bucket}.png"
                )
                Image.fromarray(normalized_data).save(png_file_path)

                png_file_path_viridis = os.path.join(
                    png_dir, f"{output}_pct_{percentile_val}_time_{time_bucket}_v.png"
                )
                Image.fromarray(apply_viridis(normalized_data)).save(
                    png_file_path_viridis
                )

    print(f"Processing complete for {dir}")


def process_raster_files_wrapper(args):
    dir, output_types, percentile_vals = args
    process_raster_files(dir, output_types, percentile_vals)
    print(f"Processing task: {args}")


def process_outputs_in_parallel(root_dir, output_types, percentile_vals):
    subdirectories = glob.glob(os.path.join(root_dir, "*/"))
    if not subdirectories:
        subdirectories = [root_dir]

    tasks = [
        (subdir, [output_type], percentile_vals)
        for subdir in subdirectories
        for output_type in output_types
    ]

    with ProcessPoolExecutor() as executor:
        executor.map(process_raster_files_wrapper, tasks)


def main():
    parser = argparse.ArgumentParser(
        description="Process raster files in subdirectories with parallelization."
    )
    parser.add_argument(
        "--root_dir",
        type=str,
        required=True,
        help="Root directory containing outputs or subdirectories with outputs.",
    )
    parser.add_argument(
        "--output_types",
        nargs="+",
        default=["time_of_arrival"],
        help="List of output types to process (e.g., 'time_of_arrival', 'flame_length').",
    )
    parser.add_argument(
        "--percentile_vals",
        nargs="+",
        type=int,
        default=[50],
        help="Percentile value to calculate (default: 50).",
    )

    args = parser.parse_args()

    print(f"Root Directory: {args.root_dir}")
    print(f"Output Types: {args.output_types}")
    print(f"Percentile Value: {args.percentile_vals}")

    process_outputs_in_parallel(args.root_dir, args.output_types, args.percentile_vals)


if __name__ == "__main__":
    main()
