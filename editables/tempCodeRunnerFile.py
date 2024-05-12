import os
from google.cloud import storage

# Set the environment variable
os.environ['GOOGLE_APPLICATION_CREDENTIALS'] = r"C:\Users\tomif\healthy-terrain-418120-bd2a22cd1281.json"

def download_blobs_in_folder(bucket_name, folder_name, destination_folder):
    """Downloads all blobs in a folder from the bucket."""
    storage_client = storage.Client()

    bucket = storage_client.bucket(bucket_name)
    blobs = bucket.list_blobs(prefix=folder_name)

    for blob in blobs:
        if blob.name.endswith('.csv'):
            destination_file_name = f"{destination_folder}/{blob.name.split('/')[-1]}"
            blob.download_to_filename(destination_file_name)
            print(f"Blob {blob.name} downloaded to {destination_file_name}.")

# Specify your bucket name, folder name, and destination folder
bucket_name = "taferna"
folder_name = r"exp_gbdt_02/ZZ0001"
destination_folder = r"C:\Users\tomif\Desktop\LABO LAPTOP\guantes blancos\gbdt_02\bajadas"

download_blobs_in_folder(bucket_name, folder_name, destination_folder)