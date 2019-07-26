from datetime import datetime, timedelta

from flask import current_app
from google.auth import compute_engine
from google.auth.transport import requests
from google.cloud import storage


def _get_storage_client():
    """
    Returns project storage file.
    """
    return storage.Client(project=current_app.config['PROJECT_ID'])


def _check_extension(filename):
    """
    Checks, if the supplied image file name has supported extension.
    """
    if ('.' not in filename or
            filename.split('.').pop().lower() not in ['jpg', 'png']):
        raise Exception(
            "{0} has an invalid name or extension".format(filename))


def store_image_file(file_id, file):
    """
    Store the user-uploaded file to Google Cloud Storage and retrieve its
    publicly-accessible URL.
    """
    if not file:
        raise Exception("No image to upload!")

    _check_extension(file.filename)

    public_url = upload_file(
        file.read(),
        file_id,
        file.content_type
    )

    current_app.logger.info(
        "Uploaded file %s as %s.", file.filename, public_url)

    return public_url


def upload_file(file_stream, filename, content_type):
    """
    Uploads a file to a given Cloud Storage bucket and returns the public url
    to the new object.
    """
    client = _get_storage_client()
    bucket = client.bucket(current_app.config['CLOUD_STORAGE_BUCKET_ORIGINAL'])
    blob = bucket.blob(filename)

    blob.upload_from_string(file_stream, content_type=content_type)

    url = blob.public_url

    if isinstance(url, bytes):
        url = url.decode('utf-8')

    return url


def get_signed_image_url(bucket, image_id):
    """
    Given bucket name and image id generates signed URL to that image.
    """
    auth_request = requests.Request()
    service_email = current_app.config['SERVICE_ACCOUNT_EMAIL']
    client = _get_storage_client()
    data_bucket = client.bucket(bucket)
    image_blob = data_bucket.blob(image_id)
    expires_at_ms = datetime.now() + timedelta(minutes=30)
    signing_credentials = compute_engine.IDTokenCredentials(
        auth_request, "",
        service_account_email=service_email)
    return image_blob.generate_signed_url(
        expires_at_ms, credentials=signing_credentials)
