from datetime import datetime, timedelta
import logging
import os
import tempfile

from google.auth import compute_engine
from google.auth.transport import requests
from google.cloud import datastore, storage
from sendgrid import SendGridAPIClient, Mail
from wand.image import Image

# Config
PROJECT_ID = os.environ['PROJECT_ID']
CLOUD_STORAGE_BUCKET_ORIGINAL = os.environ['CLOUD_STORAGE_BUCKET_ORIGINAL']
CLOUD_STORAGE_BUCKET_RESIZED = os.environ['CLOUD_STORAGE_BUCKET_RESIZED']
SERVICE_ACCOUNT_EMAIL = os.environ['SERVICE_ACCOUNT_EMAIL']
SENDGRID_KEY = os.environ['SENDGRID_KEY']
IMAGE_RESIZE_X = int(os.environ['IMAGE_RESIZE_X'])
IMAGE_RESIZE_Y = int(os.environ['IMAGE_RESIZE_Y'])


logging.basicConfig(level=logging.INFO)


def _get_datastore_client():
    """
    Returns Cloud Datastore client.
    """
    return datastore.Client(PROJECT_ID)


def _get_storage_client():
    """
    Returns project storage file client.
    """
    return storage.Client(project=PROJECT_ID)


def _download_original_image(image_id):
    """
    Downloads original image from Bucket #1 and stores it
    in temporary file.
    """
    client = _get_storage_client()
    original_bucket = client.bucket(CLOUD_STORAGE_BUCKET_ORIGINAL)
    original_blob = original_bucket.get_blob(image_id)
    _, temp_local_filename = tempfile.mkstemp()
    original_blob.download_to_filename(temp_local_filename)
    content_type = original_blob.content_type
    return (temp_local_filename, content_type)


def _upload_resized_image(image_id, image_path, content_type):
    """
    Upload resized image to Bucket #2.
    """
    client = _get_storage_client()
    resized_bucket = client.bucket(CLOUD_STORAGE_BUCKET_RESIZED)
    resized_blob = resized_bucket.blob(image_id)
    resized_blob.upload_from_filename(
        image_path, content_type=content_type)

    return resized_blob.public_url


def _resize_image(file_path):
    """
    Resize image stored under given path.
    """
    with Image(filename=file_path) as image:
        image.resize(IMAGE_RESIZE_X, IMAGE_RESIZE_Y)
        image.save(filename=file_path)


def _get_image_metadata(image_id):
    """
    Retrieve image metadata.
    """
    ds = _get_datastore_client()
    key = ds.key('Image', int(image_id))
    return ds.get(key)


def _update_image_metadata(image_id, metadata):
    """
    Updates image metadata.
    """
    ds = _get_datastore_client()
    entity = _get_image_metadata(image_id)
    entity.update(metadata)
    ds.put(entity)


def _get_signed_image_url(bucket, image_id):
    """
    Given bucket name and image id generates signed URL to that image.
    """
    auth_request = requests.Request()
    client = _get_storage_client()
    data_bucket = client.bucket(bucket)
    image_blob = data_bucket.blob(image_id)
    expires_at_ms = datetime.now() + timedelta(minutes=30)
    signing_credentials = compute_engine.IDTokenCredentials(
        auth_request, "",
        service_account_email=SERVICE_ACCOUNT_EMAIL)
    return image_blob.generate_signed_url(
        expires_at_ms, credentials=signing_credentials)


def image_uploaded(data, context):
    """
    This is Cloud Function that takes freshly uploaded image,
    resize it and upload again to other bucket. After that it
    updates image metadata.
    """
    file_name = data['name']
    bucket_name = data['bucket']

    if bucket_name != CLOUD_STORAGE_BUCKET_ORIGINAL:
        return

    image_metadata = {}

    (image_path, content_type) = _download_original_image(file_name)

    try:
        _resize_image(image_path)
    except Exception as e:
        image_metadata['error'] = str(e)

    resized_image_url = _upload_resized_image(
        file_name, image_path, content_type)
    image_metadata['resized_url'] = resized_image_url

    _update_image_metadata(file_name, image_metadata)


def send_email(data, context):
    """
    This function detects that resized image was uploaded
    and send email notification to the owner of image.
    """
    file_name = data['name']
    bucket_name = data['bucket']

    if bucket_name != CLOUD_STORAGE_BUCKET_RESIZED:
        return

    image_data = _get_image_metadata(file_name)
    original_url = _get_signed_image_url(
        CLOUD_STORAGE_BUCKET_ORIGINAL, file_name)
    resized_url = _get_signed_image_url(
        CLOUD_STORAGE_BUCKET_RESIZED, file_name)

    if 'error' in image_data:
        error = image_data['error']
        message = "<p>Error when processing image: {}</p>".format(error)
    else:
        message = "<p>Good news!</p>"

    message += """Here are URLs to your images:<br><br>\n
    Original image - {0}<br><br>\n
    Resized image - {1}\n
    """.format(original_url, resized_url)

    user_email = image_data['user_email']
    mail = Mail(
        from_email='pzalas@example.com',
        to_emails=user_email,
        subject='Image processing finished!',
        html_content=message)

    sg = SendGridAPIClient(SENDGRID_KEY)
    response = sg.send(mail)
    response_log = 'SendGrid: {}, {}, {}'.format(
        response.status_code, response.body, response.headers)
    logging.info(response_log)
