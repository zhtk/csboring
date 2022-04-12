import os

SECRET_KEY = os.environ['FLASK_SECRET_KEY']
PROJECT_ID = os.environ['PROJECT_ID']
CLOUD_STORAGE_BUCKET_ORIGINAL = os.environ['CLOUD_STORAGE_BUCKET_ORIGINAL']
CLOUD_STORAGE_BUCKET_RESIZED = os.environ['CLOUD_STORAGE_BUCKET_RESIZED']
SERVICE_ACCOUNT_EMAIL = os.environ['SERVICE_ACCOUNT_EMAIL']
GOOGLE_OAUTH2_CLIENT_ID = os.environ['GOOGLE_OAUTH2_CLIENT_ID']
GOOGLE_OAUTH2_CLIENT_SECRET = os.environ['GOOGLE_OAUTH2_CLIENT_SECRET']