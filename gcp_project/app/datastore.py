from flask import current_app
from google.cloud import datastore


def get_client():
    return datastore.Client(current_app.config['PROJECT_ID'])


def user_images(email):
    ds = get_client()
    query = ds.query(
        kind='Image',
        filters=[
            ('user_email', '=', email)
        ]
    )

    return list(query.fetch())


def create_image(title, filename, user_email):
    ds = get_client()

    key = ds.key('Image')
    entity = datastore.Entity(
        key=key,
        exclude_from_indexes=['filename', 'error'])

    data = {
        'title': title,
        'filename': filename,
        'user_email': user_email,
    }

    entity.update(data)
    ds.put(entity)

    return entity.key.id


def update_image(id, data):
    ds = get_client()

    key = ds.key('Image', id)

    entity = ds.get(key)
    entity.update(data)
    ds.put(entity)


def delete_image_data(id):
    ds = get_client()
    ds.key('Image', id).delete()
