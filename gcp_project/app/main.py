#!/usr/bin/env python


import json
import logging

from flask import current_app, Flask, redirect, session, render_template, \
    url_for, request
import httplib2
from oauth2client.contrib.flask_util import UserOAuth2

import config
import datastore
import storage


oauth2 = UserOAuth2()

app = Flask(__name__)
app.config.from_object(config)

logging.basicConfig(level=logging.INFO)


def _request_user_info(credentials):
    """
    Makes an HTTP request to the Google OAuth2 API to retrieve the user's basic
    profile information, including full name and photo, and stores it in the
    Flask session.
    """
    http = httplib2.Http()
    credentials.authorize(http)
    resp, content = http.request(
        'https://www.googleapis.com/oauth2/v3/userinfo')

    if resp.status != 200:
        current_app.logger.error(
            "Error while obtaining user profile: \n%s: %s", resp, content)
        return None

    session['profile'] = json.loads(content.decode('utf-8'))


def _include_image_urls(entry):
    image_id = str(entry.key.id)

    bucket_original = current_app.config['CLOUD_STORAGE_BUCKET_ORIGINAL']
    bucket_resized = current_app.config['CLOUD_STORAGE_BUCKET_RESIZED']

    if 'original_url' in entry:
        entry['original_url'] = storage.get_signed_image_url(
            bucket_original, image_id)

    if 'resized_url' in entry:
        entry['resized_url'] = storage.get_signed_image_url(
            bucket_resized, image_id)

    return entry


oauth2.init_app(
    app,
    scopes=['email', 'profile'],
    authorize_callback=_request_user_info)


@app.route("/")
def index():
    if 'profile' in session:
        return redirect(url_for('.main'), code=302)
    else:
        return render_template("login.html")


@app.route("/main", methods=['GET', 'POST'])
@oauth2.required
def main():
    user_email = session['profile']['email']
    params = {}

    if request.method == 'POST':
        try:
            data = request.form.to_dict(flat=True)
            image_title = data['title']
            image_file = request.files.get('image')
            image_filename = image_file.filename

            image_id = datastore.create_image(
                image_title, image_filename, user_email)

            stored_url = storage.store_image_file(str(image_id), image_file)

            datastore.update_image(image_id, {'original_url': stored_url})

            params['upload_success'] = stored_url
            params['upload_title'] = image_title
        except Exception as e:
            try:
                datastore.update_image(image_id, {'error': str(e)})
            except Exception:
                pass

            params['upload_failed'] = str(e)

    images_list = datastore.user_images(user_email)
    images_list = map(_include_image_urls, images_list)
    params['images'] = images_list
    return render_template("list.html", **params)


@app.route('/logout')
def logout():
    del session['profile']
    session.modified = True
    oauth2.storage.delete()
    return redirect(url_for('.index'), code=302)


@app.errorhandler(500)
def server_error(e):
    return """
    An internal error occurred: <pre>{}</pre>
    See logs for full stacktrace.
    """.format(e), 500
