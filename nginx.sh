#!/bin/sh

exec nginx -p fixtures/nginx -c nginx.conf -g 'daemon off;'
