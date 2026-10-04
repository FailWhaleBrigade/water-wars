FROM docker.io/library/nginx:trixie

COPY public/ /usr/share/nginx/html
