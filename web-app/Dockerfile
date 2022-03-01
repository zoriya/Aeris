FROM node:17-alpine AS builder
WORKDIR /webapp
COPY package.json .
COPY package-lock.json .
RUN npm ci

ARG API_ROUTE
ENV REACT_APP_API_ROUTE=$API_ROUTE

ARG DISCORD_CLIENT_ID
ENV REACT_APP_DISCORD_CLIENT_ID=$DISCORD_CLIENT_ID

ARG GITHUB_CLIENT_ID
ENV REACT_APP_GITHUB_CLIENT_ID=$GITHUB_CLIENT_ID

ARG TWITTER_API_KEY
ENV REACT_APP_TWITTER_API_KEY=$TWITTER_API_KEY

ARG GOOGLE_CLIENT_ID
ENV REACT_APP_GOOGLE_CLIENT_ID=$GOOGLE_CLIENT_ID

ARG SPOTIFY_CLIENT_ID
ENV REACT_APP_SPOTIFY_CLIENT_ID=$SPOTIFY_CLIENT_ID
COPY . .
RUN npm run build

FROM nginx:1.21
COPY nginx.conf /etc/nginx/conf.d/default.conf
COPY --from=builder /webapp/build/ /usr/share/nginx/html
CMD cp /dist/aeris_android.apk /usr/share/nginx/html/client.apk; nginx -g "daemon off;"