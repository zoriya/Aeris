FROM node:17-alpine AS builder
WORKDIR /webapp
COPY package.json .
COPY package-lock.json .
RUN npm ci

ARG API_ROUTE
ENV REACT_APP_API_ROUTE=$API_ROUTE

ARG MOBILE_APK_LOCATION
ENV REACT_APP_MOBILE_APK_LOCATION=$MOBILE_APK_LOCATION 

COPY . .
RUN npm run build

FROM nginx:1.21
COPY nginx.conf /etc/nginx/conf.d/default.conf
COPY --from=builder /webapp/build/ /usr/share/nginx/html
CMD cp /dist/aeris_android.apk /usr/share/nginx/html/client.apk; nginx -g "daemon off;"
