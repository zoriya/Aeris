FROM node:17-alpine AS builder
WORKDIR /webapp
COPY package.json .
COPY package-lock.json .
RUN npm ci
COPY . .
RUN npm run build

FROM nginx:1.21
COPY nginx.conf /etc/nginx/conf.d/default.conf
COPY --from=builder /webapp/build/ /usr/share/nginx/html