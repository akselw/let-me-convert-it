FROM node:carbon-alpine

WORKDIR /usr/src/app

COPY package-lock.json package.json ./

COPY build/ ./build
COPY node_modules/ ./node_modules
COPY server/ ./server

EXPOSE 8080

CMD [ "npm", "start" ]
