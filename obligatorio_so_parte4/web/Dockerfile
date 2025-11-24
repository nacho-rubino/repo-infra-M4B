FROM node:18-alpine

# Carpeta de trabajo dentro del contenedor
WORKDIR /usr/src/app

# Copiamos sólo package.json y package-lock.json para aprovechar la cache
COPY package*.json ./

# Instalamos dependencias
RUN npm install --omit=dev

# Copiamos el resto del código de la app
COPY . .

# Entorno de producción (opcional pero recomendado)
ENV NODE_ENV=production

# La app escucha en el puerto 5000
EXPOSE 5000

# Arrancar usando el script "start" del package.json
CMD ["npm", "start"]
