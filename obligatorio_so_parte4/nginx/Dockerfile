FROM nginx:alpine

# Borramos el default.conf que viene por defecto
RUN rm /etc/nginx/conf.d/default.conf || true

# Copiamos nuestra config al directorio conf.d
# (la config principal ya tiene: http { include /etc/nginx/conf.d/*.conf; }
COPY nginx.conf /etc/nginx/conf.d/loadbalancer.conf
