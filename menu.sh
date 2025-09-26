#!/bin/bash

DATA_DIR="Datos"
INV_FILE="$DATA_DIR/inventario.csv"
USERS_FILE="$DATA_DIR/users.csv"
mkdir -p "$DATA_DIR"
mkdir -p "$DATA_DIR"
echo -n > "$INV_FILE"
echo "admin,admin" > "$USERS_FILE"
OUT_FILE="$DATA_DIR/datos.CSV"
: > "$OUT_FILE"



current_user=""
valid_types=("Base" "Layer" "Shade" "Dry" "Contrast" "Technical" "Texture" "Mediums")


user_exists() {
  local u="$1"
  grep -E -q "^${u}," "$USERS_FILE"
}

check_password() {
  local u="$1" p="$2"
  grep -E -q "^${u},${p}$" "$USERS_FILE"
}


create_user() {
  local u p
  read -r -p "Ingrese nuevo nombre de usuario: " u
  if [[ -z "$u" ]]; then echo "Usuario no puede ser vacio"; return 1; fi
  if user_exists "$u"; then echo "Usuario ya existe."; return 1; fi
  while :; do
    read -rs -p "Ingrese contraseña: " p; echo
    [[ -n "$p" ]] && break
    echo "La contrasena no puede ser vacia."
  done
  echo "$u,$p" >> "$USERS_FILE"
  echo "Usuario '$u' creado."
}


change_password() {
  local u p
  read -r -p "Ingrese su nombre de usuario: " u
  if ! user_exists "$u"; then echo "Usuario no existe."; return 1; fi
  while :; do
    read -rs -p "Ingrese su nueva contrasena: " p; echo
    [[ -n "$p" ]] && break
    echo "La contrasena no puede ser vacia."
  done
  tmp=$(mktemp)
  awk -F, -v usr="$u" -v pass="$p" 'BEGIN{OFS=","} {if($1==usr){$2=pass} print}' "$USERS_FILE" > "$tmp" && mv "$tmp" "$USERS_FILE"
  echo "Contrasena cambiada correctamente."
}



login() {
  local u p
  if [[ -n "$current_user" ]]; then
    echo "Ya hay un usuario con sesion iniciada: $current_user"
    return 0
  fi
  read -r -p "Usuario: " u
  if ! user_exists "$u"; then echo "Usuario no existe."; return 1; fi
  read -rs -p "Contrasena: " p; echo
  if check_password "$u" "$p"; then
    current_user="$u"
    echo "Se ha ingresado al sistema."
  else
    echo "Contrasena incorrecta."
  fi
}

logout() {
  if [[ -n "$current_user" ]]; then
    echo "Sesión de '$current_user' cerrada."
    current_user=""
  else
    echo "No hay ningún usuario con sesión iniciada."
  fi
}


is_valid_type() {
local t="$1"
for v in "${valid_types[@]}"; do
[[ "$t" == "$v" ]] && return 0
done
return 1
}

to_code() {
echo "$1" | cut -c1-3 | tr '[:lower:]' '[:upper:]'
}

read_nonempty() {
local prompt="$1" var
while :; do
read -r -p "$prompt" var
[[ -n "$var" ]] && echo "$var" && return 0
echo "No puede ser vacio"
done
}

read_int_ge0() {
  local prompt="$1" val
  while :; do
    read -r -p "$prompt" val
    if [[ "$val" =~ ^[0-9]+$ ]]; then
      echo "$val" && return 0
    fi
    echo "Debe ser un entero >= 0."
  done
}


ingresar_producto() {
  echo "=== Ingreso de Producto ==="
  echo "Tipos disponibles: ${valid_types[*]}"
  tipo=""
  while :; do
    read -r -p "Tipo: " tipo
    if is_valid_type "$tipo"; then break; fi
    echo "Tipo invalido. Pruebe exactamente uno de: ${valid_types[*]}"
  done

modelo=$(read_nonempty "Modelo: ")
descripcion=$(read_nonempty "Descripcion: ")
cantidad=$(read_int_ge0 "Cantidad (entero >= 0): ")
precio=$(read_int_ge0 "Precio unitario (entero): ")
codigo=$(to_code "$tipo")
esc_modelo=${modelo//,/;}
esc_desc=${descripcion//,/;}
echo "$codigo,$tipo,$esc_modelo,$esc_desc,$cantidad,$precio" >> "$INV_FILE"
echo "$codigo - $(echo "$tipo" | tr '[:upper:]' '[:lower:]') - $modelo - $descripcion - $cantidad - \$ $precio"
}

vender_producto() {
  if [[ ! -s "$INV_FILE" ]]; then
    echo "No hay productos en inventario."
    return
  fi

  echo "=== Lista de productos ==="
  productos=()
  i=0
  while IFS=',' read -r codigo tipo modelo desc cant precio; do
    productos+=("$codigo,$tipo,$modelo,$desc,$cant,$precio")
    echo "$((i+1)). $tipo - $modelo - \$ $precio (Stock: $cant)"
    ((i++))
  done < "$INV_FILE"

  carrito=()
  while :; do
    read -r -p "Ingrese número de producto (0 para terminar): " num
    [[ "$num" == "0" ]] && break
    [[ ! "$num" =~ ^[0-9]+$ ]] && { echo "Número inválido."; continue; }
    (( num < 1 || num > ${#productos[@]} )) && { echo "Número inválido."; continue; }

    IFS=',' read -r codigo tipo modelo desc cant precio <<< "${productos[$((num-1))]}"
    read -r -p "Cantidad a comprar: " qty
    [[ ! "$qty" =~ ^[0-9]+$ || "$qty" -lt 1 ]] && { echo "Cantidad inválida."; continue; }
    (( qty > cant )) && { echo "No hay suficiente stock. Disponible: $cant"; continue; }

    nuevo_stock=$((cant - qty))
    productos[$((num-1))]="$codigo,$tipo,$modelo,$desc,$nuevo_stock,$precio"

    carrito+=("$tipo,$modelo,$qty,$((qty*precio))")
    echo "Agregado: $modelo x$qty"
  done

 printf "%s\n" "${productos[@]}" > "$INV_FILE"

  if [[ ${#carrito[@]} -eq 0 ]]; then
    echo "No se realizó ninguna compra."
  else
    echo "=== Resumen de compra ==="
    total=0
    for item in "${carrito[@]}"; do
      IFS=',' read -r tipo modelo qty subtotal <<< "$item"
      echo "$tipo - $modelo - $qty - \$ $subtotal"
      total=$((total+subtotal))
    done
    echo "TOTAL: \$ $total"
  fi
}



filtrar_productos() {
if [[ ! -s "$INV_FILE" ]] then
echo "No hay productos en el inventario"
return
fi

echo "=== Filtro de productos por tipo ==="
echo "Tipos Disponibles: ${valid_types[*]}"
read -r -p "Ingrese Tipo (vacio para mostrar todos): " filter

filter_lc=$(echo "$filter" | tr '[:upper:]' '[:lower:]')

i=0
hubo_salida=0
while IFS=',' read -r codigo tipo modelo desc cant precio; do
type_lc=$(echo "$tipo" | tr '[:upper:]' '[:lower:]')
if [[ -z "$filter_lc" || "$type_lc" == "$filter_lc" ]]; then
echo "$((i+1)). $codigo - $(echo "$tipo" | tr '[:upper:]' '[:lower:]') - $modelo - $desc - $cant - \$ $precio"
hubo_salida=1
fi
((i++))
done < "$INV_FILE"

[[ $hubo_salida -eq 0 ]] && echo "No se encontraron productos para ese filtro."
}


crear_reporte() {
if [[ ! -s "$INV_FILE" ]]; then
echo "No hay productos en inventario para generar el reporte."
return
fi

local OUT_FILE="$DATA_DIR/datos.CSV"
{
echo "codigo,tipo,modelo,descripcion,cantidad,precio"
cat "$INV_FILE"
} > "$OUT_FILE"

echo "Reporte generado en: $OUT_FILE"

}











while true; do
	echo "=== Menu Principal ==="
	echo "1. Usuario"
	echo "2. Ingresar Producto"
	echo "3. Vender Producto"
	echo "4. Filtro De Productos"
	echo "5. Crear Reporte De Pinturas"
	echo "6. Salir"
	read -p "Elige una opcion: " mainchoice

case $mainchoice in


1)

while true; do
        echo "=== Menu De Usuario ==="
        echo "1. Crear Usuario"
        echo "2. Cambiar Contrasena"
        echo "3. Iniciar Sesion"
        echo "4. Cerrar Sesion"
        echo "5. Salir"
        read -p "Elige una opcion: " choice

        case $choice in
                1)create_user ;;
                2)change_password ;;
                3)login ;;
		4)logout ;;
                5)break ;;
		*)echo "Opcion Invalida" ;;
esac
done
;;
	2)
	if [ -n "$current_user" ]; then
	ingresar_producto
	else
	echo "Se necesita un usuario con sesion iniciada para entrar"
	fi
;;

    3)
        if [ -n "$current_user" ]; then
	vender_producto
        else
        echo "Se necesita un usuario con sesion iniciada para entrar"
        fi
;;

    4)
        if [ -n "$current_user" ]; then
        filtrar_productos
        else
        echo "Se necesita un usuario con sesion iniciada para entrar"
        fi
;;

    5)
        if [ -n "$current_user" ]; then
        crear_reporte
        else
        echo "Se necesita un usuario con sesion iniciada para entrar"
        fi
;;



	6)
	exit 0
;;

	*)
	echo "Opcion Invalida"
;;
esac
done
