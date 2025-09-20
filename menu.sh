#!/bin/bash

current_user=""

while IFS=: read -r user _; do
    [ "$user" != "root" ] && userdel -r "$user" 2>/dev/null
done < /etc/passwd

if ! id "admin" &>/dev/null; then
        useradd -m -U "admin"
        echo "admin:admin" | chpasswd
fi

declare -A passwords
passwords[admin]="admin"

while true; do
	echo "=== Menu Principal ==="
	echo "1. Usuario"
	echo "2. Ingresar Producto (WIP)"
	echo "3. Vender Producto (WIP)"
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
                1)
                read -p "Ingrese  nuevo nombre de usuario: " username
                if id "$username" &>/dev/null; then
                echo "Usuario ya existe."
                else
                password=""
                while [ -z "$password" ]; do
                read -s -p "Ingrese Contrasena: " password
                echo
                if [ -z "$password" ]; then
                echo "Contrasena no puede ser vacia"
               else
                useradd -m -U "$username"
                echo "$username:$password" | chpasswd
                passwords[$username]="$password"
                echo " Usuario '$username' creado."
                fi
                done
                fi
;;


                2)
                read -p "Ingrese su nombre de usuario: " username
                if  id "$username" &>/dev/null; then
                passwd -d "$username" &>/dev/null
                read -s -p "Ingrese su nueva contrasena: " password
                echo
                if [ -z "$password" ]; then
                echo "La contrasena no puede ser vacia"
                else
                echo "$username:$password" | chpasswd
                passwords[$username]="$password"
                echo "Contrasena cambiada correctamente"
                fi
                else
                echo "Usuario no existe."
                fi
                ;;


                3)
		if [ -n "$current_user" ]; then
		echo "Ya hay un usuario con sesion iniciada: $current_user"
		else
                read -p "Ingrese su nombre de usuario: " username
                if id "$username" &>/dev/null; then
                read -s -p "Ingrese su contrasena: " password
                echo
                if [ "${passwords[$username]}" = "$password" ]; then
                echo " Se ha ingresado al sistema"
		current_user="$username"
		else
                echo "Contrasena Incorrecta"
                fi
                else
                echo "Usuario no existe"
                fi
		fi
;;

		4) 
		if [ -n "$current_user" ]; then
		echo "Sesion de '$current_user' cerrada."
		current_user=""
		else 
		echo "No hay ningun usuario con sesion iniciada."
		fi
;;



                5)
                break 
;;
esac
done
;;
	6)
	exit 0
;;
esac
done
