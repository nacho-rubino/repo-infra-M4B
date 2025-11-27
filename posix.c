#include <stdio.h>
<<<<<<< HEAD

=======
>>>>>>> c2d7810510bfe39a416e6edfe2bb0d03a8079f8d
#include <stdlib.h>
#include <pthread.h>
#include <semaphore.h>
#include <unistd.h>
#include <time.h>

#define NUM_PASAJEROS 100
#define NUM_OFICINISTAS 5
#define CAMBIOS_POR_OFICINISTA 3
#define MAX_DELAY_OFICINISTA 5
#define MAX_DELAY_PASAJERO 3
#define TOTAL_ESCRITURAS (NUM_OFICINISTAS * CAMBIOS_POR_OFICINISTA)

int lectores_activos = 0;
int escritura_actual = 0;
int contador_por_grupo[TOTAL_ESCRITURAS];
int asignacion_grupos[NUM_PASAJEROS];
pthread_mutex_t mutex_lectores;
pthread_mutex_t mutex_escritura;
pthread_mutex_t mutex_contador[TOTAL_ESCRITURAS];
sem_t sem_escritura;
sem_t sem_puede_escribir;
sem_t sem_puede_leer[TOTAL_ESCRITURAS];

typedef struct {
int id;
} thread_data_t;

int pasajeros_por_grupo(int grupo) {
int resto = NUM_PASAJEROS % TOTAL_ESCRITURAS;
if (grupo < resto) {
return (NUM_PASAJEROS / TOTAL_ESCRITURAS) + 1;
} else {
return NUM_PASAJEROS / TOTAL_ESCRITURAS;
}
}

void barajar_pasajeros() {
int idx = 0;
for (int grupo = 0; grupo < TOTAL_ESCRITURAS; grupo++) {
int cant = pasajeros_por_grupo(grupo);
for (int j = 0; j < cant; j++) {
asignacion_grupos[idx] = grupo;
idx++;
}
}
for (int i = NUM_PASAJEROS - 1; i > 0; i--) {
int j = rand() % (i + 1);
int temp = asignacion_grupos[i];
asignacion_grupos[i] = asignacion_grupos[j];
asignacion_grupos[j] = temp;
}
}

void* pasajero(void* arg) {
thread_data_t* data = (thread_data_t*)arg;
int id = data->id;
int escritura_asignada = asignacion_grupos[id - 1];
sem_wait(&sem_puede_leer[escritura_asignada]);
sem_post(&sem_puede_leer[escritura_asignada]);
usleep(rand() % (MAX_DELAY_PASAJERO + 1) * 1000);
pthread_mutex_lock(&mutex_lectores);
lectores_activos++;
if (lectores_activos == 1) {
sem_wait(&sem_escritura);
}
pthread_mutex_unlock(&mutex_lectores);
printf("Pasajero %d esta mirando el cartel\n", id);
sleep(rand() % (MAX_DELAY_PASAJERO + 1));
pthread_mutex_lock(&mutex_contador[escritura_asignada]);
contador_por_grupo[escritura_asignada]++;
int pasajeros_en_grupo = pasajeros_por_grupo(escritura_asignada);
int soy_ultimo = (contador_por_grupo[escritura_asignada] == pasajeros_en_grupo);
pthread_mutex_unlock(&mutex_contador[escritura_asignada]);
pthread_mutex_lock(&mutex_lectores);
lectores_activos--;
if (lectores_activos == 0) {
sem_post(&sem_escritura);
if (soy_ultimo) {
sem_post(&sem_puede_escribir);
}
}
pthread_mutex_unlock(&mutex_lectores);
free(data);
return NULL;
}

void* oficinista(void* arg) {
thread_data_t* data = (thread_data_t*)arg;
int id = data->id;
for (int cambio = 1; cambio <= CAMBIOS_POR_OFICINISTA; cambio++) {
sleep(rand() % (MAX_DELAY_OFICINISTA + 1));
pthread_mutex_lock(&mutex_escritura);
int num_escritura = escritura_actual;
escritura_actual++;
pthread_mutex_unlock(&mutex_escritura);
if (num_escritura > 0) {
sem_wait(&sem_puede_escribir);
}
sem_wait(&sem_escritura);
printf("Oficinista %d esta modificando el cartel (cambio %d/%d)\n", id, cambio, CAMBIOS_POR_OFICINISTA);
sleep(rand() % (MAX_DELAY_OFICINISTA + 1));
sem_post(&sem_escritura);
if (num_escritura < TOTAL_ESCRITURAS) {
sem_post(&sem_puede_leer[num_escritura]);
}
}
free(data);
return NULL;
}

int main(){
pthread_t hilos_pasajeros[NUM_PASAJEROS];
pthread_t hilos_oficinistas[NUM_OFICINISTAS];
srand(time(NULL));
barajar_pasajeros();
pthread_mutex_init(&mutex_lectores, NULL);
pthread_mutex_init(&mutex_escritura, NULL);
for (int i = 0; i < TOTAL_ESCRITURAS; i++) {
pthread_mutex_init(&mutex_contador[i], NULL);
contador_por_grupo[i] = 0;
}
sem_init(&sem_escritura, 0, 1);
sem_init(&sem_puede_escribir, 0, 0);
for (int i = 0; i < TOTAL_ESCRITURAS; i++) {
sem_init(&sem_puede_leer[i], 0, 0);
}
printf("Sistema de Panel de Vuelos Iniciado\n");
printf("========================================\n");
printf("Pasajeros: %d | Oficinistas: %d\n\n", NUM_PASAJEROS, NUM_OFICINISTAS);
for (int i = 0; i < NUM_OFICINISTAS; i++) {
thread_data_t* data = malloc(sizeof(thread_data_t));
data->id = i + 1;
pthread_create(&hilos_oficinistas[i], NULL, oficinista, data);
}
for (int i = 0; i < NUM_PASAJEROS; i++) {
thread_data_t* data = malloc(sizeof(thread_data_t));
data->id = i + 1;
pthread_create(&hilos_pasajeros[i], NULL, pasajero, data);
usleep(10000);
}
for (int i = 0; i < NUM_OFICINISTAS; i++) {
pthread_join(hilos_oficinistas[i], NULL);
}
for (int i = 0; i < NUM_PASAJEROS; i++) {
pthread_join(hilos_pasajeros[i], NULL);
}
printf("\n========================================\n");
printf("Todos los pasajeros y oficinistas han terminado\n");
pthread_mutex_destroy(&mutex_lectores);
pthread_mutex_destroy(&mutex_escritura);
for (int i = 0; i < TOTAL_ESCRITURAS; i++) {
pthread_mutex_destroy(&mutex_contador[i]);
}
sem_destroy(&sem_escritura);
sem_destroy(&sem_puede_escribir);
for (int i = 0; i < TOTAL_ESCRITURAS; i++) {
sem_destroy(&sem_puede_leer[i]);
}
return 0;
}
<<<<<<< HEAD
=======

>>>>>>> c2d7810510bfe39a416e6edfe2bb0d03a8079f8d
