#include <stdio.h>
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

int lectores_activos = 0;
pthread_mutex_t mutex_lectores;
sem_t sem_escritura;

typedef struct {
int id;
} thread_data_t;

void* pasajero(void* arg) {
thread_data_t* data = (thread_data_t*)arg;
int id = data->id;
usleep(rand() % (MAX_DELAY_PASAJERO + 1));
pthread_mutex_lock(&mutex_lectores);
lectores_activos++;
if(lectores_activos == 1) {
sem_wait(&sem_escritura);
}
pthread_mutex_unlock(&mutex_lectores);
printf("Pasajero %d esta mirando el cartel\n", id);
sleep(rand() % (MAX_DELAY_PASAJERO + 1));
pthread_mutex_lock(&mutex_lectores);
lectores_activos--;
if (lectores_activos == 0) {
sem_post(&sem_escritura);
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
sem_wait(&sem_escritura);
printf("Oficinista %d esta modificando el cartel (cambio %d/%d)\n", id, cambio, CAMBIOS_POR_OFICINISTA);
sleep(rand() % (MAX_DELAY_OFICINISTA + 1));
sem_post(&sem_escritura);
}
free(data);
return NULL;
}

int main(){
pthread_t hilos_pasajeros[NUM_PASAJEROS];
pthread_t hilos_oficinistas[NUM_OFICINISTAS];
srand(time(NULL));
pthread_mutex_init(&mutex_lectores, NULL);
sem_init(&sem_escritura, 0, 1);
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
sem_destroy(&sem_escritura);

return 0;

}





