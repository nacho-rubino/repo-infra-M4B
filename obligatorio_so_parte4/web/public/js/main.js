
document.addEventListener('DOMContentLoaded', function() {
    const taskTableBody = document.getElementById('taskTableBody');
    const taskSearch = document.getElementById('taskSearch');
    const saveNewTaskBtn = document.getElementById('saveNewTask');
    const saveEditTaskBtn = document.getElementById('saveEditTask');
    const confirmDeleteBtn = document.getElementById('confirmDelete');
    const filterStatusLinks = document.querySelectorAll('.filter-status');

    const taskToast = document.getElementById('taskToast');
    const toast = new bootstrap.Toast(taskToast);
    const toastTitle = document.getElementById('toastTitle');
    const toastMessage = document.getElementById('toastMessage');


    function showNotification(title, message, type = 'success') {
        toastTitle.textContent = title;
        toastMessage.textContent = message;
        
        taskToast.classList.remove('bg-success', 'bg-danger', 'bg-warning', 'text-dark');
        
        if (type === 'success') {
            taskToast.classList.add('bg-success', 'text-white');
        } else if (type === 'error') {
            taskToast.classList.add('bg-danger', 'text-white');
        } else if (type === 'warning') {
            taskToast.classList.add('bg-warning', 'text-dark');
        }
        
        toast.show();
    }

    saveNewTaskBtn.addEventListener('click', function() {
        const title = document.getElementById('taskTitle').value;
        const description = document.getElementById('taskDescription').value;
        const priority = document.getElementById('taskPriority').value;
        const category = document.getElementById('taskCategory').value;
        
        if (!title) {
            showNotification('Error', 'El título es requerido', 'error');
            return;
        }
        
 
        showNotification('Procesando', 'Iniciando proceso de creación de tarea...', 'warning');
        
        
        fetch('/api/tasks', {
            method: 'POST',
            headers: {
                'Content-Type': 'application/json',
            },
            body: JSON.stringify({
                title,
                description,
                priority,
                category
            }),
        })
        .then(response => {
            if (!response.ok) {
                throw new Error('Error al crear la tarea');
            }
            return response.json();
        })
        .then(data => {

            const modal = bootstrap.Modal.getInstance(document.getElementById('newTaskModal'));
            modal.hide();
            

            document.getElementById('newTaskForm').reset();
            

            showNotification('Éxito', 'Tarea creada correctamente', 'success');
            

            setTimeout(() => {
                window.location.reload();
            }, 1000);
        })
        .catch(error => {
            console.error('Error:', error);
            showNotification('Error', error.message, 'error');
        });
    });


    taskTableBody.addEventListener('click', function(e) {

        if (e.target.closest('.edit-task')) {
            const row = e.target.closest('tr');
            const taskId = row.dataset.id;
            const title = row.cells[1].textContent;
            const description = row.cells[2].textContent;
            const priorityBadge = row.cells[3].querySelector('.badge');
            const categoryBadge = row.cells[4].querySelector('.badge');
            const statusBadge = row.cells[5].querySelector('.badge');
            
            let priority = 'media';
            if (priorityBadge.classList.contains('bg-danger')) {
                priority = 'alta';
            } else if (priorityBadge.classList.contains('bg-info')) {
                priority = 'baja';
            }
            
            let category = 'general';
            if (categoryBadge.textContent.trim() === 'conceptos') {
                category = 'conceptos';
            } else if (categoryBadge.textContent.trim() === 'práctica') {
                category = 'práctica';
            } else if (categoryBadge.textContent.trim() === 'investigación') {
                category = 'investigación';
            }
            
            let status = 'pendiente';
            if (statusBadge.textContent.trim() === 'En Progreso') {
                status = 'en-progreso';
            } else if (statusBadge.textContent.trim() === 'Completada') {
                status = 'completada';
            }
            

            document.getElementById('editTaskId').value = taskId;
            document.getElementById('editTaskTitle').value = title;
            document.getElementById('editTaskDescription').value = description;
            document.getElementById('editTaskPriority').value = priority;
            document.getElementById('editTaskCategory').value = category;
            document.getElementById('editTaskStatus').value = status;
        }
        

        if (e.target.closest('.delete-task')) {
            const row = e.target.closest('tr');
            const taskId = row.dataset.id;
            document.getElementById('deleteTaskId').value = taskId;
            

            const confirmDeleteModal = new bootstrap.Modal(document.getElementById('confirmDeleteModal'));
            confirmDeleteModal.show();
        }
    });


    saveEditTaskBtn.addEventListener('click', function() {
        const taskId = document.getElementById('editTaskId').value;
        const title = document.getElementById('editTaskTitle').value;
        const description = document.getElementById('editTaskDescription').value;
        const priority = document.getElementById('editTaskPriority').value;
        const category = document.getElementById('editTaskCategory').value;
        const status = document.getElementById('editTaskStatus').value;
        
        if (!title) {
            showNotification('Error', 'El título es requerido', 'error');
            return;
        }
        

        showNotification('Procesando', 'Actualizando tarea en memoria...', 'warning');
        
       
        fetch(`/api/tasks/${taskId}`, {
            method: 'PUT',
            headers: {
                'Content-Type': 'application/json',
            },
            body: JSON.stringify({
                title,
                description,
                priority,
                category,
                status
            }),
        })
        .then(response => {
            if (!response.ok) {
                throw new Error('Error al actualizar la tarea');
            }
            return response.json();
        })
        .then(data => {

            const modal = bootstrap.Modal.getInstance(document.getElementById('editTaskModal'));
            modal.hide();
            

            showNotification('Éxito', 'Tarea actualizada correctamente', 'success');
            

            setTimeout(() => {
                window.location.reload();
            }, 1000);
        })
        .catch(error => {
            console.error('Error:', error);
            showNotification('Error', error.message, 'error');
        });
    });

 
    confirmDeleteBtn.addEventListener('click', function() {
        const taskId = document.getElementById('deleteTaskId').value;

        showNotification('Procesando', 'Liberando espacio de memoria...', 'warning');
        
        fetch(`/api/tasks/${taskId}`, {
            method: 'DELETE',
        })
        .then(response => {
            if (!response.ok) {
                throw new Error('Error al eliminar la tarea');
            }
            return response.json();
        })
        .then(data => {
           
            const modal = bootstrap.Modal.getInstance(document.getElementById('confirmDeleteModal'));
            modal.hide();
            
           
            showNotification('Éxito', 'Tarea eliminada correctamente', 'success');
         
            setTimeout(() => {
                window.location.reload();
            }, 1000);
        })
        .catch(error => {
            console.error('Error:', error);
            showNotification('Error', error.message, 'error');
        });
    });

    taskSearch.addEventListener('keyup', function() {
        const searchTerm = this.value.toLowerCase();
        const rows = taskTableBody.querySelectorAll('tr');
        
        rows.forEach(row => {
            const title = row.cells[1].textContent.toLowerCase();
            const description = row.cells[2].textContent.toLowerCase();
            
            if (title.includes(searchTerm) || description.includes(searchTerm)) {
                row.style.display = '';
            } else {
                row.style.display = 'none';
            }
        });
    });


    filterStatusLinks.forEach(link => {
        link.addEventListener('click', function(e) {
            e.preventDefault();
            const status = this.dataset.status;
            const rows = taskTableBody.querySelectorAll('tr');
            
            rows.forEach(row => {
                if (status === 'all' || row.dataset.status === status) {
                    row.style.display = '';
                } else {
                    row.style.display = 'none';
                }
            });
            
    
            document.getElementById('filterDropdown').innerHTML = `<i class="bi bi-funnel"></i> ${this.textContent}`;
        });
    });


    taskTableBody.addEventListener('click', function(e) {
        if (e.target.classList.contains('status-badge')) {
            const row = e.target.closest('tr');
            const taskId = row.dataset.id;
            let currentStatus = row.dataset.status;
            let newStatus;
            

            if (currentStatus === 'pendiente') {
                newStatus = 'en-progreso';
            } else if (currentStatus === 'en-progreso') {
                newStatus = 'completada';
            } else {
                newStatus = 'pendiente';
            }

            fetch(`/api/tasks/${taskId}`, {
                method: 'PUT',
                headers: {
                    'Content-Type': 'application/json',
                },
                body: JSON.stringify({
                    status: newStatus
                }),
            })
            .then(response => {
                if (!response.ok) {
                    throw new Error('Error al actualizar el estado');
                }
                return response.json();
            })
            .then(data => {
                showNotification('Estado actualizado', `Tarea marcada como ${newStatus}`, 'success');

                row.dataset.status = newStatus;

                const badge = e.target;
                badge.className = 'badge status-badge';
                
                if (newStatus === 'pendiente') {
                    badge.classList.add('bg-secondary');
                    badge.textContent = 'Pendiente';
                } else if (newStatus === 'en-progreso') {
                    badge.classList.add('bg-warning', 'text-dark');
                    badge.textContent = 'En Progreso';
                } else {
                    badge.classList.add('bg-success');
                    badge.textContent = 'Completada';
                }
            })
            .catch(error => {
                console.error('Error:', error);
                showNotification('Error', error.message, 'error');
            });
        }
    });

  
    setTimeout(function() {
        showNotification('Sistema', 'Recursos cargados correctamente', 'success');
    }, 1000);
    
    setInterval(function() {
        fetch('/api/system-info')
            .then(response => response.json())
            .then(data => {

                console.log('Estadísticas del sistema actualizadas:', data);
            })
            .catch(error => {
                console.error('Error al obtener estadísticas:', error);
            });
    }, 30000);
});


function formatDate(dateString) {
    const options = { year: 'numeric', month: 'short', day: 'numeric', hour: '2-digit', minute: '2-digit' };
    return new Date(dateString).toLocaleDateString(undefined, options);
}

