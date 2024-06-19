#include <stdio.h>
#include <stdlib.h>
#include <module.h>
#include <value.h>
#include <core/error.h>
#include "cons.h"

#if defined(_WIN32) || defined(_WIN64)
#include <windows.h>
#define sleep_ Sleep
#else
#include <unistd.h>
#define sleep_(x) sleep(x / 1000)
#endif

#define MAX_THREADS 50

struct threads {
  thread_t threads[MAX_THREADS];
  Value returns[MAX_THREADS];
  int count;
  pthread_mutex_t mutex_count;
};

static struct threads threads = { 
  .count = -1,
  .mutex_count = PTHREAD_MUTEX_INITIALIZER
};

struct thread_data {
  Value thread;
  Value handler;
  Module* mod;
};

Value thread_sleep(size_t argc, Module* mod, Value* args) {
  ASSERT_ARGC("sleep", 1, argc);
  ASSERT_TYPE("sleep", args[0], TYPE_INTEGER);

  sleep_(GET_INT(args[0]));

  return make_unit(mod->gc);
}

// Define a simple thread function
#if defined(_WIN32) || defined(_WIN64)
DWORD WINAPI thread_function(LPVOID arg) {
    int *num = (int *)arg;
    printf("Hello from thread! Argument: %d\n", *num);
    Sleep(1000); // Sleep for 1000 milliseconds (1 second)
    printf("Thread finishing\n");
    return 0;
}
#else
void* thread_function(void *arg) {
  struct thread_data *data = (struct thread_data *)arg;
  Value handler = data->handler;
  Module* mod = data->mod;
  
  gc_pause(&mod->gc);
  Value result = mod->call_threaded(mod, handler, 2, (Value[]) { data->thread });

  threads.returns[threads.count] = result;

  gc_resume(&mod->gc);

  return NULL;
}
#endif

Value create_thread(size_t argc, Module* mod, Value* args) {
  // gc_pause(&mod->gc);
  #define thread_ threads.threads[threads.count]

  pthread_mutex_lock(&threads.mutex_count);
  threads.count++;
  pthread_mutex_unlock(&threads.mutex_count);

  HeapValue* thread_val = (HeapValue*) gc_malloc(&mod->gc, sizeof(HeapValue));
  thread_val->type = TYPE_THREAD;
  thread_val->as_any = thread_;

  struct thread_data *data = gc_malloc(&mod->gc, sizeof(struct thread_data));
  data->handler = args[0];
  data->mod = mod;
  data->thread = MAKE_PTR(thread_val);

#if defined(_WIN32) || defined(_WIN64)
    *thread = CreateThread(NULL, 0, (LPTHREAD_START_ROUTINE)start_routine, arg, 0, NULL);
    return *thread ? 0 : -1;
#else
    int res = pthread_create(&thread_, NULL, thread_function, data);
  

    return MAKE_INTEGER(threads.count);
#endif
}

// Function to join a thread
Value join_thread(size_t argc, Module* mod, Value* args) {
  ASSERT_ARGC("join_thread", 1, argc);
  ASSERT_TYPE("join_thread", args[0], TYPE_INTEGER);
  
#if defined(_WIN32) || defined(_WIN64)
    WaitForSingleObject(thread, INFINITE);
    CloseHandle(thread);
    return 0;
#else
    Value thread_id = args[0];
    int th_id = GET_INT(thread_id);

    if (th_id >= threads.count && th_id < 0) {
      THROW("Thread ID out of range");
    }
    
    thread_t thread_ptr = threads.threads[th_id];

    int res = pthread_join(thread_ptr, NULL);

    switch (res) {
      case ESRCH: {
        THROW("No thread could be found corresponding to that specified by the given thread ID.");
      }

      case EINVAL: {
        THROW("The value specified by thread does not refer to a joinable thread.");
      }

      case EDEADLK: {
        THROW("A deadlock was detected or the value of thread specifies the calling thread.");
      }
    }

    return threads.returns[th_id];
#endif
}