#include <core/error.h>
#include <curl/curl.h>
#include <module.h>
#include <stdio.h>
#include <value.h>

#include "cons.h"

struct MemoryStruct {
  char* memory;
  size_t size;
  GarbageCollector gc;
};

size_t write_callback(void* contents, size_t size, size_t nmemb,
                      struct MemoryStruct* mem) {
  size_t realsize = size * nmemb;
  char* ptr = realloc(mem->memory, mem->size + realsize + 1);
  if (ptr == NULL) {
    fprintf(stderr, "Failed to allocate memory\n");
    return 0;
  }

  mem->memory = ptr;
  memcpy(&(mem->memory[mem->size]), contents, realsize);
  mem->size += realsize;
  mem->memory[mem->size] = 0;

  return realsize;
}

Value fetch(size_t argc, Module* mod, Value* args) {
  ASSERT_FMT(argc == 1, "expected 1 argument, but got %zu", argc);
  Value url = args[0];
  ASSERT_FMT(get_type(url) == TYPE_STRING, "expected string, but got %s",
             type_of(url));

  const char* url_str = GET_STRING(url);

  struct MemoryStruct mem;
  mem.gc = gc;
  mem.memory = gc_malloc(&mod->gc, 1);
  mem.size = 0;

  CURL* curl = curl_easy_init();
  if (!curl)
    return make_err(mod->gc, MAKE_STRING(mod->gc, "Failed to initialize curl"));

  curl_easy_setopt(curl, CURLOPT_URL, url_str);
  curl_easy_setopt(curl, CURLOPT_WRITEFUNCTION, write_callback);
  curl_easy_setopt(curl, CURLOPT_WRITEDATA, &mem);
  CURLcode res = curl_easy_perform(curl);

  if (res != CURLE_OK) {
    char* err = (char*)curl_easy_strerror(res);
    curl_easy_cleanup(curl);
    return make_err(mod->gc, MAKE_STRING(mod->gc, err));
  }

  char* result = gc_strdup(&mod->gc, mem.memory);

  curl_easy_cleanup(curl);
  return make_ok(mod->gc, MAKE_STRING(mod->gc, result));
}

Value call_callback(size_t argc, Module* mod, Value* args) {
  ASSERT_FMT(argc == 1, "expected 1 argument, but got %zu", argc);
  Value callback = args[0];
  ASSERT_FMT(get_type(callback) == TYPE_LIST, "expected list, but got %s",
             type_of(callback));

  Value result = mod->call_function(mod, callback, 2, (Value[]) {
    MAKE_INTEGER(10)
  });

  return MAKE_STRING(mod->gc, "test");
}

#ifdef _WIN32
#include <winsock2.h>
#include <ws2tcpip.h>
#pragma comment(lib, "ws2_32.lib")
#else
#include <arpa/inet.h>
#include <netinet/in.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#endif

#define BUFFER_SIZE 1024

void handle_client(int client_socket, Module *mod, Value handler);

Value create_server(size_t argc, Module* mod, Value* args) {
  int PORT = GET_INT(args[0]);
  Value start_message = args[1];
  Value handler = args[2];

#ifdef _WIN32
  WSADATA wsaData;
  if (WSAStartup(MAKEWORD(2, 2), &wsaData) != 0) {
    fprintf(stderr, "WSAStartup failed.\n");
    exit(1);
  }
#endif

  int server_socket;
  struct sockaddr_in server_addr;

#ifdef _WIN32
  server_socket = socket(AF_INET, SOCK_STREAM, IPPROTO_TCP);
  if (server_socket == INVALID_SOCKET) {
    fprintf(stderr, "Socket creation failed.\n");
    WSACleanup();
    exit(1);
  }
#else
  server_socket = socket(AF_INET, SOCK_STREAM, 0);
  if (server_socket < 0) {
    perror("Socket creation failed");
    exit(EXIT_FAILURE);
  }
#endif

  memset(&server_addr, 0, sizeof(server_addr));
  server_addr.sin_family = AF_INET;
  server_addr.sin_addr.s_addr = INADDR_ANY;
  server_addr.sin_port = htons(PORT);

#ifdef _WIN32
  if (bind(server_socket, (struct sockaddr*)&server_addr,
           sizeof(server_addr)) == SOCKET_ERROR) {
    fprintf(stderr, "Bind failed.\n");
    closesocket(server_socket);
    WSACleanup();
    exit(1);
  }
#else
  if (bind(server_socket, (struct sockaddr*)&server_addr, sizeof(server_addr)) <
      0) {
    perror("Bind failed");
    close(server_socket);
    exit(EXIT_FAILURE);
  }
#endif

  if (listen(server_socket, 10) < 0) {
    perror("Listen failed");
#ifdef _WIN32
    closesocket(server_socket);
    WSACleanup();
#else
    close(server_socket);
#endif
    exit(EXIT_FAILURE);
  }

  if (is_some(start_message)) {
    char* msg = GET_STRING(get_some(start_message));

    printf("%s\n", msg);
  }

  while (1) {
    struct sockaddr_in client_addr;
    socklen_t client_len = sizeof(client_addr);
    int client_socket;

#ifdef _WIN32
    client_socket =
        accept(server_socket, (struct sockaddr*)&client_addr, &client_len);
    if (client_socket == INVALID_SOCKET) {
      fprintf(stderr, "Accept failed.\n");
      closesocket(server_socket);
      WSACleanup();
      exit(1);
    }
#else
    client_socket =
        accept(server_socket, (struct sockaddr*)&client_addr, &client_len);
    if (client_socket < 0) {
      perror("Accept failed");
      close(server_socket);
      exit(EXIT_FAILURE);
    }
#endif

    gc_pause(&mod->gc);
    handle_client(client_socket, mod, handler);
    gc_resume(&mod->gc);

#ifdef _WIN32
    closesocket(client_socket);
#else
    close(client_socket);
#endif
  }

#ifdef _WIN32
  closesocket(server_socket);
  WSACleanup();
#else
  close(server_socket);
#endif

  gc_run(&mod->gc);
  return 0;
}

void handle_client(int client_socket, Module *mod, Value handler) {
  char *buffer = (char *)gc_malloc(&mod->gc, BUFFER_SIZE);
  int bytes_read = recv(client_socket, buffer, BUFFER_SIZE - 1, 0);

  if (bytes_read < 0) {
    perror("recv failed");
    return;
  }

  buffer[bytes_read] = '\0';
  
  HeapValue* response_obj = gc_malloc(&mod->gc, sizeof(HeapValue));
  response_obj->type = TYPE_API;
  response_obj->as_any = &client_socket;

  Value res = MAKE_PTR(response_obj);
  Value buf = MAKE_STRING(mod->gc, buffer);
  Value ret = mod->call_function(mod, handler, 3, (Value[]) { res, buf });

  gc_free(&mod->gc, buffer);
  gc_free(&mod->gc, response_obj);
}

Value respond(size_t argc, Module* mod, Value* args) {
  ASSERT_FMT(argc == 3, "expected 3 arguments, but got %zu", argc);
  Value response_obj = args[0];
  ASSERT_FMT(get_type(response_obj) == TYPE_API, "expected Response, but got %s",
             type_of(response_obj));

  HeapValue* response_hp = GET_PTR(response_obj);
  int client_socket = *(int*) response_hp->as_any;

  const char* content = GET_STRING(args[1]);
  int status = GET_INT(args[2]);

  char* response = gc_malloc(&mod->gc, strlen(content) + 100);
  sprintf(response,
          "HTTP/1.1 %d OK\r\n"
          "Content-Type: text/plain\r\n"
          "Content-Length: %zu\r\n"
          "\r\n"
          "%s",
          status, strlen(content), content);

  send(client_socket, response, strlen(response), 0);
  gc_free(&mod->gc, response);

  return make_unit(mod->gc);
}