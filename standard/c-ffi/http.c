#include <curl/curl.h>
#include <value.h>
#include <module.h>
#include <core/error.h>
#include "cons.h"
#include <stdio.h>

struct MemoryStruct {
  char *memory;
  size_t size;
  GarbageCollector gc;
};

size_t write_callback(void* contents, size_t size, size_t nmemb, struct MemoryStruct* mem) {
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
  ASSERT_FMT(get_type(url) == TYPE_STRING, "expected string, but got %s", type_of(url));

  const char* url_str = GET_STRING(url);

  struct MemoryStruct mem;
  mem.gc = gc;
  mem.memory = malloc(1);
  mem.size = 0;

  CURL* curl = curl_easy_init();
  if (!curl)
    return make_err(mod->gc, MAKE_STRING(mod->gc, "Failed to initialize curl"));
  
  curl_easy_setopt(curl, CURLOPT_URL, url_str);
  curl_easy_setopt(curl, CURLOPT_WRITEFUNCTION, write_callback);
  curl_easy_setopt(curl, CURLOPT_WRITEDATA, &mem);
  CURLcode res = curl_easy_perform(curl);

  if (res != CURLE_OK) {
    char* err = (char*) curl_easy_strerror(res);
    curl_easy_cleanup(curl);
    return make_err(mod->gc, MAKE_STRING(mod->gc, err));
  }

  char* result = gc_strdup(&mod->gc, mem.memory);
 
  curl_easy_cleanup(curl);
  return make_ok(mod->gc, MAKE_STRING(mod->gc, result));
}