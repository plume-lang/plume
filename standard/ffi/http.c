#include <curl/curl.h>
#include <value.h>
#include <module.h>
#include <core/error.h>
#include "cons.h"
#include <stdio.h>

size_t data_size = 0;

size_t write_callback(void* contents, size_t size, size_t nmemb, char** response) {
  size_t realsize = size * nmemb;
  *response = gc_realloc(&gc, *response, realsize + 1);
  if (*response == NULL) {
    fprintf(stderr, "Failed to allocate memory\n");
    return 0;
  }
  memcpy(*response, contents, realsize);
  (*response)[realsize] = '\0';
  data_size = realsize;
  return realsize;
}

Value ffi_fetch(size_t argc, Module* mod, Value* args) {
  ASSERT_FMT(argc == 1, "expected 1 argument, but got %zu", argc);
  Value url = args[0];
  ASSERT_FMT(get_type(url) == TYPE_STRING, "expected string, but got %s", type_of(url));

  const char* url_str = GET_STRING(url);

  CURL* curl = curl_easy_init();
  if (!curl) return make_err(MAKE_STRING("Failed to initialize curl"));

  data_size = 0;

  // Store the response in a string
  char* response = gc_malloc(&gc, 1);

  curl_easy_setopt(curl, CURLOPT_URL, url_str);
  curl_easy_setopt(curl, CURLOPT_WRITEFUNCTION, write_callback);
  curl_easy_setopt(curl, CURLOPT_WRITEDATA, &response);
  CURLcode res = curl_easy_perform(curl);

  if (res != CURLE_OK) {
    curl_easy_cleanup(curl);
    char* err = (char*) curl_easy_strerror(res);
    return make_err(MAKE_STRING(err));
  }

  curl_easy_cleanup(curl);
  return make_ok(MAKE_STRING(response));
}
