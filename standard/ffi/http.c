#include <curl/curl.h>
#include <value.h>
#include <module.h>
#include <core/error.h>
#include "cons.h"
#include <stdio.h>

size_t write_callback(void* contents, size_t size, size_t nmemb, char** response) {
  size_t realsize = size * nmemb;
  *response = realloc(*response, realsize + 1);
  if (*response == NULL) {
    fprintf(stderr, "Failed to allocate memory\n");
    return 0;
  }
  memcpy(*response, contents, realsize);
  (*response)[realsize] = '\0';
  return realsize;
}

Value ffi_fetch(size_t argc, Module* mod, Value* args) {
  ASSERT_FMT(argc == 1, "expected 1 argument, but got %zu", argc);
  Value url = args[0];
  ASSERT_FMT(url.type == VALUE_STRING, "expected string, but got %u", url.type);

  const char* url_str = url.string_value;

  CURL* curl = curl_easy_init();
  if (!curl) return make_err(MAKE_STRING("Failed to initialize curl"));

  // Store the response in a string
  char* response = malloc(1);
  curl_easy_setopt(curl, CURLOPT_URL, url_str);
  curl_easy_setopt(curl, CURLOPT_WRITEFUNCTION, write_callback);
  curl_easy_setopt(curl, CURLOPT_WRITEDATA, &response);
  CURLcode res = curl_easy_perform(curl);

  if (res != CURLE_OK) {
    curl_easy_cleanup(curl);
    return make_err(MAKE_STRING((char*) curl_easy_strerror(res)));
  }

  curl_easy_cleanup(curl);
  return make_ok(MAKE_STRING(response));
}