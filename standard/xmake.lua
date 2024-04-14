add_rules("mode.debug", "mode.release", "mode.profile")
add_requires("libcurl")

target("plume-natives")
  set_kind("shared")
  add_packages("libcurl")

  add_files("ffi/**.c")
  add_includedirs("../runtime/include")
  add_files("ffi/ffi.export.txt")
  add_rules("utils.symbols.export_list")
  set_targetdir(".")
  set_basename("native")
  set_prefixname("")