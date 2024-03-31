add_rules("mode.debug", "mode.release", "mode.profile")

target("plume-natives")
  add_files("ffi/**.c")
  add_includedirs("../runtime/include")
  set_kind("shared")
  set_targetdir(".")
  set_basename("native")
  set_prefixname("")