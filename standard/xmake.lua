add_rules("mode.debug", "mode.release", "mode.profile")

target("plume-natives")
  add_files("natives/**.c")
  add_includedirs("../runtime/include")
  set_kind("shared")
  set_targetdir(".")
  set_basename("natives")
  set_prefixname("")