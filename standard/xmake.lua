add_rules("mode.release")
add_requires("libcurl", { system = false })

if has_config("local-repo") then
	add_repositories("local-repo " .. get_config("local-repo"))
end

add_requires("plume-vm")

target("plume-natives")
  set_kind("shared")
  add_packages("libcurl")
  add_packages("plume-vm")
  add_files("c-ffi/**.c")
  add_includedirs("../runtime/include")
  set_targetdir(".")
  set_basename("native")
  set_prefixname("")
  set_extension(".plmc")
  set_optimize("fastest")