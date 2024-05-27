add_rules("mode.release")
add_requires("libcurl", { system = false })

includes("runtime")

target("plume-natives")
    set_kind("shared")
    add_packages("libcurl")

    add_files("ffi/**.c")
    set_targetdir(".")
    set_basename("native")
    set_prefixname("")
    set_extension(".plmc")
