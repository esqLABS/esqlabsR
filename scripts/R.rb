require_relative 'utils'
require_relative 'colorize'

def update_package_version(build_version, description_file) 
  version = sanitized_version(build_version)
  #Replace token Version: x.y.z with the version from appveyor
  replacement = {
    /Version: \d+\.\d+\.\d+/ => "Version: #{version}"
  }
  puts "Patching #{description_file} with version #{version}".light_blue
  Utils.replace_tokens(replacement, description_file)
end

def sanitized_version(build_version) 
  pull_request_index = build_version.index('-')
  return build_version unless pull_request_index
  build_version.slice(0, pull_request_index)
end
