require 'open-uri'
require 'openssl'

require_relative 'scripts/utils'
require_relative 'scripts/R'
require_relative 'scripts/colorize'

OpenSSL::SSL::VERIFY_PEER = OpenSSL::SSL::VERIFY_NONE

APPVEYOR_ACCOUNT_NAME = 'open-systems-pharmacology-ci'

task :prepare_for_build, [:build_version] do |t, args|
  update_package_version(args.build_version, description_file)
  install_pksim('develop')
end

private
def install_pksim(branch)
  file_name ='setup.zip'
  appveyor_project_name = 'pk-sim'
  uri = "https://ci.appveyor.com/api/projects/#{APPVEYOR_ACCOUNT_NAME}/#{appveyor_project_name}/artifacts/#{file_name}?branch=#{branch}"
  zip_package = download_file(appveyor_project_name, file_name, uri)
  msi_package = unzip_package(zip_package)
  # MSI installer only works with \\ style separator
  msi_package = msi_package.split('/').join('\\')
  puts "Installing #{msi_package} silently".light_blue
  command_line = %W[/i #{msi_package} /quiet /qn /norestart]
  Utils.run_cmd('msiexec.exe', command_line)
  puts "Installation done.".light_blue
end

def download_file(project_name, file_name, uri)
  download_dir = File.join(temp_dir, project_name)
  FileUtils.mkdir_p download_dir
  file = File.join(download_dir, file_name)
  puts "Downloading #{file_name} from #{uri} under #{file}".light_blue
  open(file, 'wb') do |fo|
    fo.print URI.open(uri,:read_timeout => nil).read
  end
  file
end

def unzip_package(package_full_path)
  unzip_dir = unzip(package_full_path)
  artifact_name = ''
  Dir.glob(File.join(unzip_dir, '*.msi')) do |x|
    artifact_name = x
  end
  artifact_name
end

def unzip(package_full_path)
  unzip_dir = File.dirname(package_full_path)
  command_line = %W[e #{package_full_path} -o#{unzip_dir}]
  Utils.run_cmd('7z', command_line)
  unzip_dir
end


def solution_dir
  File.dirname(__FILE__)
end

def description_file
  File.join(solution_dir,'DESCRIPTION')
end

def temp_dir
  "C:/temp"
end
