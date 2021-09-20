require_relative 'nuget_push'

task :list_nuget_packages, [:directory]  do |cmd, args|
  print_all_packages_to_upload_from args.directory
end
