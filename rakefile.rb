require_relative 'scripts/utils'
require_relative 'scripts/R'

task :prepare_for_build, [:build_version] do |t, args|
  update_package_version(args.build_version, description_file)
end

def solution_dir
  File.dirname(__FILE__)
end

def description_file
  File.join(solution_dir,'DESCRIPTION')
end 