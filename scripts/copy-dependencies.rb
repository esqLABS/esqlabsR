require 'fileutils'
require_relative 'colorize'

#DSL to allow copying files defined under source_dir into target_dir
#Example: 
# => copy_dependencies source_dir, target_dir do
# =>   copy_file 'native', 'dll'
# =>   copy_native_dll
# => end

def copy_dependencies(source_dir, target_dir, &block)
	dependecyManager = DependencyManager.new(source_dir, target_dir)
	dependecyManager.instance_eval(&block)
	dependecyManager.perform_file_copy
end

class DependencyManager
	def initialize(source_dir, target_dir)
	  @source_dir = source_dir;
	  @target_dir = target_dir;
	end

	def dependencies
	  @dependencies ||= []
	end

	#copy files defined by file filters relative to source_dir
	#Example: 
	# => copy_file '**/native/*.dll'
	def copy_file(*filters)
		add_dep filters.map{|filter| File.join(@source_dir, filter)}
	end

	#copy files with file_extensions (either single or array)
	#Example: 
	# => copy_files 'native', ['dll', 'lib']
	# This call is equivalent to
	# => copy_file '**/native/**/*.dll'
	# => copy_file '**/native/**/*.lib'
	def copy_files(subfolder, file_extensions)
		retrieve_file_extensions(file_extensions) do |file_extension|
			copy_file File.join('**', subfolder,'**', "*.#{file_extension}")
		end
	end

	#DSL internal method only called at the end of the copy_dependencies process
	#See http://ruby-doc.org/core-1.9.3/Dir.html#method-c-glob for glob documentation
	def perform_file_copy
		retrieve_target_dir do |target_dir|
			FileUtils.mkdir_p target_dir
			copy_dependencies_to target_dir
		end	
	end

	#Allow for meta programming and supports method starting with copy and having two sub parameters
	#Example:
	# => copy_native_dll
	# => copy_native_lib
	def method_missing(method_name, *args)
		match = method_name.to_s.match(/^copy_(\w+)_(\w+)/)
		if(match)
			copy_files match[1], match[2]
		else
			super
		end
	end

	private 

	def copy_dependencies_to(target_dir)
		dependencies.each do |dep|
			Dir.glob(dep).each do |f| 
				puts "Copying #{f} to #{target_dir}".green
				FileUtils.copy f, target_dir 
			end
		end
	end

	def add_dep(dependencies_to_add)
		dependencies_to_add.each {|dep| dependencies << dep}
	end

	def retrieve_file_extensions(file_extensions,&block)
		yield_to_block file_extensions, &block
	end

	def retrieve_target_dir(&block)
		yield_to_block @target_dir, &block
	end

	def yield_to_block(params, &block)
		if(params.respond_to? :each)
			params.each(&block)
		else
			yield params
		end
	end
end	