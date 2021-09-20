require 'securerandom'
require_relative 'utils'
require_relative 'wix'

namespace :setup do
  desc "Performs the required steps to create a setup"
  task :create, [:src_dir, :setup_dir,  :product_name, :product_version, :harvest_ignored_files, :suite_name, :setup_files, :solution_dir, :manufacturer] do |task, args|
    @deploy_dir =  File.join(args.setup_dir, 'deploy');
    @harvest_dir = File.join(@deploy_dir, 'harvest');
    @product_version = args.product_version
    @product_name = args.product_name	
    @manufacturer = args.manufacturer
    @suite_name = args.suite_name
    @harvest_ignored_files = args.harvest_ignored_files || []
    setup_files = args.setup_files || []
    @solution_dir = args.solution_dir;
    
    create_deploy_dir
    copy_src_files args.src_dir
    
    harvest_app				
    
    copy_setup_files setup_files, @deploy_dir				
    Rake::Task['setup:create_setup'].invoke
  end
  
  desc "Creates the setup: requirement: all setup dependencies should have been copied to the deploy folder"
  task :create_setup => [:set_variables_for_setup, :run_light] 
  
  desc "Copy the files required to launch the setup in the deploy folder."
  task :set_variables_for_setup do
    @variables = {}
    @variables[:ProductId] = Utils.uuid.to_s
    @variables[:DeployDir] = @deploy_dir
    @variables[:SuiteName] = @suite_name
    @variables[:ProductName] =@product_name
    @variables[:ProductVersion] = @product_version
    @variables[:Manufacturer] = @manufacturer
    release_version, *, suite_version = versions_from(@product_version)
    @variables[:ProductReleaseVersion] = release_version	
    @variables[:SuiteVersion] = suite_version	
    @variables[:ProductFullName] = product_full_name_from(@product_name, release_version)
  end
  
  desc "Runs the candle executable as first step of the setup process"
  task :run_candle do 
    all_wxs = Dir.glob("#{@deploy_dir}/*.wxs")
    all_variables = @variables.each.collect do |k, v|
      "-d#{k}=#{v}"
    end
    all_options = %W[-ext WixUIExtension -ext WixNetFxExtension -o #{@deploy_dir}/]
    Utils.run_cmd(Wix.candle, all_wxs + all_variables + all_options)
  end
  
  desc "Runs the light command that actually creates the msi package"
  task :run_light => [:run_candle] do 
    all_wixobj = Dir.glob("#{@deploy_dir}/*.wixobj")
    all_options = %W[-o #{@deploy_dir}/#{@product_name}.#{@product_version}.msi -nologo -ext WixUIExtension -ext WixNetFxExtension -spdb -b #{@deploy_dir}/ -cultures:en-us]
    Utils.run_cmd(Wix.light, all_wixobj + all_options)
  end
  
  desc "Creates a portable setup"
  task :create_portable, [:solution_dir, :src_dir, :setup_dir, :setup_files, :setup_folders, :product_name, :product_version, :package_name] do |task, args|
    @solution_dir = args.solution_dir;
    release_version, full_version	 = versions_from(args.product_version)
    product_full_name = product_full_name_from(args.product_name, full_version)
    package_name = args.package_name || 'portable-setup.zip';
    setup_files = args.setup_files || []
    setup_folders = args.setup_folders || []
    
    portable_dir = File.join(args.setup_dir, product_full_name)
    archive_path = File.join(args.setup_dir, package_name)
    
    create_portable_dir portable_dir
    
    copy_src_files_to_portable_dir args.src_dir, portable_dir	
    copy_setup_files setup_files, portable_dir
    copy_setup_folders setup_folders, portable_dir
    
    archive_portable_dir archive_path, portable_dir	
  end
  
  private
  def archive_portable_dir(archive_path, portable_dir)	
    command_line = %W[a #{archive_path} #{portable_dir}]
    zip command_line
  end
  
  def product_full_name_from(product_name, release_version)
    "#{product_name} #{release_version}"
  end
  
  def versions_from(product_version)
    product_version_split = product_version.split('.')
    suite_version = product_version_split[0]
    release_version = "#{suite_version}.#{product_version_split[1]}"
    full_version = "#{release_version}.#{product_version_split[2]}"
    return release_version, full_version, suite_version
  end
  
  def zip(command_line)
    Utils.run_cmd('7z', command_line) 
  end
  
  def harvest_app
    @harvest_ignored_files.each do |file|
      FileUtils.rm File.join(@harvest_dir, file)
    end
    
    Rake::Task[:heat].execute  OpenStruct.new(source_directory: @harvest_dir, component_name: 'App', output_dir:  @deploy_dir)
  end
  
  def create_portable_dir(portable_dir)
    FileUtils.rm_rf  portable_dir
    FileUtils.mkdir_p portable_dir		
  end
  
  def create_deploy_dir
    FileUtils.rm_rf  @deploy_dir
    FileUtils.mkdir_p @deploy_dir
    FileUtils.mkdir_p @harvest_dir		  
  end
  
  def copy_src_files_to_portable_dir(src_dir, portable_dir)
    src_files = File.join(src_dir, '*.*')
    copy_to_target_dir src_files, portable_dir, %w[pdb xml]
  end
  
  def copy_src_files(src_dir)
    src_files = File.join(src_dir, '*.*')
    copy_to_deploy_dir src_files
    copy_to_target_dir src_files, @harvest_dir, %w[pdb xml]
  end
  
  def copy_setup_files(setup_files, target_dir)
    setup_files.each do |file|
      copy_to_target_dir File.join(@solution_dir, file), target_dir
    end
  end
  
  def copy_setup_folders(setup_folders, target_dir)
    setup_folders.each do |folder|
      folder_entries = folder.split('/');
      
      #first entry in path that is not a star. We will copy structure from there
      folder_base = folder_entries[folder_entries.rindex{ |x| !x.include? "*" }]
      
      copy_to_target_dir_preserve folder, target_dir, folder_base
    end
  end
  
  def copy_to_target_dir_preserve(source, target_dir, base_dir)
    Dir.glob source do |file|
      dir_entries = File.dirname(file).split('/')
      base_index = dir_entries.rindex{ |x| x == base_dir }
      
      #last element in the path containing the base. We want to keep everything AFTER that
      dir_entries = dir_entries.drop(base_index + 1)
      dest = File.join(target_dir, dir_entries)
      FileUtils.mkdir_p dest
      FileUtils.copy_file file, File.join(dest, File.basename(file)), verbose: true
    end
  end
  
  def copy_to_deploy_dir(source)
    copy_to_target_dir source, @deploy_dir
  end
  
  def copy_to_target_dir(source, target_dir, ignored_extensions=[])
    Dir.glob	source do |file|
      copy file, target_dir, verbose: false	unless file_should_be_ignored(file, ignored_extensions)
    end
  end
  
  def file_should_be_ignored(file, ignored_extensions)
    ignored_extensions.any? { |ext| file.include? ext }
  end
end

