require_relative 'utils'

module Wix
  def self.heat
    Utils.join(wix_bin,'heat.exe')
  end

  def self.candle
    Utils.join( wix_bin,'candle.exe')
  end

  def self.light
   Utils.join( wix_bin,'light.exe')
  end

  def self.wix_bin
    Utils.join(ENV['WIX'] || 'C:/Program Files (x86)/WiX Toolset v3.10', 'bin')
  end
end


desc "Create a wxs package."
task :heat, [:source_directory, :component_name, :output_dir, :install_dir] do |t, args|
  raise "Source directory missing. Use format heat[source_directory, component_name]" unless args.source_directory  
  raise "Component name is missing. Use format heat[source_directory, component_name]" unless args.component_name  
  output_dir = args.output_dir || args.source_directory
  output =  File.join(output_dir, args.component_name + '.wxs')
  install_dir = args.install_dir || 'INSTALLDIR'
  command_line = %W[dir #{args.source_directory} -cg  #{args.component_name} -dr  #{install_dir} -gg -scom -srd -sreg -out #{output}]
  
  Utils.run_cmd(Wix.heat, command_line)

  replacement = {'SourceDir' => '$(var.DeployDir)'};
  Utils.replace_tokens replacement, output
end