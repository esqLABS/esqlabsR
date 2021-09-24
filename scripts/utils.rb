require 'securerandom'

module Utils
  def self.update_go_diagram_license(file, license)
      replacement = {
        '{Environment.GetEnvironmentVariable("GO_DIAGRAM_KEY")}' => license
      }

    Dir.glob("./**/#{file}").each do |f|
      replace_tokens replacement, f
    end
  end  
  
  def self.replace_tokens(replacement,file) 
    content = File.read(file)

    replacement.each do |token,value|
      content.gsub!(token,value) if value
    end

    File.open(file, "w") { |f| f.write content }
  end

  def self.run_cmd(command, command_line)
    Rake::Task[:run_cmd].execute(OpenStruct.new(:command => command, :command_line => command_line))
  end

  def self.uuid
    SecureRandom.uuid
  end

  def self.join(path1, path2)
    File.join(path1,path2).tr '/', '\\'
  end
end

task :run_cmd, [:command, :command_line] do |cmd, args|
  sh(args.command, *args.command_line)
end