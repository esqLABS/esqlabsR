require 'zip'
require 'xmlsimple'
require 'open-uri'

def print_all_packages_to_upload_from(directory)
  packages = all_open_pharma_packages(directory)
  
  packages.each do |id, package|
    if(!nuget_has_version(package))
      push_to_nuget(package)
    end
  end
end


def all_open_pharma_packages(directory)
  projects = Dir.glob(File.join(directory, "**", "*.csproj"));
  
  packages = {};
  projects.each do |path|
    project = Project.new path
    project.packages.each do |package|
      package_id = package.id
      if(packages.key?(package_id))
        existing_version = packages[package_id].version;
        if(existing_version!=package.version)
          raise "Different version found for package #{package_id}: (#{existing_version} vs #{package.version})"
        end
      else
        packages[package_id] = package
      end
    end
  end
  
  return packages;
end

def nuget_has_version(package)
  begin  
    file = open("https://www.nuget.org/packages/#{package.id}")
    contents = file.read
    contents.include? package.version
  rescue
    false
  end
end

def push_to_nuget(package)
  #todo make this execute
  #	puts "nuget push #{package.id} -apikey somekey -Source https://www.nuget.org/api/v2/package"
  puts "Package to update: #{package.id} with version #{package.version}"
end

class Project
  attr_reader :packages 
  
  def initialize(path)
    @path = path
    @packages = []
    project_xml = XmlSimple.xml_in(path)
    project_xml["ItemGroup"].each do |item_group|
      pagckage_ref = item_group["PackageReference"];
      next if !pagckage_ref
      
      pagckage_ref.each do |package|
        id = package["Include"]
        next if !id.start_with?("OSPSuite")
        packages.push Package.new(id, package["Version"])
      end
    end
  end
end

class Package 
  def initialize(id, version)
    @version = version
    @id = id
  end
  
  attr_reader :id 
  
  attr_reader :version
end