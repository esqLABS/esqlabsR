# build-scripts

Collection of rake tasks used during build.

## nuget_push

The task `list_nuget_packages` is used to list of Open Systems Pharmacology packages that are not updated in nuget.org. That is useful in the case where nuget developers are pushing packages to the Appveyor feed and a stable version of all the packages should be copied to nuget and the packages should not be rebuilt.

### Usage:

```
rake list_nuget_packages[<path to the packages folder>]
```

### Examples

```
 rake list_nuget_packages[C:/projects/PK-Sim]
 rake list_nuget_packages[C:/projects/MoBi]
 rake list_nuget_packages[C:/projects/OSPSUite.Core]
```

This will go over all `csproj` in the folder and find nupkg files to consider updating on Nuget.org.

The nupkg files will be scanned for the nuspec. If the nuspec file indicates that the owner is Open-Systems-Pharmacology, then the version and id will be read and the comparable package will be found on nuget.org.

If the version for that package id is not found, then the script will list the nupkg file to be pushed to nuget.org

Required gems are

- rubyzip
- xml-simple

## Code of conduct

Everyone interacting in the Open Systems Pharmacology community (codebases, issue trackers, chat rooms, mailing lists etc...) is expected to follow the Open Systems Pharmacology [code of conduct](https://github.com/Open-Systems-Pharmacology/Suite/blob/master/CODE_OF_CONDUCT.md).

## Contribution

We encourage contribution to the Open Systems Pharmacology community. Before getting started please read the [contribution guidelines](https://github.com/Open-Systems-Pharmacology/Suite/blob/master/CONTRIBUTING.md). If you are contributing code, please be familiar with the [coding standard](https://github.com/Open-Systems-Pharmacology/Suite/blob/master/CODING_STANDARDS.md).

## License

build-scripts is released under the [GPLv2 License](LICENSE).
