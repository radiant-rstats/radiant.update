# radiant.update

Contains functions that update Radiant and all required packages. 

* `radiant.update::radiant.update()` will update any packages that are _behind_ the current versions in the https://radiant-rstats.github.io/minicran/ repo. 
* `radiant.update::sync_packages()` will update packages that are _behind_ or _ahead_ of the current versions in the https://radiant-rstats.github.io/minicran/ repo. `sync_packages` can be very useful to ensure everyone is using the exact same package versions.

See `?radiant.update::radiant.update` and `?radiant.update::sync_packages` for additional options
