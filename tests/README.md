# Kyua testsuite
----
To run, type "make kyua-test" from the root directory

#### Test tracking
Rolling updates are done to a Google docs spreadsheet to show which tests
from FreeBSD pkg are applicable.

[tracking spreadsheet](https://docs.google.com/spreadsheets/d/19dJPvxPgXX085FtGMbvozWl4oGao3w9nn6IRV3Esegk/edit?usp=sharing)

#### Requirements
The Ravenports rvn port will handle the requirements, but essentially the Kyua package
needs to be installed on the system, and it has to be run after rvn and xvrn are built.
