# Version 1.0.23

* Switch from using Travis to Github Actions as CI
* Update .gitignore
* Fix compatibility problems with OTP24

# Version 1.0.22

* Update copyright year to 2021
* Unit tests + plugin in release workflow
* Support Docker + VScode development

# Version 1.0.21

* Update travis config

# Version 1.0.19

* Fix compatibility issues with Erlang 23

# Version 1.0.18

* Update copyright year

# Version 1.0.17

* Fix formating of error messages

# Version 1.0.16

* Update type specs
* Avoid lengthy output of p1\_prof:m/r/q commands

# Version 1.0.15

* Add p1\_prof module

# Version 1.0.14

* Add contribution guide
* Remove exec bit from doc/style.css

# Version 1.0.13

* Add p1\_rand and shaper module

# Version 1.0.12

* Don't fetch generic\_debug option from init

# Version 1.0.11

* Fix compilation with rebar3

# Version 1.0.10

* Fix problem with edoc

# Version 1.0.9

* Add p1_options module

# Version 1.0.8

* Add p1_queue
* Only perform destructive operations in p1_file_queue:in/2
* Add garbage collector for file queues
* Add ram_to_file/1 and file_to_ram/1
* Improve exception names
* Implement limited queues
* Add ownership protection
* Add get_limit/1 and set_limit/2

# Version 1.0.7

* Fix coverall invocation (Paweł Chmielowski)
* Fix p1_server timeout handling, R18 compatibility (Alexey Shchepin)

# Version 1.0.6

* Add p1_http

# Version 1.0.5

* Erlang R19 compliance (Paweł Chmielowski)

# Version 1.0.4

* Adds p1_time_compat:unique_timestamp() that returns value resembling what now() was returning

# Version 1.0.3

* Added time related compatibility module, added API documentation (Paweł Chmielowski)
* Improve documentation readability (Marek Foss)

# Version 1.0.2

* Add p1_time_compat module to ease support for both R17 and R18
  Erlang time features (Paweł Chmielowski)

# Version 1.0.1

* Better Rebar3 support, remove warning about missing hex plugin when
  building with rebar (Mickaël Rémond)
