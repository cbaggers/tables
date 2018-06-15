# Query Language

## Misc

case statements switching on enums can have branches for elements that dont exist in
the enum. A warning will be generated but not an error. The reason for this is that
adding an element to an enum will make many case expressions non-total which in some
cases could be an error. This leniency allows the user to prepare code for the change
to the enum.
