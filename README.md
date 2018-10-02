# prolog_sandbox

Menu Data into frame representation with data access interface.

**Changed files:**

* `load_menu_db.pl`,
* `data/*`

## Example Usage

```bash
swipl load_menu_db.pl

?- get_data("TASKS", S).
S = [chioce_item=["Inbox", "Today", "Tomorrow", "Someday", "Waiting", "Overdue"], group="TASK"]

?- get_data("Menu_1", group=Group).
Group = "TEST"

```

## ToDo

This is the data access architecture only, I've stopped here as
I couldn't run the version I checked out of GitHub, so couldn't
see what the expected behaviour is.

For this to work as a menu, the UI and bridging to the data needs
to be implemented.

I'd expect a module that describes what a
menu is, this is the business rules, it will use `get_data\2`. It
probably won't need to do much other than call `get_data\2` as in
this case, the data closely matches what a menu is. This should
handle the `parent` attribute though.

Then you'd have a module that's a controller, which when a menu
is requested, fetches it from the business rules and turns it
into a format that's needed for the UI. I'd use another
controller to handle menu choices. This module will only
use predicates declared in the business rules.

Finally you'll have a UI module, which displays your menu. This
accepts requests for menus, passes them to the user, shows the
menus, and accepts menu choices and would send them to the
appropriate controller.
