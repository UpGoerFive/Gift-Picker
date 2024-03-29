# Gift-Picker
A small tool to pair gift givers and recipients for Secret Santa

The most current version is `gift_picker.py`. It's written with only modules from the standard library, so it should be usable on any Python 3.7+ installation. Each version has its own Readme with instructions.

Let me know if you have any suggestions or questions!
## Python Version Instructions

This program only requires python 3.7 or higher. You can check what version of python you have installed
from most terminals with `python --version` or `python3 --version` as for some reason system python will still be 2.7 sometimes.
Python can be installed or updated [here](https://www.python.org/).

This program takes .csv files, which most spreadsheet editors will be able to download a sheet as. I believe online Excel doesn't have this option, but desktop Excel should.
Make sure that you only have one sheet in the file. The column ordering goes:

* `Name` of the person to give a gift
* `Inclusions` or the forced pairings to be made
* `Exclusions` a list of people that this person cannot give a gift to

The column headings are optional. If you do include them, make sure the first one is called `name` or `names` the others can be what you want.
The `Exclusions` list
is just a list of names in the cell with comma-space separation (so `Kirk, Spock, Picard`). All names need to be unique.

After cloning the repo, change into the repo directory with the `cd` command; you can run the picker without doing this, but the filepaths to type in
will be more cumbersome. Likewise either move your sheet file into this directory or copy its whole path to use. The program can be run with either:

* `$ python gift_picker.py` this will bring up a file dialog to select your input file from, and select where you would like the output to go.
* `$ python gift_picker.py <name-of-your-sheet.csv> <new-name-to-write-to.csv>` if you want to specify the file paths in the command.

You may need to run it with `python3` instead, depending on if there's a python 2 version on your system.

The program has been tested and works, but I'm still adding features.