## Python Version

Santa, if he'd never delivered gifts before and decided that there really wasn't a reason *not* to use dataclasses (needs testing).

This program only requires python 3.7 or higher. You can check what version of python you have installed
from most terminals with `python --version` or `python3 --version` as for some reason system python will still be 2.7 sometimes.
Python can be installed or updated [here](https://www.python.org/).

This program takes .csv files, which most spreadsheet editors will be able to download a sheet as. I believe online Excel doesn't have this option, but desktop Excel should.
Make sure that you only have one sheet in the file. The column ordering goes: 

* `Name` of the person to give a gift 
* `Inclusions` or the forced pairings to be made
* `Exclusions` a list of people that this person cannot give a gift to 
  
The `Exclusions` list 
is just a list of names in the cell with comma separation (so `Kirk, Spock, Picard`). All names need to be unique.

After cloning the repo, change into the repo directory with the `cd` command; you can run the picker without doing this, but the filepaths to type in 
will be more cumbersome. Likewise either move your sheet file into this directory or copy its whole path to use. The program can be run with
`$ python gift-picker.py <name-of-your-sheet.csv>` and will create a new paired-sheet.csv file in the same directory as your *initial* spreadsheet. Rerunning the 
program will wipe this file so be sure to rename it or move it before doing so.

Still very much a work in progress, but getting there!
