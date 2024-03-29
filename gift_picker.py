from dataclasses import dataclass, field
import csv
import argparse
from pathlib import Path
from tkinter import Tk
from tkinter.filedialog import askopenfilename, asksaveasfilename


@dataclass
class Participant:
    name: str
    recipient: str = ''
    excluded: set = field(default_factory=set)
    possible_recipients: set = field(default_factory=set)

    def __str__(self):
        return f"{self.name} gives a gift to {self.recipient}. They could not gift to {self.excluded}, and had {self.possible_recipients} to choose from. "


class Santa:
    def __init__(self, people: list):
        """Initializes needed iterables"""
        self._people = people
        self._unmatched = []
        self._finished = []
        self.received = {person.recipient for person in people}
        self.received.difference_update({''})
        self._names = {person.name for person in people}

    def give_gifts(self):
        """Finds forced pairings first, assigns to members with exclusions listed next, then randomly assigns everyone
        leftover."""
        for person in self._people:
            if person.recipient and person.recipient in self._names:
                self._finished.append(person)
            else:
                person.excluded.update({person.name})
                person.possible_recipients = self._names - person.excluded - self.received
                self._unmatched.append(person)

        # Sort unmatched list so more restrictive entries will be paired first
        self._unmatched = sorted(self._unmatched, key=lambda matcher: len(matcher.possible_recipients), reverse=True)

        while self._unmatched:
            person = self._unmatched.pop()  # person has the least possible options currently
            try:
                person.recipient = person.possible_recipients.pop()
                self.received.update({person.recipient})
                for giver in self._unmatched:
                    # removes candidate from options for others
                    giver.possible_recipients.difference_update({person.recipient})
                # The unmatched list needs to be sorted again to prevent running into a person with no options at the
                # end of the list.

                self._unmatched = sorted(self._unmatched, key=lambda matcher: len(matcher.possible_recipients), reverse=True)

            except KeyError:
                person.recipient = "!!!This person had no available options, revise initial spreadsheet.!!!"

            self._finished.append(person)

        return self._finished


class SheetError(Exception):
    """Raised when the input sheet is unsuitable for creating pairings."""
    def __init__(self, message):
        self.message = message


def check_people(people: list):
    """Checks list of entry rows for usability and strips column headings."""
    if not people:
        raise SheetError("Sheet is empty.")
    first_entry = people[0][0].title()  # Checking for column titles.
    if first_entry in ["Name", "Names", "People", "Participants"]:
        people.pop(0)
    givers = [entry[0].title() for entry in people]
    if len(givers) != len(set(givers)):
        raise SheetError("Duplicate names are present, fix input sheet.")
    return [Participant(entry[0].title(),
                        str(entry[1]).title() if len(entry) >= 2 else '',
                        set(entry[2].title().split(', ')) if len(entry) > 2 else set()) for entry in people]


def create_sheet(source_name):
    with open(source_name, newline='') as file_name:
        santa_reader = csv.reader(file_name)
        person_list = list(santa_reader)
    return person_list


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("infile", nargs="?", default=None)
    parser.add_argument("outfile", nargs="?", default=None)
    args = parser.parse_args()
    if not args.infile:
        # tkinter file dialog taken from Stack Overflow
        Tk().withdraw()  # we don't want a full GUI, so keep the root window from appearing
        source_name = Path(askopenfilename())  # show an "Open" dialog box and return the path to the selected file
    else:
        source_name = args.infile
    gift_people = check_people(create_sheet(source_name))
    gift_picker = Santa(gift_people)
    out_list = [(paired.name, paired.recipient) for paired in gift_picker.give_gifts()]

    destination = Path(asksaveasfilename()) if not args.outfile else Path(args.outfile)
    destination = destination.with_suffix(".csv")

    with open(destination, "w", newline='') as filename:
        santa_writer = csv.writer(filename)
        santa_writer.writerows(out_list)


if __name__ == "__main__":
    main()
