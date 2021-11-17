from dataclasses import dataclass, field
import csv
import sys
from pathlib import Path


@dataclass
class Participant:
    name: str
    recipient: str = ''
    excluded: set = field(default_factory=set)
    possible_recipients: set = field(default_factory=set)

    def __str__(self):
        return f"{self.name} gives a gift to {self.recipient}. They could not gift to {self.excluded}, and had {self.possible_recipients} to choose from."


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
        self._unmatched = sorted(self._unmatched, key=lambda giver: len(giver.possible_recipients))

        while self._unmatched:
            person = self._unmatched.pop(0)  # person has the least possible options currently
            try:
                person.recipient = person.possible_recipients.pop()
                self.received.update({person.recipient})
                for giver in self._unmatched:
                    # removes candidate from options for others
                    giver.possible_recipients.difference_update({person.recipient})
                # The unmatched list needs to be sorted again to prevent running into a person with no options at the
                # end of the list.
                self._unmatched = sorted(self._unmatched, key=lambda giver: len(giver.possible_recipients))
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
    return [Participant(entry[0].title(), str(entry[1]).title(), set(entry[2].title().split(', '))) for entry in people]


def create_sheet(source_name):
    with open(source_name, newline='') as file_name:
        santa_reader = csv.reader(file_name)
        person_list = [row for row in santa_reader]
    return person_list


if __name__ == "__main__":
    sourcename = Path(sys.argv[1])
    gift_list = create_sheet(sourcename)
    gift_people = check_people(gift_list)

    gift_picker = Santa(gift_people)
    out_list = [(giver.name, giver.recipient) for giver in gift_picker.give_gifts()]

    destination = sourcename.parent.joinpath("paired-sheet.csv")

    with open(destination, "w", newline='') as filename:
        Santa_writer = csv.writer(filename)
        Santa_writer.writerows(out_list)
