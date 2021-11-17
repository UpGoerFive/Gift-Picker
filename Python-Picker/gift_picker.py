from dataclasses import dataclass, field
import random
import csv
import sys
from pathlib import Path


@dataclass
class Participant:
    name: str
    recipient: str = ''
    excluded: set = field(default_factory=set)
    possible_recipients: set = field(default_factory=set)
    received_from_count: int = 0

    def __str__(self):
        return f"{self.name} gives a gift to {self.recipient}. They could not gift to {self.excluded}, and had {self.possible_recipients} to choose from."


class Santa:
    def __init__(self, people: list):
        """Initializes needed iterables"""
        self._people = people
        self._unmatched = []
        self._finished = []
        self._names = [person.name for person in people]

    def give_gifts(self):
        """Finds forced pairings first, assigns to members with exclusions listed next, then randomly assigns everyone
        leftover."""
        for person in self._people:
            if person.recipient and person.recipient in self._names:
                self._finished.append(person)
            else:  # Updates available recipient options before adding to unmatched list
                person.possible_recipients = {giver.name for giver in self._people} - person.excluded - {person.name}
                self._unmatched.append(person)

        # Sort unmatched list so more restrictive entries will be paired first
        self._unmatched = sorted(self._unmatched, key=lambda giver: len(giver.possible_recipients))

        for person in self._unmatched:
            try:
                candidate = random.choice(list(person.possible_recipients))
                person.recipient = candidate
                for giver in self._unmatched:
                    giver.possible_recipients = giver.possible_recipients - {candidate}
                self._unmatched = sorted(self._unmatched, key=lambda giver: len(giver.possible_recipients))
            except IndexError:
                person.recipient = "!!!This person had no available options, revise initial spreadsheet.!!!"

            self._finished.append(person)

        return self._finished  # Current version fails on certain runs, in part because the unmatched list is not being
    # updated as each selection is made. As a result certain entries that had many options in the beginning have none
    # as selections are made. This could be solved by brute forcing all possibilities and stopping as soon as a success
    # happens, or by rearranging the list and possibility counters after each selection. Second option is probably
    # better because I think this is O(n!) vs O(n^2), though the list rearrangement will have a longer minimum I believe


class SheetError(Exception):
    """Raised when the input sheet is unsuitable for creating pairings."""
    def __init__(self, message):
        self.message = message


def check_people(people: list):
    """Checks list of entry rows for usability and strips column headings."""
    if not people:
        raise SheetError("Sheet is empty.")
    first_entry = people[0][0].title()
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
