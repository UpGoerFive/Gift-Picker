from dataclasses import dataclass, field
import random
import csv
import sys


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

    def give_gifts(self):
        """Finds forced pairings first, assigns to members with exclusions listed next, then randomly assigns everyone
        leftover."""
        for person in self._people:
            if person.recipient:
                self._finished.append(person)
            else:  # Updates available recipient options before adding to unmatched dictionary
                person.possible_recipients = {giver.name for giver in self._people} - person.excluded - {person.name}
                self._unmatched.append(person)

        # Sort unmatched list so more restrictive entries will be paired first
        self._unmatched = sorted(self._unmatched, key=lambda giver: len(giver.excluded), reverse=True)

        for person in self._unmatched:
            person.possible_recipients = person.possible_recipients - {giver.recipient for giver in self._finished}
            try:
                person.recipient = random.choice(list(person.possible_recipients))
            except IndexError:
                person.recipient = "This person had no available options, revise initial spreadsheet."

            self._finished.append(person)

        return self._finished


if __name__ == "__main__":
    with open(sys.argv[1], newline='') as filename:
        Santa_reader = csv.reader(filename)
        gift_list = []
        for row in Santa_reader:
            gift_list.append(row)

    gift_list.pop(0)
    gift_people = [Participant(entry[0], str(entry[1]), set(entry[2].split(', '))) for entry in gift_list]

    gift_picker = Santa(gift_people)
    gift_picker.give_gifts()
