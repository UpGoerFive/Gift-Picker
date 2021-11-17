from gift_picker import Santa, SheetError, Participant, check_people, create_sheet
import pytest
import random
from pathlib import Path


@pytest.fixture(params=[Path(r".\Testfiles\ChristmasList.csv"),
                        Path(r"Testfiles/SantaTestsheet1.csv"),
                        Path(r"Testfiles/SantaTestsheet2.csv")])
def sheet_data(request):
    return check_people(create_sheet(request.param))


@pytest.fixture()
def double(sheet_data):
    return random.sample(sheet_data, 2)


@pytest.fixture()
def invalid(sheet_data):
    for person in sheet_data:
        person.recipient = sheet_data[0].name
    return sheet_data


def test_empty_picker():
    picker = Santa([])
    assert [] == picker.give_gifts()


def test_empty_checker():
    with pytest.raises(SheetError):
        check_people([])


def test_checker_duplicates(sheet_data):
    with pytest.raises(SheetError):
        doubled_up = sheet_data.extend(sheet_data)
        check_people(doubled_up)


class TestPicker:
    def test_single(self, sheet_data):
        lonely = [random.choice(sheet_data)]
        picker = Santa(lonely)
        assert picker.give_gifts()[0].recipient == "!!!This person had no available options, revise initial spreadsheet.!!!"

    def test_two(self, double):
        picker = Santa(double)
        out_pairing = picker.give_gifts()
        names = [person.name for person in out_pairing]
        for person in out_pairing:
            assert person.recipient == "!!!This person had no available options, revise initial spreadsheet.!!!" or (person.recipient in names and person.recipient != person.name)

    def test_full(self, sheet_data):
        picker = Santa(sheet_data)
        out_pairing = picker.give_gifts()
        names = {person.name for person in out_pairing}
        for person in out_pairing:
            assert person.recipient in names and person.recipient != person.name and person.recipient not in person.excluded
        assert picker.received == names

    @pytest.mark.xfail
    def test_ivalid(self, invalid):
        picker = Santa(invalid)
        out_pairing = picker.give_gifts()
        names = [person.name for person in out_pairing]
        for person in out_pairing:
            assert person.recipient in names and person.recipient != person.name and person.recipient not in person.excluded
