from gift_picker import Santa, SheetError, Participant, check_people


def test_empty_picker():
    picker = Santa([])
    assert [] == picker.give_gifts()


def test_empty_checker():
    assert [] == check_people([])


def test_single():
    lonely = Participant("Alone")
    picker = Santa([lonely])
    assert picker.give_gifts() == [Participant("Alone", recipient="!!!This person had no available options, revise initial spreadsheet.!!!")]


def test_checker():
    pass


def test_multiples():
    pass
