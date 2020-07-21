-- increment
inc x = x + 1

-- decrement
dec x = x - 1

-- square
square x = x * x

-- discriminant for quadratic equations
disc a b c = (b * b) - (4 * a * c)

-- negate
negate x = -x

-- does line begin with "TEL"?
isphone s = (take 3 s) == "TEL"

-- remove up to colon
strip s = tail (dropWhile notcolon s)
  where notcolon c = not (c == ':')

-- phone list
getPhones card = map strip phonelines
  where phonelines = filter isphone (lines card)

johnDoe = "BEGIN:VCARD\r\nVERSION:3.0\r\nPRODID:-//Apple Inc.//Address " ++
          "Book 6.1.2//EN\r\nN:Doe;John;;;\r\nFN:Doe John\r\nORG:Queen " ++
          "Mary;\r\nEMAIL;type=INTERNET;type=HOME;type=pref:" ++
          "JohnDoe@nogmail.com\r\nTEL;type=CELL;type=VOICE;type=pref:" ++
          "0751 234567\r\nTEL;type=HOME;type=VOICE:020 7123 4567\r\nitem1." ++
          "ADR;type=HOME;type=pref:;;42 Nowhere St;London;;E1 0XX;\r\n" ++
          "item1.X-ABADR:gb\r\nX-ABUID:85152BB5-BFB5-45DA-853A-BA021C7A0FC8" ++
          ":ABPerson\r\nEND:VCARD\r\n"
