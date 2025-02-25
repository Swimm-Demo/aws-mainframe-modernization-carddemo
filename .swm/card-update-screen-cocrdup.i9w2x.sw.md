---
title: Card Update Screen (COCRDUP)
---
The Card Update Screen (COCRDUP) allows users to update credit card details such as account number, card number, name on the card, card status, and expiry date. This screen is essential for maintaining accurate and up-to-date credit card information within the system.

## Screen Preview

```
Tran: CCUP                       AWS Mainframe Modernization        Date: mm/dd/yy
Prog: COCRDUPC                              CardDemo                Time: hh:mm:ss

                              Update Credit Card Details


                      Account Number    : ___________
                      Card Number       : ________________

     Name on card      : __________________________________________________

     Card Active Y/N   : _

     Expiry Date       : __/____




                        [Information message area]

[Error message area                                                              ]
ENTER=Process F3=Exit      F5=Save F12=Cancel
```

## Fields

### Transaction Name (TRNNAME)

- Length: 4 characters
- Fixed information
- Blue color
- Positioned at (1,7)

### Current Date (CURDATE)

- Length: 8 characters
- Fixed information
- Format: mm/dd/yy
- Blue color
- Positioned at (1,71)

### Program Name (PGMNAME)

- Length: 8 characters
- Fixed information
- Blue color
- Positioned at (2,7)
- Associated document: <SwmLink doc-title="Card Update Flow (COCRDUPC)" repo-id="Z2l0aHViJTNBJTNBa3luZHJ5bC1hd3MtbWFpbmZyYW1lLW1vZGVybml6YXRpb24tY2FyZGRlbW8lM0ElM0FTd2ltbS1EZW1v" repo-name="aws-mainframe-modernization-carddemo" path="/.swm/card-update-flow-cocrdupc.dgf8f2p7.sw.md">[Card Update Flow (COCRDUPC)](https://app.swimm.io/repos/Z2l0aHViJTNBJTNBa3luZHJ5bC1hd3MtbWFpbmZyYW1lLW1vZGVybml6YXRpb24tY2FyZGRlbW8lM0ElM0FTd2ltbS1EZW1v/docs/dgf8f2p7)</SwmLink>

### Current Time (CURTIME)

- Length: 8 characters
- Fixed information
- Format: hh:mm:ss
- Blue color
- Positioned at (2,71)

### Account Number (ACCTSID)

- Length: 11 characters
- Required field
- Underlined
- Default color
- Positioned at (7,45)
- Protected field

### Card Number (CARDSID)

- Length: 16 characters
- Required field
- Underlined
- Default color
- Positioned at (8,45)
- Unprotected field

### Name on Card (CRDNAME)

- Length: 50 characters
- Required field
- Underlined
- Positioned at (11,25)
- Unprotected field

### Card Active Status (CRDSTCD)

- Length: 1 character
- Required field
- Underlined
- Positioned at (13,25)
- Unprotected field
- Accepts 'Y' or 'N'

### Expiry Date (EXPMON and EXPYEAR)

- EXPMON: Length 2 characters, Right justified, Positioned at (15,25)
- EXPYEAR: Length 4 characters, Right justified, Positioned at (15,30)
- Required fields
- Underlined
- Unprotected fields
- Format: mm/yyyy

### Information Message (INFOMSG)

- Length: 40 characters
- Neutral color
- Positioned at (20,25)
- Protected field

### Error Message (ERRMSG)

- Length: 80 characters
- Red color
- Positioned at (23,1)
- Bright attribute
- Protected field

&nbsp;

*This is an auto-generated document by Swimm 🌊 and has not yet been verified by a human*

<SwmMeta version="3.0.0"><sup>Powered by [Swimm](https://app.swimm.io/)</sup></SwmMeta>
