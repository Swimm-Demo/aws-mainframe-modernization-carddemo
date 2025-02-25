---
title: User Navigation Screen (COMEN01)
---

The User Navigation screen (COMEN01) serves as the primary navigation interface for regular users, allowing them to select from various options such as viewing and updating account information, managing credit cards, and accessing transaction reports.

## Screen Preview

```
Tran: CM00                      AWS Mainframe Modernization           Date: mm/dd/yy
Prog: COMEN01C                            CardDemo                    Time: hh:mm:ss

                                    Main Menu

                    1. Account View
                    2. Account Update
                    3. Credit Card List
                    4. Credit Card View
                    5. Credit Card Update
                    6. Transaction List
                    7. Transaction View
                    8. Transaction Add
                    9. Transaction Reports
                    10. Bill Payment



              Please select an option : __


ENTER=Continue  F3=Exit
```

## Fields

### Transaction Name (TRNNAME)

- Length: 4 characters
- Color: Blue
- Fixed field

### Current Date (CURDATE)

- Length: 8 characters
- Color: Blue
- Initial value: 'mm/dd/yy'
- Fixed field

### Program Name (PGMNAME)

- Length: 8 characters
- Color: Blue
- Fixed field
- Associated document: <SwmLink doc-title="Main Menu Interface (COMEN01C)" repo-id="Z2l0aHViJTNBJTNBa3luZHJ5bC1hd3MtbWFpbmZyYW1lLW1vZGVybml6YXRpb24tY2FyZGRlbW8lM0ElM0FTd2ltbS1EZW1v" repo-name="aws-mainframe-modernization-carddemo" path="/.swm/main-menu-interface-comen01c.qa53x6s7.sw.md">[Main Menu Interface (COMEN01C)](https://app.swimm.io/repos/Z2l0aHViJTNBJTNBa3luZHJ5bC1hd3MtbWFpbmZyYW1lLW1vZGVybml6YXRpb24tY2FyZGRlbW8lM0ElM0FTd2ltbS1EZW1v/docs/qa53x6s7)</SwmLink>

### Current Time (CURTIME)

- Length: 8 characters
- Color: Blue
- Initial value: 'hh:mm:ss'
- Fixed field

### Main Menu Title

- Length: 9 characters
- Color: Neutral
- Initial value: 'Main Menu'
- Fixed field

### Option Fields (OPTN001 - OPTN012)

- Length: 40 characters each
- Color: Blue
- Initial value: Blank
- User input fields

### Option Selection (OPTION)

- Length: 2 characters
- Color: Blue
- Underlined
- Numeric input
- Right justified with zero fill
- User input field

### Error Message (ERRMSG)

- Length: 78 characters
- Color: Red
- User input field

&nbsp;

_This is an auto-generated document by Swimm 🌊 and has not yet been verified by a human_

<SwmMeta version="3.0.0"><sup>Powered by [Swimm](https://app.swimm.io/)</sup></SwmMeta>
