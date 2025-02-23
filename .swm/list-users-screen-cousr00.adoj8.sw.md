---
title: List Users Screen (COUSR00)
---
The List Users screen (COUSR00) provides an interface for displaying and managing user information. It allows users to search for specific user IDs, view details such as first name, last name, and user type, and perform actions like updating or deleting user records.

## Screen Preview

```
Tran: CU00                      AWS Mainframe Modernization          Date: mm/dd/yy
Prog: COUSR00C                            CardDemo                   Time: hh:mm:ss

                                   List Users

     Search User ID: ___________

     Sel     User ID       First Name            Last Name             Type
     ---     --------      --------------------  --------------------  ----
     _       ________      ____________________  ____________________  _
     _       ________      ____________________  ____________________  _
     _       ________      ____________________  ____________________  _
     _       ________      ____________________  ____________________  _
     _       ________      ____________________  ____________________  _
     _       ________      ____________________  ____________________  _
     _       ________      ____________________  ____________________  _
     _       ________      ____________________  ____________________  _
     _       ________      ____________________  ____________________  _
     _       ________      ____________________  ____________________  _

     Type 'U' to Update or 'D' to Delete a User from the list

[Error/Status Message Area]

ENTER=Continue  F3=Back  F7=Backward  F8=Forward
```

## Fields

### Program Name (PGMNAME)

### Transaction Name (TRNNAME)

- Length: 4 characters
- Fixed value: 'CU00'
- Color: Blue
- Attribute: Skip, Field Set

### Current Date (CURDATE)

- Length: 8 characters
- Fixed value: 'COUSR00C'
- Color: Blue
- Attribute: Skip, Field Set

### Current Time (CURTIME)

- Length: 8 characters
- Initial value: 'mm/dd/yy'
- Color: Blue
- Attribute: Skip, Field Set

### Page Number (PAGENUM)

- Length: 8 characters
- Initial value: 'hh:mm:ss'
- Color: Blue
- Attribute: Skip, Field Set

### Search User ID (USRIDIN)

- Length: 8 characters
- Initial value: Blank
- Color: Blue
- Attribute: Skip, Field Set

### User Selection (SEL0001 - SEL0010)

- Length: 8 characters
- Color: Green
- Attribute: Field Set, Normal, Unprotected
- Highlight: Underline

### User ID (USRID01 - USRID10)

- Length: 1 character
- Color: Green
- Attribute: Field Set, Normal, Unprotected
- Highlight: Underline
- Valid values: 'U' for Update, 'D' for Delete

### First Name (FNAME01 - FNAME10)

- Length: 8 characters
- Initial value: Blank
- Color: Blue
- Attribute: Skip, Field Set

### Last Name (LNAME01 - LNAME10)

- Length: 20 characters
- Initial value: Blank
- Color: Blue
- Attribute: Skip, Field Set

### User Type (UTYPE01 - UTYPE10)

- Length: 20 characters
- Initial value: Blank
- Color: Blue
- Attribute: Skip, Field Set

### Error/Status Message (ERRMSG)

- Length: 1 character
- Initial value: Blank
- Color: Blue
- Attribute: Skip, Field Set

&nbsp;

*This is an auto-generated document by Swimm 🌊 and has not yet been verified by a human*

<SwmMeta version="3.0.0"><sup>Powered by [Swimm](https://staging.swimm.cloud/)</sup></SwmMeta>
