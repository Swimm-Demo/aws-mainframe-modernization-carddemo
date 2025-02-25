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

### Transaction Name (TRNNAME)

- Length: 4 characters
- Fixed information
- Color: Blue

### Title (TITLE01)

- Length: 40 characters
- Fixed information
- Color: Yellow

### Current Date (CURDATE)

- Length: 8 characters
- Fixed information
- Format: mm/dd/yy
- Color: Blue

### Program Name (PGMNAME)

- Length: 8 characters
- Fixed information
- Color: Blue
- Associated document: <SwmLink doc-title="List Users (COUSR00C)" repo-id="Z2l0aHViJTNBJTNBa3luZHJ5bC1hd3MtbWFpbmZyYW1lLW1vZGVybml6YXRpb24tY2FyZGRlbW8lM0ElM0FTd2ltbS1EZW1v" repo-name="aws-mainframe-modernization-carddemo" path="/.swm/list-users-cousr00c.7fm42ie0.sw.md">[List Users (COUSR00C)](https://app.swimm.io/repos/Z2l0aHViJTNBJTNBa3luZHJ5bC1hd3MtbWFpbmZyYW1lLW1vZGVybml6YXRpb24tY2FyZGRlbW8lM0ElM0FTd2ltbS1EZW1v/docs/7fm42ie0)</SwmLink>

### Title (TITLE02)

- Length: 40 characters
- Fixed information
- Color: Yellow

### Current Time (CURTIME)

- Length: 8 characters
- Fixed information
- Format: hh:mm:ss
- Color: Blue

### Page Number (PAGENUM)

- Length: 8 characters
- Fixed information
- Color: Blue

### Search User ID (USRIDIN)

- Length: 8 characters
- User input field
- Underlined, green color

### Selection (SEL0001 - SEL0010)

- Length: 1 character
- User input field
- Underlined, green color

### User ID (USRID01 - USRID10)

- Length: 8 characters
- Fixed information
- Color: Blue

### First Name (FNAME01 - FNAME10)

- Length: 20 characters
- Fixed information
- Color: Blue

### Last Name (LNAME01 - LNAME10)

- Length: 20 characters
- Fixed information
- Color: Blue

### User Type (UTYPE01 - UTYPE10)

- Length: 1 character
- Fixed information
- Color: Blue

&nbsp;

*This is an auto-generated document by Swimm 🌊 and has not yet been verified by a human*

<SwmMeta version="3.0.0"><sup>Powered by [Swimm](https://app.swimm.io/)</sup></SwmMeta>
