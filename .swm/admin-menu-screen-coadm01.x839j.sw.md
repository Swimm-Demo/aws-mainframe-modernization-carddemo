---
title: Admin Menu Screen (COADM01)
---
The Admin Menu screen (COADM01) serves as the central navigation point for administrative users, providing options to manage user security, including listing, adding, updating, and deleting users.

## Screen Preview

```
Tran: CA00                      AWS Mainframe Modernization       Date: mm/dd/yy
Prog: COADM01C                            CardDemo               Time: hh:mm:ss

                                  Admin Menu

                    1. User List (Security)               
                    2. User Add (Security)                
                    3. User Update (Security)             
                    4. User Delete (Security)             




               Please select an option : __


ENTER=Continue  F3=Exit
```

## Fields

### Title 01 (TITLE01)

### Transaction Name (TRNNAME)

- Length: 4 characters
- Color: Blue
- Fixed field

### Current Date (CURDATE)

- Length: 40 characters
- Color: Yellow
- Fixed field

### Program Name (PGMNAME)

- Length: 8 characters
- Color: Blue
- Fixed field
- Format: mm/dd/yy

### Title 02 (TITLE02)

- Length: 8 characters
- Color: Blue
- Fixed field

### Current Time (CURTIME)

- Length: 40 characters
- Color: Yellow
- Fixed field

### Admin Menu Title

- Length: 8 characters
- Color: Blue
- Fixed field
- Format: hh:mm:ss

### Option Fields (OPTN001 - OPTN012)

- Length: 10 characters
- Color: Neutral
- Fixed field
- Bright attribute

### Option Input (OPTION)

- Length: 40 characters
- Color: Blue
- Initial value: Blank
- These fields are used to display the menu options available to the admin user.

### Error Message (ERRMSG)

- Length: 2 characters
- Underlined
- Numeric
- Unprotected field
- Justified to the right with zero fill
- This field is used for the user to input their menu selection.

### Footer

- Length: 78 characters
- Color: Red
- Bright attribute
- This field is used to display error messages to the user.

&nbsp;

*This is an auto-generated document by Swimm 🌊 and has not yet been verified by a human*

<SwmMeta version="3.0.0"><sup>Powered by [Swimm](https://app.swimm.io/)</sup></SwmMeta>
