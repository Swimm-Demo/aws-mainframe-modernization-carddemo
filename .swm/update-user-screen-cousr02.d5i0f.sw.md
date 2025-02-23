---
title: Update User Screen (COUSR02)
---
The Update User screen (COUSR02) allows administrators to modify user details such as first name, last name, password, and user type. This screen is essential for maintaining accurate and up-to-date user information within the system.

## Screen Preview

```
Tran: CU02                AWS Mainframe Modernization             Date: mm/dd/yy
Prog: COUSR02C                      CardDemo                      Time: hh:mm:ss

                              Update User

     Enter User ID: ________

     **********************************************************

          First Name: __________________    Last Name: __________________

          Password: ________  (8 Char)

          User Type: _  (A=Admin, U=User)






[Error message area                                                           ]
ENTER=Fetch  F3=Save&Exit  F4=Clear  F5=Save  F12=Cancel
```

## Fields

### Current Date (CURDATE)

### Transaction Name (TRNNAME)

- Length: 4 characters
- Color: Blue
- This field is skipped and set by the program.

### Program Name (PGMNAME)

- Length: 8 characters
- Color: Blue
- Initial value: 'mm/dd/yy'
- This field is skipped and set by the program.

### Current Time (CURTIME)

- Length: 8 characters
- Color: Blue
- This field is skipped and set by the program.

### User ID Input (USRIDIN)

- Length: 8 characters
- Color: Blue
- Initial value: 'hh:mm:ss'
- This field is skipped and set by the program.

### First Name (FNAME)

- Length: 8 characters
- Color: Green
- Underlined
- Required field
- Initial cursor position
- Validation: Cannot be empty

### Last Name (LNAME)

- Length: 20 characters
- Color: Green
- Underlined
- Required field
- Validation: Cannot be empty

### Password (PASSWD)

- Length: 20 characters
- Color: Green
- Underlined
- Required field
- Validation: Cannot be empty

### User Type (USRTYPE)

- Length: 8 characters
- Color: Green
- Underlined
- Required field
- Dark attribute
- Validation: Cannot be empty

### Error Message (ERRMSG)

- Length: 1 character
- Color: Green
- Underlined
- Required field
- Validation: Must be 'A' for Admin or 'U' for User

&nbsp;

*This is an auto-generated document by Swimm 🌊 and has not yet been verified by a human*

<SwmMeta version="3.0.0"><sup>Powered by [Swimm](https://app.swimm.io/)</sup></SwmMeta>
