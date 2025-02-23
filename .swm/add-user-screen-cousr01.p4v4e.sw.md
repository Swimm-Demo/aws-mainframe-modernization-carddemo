---
title: Add User Screen (COUSR01)
---
The Add User screen (COUSR01) allows administrators to add new users to the system by entering their first name, last name, user ID, password, and user type. This screen ensures that all necessary user information is collected and validated before adding the user to the database.

## Screen Preview

```
Tran: CU01                      AWS Mainframe Modernization          Date: mm/dd/yy
Prog: COUSR01C                            CardDemo                   Time: hh:mm:ss

                                   Add User

     First Name: ____________________          Last Name: ____________________

     User ID: __________ (8 Char)          Password: ________ (8 Char)

     User Type: _ (A=Admin, U=User)




[Error/Status Message Area]

ENTER=Add User F3=Back F4=Clear F12=Exit
```

## Fields

### Current Date (CURDATE)

### Transaction Name (TRNNAME)

- Length: 4 characters
- Fixed value: 'CU01'
- Color: Blue
- Attribute: Skip, Field Set

### Program Name (PGMNAME)

- Length: 8 characters
- Fixed value: 'mm/dd/yy'
- Color: Blue
- Attribute: Skip, Field Set

### Current Time (CURTIME)

- Length: 8 characters
- Fixed value: 'COUSR01C'
- Color: Blue
- Attribute: Skip, Field Set

### First Name (FNAME)

- Length: 8 characters
- Fixed value: 'hh:mm:ss'
- Color: Blue
- Attribute: Skip, Field Set

### Last Name (LNAME)

- Length: 20 characters
- Required field
- Color: Green
- Attribute: Field Set, Initial Cursor, Unprotected, Underlined
- Validation: Cannot be empty

### User ID (USERID)

- Length: 20 characters
- Required field
- Color: Green
- Attribute: Field Set, Unprotected, Underlined
- Validation: Cannot be empty

### Password (PASSWD)

- Length: 8 characters
- Required field
- Color: Green
- Attribute: Field Set, Unprotected, Underlined
- Validation: Cannot be empty
- Note: '(8 Char)' is displayed next to the field

### User Type (USRTYPE)

- Length: 8 characters
- Required field
- Color: Green
- Attribute: Dark, Field Set, Unprotected, Underlined
- Validation: Cannot be empty
- Note: '(8 Char)' is displayed next to the field

### Error/Status Message Area (ERRMSG)

- Length: 1 character
- Required field
- Color: Green
- Attribute: Field Set, Unprotected, Underlined
- Validation: Cannot be empty
- Note: '(A=Admin, U=User)' is displayed next to the field

&nbsp;

*This is an auto-generated document by Swimm 🌊 and has not yet been verified by a human*

<SwmMeta version="3.0.0"><sup>Powered by [Swimm](https://staging.swimm.cloud/)</sup></SwmMeta>
