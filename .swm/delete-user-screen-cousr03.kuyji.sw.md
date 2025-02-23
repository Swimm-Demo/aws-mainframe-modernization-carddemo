---
title: Delete User Screen (COUSR03)
---
The Delete User screen (COUSR03) allows administrators to remove a user from the system by entering the user's ID. The screen displays the user's first name, last name, and user type for confirmation before deletion.

## Screen Preview

```
Tran: CU03                      AWS Mainframe Modernization          Date: mm/dd/yy
Prog: COUSR03C                            CardDemo                   Time: hh:mm:ss

                                   Delete User

     Enter User ID: __________

     ***********************************************

     First Name: ____________________

     Last Name: ____________________

     User Type: _ (A=Admin, U=User)




[Error/Status Message Area]

ENTER=Fetch F3=Back F4=Clear F5=Delete
```

## Fields

### Current Date (CURDATE)

### Transaction Name (TRNNAME)

- Length: 4 characters
- Fixed value: 'CU03'
- Color: Blue
- Attribute: Skip, Field Set, Normal

### Program Name (PGMNAME)

- Length: 8 characters
- Fixed value: 'mm/dd/yy'
- Color: Blue
- Attribute: Skip, Field Set, Normal

### Current Time (CURTIME)

- Length: 8 characters
- Fixed value: 'COUSR03C'
- Color: Blue
- Attribute: Skip, Field Set, Normal

### User ID Input (USRIDIN)

- Length: 8 characters
- Fixed value: 'hh:mm:ss'
- Color: Blue
- Attribute: Skip, Field Set, Normal

### First Name (FNAME)

- Length: 8 characters
- Required field
- Color: Green
- Attribute: Field Set, Initial Cursor, Normal, Unprotected
- Highlight: Underline
- Validation: Cannot be empty

### Last Name (LNAME)

- Length: 20 characters
- Color: Blue
- Attribute: Skip, Field Set, Normal
- Highlight: Underline

### User Type (USRTYPE)

- Length: 20 characters
- Color: Blue
- Attribute: Skip, Field Set, Normal
- Highlight: Underline

### Error/Status Message Area (ERRMSG)

- Length: 1 character
- Color: Blue
- Attribute: Skip, Field Set, Normal
- Highlight: Underline
- Valid values: 'A' for Admin, 'U' for User

&nbsp;

*This is an auto-generated document by Swimm 🌊 and has not yet been verified by a human*

<SwmMeta version="3.0.0"><sup>Powered by [Swimm](https://staging.swimm.cloud/)</sup></SwmMeta>
