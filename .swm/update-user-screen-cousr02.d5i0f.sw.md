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

### Transaction Name (TRNNAME)

- Length: 4 characters
- Display-only field
- Color: Blue

### Current Date (CURDATE)

- Length: 8 characters
- Display-only field
- Color: Blue
- Format: mm/dd/yy

### Program Name (PGMNAME)

- Length: 8 characters
- Display-only field
- Color: Blue
- Associated document: <SwmLink doc-title="Update Users (COUSR02C)" repo-id="Z2l0aHViJTNBJTNBa3luZHJ5bC1hd3MtbWFpbmZyYW1lLW1vZGVybml6YXRpb24tY2FyZGRlbW8lM0ElM0FTd2ltbS1EZW1v" repo-name="aws-mainframe-modernization-carddemo" path="/.swm/update-users-cousr02c.ksabdybr.sw.md">[Update Users (COUSR02C)](https://app.swimm.io/repos/Z2l0aHViJTNBJTNBa3luZHJ5bC1hd3MtbWFpbmZyYW1lLW1vZGVybml6YXRpb24tY2FyZGRlbW8lM0ElM0FTd2ltbS1EZW1v/docs/ksabdybr)</SwmLink>

### Current Time (CURTIME)

- Length: 8 characters
- Display-only field
- Color: Blue
- Format: hh:mm:ss

### User ID Input (USRIDIN)

- Length: 8 characters
- Required field
- Underlined, green color
- Initial cursor position

### First Name (FNAME)

- Length: 20 characters
- Editable field
- Underlined, green color

### Last Name (LNAME)

- Length: 20 characters
- Editable field
- Underlined, green color

### Password (PASSWD)

- Length: 8 characters
- Editable field
- Underlined, green color
- Hidden input (dark)

### User Type (USRTYPE)

- Length: 1 character
- Editable field
- Underlined, green color
- Valid values: A (Admin), U (User)

&nbsp;

*This is an auto-generated document by Swimm 🌊 and has not yet been verified by a human*

<SwmMeta version="3.0.0"><sup>Powered by [Swimm](https://app.swimm.io/)</sup></SwmMeta>
