---
title: Account Update Screen (COACTUP)
---

The Account Update screen (COACTUP) allows users to update various details related to an account, including account status, credit limits, balance, customer details, and more. This screen is essential for maintaining up-to-date account information and ensuring accurate data management.

## Screen Preview

```
Tran:     Date: mm/dd/yy
Prog:     Time: hh:mm:ss

                                   Update Account

     Account Number : ___________

     Active Y/N: _

     Opened : ____-__-__

     Credit Limit : _______________

     Expiry : ____-__-__

     Cash credit Limit : _______________

     Reissue: ____-__-__

     Current Balance : _______________

     Current Cycle Credit: _______________

     Account Group: __________

     Current Cycle Debit : _______________

                                   Customer Details

     Customer id : _________

     SSN: 999-99-9999

     Date of birth: ____-__-__

     FICO Score: ___

     First Name

     Middle Name:

     Last Name :

     Address:

     State

     Zip

     City

     Country

     Phone 1: ___ ___ ____

     Government Issued Id Ref : ____________________

     Phone 2: ___ ___ ____

     EFT Account Id: __________

     Primary Card Holder Y/N:_

[Error/Status Message Area]

ENTER=Process F3=Exit  F5=Save  F12=Cancel
```

## Fields

### Transaction Name (TRNNAME)

- Length: 4 characters
- Fixed field
- Blue color
- Positioned at row 1, column 7

### Current Date (CURDATE)

- Length: 8 characters
- Fixed field
- Blue color
- Positioned at row 1, column 71
- Initial value: 'mm/dd/yy'

### Program Name (PGMNAME)

- Length: 8 characters
- Fixed field
- Blue color
- Positioned at row 2, column 7
- Associated document: <SwmLink doc-title="Account Update Processing Flow (COACTUPC)" repo-id="Z2l0aHViJTNBJTNBa3luZHJ5bC1hd3MtbWFpbmZyYW1lLW1vZGVybml6YXRpb24tY2FyZGRlbW8lM0ElM0FTd2ltbS1EZW1v" repo-name="aws-mainframe-modernization-carddemo" path="/.swm/account-update-processing-flow-coactupc.hf1esjw9.sw.md">[Account Update Processing Flow (COACTUPC)](https://app.swimm.io/repos/Z2l0aHViJTNBJTNBa3luZHJ5bC1hd3MtbWFpbmZyYW1lLW1vZGVybml6YXRpb24tY2FyZGRlbW8lM0ElM0FTd2ltbS1EZW1v/docs/hf1esjw9)</SwmLink>

### Current Time (CURTIME)

- Length: 8 characters
- Fixed field
- Blue color
- Positioned at row 2, column 71
- Initial value: 'hh:mm:ss'

### Account Number (ACCTSID)

- Length: 11 characters
- Input field
- Underlined
- Positioned at row 5, column 38

### Active Status (ACSTTUS)

- Length: 1 character
- Input field
- Underlined
- Positioned at row 5, column 70

### Opened Date (OPNYEAR, OPNMON, OPNDAY)

- Year: 4 characters, Month: 2 characters, Day: 2 characters
- Input fields
- Underlined
- Positioned at row 6, columns 17, 24, 29 respectively
- Separated by hyphens

### Credit Limit (ACRDLIM)

- Length: 15 characters
- Input field
- Underlined
- Positioned at row 6, column 61

### Expiry Date (EXPYEAR, EXPMON, EXPDAY)

- Year: 4 characters, Month: 2 characters, Day: 2 characters
- Input fields
- Underlined
- Positioned at row 7, columns 17, 24, 29 respectively
- Separated by hyphens

### Cash Credit Limit (ACSHLIM)

- Length: 15 characters
- Input field
- Underlined
- Positioned at row 7, column 61

### Reissue Date (RISYEAR, RISMON, RISDAY)

- Year: 4 characters, Month: 2 characters, Day: 2 characters
- Input fields
- Underlined
- Positioned at row 8, columns 17, 24, 29 respectively
- Separated by hyphens

### Current Balance (ACURBAL)

- Length: 15 characters
- Input field
- Underlined
- Positioned at row 8, column 61

### Current Cycle Credit (ACRCYCR)

- Length: 15 characters
- Input field
- Underlined
- Positioned at row 9, column 61

### Account Group (AADDGRP)

- Length: 10 characters
- Input field
- Underlined
- Positioned at row 10, column 23

### Current Cycle Debit (ACRCYDB)

- Length: 15 characters
- Input field
- Underlined
- Positioned at row 10, column 61

### Customer ID (ACSTNUM)

- Length: 9 characters
- Input field
- Underlined
- Positioned at row 12, column 23

### SSN (ACTSSN1, ACTSSN2, ACTSSN3)

- Format: 999-99-9999
- Input fields
- Underlined
- Positioned at row 12, columns 55, 61, 66 respectively
- Separated by hyphens

### Date of Birth (DOBYEAR, DOBMON, DOBDAY)

- Year: 4 characters, Month: 2 characters, Day: 2 characters
- Input fields
- Underlined
- Positioned at row 13, columns 23, 30, 35 respectively
- Separated by hyphens

### FICO Score (ACSTFCO)

- Length: 3 characters
- Input field
- Underlined
- Positioned at row 13, column 62

### First Name (ACSFNAM)

- Length: 25 characters
- Input field
- Underlined
- Positioned at row 15, column 1

### Middle Name (ACSMNAM)

- Length: 25 characters
- Input field
- Underlined
- Positioned at row 15, column 28

### Last Name (ACSLNAM)

- Length: 25 characters
- Input field
- Underlined
- Positioned at row 15, column 55

### Address Line 1 (ACSADL1)

- Length: 50 characters
- Input field
- Underlined
- Positioned at row 16, column 10

### State (ACSSTTE)

- Length: 2 characters
- Input field
- Underlined
- Positioned at row 16, column 73

### Address Line 2 (ACSADL2)

- Length: 50 characters
- Input field
- Underlined
- Positioned at row 17, column 10

### Zip Code (ACSZIPC)

- Length: 5 characters
- Input field
- Underlined
- Positioned at row 17, column 73

### City (ACSCITY)

- Length: 50 characters
- Input field
- Underlined
- Positioned at row 18, column 10

### Country (ACSCTRY)

- Length: 3 characters
- Input field
- Underlined
- Positioned at row 18, column 73

### Phone 1 (ACSPH1A, ACSPH1B, ACSPH1C)

- Format: 3-3-4
- Input fields
- Underlined
- Positioned at row 19, columns 10, 14, 18 respectively

### Government Issued ID Reference (ACSGOVT)

- Length: 20 characters
- Input field
- Underlined
- Positioned at row 19, column 58

### Phone 2 (ACSPH2A, ACSPH2B, ACSPH2C)

- Format: 3-3-4
- Input fields
- Underlined
- Positioned at row 20, columns 10, 14, 18 respectively

### EFT Account ID (ACSEFTC)

- Length: 10 characters
- Input field
- Underlined
- Positioned at row 20, column 41

### Primary Card Holder (ACSPFLG)

- Length: 1 character
- Input field
- Underlined
- Positioned at row 20, column 78

### Information Message (INFOMSG)

- Length: 45 characters
- Display field
- Neutral color
- Positioned at row 22, column 23

### Error Message (ERRMSG)

- Length: 78 characters
- Display field
- Red color
- Positioned at row 23, column 1

&nbsp;

_This is an auto-generated document by Swimm 🌊 and has not yet been verified by a human_

<SwmMeta version="3.0.0"><sup>Powered by [Swimm](https://app.swimm.io/)</sup></SwmMeta>
