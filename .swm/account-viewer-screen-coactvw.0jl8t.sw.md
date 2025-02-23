---
title: Account Viewer Screen (COACTVW)
---
The Account Viewer screen (COACTVW) allows users to view detailed information about a specific account, including account status, balance, credit limits, and customer details. This screen is essential for users to verify and manage account information efficiently.

## Screen Preview

```
Tran: CAVW                  AWS Mainframe Modernization       Date: mm/dd/yy
Prog: COACTVWC                    CardDemo                   Time: hh:mm:ss

                                View Account

                  Account Number : ___________    Active Y/N: _

       Opened: __________                    Credit Limit        : +___,___,___.99
       Expiry: __________                    Cash credit Limit   : +___,___,___.99
      Reissue: __________                    Current Balance     : +___,___,___.99
                                            Current Cycle Credit : +___,___,___.99
Account Group: __________                    Current Cycle Debit : +___,___,___.99

                                Customer Details
Customer id  : _________                                SSN: ____________
Date of birth: __________                            FICO Score: ___

First Name                   Middle Name:                    Last Name : 
_________________________   _________________________   _________________________

Address: __________________________________________________  State __
        __________________________________________________  Zip _____
City    __________________________________________________ Country ___

Phone 1: _____________     Government Issued Id Ref    : ____________________
Phone 2: _____________     EFT Account Id: __________  Primary Card Holder Y/N:_


[message area - 45 characters                         ]
[error message area - 78 characters                                             ]
  F3=Exit
```

## Fields

### Current Date (CURDATE)

### Transaction Name (TRNNAME)

- Length: 4 characters
- Color: Blue
- Fixed information

### Program Name (PGMNAME)

- Length: 8 characters
- Color: Blue
- Fixed format: 'mm/dd/yy'

### Current Time (CURTIME)

- Length: 8 characters
- Color: Blue
- Fixed information

### Account Number (ACCTSID)

- Length: 8 characters
- Color: Blue
- Fixed format: 'hh:mm:ss'

### Active Status (ACSTTUS)

- Length: 11 characters
- Color: Green
- Underlined
- Must be filled
- Numeric validation: Must be a non-zero 11-digit number

### Account Open Date (ADTOPEN)

- Length: 1 character
- Underlined
- Accepts 'Y' or 'N'

### Credit Limit (ACRDLIM)

- Length: 10 characters
- Underlined
- Date format

### Expiry Date (AEXPDT)

- Length: 15 characters
- Right justified
- Numeric format: '+ZZZ,ZZZ,ZZZ.99'

### Cash Credit Limit (ACSHLIM)

- Length: 10 characters
- Underlined
- Date format

### Reissue Date (AREISDT)

- Length: 15 characters
- Right justified
- Numeric format: '+ZZZ,ZZZ,ZZZ.99'

### Current Balance (ACURBAL)

- Length: 10 characters
- Underlined
- Date format

### Current Cycle Credit (ACRCYCR)

- Length: 15 characters
- Right justified
- Numeric format: '+ZZZ,ZZZ,ZZZ.99'

### Account Group (AADDGRP)

- Length: 15 characters
- Right justified
- Numeric format: '+ZZZ,ZZZ,ZZZ.99'

### Current Cycle Debit (ACRCYDB)

- Length: 10 characters
- Underlined

### Customer ID (ACSTNUM)

- Length: 15 characters
- Right justified
- Numeric format: '+ZZZ,ZZZ,ZZZ.99'

### SSN (ACSTSSN)

- Length: 9 characters
- Underlined

### Date of Birth (ACSTDOB)

- Length: 12 characters
- Underlined
- Format: 'XXX-XX-XXXX'

### FICO Score (ACSTFCO)

- Length: 10 characters
- Underlined
- Date format

### First Name (ACSFNAM)

- Length: 3 characters
- Underlined
- Numeric

### Middle Name (ACSMNAM)

- Length: 25 characters
- Underlined

### Last Name (ACSLNAM)

- Length: 25 characters
- Underlined

### Address Line 1 (ACSADL1)

- Length: 25 characters
- Underlined

### State (ACSSTTE)

- Length: 50 characters
- Underlined

### Address Line 2 (ACSADL2)

- Length: 2 characters
- Underlined

### Zip Code (ACSZIPC)

- Length: 50 characters
- Underlined

### City (ACSCITY)

- Length: 5 characters
- Right justified
- Numeric

### Country (ACSCTRY)

- Length: 50 characters
- Underlined

### Phone 1 (ACSPHN1)

- Length: 3 characters
- Underlined

### Government Issued ID (ACSGOVT)

- Length: 13 characters
- Underlined

### Phone 2 (ACSPHN2)

- Length: 20 characters
- Underlined

### EFT Account ID (ACSEFTC)

- Length: 13 characters
- Underlined

### Primary Card Holder (ACSPFLG)

- Length: 10 characters
- Underlined

### Information Message (INFOMSG)

- Length: 1 character
- Underlined
- Accepts 'Y' or 'N'

### Error Message (ERRMSG)

- Length: 45 characters
- Neutral color
- Protected field

&nbsp;

*This is an auto-generated document by Swimm 🌊 and has not yet been verified by a human*

<SwmMeta version="3.0.0"><sup>Powered by [Swimm](https://app.swimm.io/)</sup></SwmMeta>
