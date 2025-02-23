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

<SwmSnippet path="/app/bms/COACTUP.bms" line="50" repo-id="Z2l0aHViJTNBJTNBa3luZHJ5bC1hd3MtbWFpbmZyYW1lLW1vZGVybml6YXRpb24tY2FyZGRlbW8lM0ElM0FTd2ltbS1EZW1v">

---

### Account Number

The Account Number field is an 11-character input field that is underlined. It is used to input the account number for the account being updated. This field is mandatory and must be numeric. If the account number is not provided or is invalid, an error message will be displayed.

```bms
               POS=(1,71),                                             -
               INITIAL='mm/dd/yy'
        DFHMDF ATTRB=(ASKIP,NORM),                                     -
```

---

</SwmSnippet>

<SwmSnippet path="/app/bms/COACTUP.bms" line="54" repo-id="Z2l0aHViJTNBJTNBa3luZHJ5bC1hd3MtbWFpbmZyYW1lLW1vZGVybml6YXRpb24tY2FyZGRlbW8lM0ElM0FTd2ltbS1EZW1v">

---

### Active Status

The Active Status field is a single-character input field that accepts 'Y' or 'N' to indicate whether the account is active. This field is underlined and must be provided. If the input is not 'Y' or 'N', an error message will be displayed.

```bms
               LENGTH=5,                                               -
               POS=(2,1),                                              -
               INITIAL='Prog:'
```

---

</SwmSnippet>

<SwmSnippet path="/app/bms/COACTUP.bms" line="58" repo-id="Z2l0aHViJTNBJTNBa3luZHJ5bC1hd3MtbWFpbmZyYW1lLW1vZGVybml6YXRpb24tY2FyZGRlbW8lM0ElM0FTd2ltbS1EZW1v">

---

### Open Date

The Open Date field consists of three parts: year (4 characters), month (2 characters), and day (2 characters). Each part is underlined and must be provided. The date must be in the format 'YYYY-MM-DD'. If any part of the date is not provided or is invalid, an error message will be displayed.

```bms
               COLOR=BLUE,                                             -
               LENGTH=8,                                               -
               POS=(2,7)
TITLE02 DFHMDF ATTRB=(ASKIP,NORM),                                     -
               COLOR=YELLOW,                                           -
               LENGTH=40,                                              -
               POS=(2,21)
```

---

</SwmSnippet>

<SwmSnippet path="/app/bms/COACTUP.bms" line="66" repo-id="Z2l0aHViJTNBJTNBa3luZHJ5bC1hd3MtbWFpbmZyYW1lLW1vZGVybml6YXRpb24tY2FyZGRlbW8lM0ElM0FTd2ltbS1EZW1v">

---

### Credit Limit

The Credit Limit field is a 15-character input field that is underlined. It is used to input the credit limit for the account. This field must be numeric and provided. If the credit limit is not provided or is invalid, an error message will be displayed.

```bms
               COLOR=BLUE,                                             -
               LENGTH=5,                                               -
               POS=(2,65),                                             -
```

---

</SwmSnippet>

<SwmSnippet path="/app/bms/COACTUP.bms" line="70" repo-id="Z2l0aHViJTNBJTNBa3luZHJ5bC1hd3MtbWFpbmZyYW1lLW1vZGVybml6YXRpb24tY2FyZGRlbW8lM0ElM0FTd2ltbS1EZW1v">

---

### Expiry Date

The Expiry Date field consists of three parts: year (4 characters), month (2 characters), and day (2 characters). Each part is underlined and must be provided. The date must be in the format 'YYYY-MM-DD'. If any part of the date is not provided or is invalid, an error message will be displayed.

```bms
CURTIME DFHMDF ATTRB=(ASKIP,NORM),                                     -
               COLOR=BLUE,                                             -
               LENGTH=8,                                               -
               POS=(2,71),                                             -
               INITIAL='hh:mm:ss'
        DFHMDF COLOR=NEUTRAL,                                          -
               LENGTH=14,                                              -
```

---

</SwmSnippet>

<SwmSnippet path="/app/bms/COACTUP.bms" line="78" repo-id="Z2l0aHViJTNBJTNBa3luZHJ5bC1hd3MtbWFpbmZyYW1lLW1vZGVybml6YXRpb24tY2FyZGRlbW8lM0ElM0FTd2ltbS1EZW1v">

---

### Cash Credit Limit

The Cash Credit Limit field is a 15-character input field that is underlined. It is used to input the cash credit limit for the account. This field must be numeric and provided. If the cash credit limit is not provided or is invalid, an error message will be displayed.

```bms
               INITIAL='Update Account'
        DFHMDF ATTRB=(ASKIP,NORM),                                     -
               COLOR=TURQUOISE,                                        -
```

---

</SwmSnippet>

<SwmSnippet path="/app/bms/COACTUP.bms" line="82" repo-id="Z2l0aHViJTNBJTNBa3luZHJ5bC1hd3MtbWFpbmZyYW1lLW1vZGVybml6YXRpb24tY2FyZGRlbW8lM0ElM0FTd2ltbS1EZW1v">

---

### Reissue Date

The Reissue Date field consists of three parts: year (4 characters), month (2 characters), and day (2 characters). Each part is underlined and must be provided. The date must be in the format 'YYYY-MM-DD'. If any part of the date is not provided or is invalid, an error message will be displayed.

```bms
               POS=(5,19),                                             -
               INITIAL='Account Number :'
ACCTSID DFHMDF ATTRB=(IC,UNPROT),                                      -
               HILIGHT=UNDERLINE,                                      -
               LENGTH=11,                                              -
               POS=(5,38)
        DFHMDF LENGTH=0,                                               -
```

---

</SwmSnippet>

<SwmSnippet path="/app/bms/COACTUP.bms" line="90" repo-id="Z2l0aHViJTNBJTNBa3luZHJ5bC1hd3MtbWFpbmZyYW1lLW1vZGVybml6YXRpb24tY2FyZGRlbW8lM0ElM0FTd2ltbS1EZW1v">

---

### Current Balance

The Current Balance field is a 15-character input field that is underlined. It is used to input the current balance for the account. This field must be numeric and provided. If the current balance is not provided or is invalid, an error message will be displayed.

```bms
        DFHMDF COLOR=TURQUOISE,                                        -
               LENGTH=12,                                              -
               POS=(5,57),                                             -
```

---

</SwmSnippet>

<SwmSnippet path="/app/bms/COACTUP.bms" line="94" repo-id="Z2l0aHViJTNBJTNBa3luZHJ5bC1hd3MtbWFpbmZyYW1lLW1vZGVybml6YXRpb24tY2FyZGRlbW8lM0ElM0FTd2ltbS1EZW1v">

---

### Current Cycle Credit

The Current Cycle Credit field is a 15-character input field that is underlined. It is used to input the current cycle credit for the account. This field must be numeric and provided. If the current cycle credit is not provided or is invalid, an error message will be displayed.

```bms
ACSTTUS DFHMDF ATTRB=(UNPROT),                                         -
               HILIGHT=UNDERLINE,                                      -
               LENGTH=1,                                               -
```

---

</SwmSnippet>

<SwmSnippet path="/app/bms/COACTUP.bms" line="98" repo-id="Z2l0aHViJTNBJTNBa3luZHJ5bC1hd3MtbWFpbmZyYW1lLW1vZGVybml6YXRpb24tY2FyZGRlbW8lM0ElM0FTd2ltbS1EZW1v">

---

### Account Group

The Account Group field is a 10-character input field that is underlined. It is used to input the account group for the account. This field must be provided. If the account group is not provided, an error message will be displayed.

```bms
        DFHMDF LENGTH=0,                                               -
               POS=(5,72)
        DFHMDF COLOR=TURQUOISE,                                        -
```

---

</SwmSnippet>

<SwmSnippet path="/app/bms/COACTUP.bms" line="102" repo-id="Z2l0aHViJTNBJTNBa3luZHJ5bC1hd3MtbWFpbmZyYW1lLW1vZGVybml6YXRpb24tY2FyZGRlbW8lM0ElM0FTd2ltbS1EZW1v">

---

### Current Cycle Debit

The Current Cycle Debit field is a 15-character input field that is underlined. It is used to input the current cycle debit for the account. This field must be numeric and provided. If the current cycle debit is not provided or is invalid, an error message will be displayed.

```bms
               POS=(6,8),                                              -
               INITIAL='Opened :'
OPNYEAR DFHMDF ATTRB=(FSET,UNPROT),                                    -
```

---

</SwmSnippet>

<SwmSnippet path="/app/bms/COACTUP.bms" line="106" repo-id="Z2l0aHViJTNBJTNBa3luZHJ5bC1hd3MtbWFpbmZyYW1lLW1vZGVybml6YXRpb24tY2FyZGRlbW8lM0ElM0FTd2ltbS1EZW1v">

---

### Customer ID

The Customer ID field is a 9-character input field that is underlined. It is used to input the customer ID for the account. This field is not editable and is used for display purposes only.

```bms
               JUSTIFY=(RIGHT),                                        -
               LENGTH=4,                                               -
               POS=(6,17)
```

---

</SwmSnippet>

<SwmSnippet path="/app/bms/COACTUP.bms" line="110" repo-id="Z2l0aHViJTNBJTNBa3luZHJ5bC1hd3MtbWFpbmZyYW1lLW1vZGVybml6YXRpb24tY2FyZGRlbW8lM0ElM0FTd2ltbS1EZW1v">

---

### Social Security Number

The Social Security Number (SSN) field consists of three parts: first part (3 characters), second part (2 characters), and third part (4 characters). Each part is underlined and must be provided. The SSN must be in the format '999-99-9999'. If any part of the SSN is not provided or is invalid, an error message will be displayed.

```bms
               POS=(6,22),                                             -
               INITIAL='-'
OPNMON  DFHMDF ATTRB=(UNPROT),                                         -
               HILIGHT=UNDERLINE,                                      -
               JUSTIFY=(RIGHT),                                        -
               LENGTH=2,                                               -
               POS=(6,24)
```

---

</SwmSnippet>

<SwmSnippet path="/app/bms/COACTUP.bms" line="118" repo-id="Z2l0aHViJTNBJTNBa3luZHJ5bC1hd3MtbWFpbmZyYW1lLW1vZGVybml6YXRpb24tY2FyZGRlbW8lM0ElM0FTd2ltbS1EZW1v">

---

### Date of Birth

The Date of Birth field consists of three parts: year (4 characters), month (2 characters), and day (2 characters). Each part is underlined and must be provided. The date must be in the format 'YYYY-MM-DD'. If any part of the date is not provided or is invalid, an error message will be displayed.

```bms
               POS=(6,27),                                             -
               INITIAL='-'
OPNDAY  DFHMDF ATTRB=(UNPROT),                                         -
               HILIGHT=UNDERLINE,                                      -
               JUSTIFY=(RIGHT),                                        -
               LENGTH=2,                                               -
               POS=(6,29)
```

---

</SwmSnippet>

<SwmSnippet path="/app/bms/COACTUP.bms" line="126" repo-id="Z2l0aHViJTNBJTNBa3luZHJ5bC1hd3MtbWFpbmZyYW1lLW1vZGVybml6YXRpb24tY2FyZGRlbW8lM0ElM0FTd2ltbS1EZW1v">

---

### FICO Score

The FICO Score field is a 3-character input field that is underlined. It is used to input the FICO score for the customer. This field must be numeric and provided. If the FICO score is not provided or is invalid, an error message will be displayed.

```bms
               POS=(6,32)
        DFHMDF ATTRB=(ASKIP,NORM),                                     -
               COLOR=TURQUOISE,                                        -
```

---

</SwmSnippet>

<SwmSnippet path="/app/bms/COACTUP.bms" line="130" repo-id="Z2l0aHViJTNBJTNBa3luZHJ5bC1hd3MtbWFpbmZyYW1lLW1vZGVybml6YXRpb24tY2FyZGRlbW8lM0ElM0FTd2ltbS1EZW1v">

---

### First Name

The First Name field is a 25-character input field that is underlined. It is used to input the first name of the customer. This field must be provided and can only contain alphabets and spaces. If the first name is not provided or contains invalid characters, an error message will be displayed.

```bms
               POS=(6,39),                                             -
               INITIAL='Credit Limit        :'
ACRDLIM DFHMDF ATTRB=(FSET,UNPROT),                                    -
```

---

</SwmSnippet>

<SwmSnippet path="/app/bms/COACTUP.bms" line="134" repo-id="Z2l0aHViJTNBJTNBa3luZHJ5bC1hd3MtbWFpbmZyYW1lLW1vZGVybml6YXRpb24tY2FyZGRlbW8lM0ElM0FTd2ltbS1EZW1v">

---

### Middle Name

The Middle Name field is a 25-character input field that is underlined. It is used to input the middle name of the customer. This field is optional and can only contain alphabets and spaces. If the middle name contains invalid characters, an error message will be displayed.

```bms
               LENGTH=15,                                              -
               POS=(6,61)
        DFHMDF LENGTH=0,                                               -
```

---

</SwmSnippet>

&nbsp;

*This is an auto-generated document by Swimm 🌊 and has not yet been verified by a human*

<SwmMeta version="3.0.0"><sup>Powered by [Swimm](https://staging.swimm.cloud/)</sup></SwmMeta>
