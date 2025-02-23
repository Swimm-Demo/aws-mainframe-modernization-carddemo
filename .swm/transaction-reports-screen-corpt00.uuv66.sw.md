---
title: Transaction Reports Screen (CORPT00)
---
The Transaction Reports screen (CORPT00) serves as the primary interface for users to select and generate various transaction reports. Users can choose between monthly, yearly, or custom date range reports, and confirm their selection for printing.

## Screen Preview

```
Tran: CR00                      AWS Mainframe Modernization          Date: mm/dd/yy
Prog: CORPT00C                            CardDemo                   Time: hh:mm:ss

                                   Transaction Reports

     _  Monthly (Current Month)

     _  Yearly (Current Year)

     _  Custom (Date Range)

          Start Date : __ / __ / ____ (MM/DD/YYYY)
          End Date   : __ / __ / ____ (MM/DD/YYYY)

     The Report will be submitted for printing. Please confirm: _ (Y/N)

[Error/Status Message Area]

ENTER=Continue F3=Back
```

## Fields

### Current Date (CURDATE)

### Transaction Name (TRNNAME)

This field displays the transaction name. It is a fixed field with a length of 4 characters and is displayed in blue color.

### Program Name (PGMNAME)

This field displays the current date in the format 'mm/dd/yy'. It is a fixed field with a length of 8 characters and is displayed in blue color.

### Current Time (CURTIME)

This field displays the program name. It is a fixed field with a length of 8 characters and is displayed in blue color.

### Monthly Report Selection (MONTHLY)

This field displays the current time in the format 'hh:mm:ss'. It is a fixed field with a length of 8 characters and is displayed in blue color.

### Yearly Report Selection (YEARLY)

This field allows the user to select the monthly report option. It is an input field with a length of 1 character, underlined, and displayed in green color. The field is validated to ensure it is not empty or contains low-values.

### Custom Report Selection (CUSTOM)

This field allows the user to select the yearly report option. It is an input field with a length of 1 character, underlined, and displayed in green color. The field is validated to ensure it is not empty or contains low-values.

### Start Date (SDTMM, SDTDD, SDTYYYY)

This field allows the user to select the custom report option. It is an input field with a length of 1 character, underlined, and displayed in green color. The field is validated to ensure it is not empty or contains low-values.

### End Date (EDTMM, EDTDD, EDTYYYY)

These fields allow the user to input the start date for the custom report. The date is divided into three fields: month (SDTMM), day (SDTDD), and year (SDTYYYY). Each field is underlined, displayed in green color, and validated to ensure they are not empty, contain valid numeric values, and represent a valid date.

### Confirmation (CONFIRM)

These fields allow the user to input the end date for the custom report. The date is divided into three fields: month (EDTMM), day (EDTDD), and year (EDTYYYY). Each field is underlined, displayed in green color, and validated to ensure they are not empty, contain valid numeric values, and represent a valid date.

### Error/Status Message (ERRMSG)

This field allows the user to confirm the report submission. It is an input field with a length of 1 character, underlined, and displayed in green color. The field is validated to ensure it contains either 'Y' or 'N' (case-insensitive).

&nbsp;

*This is an auto-generated document by Swimm 🌊 and has not yet been verified by a human*

<SwmMeta version="3.0.0"><sup>Powered by [Swimm](https://app.swimm.io/)</sup></SwmMeta>
