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

### Transaction Name (TRNNAME)

- Length: 4 characters
- Fixed field
- Color: Blue

### Current Date (CURDATE)

- Length: 8 characters
- Fixed field
- Color: Blue
- Initial value: 'mm/dd/yy'

### Program Name (PGMNAME)

- Length: 8 characters
- Fixed field
- Color: Blue
- Associated document: <SwmLink doc-title="Report Processing Flow (CORPT00C)" repo-id="Z2l0aHViJTNBJTNBa3luZHJ5bC1hd3MtbWFpbmZyYW1lLW1vZGVybml6YXRpb24tY2FyZGRlbW8lM0ElM0FTd2ltbS1EZW1v" repo-name="aws-mainframe-modernization-carddemo" path="/.swm/report-processing-flow-corpt00c.ivkytwt9.sw.md">[Report Processing Flow (CORPT00C)](https://app.swimm.io/repos/Z2l0aHViJTNBJTNBa3luZHJ5bC1hd3MtbWFpbmZyYW1lLW1vZGVybml6YXRpb24tY2FyZGRlbW8lM0ElM0FTd2ltbS1EZW1v/docs/ivkytwt9)</SwmLink>

### Current Time (CURTIME)

- Length: 8 characters
- Fixed field
- Color: Blue
- Initial value: 'hh:mm:ss'

### Monthly Report Selection (MONTHLY)

- Length: 1 character
- Input field
- Color: Green
- Underlined

### Yearly Report Selection (YEARLY)

- Length: 1 character
- Input field
- Color: Green
- Underlined

### Custom Report Selection (CUSTOM)

- Length: 1 character
- Input field
- Color: Green
- Underlined

### Start Date Month (SDTMM)

- Length: 2 characters
- Input field
- Color: Green
- Underlined
- Numeric

### Start Date Day (SDTDD)

- Length: 2 characters
- Input field
- Color: Green
- Underlined
- Numeric

### Start Date Year (SDTYYYY)

- Length: 4 characters
- Input field
- Color: Green
- Underlined
- Numeric

### End Date Month (EDTMM)

- Length: 2 characters
- Input field
- Color: Green
- Underlined
- Numeric

### End Date Day (EDTDD)

- Length: 2 characters
- Input field
- Color: Green
- Underlined
- Numeric

### End Date Year (EDTYYYY)

- Length: 4 characters
- Input field
- Color: Green
- Underlined
- Numeric

### Confirmation (CONFIRM)

- Length: 1 character
- Input field
- Color: Green
- Underlined
- Valid values: 'Y' or 'N'

&nbsp;

_This is an auto-generated document by Swimm 🌊 and has not yet been verified by a human_

<SwmMeta version="3.0.0"><sup>Powered by [Swimm](https://app.swimm.io/)</sup></SwmMeta>
