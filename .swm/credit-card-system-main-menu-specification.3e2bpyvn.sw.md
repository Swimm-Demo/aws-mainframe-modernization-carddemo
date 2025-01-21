---
title: Credit Card System Main Menu Specification
---
Screen preview

&nbsp;

<p align="center"><img src="/.swm/images/%7B6F2F4408-B0C5-45FE-9A6E-3765FCE0D8C9%7D-2025-0-21-8-41-16-378.png"></p>

# 

## 1\. Overview

The main menu provides access to four primary function groups:

- Account Management

- Card Management

- Transaction Management

- Bill Payment

## 2\. Available Functions

### 2.1 Account Management

1. Account View (COACTVWC)

   - View account details and status

2. Account Update (COACTUPC)

   - Modify account information

### 2.2 Card Management

3. Credit Card List (COCRDLIC)

   - View all credit cards

4. Credit Card View (COCRDSLC)

   - Detailed card information

5. Credit Card Update (COCRDUPC)

   - Modify card details

### 2.3 Transaction Management

6. Transaction List (COTRN00C)

   - View all transactions

7. Transaction View (COTRN01C)

   - Detailed transaction information

8. Transaction Add (COTRN02C)

   - Add new transactions

9. Transaction Reports (CORPT00C)

   - Generate transaction reports

### 2.4 Payment Functions

10. Bill Payment (COBIL00C)

    - Process bill payments

## 3\. Interface Requirements

### 3.1 Screen Layout

- Header shows system info and date/time

- Options displayed in numbered list (1-10)

- Each option shows number and full description

- Input field for option selection

- Error message area at bottom

- Navigation instructions at bottom

### 3.2 Color Scheme

- Menu options in blue

- Error messages in red

- Success messages in green

- Titles in yellow

- Navigation instructions in yellow

## 4\. Business Rules

### 4.1 Access Control

- All options available to regular users (type 'U')

- Valid session required

- No special privileges needed

### 4.2 Navigation Rules

- Direct access to any function with valid option number (1-10)

- Return to menu after function completion

- Can exit to login screen at any time

- Invalid options show error message

## 5\. Error Handling

### 5.1 Input Validation

- Must be numeric

- Must be between 1 and 10

- Clear error messages for:

  - Non-numeric input

  - Out of range values

  - Invalid function keys

### 5.2 Error Messages

- Standard format for all errors

- Clear indication of the issue

- Guidance for correction

## 6\. Technical Integration

### 6.1 Program Flow

- Each option links to specific program (e.g., COACTVWC for Account View)

- Consistent COMMAREA structure maintained

- Session context preserved during navigation

### 6.2 Program Mapping

| Option | Program  | Function            |
| ------ | -------- | ------------------- |
| 1      | COACTVWC | Account View        |
| 2      | COACTUPC | Account Update      |
| 3      | COCRDLIC | Credit Card List    |
| 4      | COCRDSLC | Credit Card View    |
| 5      | COCRDUPC | Credit Card Update  |
| 6      | COTRN00C | Transaction List    |
| 7      | COTRN01C | Transaction View    |
| 8      | COTRN02C | Transaction Add     |
| 9      | CORPT00C | Transaction Reports |
| 10     | COBIL00C | Bill Payment        |

<SwmMeta version="3.0.0" repo-id="Z2l0aHViJTNBJTNBa3luZHJ5bC1hd3MtbWFpbmZyYW1lLW1vZGVybml6YXRpb24tY2FyZGRlbW8lM0ElM0FTd2ltbS1EZW1v" repo-name="aws-mainframe-modernization-carddemo"><sup>Powered by [Swimm](https://app.swimm.io/)</sup></SwmMeta>
