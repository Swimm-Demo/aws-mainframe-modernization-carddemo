---
title: |
  Credit Card Update Function Specification
---
&nbsp;

<p align="center"><img src="/.swm/images/%7B443F9D0D-6F30-4D43-B4DE-862D575EDAA0%7D-2025-0-21-9-58-2-715.png"></p>

## 1\. Overview

The Credit Card Update function allows users to modify existing credit card details including name on card, active status, and expiration date.

## 2\. Screen Layout

### 2.1 Header Information

- Display transaction ID (CCUP)

- Display program name (COCRDUPC)

- Show current date and time

- Display system titles

### 2.2 Input Fields

- Account Number (11 digits, protected after initial search)

- Card Number (16 digits, protected after initial search)

- Name on Card (50 characters maximum)

- Card Active Status (Y/N)

- Expiry Date

  - Month (2 digits, 1-12)

  - Year (4 digits)

  - Day (2 digits, display only)

### 2.3 Information Areas

- Information message area (center screen)

- Error message area (bottom, red)

- Function key descriptions (bottom, yellow)

## 3\. Business Rules

### 3.1 Search Criteria Validation

- Account Number:

  - Must be numeric

  - Must be 11 digits

  - Must exist in system

- Card Number:

  - Must be numeric

  - Must be 16 digits

  - Must exist for the account

### 3.2 Update Field Validation

- Name on Card:

  - Required field

  - Alphabetic characters and spaces only

  - Maximum 50 characters

  - Stored in uppercase

- Active Status:

  - Required field

  - Must be 'Y' or 'N' only

- Expiry Month:

  - Required field

  - Must be 1-12

  - Numeric only

- Expiry Year:

  - Required field

  - Must be between 1950-2099

  - Numeric only

### 3.3 Update Process Rules

- Cannot modify account number or card number

- All changes require confirmation before saving

- Lock record during update to prevent concurrent modifications

- Validate record hasn't changed before applying updates

- Clear screen for new search after successful update

## 4\. Function Keys

### 4.1 Available Keys

- Enter: Process input

- F3: Exit to menu

- F5: Save changes (only after validation)

- F12: Cancel changes

### 4.2 Context Sensitivity

- F5 only enabled after valid changes

- F12 only available during edit

## 5\. Processing Steps

### 5.1 Initial Search

1. User enters account and card number

2. System validates input format

3. System searches for matching card

4. Display card details if found

### 5.2 Update Process

1. User modifies allowed fields

2. System validates all inputs

3. System shows confirmation if valid

4. User confirms with F5

5. System attempts update

6. Show success/failure message

### 5.3 Error Handling

- Clear error messages on new action

- Position cursor to error field

- Show specific error messages for each validation rule

- Preserve valid input on error

## 6\. Security

### 6.1 Access Control

- Available to both admin and regular users

- Maintain user context during navigation

### 6.2 Data Protection

- Lock records during update

- Validate record version before update

- Clean exit on session termination

## 7\. Messages

### 7.1 Information Messages

- "Please enter Account and Card Number"

- "Details of selected card shown above"

- "Update card details presented above"

- "Changes validated. Press F5 to save"

- "Changes committed to database"

### 7.2 Error Messages

- "Card number if supplied must be a 16 digit number"

- "Account number must be a non zero 11 digit number"

- "Card name can only contain alphabets and spaces"

- "Card Active Status must be Y or N"

- "Card expiry month must be between 1 and 12"

- "Invalid card expiry year"

&nbsp;

<SwmMeta version="3.0.0" repo-id="Z2l0aHViJTNBJTNBa3luZHJ5bC1hd3MtbWFpbmZyYW1lLW1vZGVybml6YXRpb24tY2FyZGRlbW8lM0ElM0FTd2ltbS1EZW1v" repo-name="aws-mainframe-modernization-carddemo"><sup>Powered by [Swimm](https://app.swimm.io/)</sup></SwmMeta>
