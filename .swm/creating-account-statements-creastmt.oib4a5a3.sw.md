---
title: Creating Account Statements (CREASTMT)
---
The CREASTMT job is responsible for creating account statements for each card present in the XREF file. This process involves preparing transaction data, clearing previous reports, and generating new account statements. The job starts by deleting and recreating transaction files, then it deletes outdated reports, and finally, it generates new statements in both plain text and HTML formats using the CBSTM03A program.

Here is a high level diagram of the file:

```mermaid
graph TD
subgraph a00d042fb["Prepare Transaction Data"]
  
end
      
subgraph a2ac53612["Clear Previous Reports"]
  
end
      
subgraph a7eccf98b["Generate Account Statements"]
  773o7("CBSTM03A")
end
      



a00d042fb --> a2ac53612
a2ac53612 --> a7eccf98b
style a00d042fb color:#000000,fill:#7CB9F4
style a2ac53612 color:#000000,fill:#00FFAA
style a7eccf98b color:#000000,fill:#00FFF4
```

## Prepare Transaction Data

Steps in this section: `DELDEF01`, `STEP010`, `STEP020`.

This section is about preparing transaction data for statement creation. It involves deleting and recreating transaction files, creating a copy of the transaction file with card number and transaction ID as the key, and copying data from a sequential file to a VSAM KSDS file.

## Clear Previous Reports

Steps in this section: `STEP030`.

This section is responsible for deleting transaction reports from the previous run to ensure that outdated data does not interfere with the new reports being generated.

## Generate Account Statements

Steps in this section: `STEP040`.

This section is responsible for generating account statements from transaction data, producing reports in both plain text and HTML formats. It involves reading data from multiple files and creating the statements using the CBSTM03A program.

&nbsp;

*This is an auto-generated document by Swimm 🌊 and has not yet been verified by a human*

<SwmMeta version="3.0.0" repo-id="Z2l0aHViJTNBJTNBa3luZHJ5bC1hd3MtbWFpbmZyYW1lLW1vZGVybml6YXRpb24tY2FyZGRlbW8lM0ElM0FTd2ltbS1EZW1v" repo-name="kyndryl-aws-mainframe-modernization-carddemo"><sup>Powered by [Swimm](/)</sup></SwmMeta>
