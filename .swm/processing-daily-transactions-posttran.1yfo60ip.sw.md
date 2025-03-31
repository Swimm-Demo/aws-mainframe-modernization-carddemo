---
title: Processing Daily Transactions (POSTTRAN)
---
The POSTTRAN job is responsible for processing the daily transaction file, updating the transaction master, and handling rejected transactions. This is achieved by executing a batch COBOL program that posts records, validates transactions, and updates account balances. The job involves reading various VSAM and PS datasets, processing the data, and updating the relevant files.

For example, the job reads the daily transaction file, processes each transaction to update the account balances, and logs any rejected transactions for further review.

## Processing Daily Transactions and Updating Masters

Steps in this section: <SwmToken path="app/jcl/POSTTRAN.jcl" pos="23:1:1" line-data="//STEP15 EXEC PGM=CBTRN02C                                                      ">`STEP15`</SwmToken>.

This section is responsible for processing the daily transaction file, updating the transaction master, and handling rejected transactions. It involves executing a batch COBOL program to post records, validate transactions, and update account balances.

<SwmSnippet path="/app/jcl/POSTTRAN.jcl" line="21">

---

Here we call the <SwmToken path="app/jcl/POSTTRAN.jcl" pos="23:7:7" line-data="//STEP15 EXEC PGM=CBTRN02C                                                      ">`CBTRN02C`</SwmToken> program.

More about <SwmToken path="app/jcl/POSTTRAN.jcl" pos="23:7:7" line-data="//STEP15 EXEC PGM=CBTRN02C                                                      ">`CBTRN02C`</SwmToken>: <SwmLink doc-title="Posting Daily Transactions (CBTRN02C)">[Posting Daily Transactions (CBTRN02C)](/.swm/posting-daily-transactions-cbtrn02c.o2e7sfan.sw.md)</SwmLink>

```jcl
//* category balance and update transaction master vsam                         
//* *******************************************************************         
//STEP15 EXEC PGM=CBTRN02C                                                      
```

---

</SwmSnippet>

&nbsp;

*This is an auto-generated document by Swimm 🌊 and has not yet been verified by a human*

<SwmMeta version="3.0.0" repo-id="Z2l0aHViJTNBJTNBa3luZHJ5bC1hd3MtbWFpbmZyYW1lLW1vZGVybml6YXRpb24tY2FyZGRlbW8lM0ElM0FTd2ltbS1EZW1v" repo-name="kyndryl-aws-mainframe-modernization-carddemo"><sup>Powered by [Swimm](/)</sup></SwmMeta>
