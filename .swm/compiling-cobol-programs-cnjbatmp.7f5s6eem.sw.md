---
title: Compiling COBOL Programs (CNJBATMP)
---
This document describes the process of compiling COBOL programs using the CNJBATMP job. The job utilizes the BUILDBAT procedure to compile the code, copy the listing to SYSOUT, and perform a link-edit step. The input to this process is the COBOL source code, and the output is the compiled program ready for execution.

For example, if you have a COBOL program named 'BATCHPGM', you would replace the placeholder in the job file with your program name and execute the job to compile it.

## Compiling the COBOL code

Steps in this section: `BATCMP`.

This section is responsible for compiling COBOL programs using the BUILDBAT procedure, which includes steps for compiling the code, copying the listing to SYSOUT, and performing a link-edit step.

&nbsp;

*This is an auto-generated document by Swimm 🌊 and has not yet been verified by a human*

<SwmMeta version="3.0.0" repo-id="Z2l0aHViJTNBJTNBa3luZHJ5bC1hd3MtbWFpbmZyYW1lLW1vZGVybml6YXRpb24tY2FyZGRlbW8lM0ElM0FTd2ltbS1EZW1v" repo-name="kyndryl-aws-mainframe-modernization-carddemo"><sup>Powered by [Swimm](/)</sup></SwmMeta>
