---
title: Defining VSAM File Deletion (DALYREJS)
---
The DALYREJS job is responsible for defining the deletion of a VSAM file if it already exists. This is achieved using the IDCAMS utility program within the job's execution steps. The job ensures that any pre-existing VSAM file is removed before proceeding with further operations, maintaining data integrity and preventing conflicts.

For instance, if a VSAM file named AWS.M2.CARDDEMO.DALYREJS already exists, the job will delete it to allow for the creation of a new file without issues.

## Defining VSAM file deletion

Steps in this section: `STEP05`.

This section is responsible for defining the deletion of a VSAM file if it already exists, using the IDCAMS utility program.

&nbsp;

*This is an auto-generated document by Swimm 🌊 and has not yet been verified by a human*

<SwmMeta version="3.0.0" repo-id="Z2l0aHViJTNBJTNBa3luZHJ5bC1hd3MtbWFpbmZyYW1lLW1vZGVybml6YXRpb24tY2FyZGRlbW8lM0ElM0FTd2ltbS1EZW1v" repo-name="kyndryl-aws-mainframe-modernization-carddemo"><sup>Powered by [Swimm](/)</sup></SwmMeta>
