
# What is this?

Library to manage a file system, where files consist in binary stores, and when
data that has been recently requested is recovered quickly. One of the purposes
is to minimize disk readings, while keeping memory usage under a reasonable
limit.

# Ideas for the implementation

* Every time a file is open, a counter is attached to it. This counter increases
  every day, and is reset every time the file is accessed. Therefore, the counter
  counts the number of days since the last time the file was requested.

* When a counter reaches a limit (the first limit), the file is automatically closed.
  However, the counter keeps growing, until it reaches a limit (the second limit)
  where the file is moved away (S3). At that point, neither the data in the file
  nor the counter of that file are kept in memory.

* If a file is requested, and that file is away (S3), the file will be copied to
  the local disk and then treated as a normal file. Note that this means reuploading
  to S3 once it gets old again.

# S3 usage

S3 is only used in two situations:

* A file has not been requested in a long time. This causes the file to be
  _uploaded_ to S3.
* A file is requested after being uploaded to S3. This causes the file to be _downloaded_
  from S3.

The second limit of the counter plays a fundamental role in how much S3 is used.
