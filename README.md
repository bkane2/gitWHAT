# gitWHAT
DVCS for CSC 253/453

## GitHub page
https://github.com/bkane2/gitWHAT

## How to compile
ghc -outputdir obj Run.hs

## How to run
./Run

## Supported commands
* init \<repoName>
* clone \<newRepoName> \<oldRepoName>
* repos
* add \<repoName> \<filePath1> \[filePath2\] \[filePath3\] ...
* remove \<repoName> \<filePath1> \[filePath2\] \[filePath3\] ...
* status \<repoName> \[-v\] (The -v flag prints a verbose status with tracked file contents)
* heads \<repoName> \[-a\] (The -a flag shows only the active head, i.e. the one pointing to the working directory)
* cat \<repoName> \<revID> \<filePath>
* checkout \<repoName> \<revID>
* commit \<repoName> \<revID>
* log \<repoName> \[-v\] (The -v flag prints a verbose log showing individual FileLogs (from each head Revision))

Note that all filePaths are assumed to be relative to the repoName (see the acceptance tests for examples).

## Acceptance test 1
* init test
* repos
* \**create test/a.txt and test/b.txt with some contents*\*
* add test a.txt b.txt
* status test
* remove test a.txt
* status test
* add test a.txt
* commit test Rev1
* status test
* log test
* \**modify test/a.txt and create test/c.txt with some contents*\*
* add test a.txt c.txt
* status test
* commit test Rev2
* log test -v

## Acceptance test 2
* init test
* \**create test/a.txt with some contents*\*
* add test a.txt
* commit test Rev1
* log test -v
* \**modify test/a.txt and create test/b.txt with some contents*\*
* add test a.txt b.txt
* commit test Rev2
* heads test
* heads test -a
* cat test Rev1 a.txt
* checkout test Rev1
* heads test
* heads test -a
* \**modify test/a.txt and create test/c.txt with some contents*\*
* add test a.txt c.txt
* commit test Rev3
* log test -v
* heads test
* heads test -a
* cat test Rev1 a.txt
* cat test Rev2 a.txt
* cat test Rev2 b.txt
* checkout test Rev1
* checkout test Rev2
* checkout test Rev3
* clone test2 test
* log test2 -v
* heads test2
* \**modify test2/a.txt and create test2/d.txt with some contents*\*
* add test2 a.txt d.txt
* commit test2 Rev4
* log test2 -v
* checkout test2 Rev1