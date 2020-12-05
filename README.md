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
* repos
* add \<repoName> \<filePath1> \[filePath2\] \[filePath3\] ...
* remove \<repoName> \<filePath1> \[filePath2\] \[filePath3\] ...
* status \<repoName>
* commit \<repoName> \<revID>
* log \<repoName> \[-v\]

## Acceptance test 1
* init test
* repos
* \**create test/a.txt and test/b.txt with some contents*\*
* add test test/a.txt test/b.txt
* status test
* remove test test/a.txt
* status test
* add test test/a.txt
* commit test Rev1
* status test
* log test
* \**modify test/a.txt and create test/c.txt with some contents*\*
* add test test/a.txt test/c.txt
* status test
* commit test Rev2
* log test -v