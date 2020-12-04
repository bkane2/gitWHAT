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
* add \<repoName> \<filePath1> \[filePath2\] \[filePath3\] ...
* remove \<repoName> \<filePath1> \[filePath2\] \[filePath3\] ...
* status \<repoName>
* commit \<repoName> \<revID>
* log \<repoName> \[-v\]

## Acceptance test 1
* init Repo1
* repos
* \**create test/a.txt and test/b.txt with some contents*\*
* add Repo1 test/a.txt test/b.txt
* status Repo1
* remove Repo1 test/a.txt
* status Repo1
* add Repo1 test/a.txt
* commit Repo1 Rev1
* status Repo1
* log Repo1
* \**modify test/a.txt and create test/c.txt with some contents*\*
* add Repo1 test/a.txt test/c.txt
* status Repo1
* commit Repo1 Rev2
* log Repo1 -v