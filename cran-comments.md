## Test environments

* local R installation, macOS 27.0, R 4.6.1

## R CMD check results

0 errors | 0 warnings | 0 notes

- Moved 'text2vec' from Imports to Suggests, 
so that this package no longer strongly depends on 'float' (via 'rsparse'),
which is scheduled for archival on 2026-10-21
- Fixed the error in the additional 'donttest' check
