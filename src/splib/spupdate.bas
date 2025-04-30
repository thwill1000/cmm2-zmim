#!/usr/local/bin/mmbasic

' Copyright (c) 2023 Thomas Hugo Williams
' License MIT <https://opensource.org/licenses/MIT>
' For MMB4L 0.6.0

' Updates contents of 'splib' directory from current contents of
' git@github.com:thwill1000/mmbasic-sptools.git - 'master' branch.

Option Base 0
Option Explicit On
Option Default Integer

#Include "system.inc"
#Include "file.inc"
#Include "string.inc"

Const TMP_SPTOOLS_DIR$ = sys.TMPDIR$() + "/sptools"
Const GIT_REPOSITORY$ = "git@github.com:thwill1000/mmbasic-sptools.git"

Dim cmd$, out%(256), status%

' There must be an 'splib/' directory.
If Not file.exists%("splib", "dir") Then Error "'splib' directory not found"

' But if it contains a 'tests/' directory then we may be trying
' to update the copy in a clone of the 'mmbasic-sptools' repository,
' don't allow this.
If file.exists%("splib/tests") Then Error "'splib/tests' exists"

' Delete existing temporary 'splib/' directory.
If file.exists%(TMP_SPTOOLS_DIR$) Then
  ? "Deleting " str.quote$(TMP_SPTOOLS_DIR$)
  System "rm -Rf " + str.quote$(TMP_SPTOOLS_DIR$)
EndIf

' Clone git repository.
? "Cloning " + GIT_REPOSITORY$ + " to " + str.quote$(TMP_SPTOOLS_DIR$)
System "git clone " + GIT_REPOSITORY$ + " " + TMP_SPTOOLS_DIR$

' Delete existing 'splib/' directory.
? "Deleting " str.quote$("splib")
System "rm -Rf " + str.quote$("splib")

' Create new 'splib/' directory.
? "Making " str.quote$("splib")
System "mkdir " + str.quote$("splib")

' Copy files from temporary directory into 'splib/' directory.
Const src$ = TMP_SPTOOLS_DIR$ + "/src/splib"
Const dst$ = Cwd$ + "/splib"
? "Copying files from '" src$ "' to '" dst$ "'"
cmd$ = "find <src> -maxdepth 1 -type f -execdir cp '{}' <dst> ';'"
cmd$ = str.replace$(cmd$, "'", Chr$(34))
cmd$ = str.replace$(cmd$, "<src>", src$)
cmd$ = str.replace$(cmd$, "<dst>", dst$)
System cmd$, out%(), status%
If status% Then Error cmd$ + " failed with code " + Str$(status%)

' Get SHA-256 for HEAD of sptools repository
cmd$ = "(cd " + TMP_SPTOOLS_DIR$ + " && git rev-parse HEAD)"
System cmd$, out%(), status%
If status% Then Error cmd$ + " failed with code " + Str$(status%)
Const sha256$ = LGetStr$(out%(), 1, 12)

' Add and commit update to the git repository.
? "Updating local git repository"
System "git add -u"
System "git commit -m " + str.quote$("Update 'splib' to master@" + sha256$)
