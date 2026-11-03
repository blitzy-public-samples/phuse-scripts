/*** WPCT-F.07.02 SAS capture launcher: full-precision reference exports for the Python port

  UNVERIFIED IN SAS until run in a licensed session.

  This program was written without a SAS engine. Nobody has run it, and it is not claimed to work.
  It makes no claim of SAS equivalence. It only captures the SAS results that the Python port
  compares against, in the form python/wpct/reference/sas_exports/README.md and wpct/schemas.py
  specify. See RUNBOOK.md in this folder for the run steps and the comparison commands.

  REQUIREMENTS
    Licensed SAS 9.4 with Base SAS and SAS/STAT, run once by a person in a fresh session.
    Match the historical references where possible: SAS 9.4 TS1M2 with SAS/STAT 13.2.
    A session whose WORK library lies outside the checkout and that has no USER library; the
    launcher refuses any other session before it writes any file, dataset or template of its
    own. A WORK location inside any checkout of this repository (a directory holding
    whitepapers/WPCT/WPCT-F.07.02.sas and data/adam/cdisc/advs.xpt) is refused, even when it is
    reached through a symbolic link.
    No network connection and no credentials are needed.
    Edit only the USER SETTINGS block below.

  APPROACH: a staged working copy, generated at run time
    The unmodified whitepapers/WPCT/WPCT-F.07.02.sas cannot be captured from outside:
      - The full ParameterEstimates table (ODS OUTPUT at line 274) is overwritten under the same
        name at line 289, and no macro call falls between line 280 and line 289.
      - A persistent ODS OUTPUT request made before the program starts is modified by the ODS
        OUTPUT statement of the program itself at line 274.
      - WORK.CSS_STATS is overwritten for every parameter and timepoint and deleted at line 396.
        The only hook is the repository macro util_delete_dsets at line 307, and replacing it
        would mean the repository macros no longer run unmodified.
      - An unmodified run reads the GitHub URL instead of data/adam/cdisc/advs.xpt (ENV-02, line 85).
    The launcher therefore reads the original read-only and writes WPCT-F.07.02_staged.sas into
    the work directory, outside the repository. The staged copy is the original line for line,
    with any trailing CR removed, except for four anchored edits:
      (a) line 85 is replaced by the same call with local=<repo>/data/adam/cdisc/ (no network).
          The comment after the call is not kept: it holds macro LET text that would swallow
          the DATA ADVS_SUB statement on line 90.
      (b) line 126 is replaced by outputs_folder=<work>/pdf, so no PDF lands in the repository.
          The comment after the statement is not kept: its second sentence is an invalid statement.
      (c) after line 280 (run and quit of PROC GLM), the full ODS ParameterEstimates table of the
          stratum, plus PARAMCD and ATPTN, is appended to the capture library before line 289
          overwrites it.
      (d) after line 308 (the end of the GLM block, after the merge at lines 302-305), the merged
          css_stats of the stratum, plus PARAMCD and ATPTN, is appended to the capture library.
    Each anchor is checked by line number and text. Any mismatch stops the launcher before the
    program runs. The repository macros run unmodified from whitepapers/utilities through
    SASAUTOS. WORK.CSS_ANADATA, which nothing later changes or deletes, is copied after the
    program returns. The repository files are only read, never written. Before anything runs,
    the launcher checks that each of the 15 repository macro files WPCT-F.07.02 calls, directly
    or through another macro, can be read from whitepapers/utilities, and stops if one cannot.

  FILES PRODUCED, all below the settings-block directories. The launcher refuses settings that
  would put any of them in the checkout (point 17).
    <work>/WPCT-F.07.02_staged.sas                 the staged working copy
    <work>/WPCT-F.07.02_capture.log and .lst       log and listing, from the redirect onwards
    <work>/capture/css_anadata.sas7bdat            native copies: the authoritative capture
    <work>/capture/css_stats.sas7bdat
    <work>/capture/parameter_estimates.sas7bdat
    <work>/pdf/*.pdf on Windows, <work>/pdf\*.pdf on UNIX    box plots of the program (point 12)
    <csv>/WPCT-F.07.02_css_anadata.csv             the contract CSVs
    <csv>/WPCT-F.07.02_css_stats.csv
    <csv>/WPCT-F.07.02_parameter_estimates.csv

  STATUS
    A complete capture sets SYSCC back to its value before the staged program ran (0 in a fresh
    session without SAS warnings), so a batch run ends with exit status 0, or 1 when a SAS step of
    the launcher itself, before or after the staged program, issued a SAS WARNING. The expected
    ERRORs of lines 76 and 82 (point 8) do not count against a complete capture. Any refused or
    failed run ends with SYSCC 8 or more: batch exit status 2. A SAS ERROR in a step of the
    launcher itself, before or after the staged program, also makes the run a failed one (SYSCC 8
    or more, exit status 2). Nothing ends the SAS session, so an interactive session stays open.

  LIKELY FAILURE POINTS, for the first licensed run
    1. ODS table name and columns. The capture relies on the ODS table ParameterEstimates of
       PROC GLM. The CSV requires its variables Parameter Estimate StdErr tValue Probt. If this
       SAS/STAT release names one of them differently, the parameter_estimates export stops with
       an ERROR naming the missing variable. Dependent and Biased are kept in the native copy as
       ODS extras, left out of the CSV and not checked.
    2. Macro variable scoping. The inserted DATA steps run inside macro boxplot_each_param_tp and
       resolve its local pdx, tdx and css_pval_ds, and the global paramcd_val<n> and atptn_val<n>
       that util_labels_from_var creates. Check the MPRINT lines of DATA WORK._WPCT_PE and
       DATA WORK._WPCT_STATS: PARAMCD must read DIABP or SYSBP and ATPTN 815 or 816.
    3. Format stripping. The CSV step removes every format (FORMAT _ALL_) and writes each numeric
       with PUTN and the explicit WPCT_NUMFMT format, without leading blanks, so no estimate is
       rounded and no p-value becomes text such as <.0001. Character values are written as
       stored, without trailing blanks, and as an empty field when missing. One holding a comma,
       a double quote or a line break is enclosed in double quotes, with its double quotes
       doubled. Each record is written by one formatted PUT, so no DSD rule applies. The
       read-back check uses list input, which removes leading blanks and enclosing quotes, so
       such values can draw a read-back WARNING although the CSV holds them as stored.
    4. E32.17. The SAS 9.4 documentation lists the E format as Ew. only, so whether E32.17 is
       accepted is unverified, and SAS documents formatted values beyond about 15 significant
       digits as possibly inaccurate. A pre-flight check writes test values with WPCT_NUMFMT and
       stops if SAS cannot read them back: set WPCT_NUMFMT to BEST32. then. After the export, a
       read-back check reports per column whether each CSV value reads back exactly as the
       native value. The sas7bdat copies stay exact either way.
    5. CRLF line endings. WPCT-F.07.02.sas has CRLF line endings. On UNIX each record then ends
       with a CR, which the staged-copy step removes.
    6. Anchor mismatch. Lines 85, 126, 280 and 308 are checked. A different revision of the
       program stops the launcher with an ERROR line in the capture log naming the line.
    7. SASAUTOS. util_resolve_sasautos, used by assert_macro_exist, parses one flat list of
       filerefs and double-quoted paths, and line 76 wraps the current SASAUTOS value in another
       pair of parentheses with an empty "" entry. The launcher therefore sets SASAUTOS to the
       single fileref WPCTAUTO, so line 76 yields (WPCTAUTO ""). WPCTAUTO concatenates the
       double-quoted "<repo>/whitepapers/utilities" first, then every entry of the SASAUTOS option
       the session started with, in order: a quoted path as given, and a fileref expanded to its
       directories from SASHELP.VEXTFL, or through PATHNAME when the view has no row for it. An
       empty "" entry is skipped with a NOTE. An entry that names no directory gives a WARNING and
       is not carried. If assert_depend reports a missing macro, read the SASAUTOS entry NOTE and
       WARNING lines, then compare the WPCTAUTO rows of SASHELP.VEXTFL.
    8. Lines 76 and 82 of the original. Line 76 continues after its asterisk comment with text
       that SAS reads as an invalid statement, and the asterisk comment on line 82 does not stop
       its macro call from running. The URL FILENAME statement of that call is absorbed by the
       invalid statement, so no network connection is made, but its DATA step fails. In batch
       those ERROR lines would put SAS into syntax-check mode, so the launcher sets NOSYNTAXCHECK
       for the run and tests the halting symbol CONTINUE of the program instead. The ERROR lines
       from lines 76 and 82 are expected.
    9. MISSING= option. Line 324 sets options missing=' ' for the rest of the session. The
       launcher sets missing='.' again before writing the CSVs.
   10. FILE ENCODING=. WPCT_ENCODING is latin1, the UNIX name. Use wlatin1 on Windows if the
       session refuses latin1.
   11. Halts through assert_continue. When the dependency checks of the program fail, it sets
       CONTINUE=0 and forces syntax-checking mode (OBS=0), which NOSYNTAXCHECK keeps from taking
       effect in batch. The launcher detects OBS=0 or CONTINUE other than 1 and writes no CSV.
   12. Backslash in the PDF path. Line 339 writes "&outputs_folder\WPCT-F.07.02_Box_plot_...".
       On UNIX that is a file named pdf\WPCT-F.07.02_Box_plot_... inside <work>.
   13. SASUSER template store. util_proc_template compiles the PhUSEboxplot template. The
       launcher puts WORK.TEMPLAT(UPDATE) first on the ODS path, so a read-only SASUSER store
       does not matter and nothing is written to it.
   14. Directory creation. DCREATE creates one level only: the parents of WPCT_WORKDIR and
       WPCT_CSVDIR must exist.
   15. Log location. Settings and guard messages go to the session log. From the redirect on,
       the log goes to <work>/WPCT-F.07.02_capture.log. In batch, start SAS outside the
       repository, or pass -log, so the session log is not written into the checkout.
       PROC PRINTTO does not move ODS output, so right after the redirect the launcher lists
       every open ODS destination in the capture log, clears every earlier ODS OUTPUT request,
       persistent ones included, then closes every destination and opens only LISTING: print
       file <work>/WPCT-F.07.02_capture.lst, graphics in <work>. The closed destinations
       stay closed after the run. In an interactive session the Results window therefore stays
       empty: read the capture log and listing instead.
   16. PROC APPEND runs without FORCE. If the second stratum has a variable of another length,
       the append stops with an ERROR instead of truncating values. WPCT_APPEND compares the row
       counts around each append, so a rejected or partial append fails the capture with the
       ERROR Appending ... failed: SYSERR=..., and no CSV is written.
   17. Destination guard. The settings are compared in one canonical text per OS: separators
       become /, and every run of them one / (on Windows except the leading pair of a UNC path).
       On UNIX a path starts with /. On Windows it starts with a drive letter, as in C:/work, or
       is a UNC path //server/share, and the comparison ignores case. WPCT_WORKDIR and
       WPCT_CSVDIR must be neither the checkout, nor inside it, nor a parent of it, so no path
       below them is in the checkout. A physical check then compares directory listings and
       only reads, so nothing is written before its verdict: one physical directory has one set
       of member names at a given moment, whatever path lists it. It lists each destination
       directory, or the nearest existing directory above it, then every directory reachable
       from the checkout root. A checkout directory with the same member names means that a
       symbolic link, a junction, a mount or another spelling may lead into the checkout, and
       the run stops; a destination that only happens to hold the names of a checkout directory
       is refused as well. A checkout entry that can be neither listed as a directory nor opened
       as a file stops the run, because a destination could lie unseen below it. An empty
       destination cannot be told apart from an empty checkout directory and is refused. The
       exception is an empty capture, pdf or default csv folder that an earlier run left in the
       work directory: it is checked as a folder still to be created there, removed once the
       check has passed and created again. The work directory holds the launcher copy (run step
       2 of RUNBOOK.md), so it is not empty. Every entry at an output name in <work> and
       <work>/pdf is removed before the log redirect, and every entry at a contract CSV name in
       <csv> before the program runs, so no write goes through a link left there. Limits: a
       link created while the launcher runs is not seen, nor a destination whose members change
       during the check, and an entry that a checkout listing names but the session cannot
       reach (a link to nothing, or a member of a directory it can list but not enter) is
       skipped, since no path through the checkout leads into it. Keep WPCT_REPO spelled as the
       checkout, and keep the destinations off links into it.
***/


/*=====================================================================================
  USER SETTINGS: the only block to edit. Absolute paths, with / or \ as separators.
=====================================================================================*/

%let wpct_repo     = ;                  /* the checkout that holds whitepapers/, data/ and python/ */
%let wpct_workdir  = ;                  /* capture work directory, outside the checkout            */
%let wpct_csvdir   = &wpct_workdir/csv; /* CSV directory, by default a subfolder of the work one   */
%let wpct_encoding = latin1;            /* CSV encoding: latin1 on UNIX, wlatin1 on Windows        */
%let wpct_numfmt   = E32.17;            /* explicit numeric CSV format, BEST32. as the fallback    */

/*=====================================================================================
  end of USER SETTINGS. Nothing below holds a path or needs editing.
=====================================================================================*/


options mprint;


/*** CSV export of one captured dataset, or removal of the CSV of an earlier run.
  DATA   the native capture dataset
  FILE   the contract file name, written to WPCT_CSVDIR
  ROWS   the row count the shipped SAS log reports (export README, Row counts)
  VARS   the contract columns, in schemas.py order; the type of each comes from DATA
  The global WPCT_ACTION selects CLEAR (remove FILE) or WRITE (write and read back FILE).
***/
%macro wpct_export_csv(data=, file=, rows=, vars=);
  %local path dsid nobs rbobs n i var vnum types header missing rc toolong;
  %let path = &wpct_csvdir/&file;

  %if &wpct_action = clear %then %do;
    data _null_;
      length _wpct_fref _wpct_dref $8 _wpct_name $1024;
      _wpct_fref = '_wpctcsv';
      _wpct_dref = '_wpctcsd';
      _wpct_rc = filename(_wpct_fref, "&path");
      if _wpct_rc = 0 then do;
        if fexist(_wpct_fref) then do;
          _wpct_rc = fdelete(_wpct_fref);
          if _wpct_rc = 0 then put "NOTE: (WPCT CAPTURE) Removed &path, left by an earlier run.";
          else do;
            put "ERROR: (WPCT CAPTURE) Cannot remove &path, left by an earlier run.";
            call symputx('wpct_ok', '0', 'G');
          end;
        end;
        /* FEXIST follows a link, so it does not see a link that points to no existing file, and
           the FILE write would create the target of that link. The CSV directory is therefore
           listed: an entry at the file name is removed, and the run stops when that entry
           cannot be removed or the directory cannot be opened (failure point 17). */
        else do;
          _wpct_found = 0;
          _wpct_did = 0;
          if filename(_wpct_dref, "&wpct_csvdir") = 0 then _wpct_did = dopen(_wpct_dref);
          if _wpct_did = 0 then do;
            put "ERROR: (WPCT CAPTURE) Cannot open &wpct_csvdir to check for an earlier &file..";
            call symputx('wpct_ok', '0', 'G');
          end;
          else do;
            do _wpct_i = 1 to dnum(_wpct_did) while (not _wpct_found);
              _wpct_name = dread(_wpct_did, _wpct_i);
              if "&sysscp" = 'WIN' then _wpct_found = (upcase(_wpct_name) = upcase("&file"));
              else _wpct_found = (_wpct_name = "&file");
            end;
            _wpct_rc = dclose(_wpct_did);
          end;
          _wpct_rc = filename(_wpct_dref);
          if _wpct_found then do;
            if fdelete(_wpct_fref) = 0 then put "NOTE: (WPCT CAPTURE) Removed &path, a link to no existing file.";
            else do;
              put "ERROR: (WPCT CAPTURE) Cannot remove &path, which stands at the name of a contract CSV. Remove it by hand.";
              call symputx('wpct_ok', '0', 'G');
            end;
          end;
        end;
        _wpct_rc = filename(_wpct_fref);
      end;
      else do;
        put "ERROR: (WPCT CAPTURE) Cannot assign a fileref to &path to check for an earlier &file..";
        call symputx('wpct_ok', '0', 'G');
      end;
    run;
  %end;
  %else %do;
    %let missing = 0;
    %let n = %sysfunc(countw(&vars, %str( )));
    %let dsid = %sysfunc(open(&data, i));
    %if &dsid = 0 %then %do;
      %put ERROR: (WPCT CAPTURE) Cannot open &data, so &file is not written.;
      %let missing = 1;
    %end;
    %else %do;
      %let nobs = %sysfunc(attrn(&dsid, nlobs));
      %do i = 1 %to &n;
        %let var = %scan(&vars, &i, %str( ));
        %if &i = 1 %then %let header = &var;
        %else %let header = &header,&var;
        %let vnum = %sysfunc(varnum(&dsid, &var));
        %if &vnum = 0 %then %do;
          %put ERROR: (WPCT CAPTURE) &data has no variable &var, which &file requires.;
          %let missing = 1;
        %end;
        %else %if %sysfunc(vartype(&dsid, &vnum)) = N %then %do;
          %let types = &types N;
        %end;
        %else %do;
          %let types = &types C;
        %end;
      %end;
      %let rc = %sysfunc(close(&dsid));
    %end;

    %if &missing = 1 %then %let wpct_ok = 0;
    %else %do;
      %if &nobs = &rows %then
        %put NOTE: (WPCT CAPTURE) &data has &nobs rows, the count the shipped SAS log reports for &file..;
      %else
        %put WARNING: (WPCT CAPTURE) &data has &nobs rows, but the shipped SAS log reports &rows for &file..;

      /* Formats are removed. A numeric is written with PUTN and the explicit WPCT_NUMFMT format,
         the function the pre-flight checks, without leading blanks; special missing values are
         written as the ordinary missing value. A character value is written as stored, without
         its trailing blanks, leading and interior blanks kept, and as an empty field when missing.
         A character value that holds a comma, a double quote or a line break is enclosed in
         double quotes, with each double quote in it doubled. Each record is assembled in
         _WPCT_REC and written by one formatted PUT, so no DSD or list-output rule changes a
         value. A record longer than 32767 bytes stops the step, and the file counts as failed. */
      %let toolong = 0;
      data _null_;
        file "&path" encoding="&wpct_encoding" lrecl=32767;
        length _wpct_rec _wpct_fld $32767;
        if _n_ = 1 then put "&header";
        set &data;
        format _all_;
        _wpct_rlen = 0;
        %do i = 1 %to &n;
          %let var = %scan(&vars, &i, %str( ));
          %if &i > 1 %then %do;
            _wpct_fld = ',';
            _wpct_flen = 1;
            link _wpct_add;
          %end;
          %if %scan(&types, &i) = N %then %do;
            if missing(&var) then &var = .;
            _wpct_fld = left(putn(&var, "&wpct_numfmt"));
            _wpct_flen = lengthn(_wpct_fld);
          %end;
          %else %do;
            _wpct_flen = lengthn(&var);
            if _wpct_flen > 0 then do;
              _wpct_fld = substr(&var, 1, _wpct_flen);
              if indexc(_wpct_fld, ',"', '0D0A'x) then do;
                _wpct_flen = _wpct_flen + countc(_wpct_fld, '"') + 2;
                if _wpct_flen <= 32767 then do;
                  _wpct_fld = tranwrd(substr(&var, 1, lengthn(&var)), '"', '""');
                  _wpct_fld = '"' || trimn(_wpct_fld) || '"';
                end;
              end;
            end;
          %end;
          link _wpct_add;
        %end;
        put _wpct_rec $varying32767. _wpct_rlen;
        return;

      _wpct_add:
        if _wpct_flen > 0 then do;
          if _wpct_rlen + _wpct_flen > 32767 then do;
            putlog "ERROR: (WPCT CAPTURE) Row " _n_ "of &data makes a record of &file longer than 32767 bytes.";
            call symputx('toolong', '1', 'L');
            stop;
          end;
          substr(_wpct_rec, _wpct_rlen + 1, _wpct_flen) = _wpct_fld;
          _wpct_rlen = _wpct_rlen + _wpct_flen;
        end;
        return;
      run;
      %if &syserr > 4 or &toolong = 1 %then %do;
        %put ERROR: (WPCT CAPTURE) Writing &path failed.;
        %let wpct_ok = 0;
      %end;
      %else %put NOTE: (WPCT CAPTURE) Wrote &path from &data..;

      /* Read-back check: SAS reads the CSV again and compares every value with the native copy.
         SAS list input reads one line per record and removes leading blanks and enclosing
         quotes, so a character value with leading blanks, a quoted value or one with a line
         break can read back as other text, or as an extra row, and draw a WARNING although the
         CSV holds it as stored. */
      data work._wpct_rb;
        infile "&path" dsd firstobs=2 lrecl=32767 truncover encoding="&wpct_encoding";
        input (_wpct_f1-_wpct_f&n) (:$512.);
      run;
      %let dsid = %sysfunc(open(work._wpct_rb, i));
      %if &dsid = 0 %then %let rbobs = -1;
      %else %do;
        %let rbobs = %sysfunc(attrn(&dsid, nlobs));
        %let rc = %sysfunc(close(&dsid));
      %end;
      %if &rbobs ne &nobs %then
        %put WARNING: (WPCT CAPTURE) &file reads back with &rbobs data rows, but &data has &nobs..;
      %else %do;
        data _null_;
          set &data end=_wpct_last;
          set work._wpct_rb;
          retain _wpct_d1-_wpct_d&n 0 _wpct_m1-_wpct_m&n;
          %do i = 1 %to &n;
            %let var = %scan(&vars, &i, %str( ));
            %if %scan(&types, &i) = N %then %do;
              _wpct_x = input(_wpct_f&i, ?? 32.);
              if _wpct_x ne &var then do;
                _wpct_d&i + 1;
                _wpct_m&i = max(_wpct_m&i, abs(_wpct_x - &var));
              end;
            %end;
            %else %do;
              if _wpct_f&i ne &var then _wpct_d&i + 1;
            %end;
          %end;
          if _wpct_last then do;
            %do i = 1 %to &n;
              %let var = %scan(&vars, &i, %str( ));
              if _wpct_d&i = 0 then
                put "NOTE: (WPCT CAPTURE) &file &var: every CSV value reads back as the native value.";
              %if %scan(&types, &i) = N %then %do;
              else put "WARNING: (WPCT CAPTURE) &file &var: " _wpct_d&i "CSV values read back as another number than the native value, largest absolute difference " _wpct_m&i best32.;
              %end;
              %else %do;
              else put "WARNING: (WPCT CAPTURE) &file &var: " _wpct_d&i "CSV values read back as other text than the native value.";
              %end;
            %end;
          end;
        run;
      %end;
      proc datasets library=work memtype=data nolist nodetails;
        delete _wpct_rb;
      quit;
    %end;
  %end;
%mend wpct_export_csv;


/*** The export statements: one call per WPCT-F.07.02 contract file. File names and columns
  mirror wpct/schemas.py, which is binding; test_sas_capture_contract.py in this folder checks
  that every 07.02 file and column listed there appears here, and no other file. ***/
%macro wpct_exports;
  %wpct_export_csv(data=wpctcap.css_anadata, file=WPCT-F.07.02_css_anadata.csv, rows=3548,
                   vars=STUDYID USUBJID SAFFL TRTPN PARAMCD PARAM ATPTN ATPT AVISIT AVISITN BASE CHG
                        ANL01FL trtp_short)
  %wpct_export_csv(data=wpctcap.css_stats, file=WPCT-F.07.02_css_stats.csv, rows=60,
                   vars=PARAMCD ATPTN AVISITN TRTPN AVISIT trtp_short n mean std median datamin
                        datamax q1 q3 pval)
  %wpct_export_csv(data=wpctcap.parameter_estimates, file=WPCT-F.07.02_parameter_estimates.csv,
                   rows=10, vars=PARAMCD ATPTN Parameter Estimate StdErr tValue Probt)
%mend wpct_exports;


/*** Row count of a dataset, as macro text: 0 when DATA does not exist, -1 when it cannot be
  opened, otherwise its number of logical observations. ***/
%macro wpct_nobs(data);
  %local dsid nobs rc;
  %let nobs = 0;
  %if %sysfunc(exist(&data)) %then %do;
    %let dsid = %sysfunc(open(&data, i));
    %if &dsid = 0 %then %let nobs = -1;
    %else %do;
      %let nobs = %sysfunc(attrn(&dsid, nlobs));
      %let rc = %sysfunc(close(&dsid));
    %end;
  %end;
&nobs
%mend wpct_nobs;


/*** Append of one stratum to a capture dataset, called by the capture inserts of the staged copy.
  BASE   the capture dataset in WPCTCAP
  DATA   the WORK dataset the insert built
  RC     SYSERR of the DATA step that built DATA
  A failed build, a failed append, or an append that does not add exactly the rows of DATA sets
  the global WPCT_CAPFAIL, which WPCT_FINISH checks. The row counts are compared as well as
  SYSERR, because a failed PROC APPEND can leave SYSERR=4 (SAS Sample 45143). DATA is deleted
  from WORK afterwards. ***/
%macro wpct_append(base=, data=, rc=);
  %local before added after;
  %if &rc > 4 or not %sysfunc(exist(&data)) %then %do;
    %put ERROR: (WPCT CAPTURE) Building &data for &base failed (SYSERR=&rc), so this stratum is not captured.;
    %let wpct_capfail = 1;
  %end;
  %else %do;
    %let before = %wpct_nobs(&base);
    %let added = %wpct_nobs(&data);
    proc append base=&base data=&data;
    run;
    %let rc = &syserr;
    %let after = %wpct_nobs(&base);
    %if &rc > 4 or &added < 1 or &before < 0 or &after ne %eval(&before + &added) %then %do;
      %put ERROR: (WPCT CAPTURE) Appending &data to &base failed: SYSERR=&rc, &added rows to add, &before rows before the append and &after after it.;
      %let wpct_capfail = 1;
    %end;
  %end;
  %if %sysfunc(exist(&data)) %then %do;
    proc datasets library=work memtype=data nolist nodetails;
      delete %scan(&data, 2, .);
    quit;
  %end;
%mend wpct_append;


/*** Guards, environment record, autocall setup, capture library and staged copy. On success it
  points fileref WPCTRUN at the staged copy; otherwise WPCTRUN stays the DUMMY device, so the
  INCLUDE reads nothing. ***/
%macro wpct_prepare;
  %global wpct_ok wpct_staged wpct_printto wpct_syntaxcheck wpct_sasdirs wpct_action wpct_capfail
          wpct_syscc;
  %local wpct_t wpct_rc wpct_old;
  %let wpct_ok = 0;
  %let wpct_staged = 0;
  %let wpct_printto = 0;
  %let wpct_syntaxcheck = %sysfunc(getoption(syntaxcheck));
  %let wpct_sasdirs = ;
  %let wpct_action = ;
  %let wpct_capfail = 0;
  %let wpct_syscc = ;

  filename wpctrun dummy;

  %if "%sysfunc(getoption(obs))" = "0" %then
    %put ERROR: (WPCT CAPTURE) This session is in syntax-check mode (OBS=0). Start a new SAS session and run the launcher again.;
  %else %do;
    /* Settings: present, usable in the macro calls of the program, absolute, and outside the
       checkout (failure point 17). Paths are written back in their canonical text: / separators,
       no repeated separator except the leading pair of a Windows UNC path, and no trailing
       separator. */
    data _null_;
      length _wpct_name $16 _wpct_value _wpct_clean $1024 _wpct_enc _wpct_fmt _wpct_probe $64
             _wpct_rs _wpct_ds _wpct_mpath $1024 _wpct_mac $32 _wpct_mref $8;
      array _wpct_p {3} $1024 _wpct_p1-_wpct_p3;
      array _wpct_c {3} $1024 _wpct_c1-_wpct_c3;
      array _wpct_n {3} $1024 _wpct_n1-_wpct_n3;
      array _wpct_m {3} $16 _wpct_m1-_wpct_m3 ('WPCT_REPO' 'WPCT_WORKDIR' 'WPCT_CSVDIR');
      _wpct_ok = 1;
      _wpct_fold = ("&sysscp" = 'WIN');
      _wpct_enc = strip(symget('wpct_encoding'));
      _wpct_fmt = strip(symget('wpct_numfmt'));

      do _wpct_i = 1 to 3;
        _wpct_name = _wpct_m{_wpct_i};
        _wpct_value = strip(symget(strip(_wpct_name)));
        _wpct_clean = ' ';
        if _wpct_value = ' ' then do;
          put 'ERROR: (WPCT CAPTURE) Setting ' _wpct_name 'is blank.';
          _wpct_ok = 0;
        end;
        else if indexc(_wpct_value, '&%;,"()|', byte(39)) then do;
          put 'ERROR: (WPCT CAPTURE) Setting ' _wpct_name 'holds a quotation mark or one of & % ; , ( ) |, which the macro calls of the program cannot carry: ' _wpct_value;
          _wpct_ok = 0;
        end;
        else do;
          _wpct_clean = translate(_wpct_value, '/', '\');
          /* One canonical text per OS. On Windows a leading double separator starts a UNC path
             and stays. On UNIX a leading double separator is the same as a single one, so every
             run of separators becomes one. */
          if _wpct_fold then _wpct_clean = prxchange('s#(?<=.)/{2,}#/#', -1, trim(_wpct_clean));
          else _wpct_clean = prxchange('s#/{2,}#/#', -1, trim(_wpct_clean));
          do while (length(_wpct_clean) > 1 and char(_wpct_clean, length(_wpct_clean)) = '/');
            _wpct_clean = substr(_wpct_clean, 1, length(_wpct_clean) - 1);
          end;
          /* Windows accepts a drive-qualified path or a UNC path with server and share only: a
             path rooted on the current drive, a device path and a relative path are refused. */
          if _wpct_fold and not prxmatch('#^([A-Za-z]:/|//[^/?.][^/]{0,}/[^/])#', trim(_wpct_clean)) then do;
            put 'ERROR: (WPCT CAPTURE) Setting ' _wpct_name 'is not an absolute path. On Windows a path starts with a drive letter, as in C:/work, or is a UNC path //server/share: ' _wpct_value;
            _wpct_ok = 0;
          end;
          else if not _wpct_fold and char(_wpct_clean, 1) ne '/' then do;
            put 'ERROR: (WPCT CAPTURE) Setting ' _wpct_name 'is not an absolute path. On UNIX a path starts with /: ' _wpct_value;
            _wpct_ok = 0;
          end;
          else if index(cats('/', _wpct_clean, '/'), '/./') or index(cats('/', _wpct_clean, '/'), '/../') then do;
            put 'ERROR: (WPCT CAPTURE) Setting ' _wpct_name 'holds a . or .. segment: ' _wpct_value;
            _wpct_ok = 0;
          end;
          /* Windows drops a dot or a blank at the end of a folder name and reads a colon after the
             drive as a stream name, so such a path is another spelling of another path. */
          else if _wpct_fold and (index(substr(_wpct_clean, 3), ':') or prxmatch('#[. ](/|$)#', trim(_wpct_clean))) then do;
            put 'ERROR: (WPCT CAPTURE) Setting ' _wpct_name 'holds a colon after the drive, or a folder name that ends in a dot or a blank, which Windows reads as another path: ' _wpct_value;
            _wpct_ok = 0;
          end;
        end;
        _wpct_p{_wpct_i} = _wpct_value;
        _wpct_c{_wpct_i} = _wpct_clean;
        if _wpct_fold then _wpct_n{_wpct_i} = upcase(_wpct_clean);
        else _wpct_n{_wpct_i} = _wpct_clean;
      end;
      if _wpct_enc = ' ' or not prxmatch('/^[A-Za-z0-9_-]+$/', trim(_wpct_enc)) then do;
        put 'ERROR: (WPCT CAPTURE) Setting WPCT_ENCODING is not an encoding name: ' _wpct_enc;
        _wpct_ok = 0;
      end;
      if _wpct_fmt = ' ' or not prxmatch('/^[A-Za-z_]+[0-9]+\.[0-9]*$/', trim(_wpct_fmt)) then do;
        put 'ERROR: (WPCT CAPTURE) Setting WPCT_NUMFMT is not a numeric format such as E32.17 or BEST32.: ' _wpct_fmt;
        _wpct_ok = 0;
      end;

      if _wpct_ok then do;
        /* One rule for WPCT_WORKDIR and WPCT_CSVDIR, on the canonical texts: the directory must
           be neither the repository, nor inside it, nor a parent of it. A path lies within a
           directory when the path plus a separator starts with the directory plus a separator,
           which holds for the root directory too. With the directory neither equal to, inside nor
           a parent of the repository, every path below it (capture, pdf, csv, the staged copy,
           the logs and the CSVs) lies outside the repository as well. */
        if char(_wpct_n1, length(_wpct_n1)) = '/' then _wpct_rs = _wpct_n1;
        else _wpct_rs = cats(_wpct_n1, '/');
        do _wpct_i = 2 to 3;
          _wpct_name = _wpct_m{_wpct_i};
          if char(_wpct_n{_wpct_i}, length(_wpct_n{_wpct_i})) = '/' then _wpct_ds = _wpct_n{_wpct_i};
          else _wpct_ds = cats(_wpct_n{_wpct_i}, '/');
          if index(cats(_wpct_n{_wpct_i}, '/'), trim(_wpct_rs)) = 1 then do;
            put 'ERROR: (WPCT CAPTURE) ' _wpct_name 'must lie outside the repository ' _wpct_c1;
            _wpct_ok = 0;
          end;
          else if index(cats(_wpct_n1, '/'), trim(_wpct_ds)) = 1 then do;
            put 'ERROR: (WPCT CAPTURE) ' _wpct_name 'must not be a parent directory of the repository ' _wpct_c1;
            _wpct_ok = 0;
          end;
        end;
        if not fileexist(cats(_wpct_c1, '/whitepapers/WPCT/WPCT-F.07.02.sas')) then do;
          put 'ERROR: (WPCT CAPTURE) Not found: ' _wpct_c1 +(-1) '/whitepapers/WPCT/WPCT-F.07.02.sas';
          _wpct_ok = 0;
        end;
        if not fileexist(cats(_wpct_c1, '/data/adam/cdisc/advs.xpt')) then do;
          put 'ERROR: (WPCT CAPTURE) Not found: ' _wpct_c1 +(-1) '/data/adam/cdisc/advs.xpt';
          _wpct_ok = 0;
        end;
        if not fileexist(cats(_wpct_c1, '/whitepapers/utilities')) then do;
          put 'ERROR: (WPCT CAPTURE) Not found: ' _wpct_c1 +(-1) '/whitepapers/utilities';
          _wpct_ok = 0;
        end;
        /* Each repository macro that WPCT-F.07.02 calls, directly or through another macro, must
           be a file that opens for input before the staged copy runs, so that no same-name macro
           elsewhere on the autocall path stands in. FOPEN also rejects a directory, which
           FILEEXIST accepts. Every file that fails is reported. */
        else do _wpct_mac = 'util_access_test_data', 'util_delete_dsets', 'assert_depend',
                            'assert_continue', 'assert_dset_exist', 'assert_var_exist',
                            'assert_macro_exist', 'util_resolve_sasautos', 'util_labels_from_var',
                            'util_count_unique_values', 'util_get_var_min_max', 'util_value_format',
                            'util_boxplot_block_ranges', 'util_proc_template', 'util_axis_order';
          _wpct_mpath = cats(_wpct_c1, '/whitepapers/utilities/', _wpct_mac, '.sas');
          _wpct_mref = '_wpctmac';
          _wpct_mfid = 0;
          if filename(_wpct_mref, trim(_wpct_mpath)) = 0 then do;
            _wpct_mfid = fopen(_wpct_mref, 'I');
            if _wpct_mfid > 0 then _wpct_rc = fclose(_wpct_mfid);
            _wpct_rc = filename(_wpct_mref);
          end;
          if _wpct_mfid = 0 then do;
            put 'ERROR: (WPCT CAPTURE) Not found or not readable: ' _wpct_mpath;
            _wpct_ok = 0;
          end;
        end;
      end;

      /* Pre-flight of the numeric CSV format: each test value must be written as text that SAS
         reads back as a number. Exact read-back is reported, not required. */
      if _wpct_ok then do _wpct_x = -1.2345678901234567E-3, 0.30000000000000004, 0.33333333333333331, 123456.789;
        _wpct_probe = left(putn(_wpct_x, strip(_wpct_fmt)));
        _wpct_back = input(_wpct_probe, ?? 32.);
        if _wpct_back = . then do;
          put 'ERROR: (WPCT CAPTURE) WPCT_NUMFMT ' _wpct_fmt 'did not write a number that SAS reads back: ' _wpct_probe '- set WPCT_NUMFMT to BEST32. and run again.';
          _wpct_ok = 0;
          leave;
        end;
        else if _wpct_back = _wpct_x then
          put 'NOTE: (WPCT CAPTURE) WPCT_NUMFMT pre-flight: ' _wpct_x= hex16. 'is written as ' _wpct_probe 'and reads back exactly.';
        else
          put 'WARNING: (WPCT CAPTURE) WPCT_NUMFMT pre-flight: ' _wpct_x= hex16. 'is written as ' _wpct_probe 'and reads back as ' _wpct_back= hex16.;
      end;

      if _wpct_ok then do _wpct_i = 1 to 3;
        call symputx(_wpct_m{_wpct_i}, _wpct_c{_wpct_i}, 'G');
      end;
      call symputx('wpct_ok', _wpct_ok, 'G');
    run;
  %end;

  /* Session scratch storage: the WORK library must lie outside the checkout, and one-level
     dataset names must go to WORK, so no USER library may be set. Both are checked before
     the launcher writes any file, dataset or template of its own. */
  %if &wpct_ok = 1 %then %do;
    data _null_;
      length _wpct_repo _wpct_wd _wpct_wk _wpct_prefix _wpct_user _wpct_uopt _wpct_wkp
             _wpct_base $1024;
      _wpct_ok = 1;
      _wpct_repo = symget('wpct_repo');
      _wpct_wd = pathname('work');
      _wpct_wk = translate(strip(_wpct_wd), '/', '\');
      _wpct_wk = prxchange('s#(?<=.)/{2,}#/#', -1, trim(_wpct_wk));
      do while (length(_wpct_wk) > 1 and char(_wpct_wk, length(_wpct_wk)) = '/');
        _wpct_wk = substr(_wpct_wk, 1, length(_wpct_wk) - 1);
      end;
      _wpct_wkp = _wpct_wk;
      if "&sysscp" = 'WIN' then do;
        _wpct_repo = upcase(_wpct_repo);
        _wpct_wk = upcase(_wpct_wk);
      end;
      if char(_wpct_repo, length(_wpct_repo)) = '/' then _wpct_prefix = _wpct_repo;
      else _wpct_prefix = cats(_wpct_repo, '/');
      if _wpct_wk = ' ' then do;
        put 'ERROR: (WPCT CAPTURE) The physical location of the WORK library is unknown, so it cannot be shown to lie outside the repository.';
        _wpct_ok = 0;
      end;
      else if _wpct_wk = _wpct_repo or index(_wpct_wk, trim(_wpct_prefix)) = 1 then do;
        put 'ERROR: (WPCT CAPTURE) The WORK library of this session lies inside the repository: ' _wpct_wd;
        put 'ERROR: (WPCT CAPTURE) Start SAS with a WORK location outside the checkout (the -WORK option) and run the launcher again.';
        _wpct_ok = 0;
      end;
      else if not fileexist(strip(_wpct_wd)) then do;
        put 'ERROR: (WPCT CAPTURE) The WORK library directory ' _wpct_wd 'cannot be inspected, so it cannot be shown to lie outside the repository.';
        _wpct_ok = 0;
      end;
      else if length(_wpct_wkp) + 3 * 64 > vlength(_wpct_base) then do;
        put 'ERROR: (WPCT CAPTURE) The WORK library path is too long to inspect the 64 directories above it, so it cannot be shown to lie outside the repository: ' _wpct_wd;
        _wpct_ok = 0;
      end;
      else do;
        /* No directory up to 64 levels above WORK may hold both checkout markers, whatever the
           spelling of WPCT_REPO: each level is reached with .. because the operating system
           resolves .. after following symbolic links, which trimming path components would not. */
        _wpct_base = _wpct_wkp;
        do _wpct_k = 1 to 64;
          _wpct_base = cats(_wpct_base, '/..');
          if fileexist(cats(_wpct_base, '/whitepapers/WPCT/WPCT-F.07.02.sas')) and fileexist(cats(_wpct_base, '/data/adam/cdisc/advs.xpt')) then do;
            put 'ERROR: (WPCT CAPTURE) The WORK library of this session lies inside a checkout of this repository: ' _wpct_wd;
            put 'ERROR: (WPCT CAPTURE) Start SAS with a WORK location outside the checkout (the -WORK option) and run the launcher again.';
            _wpct_ok = 0;
            leave;
          end;
        end;
      end;
      /* A USER libref, or a USER= system option that names a libref other than WORK, takes the
         one-level names of the program and of SAS procedures away from WORK. */
      _wpct_uopt = strip(getoption('user'));
      if libref('USER') = 0 or (_wpct_uopt ne ' ' and upcase(_wpct_uopt) ne 'WORK') then do;
        if libref('USER') = 0 then _wpct_user = pathname('USER');
        else _wpct_user = cats('USER=', _wpct_uopt);
        put 'ERROR: (WPCT CAPTURE) A USER library is assigned, so one-level dataset names would not go to WORK: ' _wpct_user;
        put 'ERROR: (WPCT CAPTURE) Start a SAS session without a USER library (no LIBNAME USER statement and no USER= system option) and run the launcher again.';
        _wpct_ok = 0;
      end;
      call symputx('wpct_ok', _wpct_ok, 'G');
    run;
  %end;

  /* Physical containment (failure point 17). The checks above compare path text, which a
     symbolic link, a junction, a mount or another spelling of a repository directory passes.
     This check compares directory listings instead: one physical directory has one set of
     member names at a given moment, whatever path lists it. Each destination directory, or the
     nearest existing directory above it when it does not exist yet, is listed, and then every
     directory reachable from the repository root. A repository directory with the member names
     of a destination listing may be that destination, and the run stops; when no repository
     directory has them, the destination is not in the repository. The check only reads: it
     lists directories and opens files for input, so nothing is created, written or removed, in
     the destinations or in the repository, before the verdict. A repository entry that can be
     neither listed nor opened, a repository root that cannot be listed and a search beyond its
     bounds stop the run, because a destination could lie unseen below them. An empty
     destination has the listing of every empty repository directory, so the two cannot be told
     apart, and it is refused. The exception is a capture, pdf or default csv folder of the work
     directory that an earlier run left empty. The launcher creates those folders itself, so
     such a folder is checked as one still to be created in the work directory, removed once
     every check has passed, and created again by the directory step below. Only DATA step
     functions are used, so the check also runs where the X command is disabled. */
  %if &wpct_ok = 1 %then %do;
    data _null_;
      length _wpct_repo _wpct_target _wpct_dir _wpct_parent _wpct_root _wpct_name _wpct_path
             _wpct_y _wpct_qy _wpct_where $1024 _wpct_label $24 _wpct_perm $64
             _wpct_dref _wpct_mref _wpct_rref $8;
      array _wpct_t {4} $1024 _wpct_t1-_wpct_t4;
      array _wpct_s {4} $24 _wpct_s1-_wpct_s4 ('WPCT_WORKDIR' 'WPCT_WORKDIR/capture' 'WPCT_WORKDIR/pdf' 'WPCT_CSVDIR');
      array _wpct_e {4} $1024 _wpct_e1-_wpct_e4;
      array _wpct_j {4} _wpct_j1-_wpct_j4;
      array _wpct_o {4} _wpct_o1-_wpct_o4;
      array _wpct_ed {4} $1024 _wpct_ed1-_wpct_ed4;
      array _wpct_ec {4} _wpct_ec1-_wpct_ec4;
      array _wpct_nf {4} _wpct_nf1-_wpct_nf4;
      array _wpct_py {4} $1024 _wpct_py1-_wpct_py4;
      declare hash _wpct_q();
      _wpct_q.defineKey('_wpct_qk');
      _wpct_q.defineData('_wpct_qy');
      _wpct_q.defineDone();
      declare hash _wpct_l();
      _wpct_l.defineKey('_wpct_ek', '_wpct_name');
      _wpct_l.defineDone();
      _wpct_ok = 1;
      _wpct_fold = ("&sysscp" = 'WIN');
      _wpct_repo = symget('wpct_repo');
      _wpct_t1 = symget('wpct_workdir');
      _wpct_t2 = cats(_wpct_t1, '/capture');
      _wpct_t3 = cats(_wpct_t1, '/pdf');
      _wpct_t4 = symget('wpct_csvdir');
      _wpct_dref = '_wpctdir';
      _wpct_mref = '_wpctmem';
      _wpct_rref = '_wpctrmd';
      _wpct_ne = 0;
      _wpct_ek = 0;
      _wpct_qk = 0;
      do _wpct_i = 1 to 4;
        _wpct_o{_wpct_i} = 0;
        _wpct_ec{_wpct_i} = 0;
        _wpct_py{_wpct_i} = ' ';
      end;

      /* The existing directory each destination is, or is created in. Parents are taken as the
         directory step below takes them, and never above the root of the path. */
      do _wpct_i = 1 to 4 while (_wpct_ok);
        _wpct_target = _wpct_t{_wpct_i};
        _wpct_label = _wpct_s{_wpct_i};
        _wpct_dir = _wpct_target;
        if _wpct_fold and char(_wpct_dir, 2) = ':' then _wpct_root = substr(_wpct_dir, 1, 3);
        else if _wpct_fold then _wpct_root = prxchange('s#^(//[^/]+/[^/]+).*$#$1#', 1, trim(_wpct_dir));
        else _wpct_root = '/';
        do while (_wpct_ok and not fileexist(_wpct_dir));
          if _wpct_dir = _wpct_root then do;
            put 'ERROR: (WPCT CAPTURE) ' _wpct_label _wpct_target 'cannot be placed: neither it nor any directory above it exists.';
            _wpct_ok = 0;
          end;
          else do;
            _wpct_name = scan(_wpct_dir, -1, '/');
            _wpct_plen = length(_wpct_dir) - length(_wpct_name) - 1;
            if _wpct_plen > 0 then _wpct_parent = substr(_wpct_dir, 1, _wpct_plen);
            else _wpct_parent = ' ';
            if _wpct_parent = ' ' or char(_wpct_parent, length(_wpct_parent)) = ':' then _wpct_parent = cats(_wpct_parent, '/');
            if length(_wpct_parent) < length(_wpct_root) then _wpct_dir = _wpct_root;
            else _wpct_dir = _wpct_parent;
          end;
        end;
        /* The capture and pdf folders, and the CSV folder when it is the default <work>/csv, are
           created by the launcher. Such a folder that exists, can be listed and is empty was left
           by an earlier run. It is checked as a folder still to be created in the work directory,
           whose existing directory is then the work directory, and it is removed after the
           check, so an empty folder never needs to be told apart from an empty repository
           directory. */
        if _wpct_ok and _wpct_i > 1 and _wpct_dir = _wpct_target then do;
          if _wpct_i < 4 then _wpct_own = 1;
          else if _wpct_fold then _wpct_own = (upcase(_wpct_target) = upcase(cats(_wpct_t1, '/csv')));
          else _wpct_own = (_wpct_target = cats(_wpct_t1, '/csv'));
          if _wpct_own then do;
            if filename(_wpct_dref, trim(_wpct_target)) = 0 then do;
              _wpct_did = dopen(_wpct_dref);
              if _wpct_did > 0 then do;
                _wpct_nm = 0;
                do _wpct_mi = 1 to dnum(_wpct_did);
                  _wpct_name = dread(_wpct_did, _wpct_mi);
                  if _wpct_name ne '.' and _wpct_name ne '..' then _wpct_nm = _wpct_nm + 1;
                end;
                _wpct_o{_wpct_i} = (_wpct_nm = 0);
                _wpct_rc = dclose(_wpct_did);
              end;
              _wpct_rc = filename(_wpct_dref);
            end;
            if _wpct_o{_wpct_i} then _wpct_dir = _wpct_e1;
          end;
        end;
        _wpct_e{_wpct_i} = _wpct_dir;
        _wpct_j{_wpct_i} = 0;
        do _wpct_k = 1 to _wpct_ne;
          if _wpct_j{_wpct_i} = 0 and (_wpct_ed{_wpct_k} = _wpct_dir or (_wpct_fold and upcase(_wpct_ed{_wpct_k}) = upcase(_wpct_dir))) then _wpct_j{_wpct_i} = _wpct_k;
        end;
        if _wpct_ok and _wpct_j{_wpct_i} = 0 then do;
          _wpct_ne = _wpct_ne + 1;
          _wpct_j{_wpct_i} = _wpct_ne;
          _wpct_ed{_wpct_ne} = _wpct_dir;
        end;
      end;

      /* The listing of each of those directories: its member count, and every member name in the
         hash object _wpct_l, keyed by the position of the directory and the name. */
      do _wpct_k = 1 to _wpct_ne while (_wpct_ok);
        _wpct_dir = _wpct_ed{_wpct_k};
        _wpct_did = 0;
        if filename(_wpct_dref, trim(_wpct_dir)) = 0 then _wpct_did = dopen(_wpct_dref);
        if _wpct_did = 0 then do;
          put 'ERROR: (WPCT CAPTURE) Cannot list ' _wpct_dir +(-1) ' to establish its physical location.';
          _wpct_ok = 0;
        end;
        else do;
          _wpct_ek = _wpct_k;
          do _wpct_mi = 1 to dnum(_wpct_did);
            _wpct_name = dread(_wpct_did, _wpct_mi);
            if _wpct_name ne '.' and _wpct_name ne '..' then do;
              _wpct_ec{_wpct_k} = _wpct_ec{_wpct_k} + 1;
              _wpct_rc = _wpct_l.add();
            end;
          end;
          _wpct_rc = dclose(_wpct_did);
        end;
        _wpct_rc = filename(_wpct_dref);
      end;

      /* Breadth-first search of every directory reachable from the repository root. The hash
         object _wpct_q holds the queue, keyed by the position of each directory in it. A
         repository directory has the listing of a destination when it holds as many members and
         each of its member names is in that listing. Each member is then classified: a member
         DOPEN lists is a directory and is queued; a member FILEEXIST does not find is a link to
         nothing, below which nothing can lie; a member FOPEN opens for input is a file, unless
         its permissions show a directory or a link that DOPEN could not list. Any other member,
         and any repository directory that cannot be listed, stops the run. */
      _wpct_qn = 0;
      _wpct_qi = 0;
      if _wpct_ok then do;
        _wpct_qn = 1;
        _wpct_qk = 1;
        _wpct_qy = _wpct_repo;
        _wpct_rc = _wpct_q.add();
      end;
      do while (_wpct_ok and _wpct_qi < _wpct_qn);
        _wpct_qi = _wpct_qi + 1;
        _wpct_qk = _wpct_qi;
        _wpct_rc = _wpct_q.find();
        _wpct_y = _wpct_qy;
        _wpct_rc = _wpct_q.remove();
        _wpct_did = 0;
        if filename(_wpct_dref, trim(_wpct_y)) = 0 then _wpct_did = dopen(_wpct_dref);
        if _wpct_did = 0 then do;
          put 'ERROR: (WPCT CAPTURE) Cannot list the repository directory ' _wpct_y +(-1) ', so the launcher cannot establish that no destination lies in it.';
          _wpct_ok = 0;
        end;
        else do;
          _wpct_ny = 0;
          do _wpct_k = 1 to _wpct_ne;
            _wpct_nf{_wpct_k} = 0;
          end;
          do _wpct_mi = 1 to dnum(_wpct_did) while (_wpct_ok);
            _wpct_name = dread(_wpct_did, _wpct_mi);
            if _wpct_name ne '.' and _wpct_name ne '..' then do;
              _wpct_ny = _wpct_ny + 1;
              do _wpct_k = 1 to _wpct_ne;
                _wpct_ek = _wpct_k;
                if _wpct_l.check() ne 0 then _wpct_nf{_wpct_k} = _wpct_nf{_wpct_k} + 1;
              end;
              if length(_wpct_y) + 1 + length(_wpct_name) >= 1024 then do;
                put 'ERROR: (WPCT CAPTURE) A repository path is too long for the search: ' _wpct_y +(-1) '/' _wpct_name;
                _wpct_ok = 0;
              end;
              else do;
                _wpct_path = trim(_wpct_y) || '/' || trim(_wpct_name);
                /* 0: neither listed nor opened, 1: a directory, 2: a file, 3: nothing there,
                   4: a directory or a link that cannot be listed. */
                _wpct_kind = 0;
                if filename(_wpct_mref, trim(_wpct_path)) = 0 then do;
                  _wpct_mid = dopen(_wpct_mref);
                  if _wpct_mid > 0 then do;
                    _wpct_kind = 1;
                    _wpct_rc = dclose(_wpct_mid);
                  end;
                  else if not fileexist(trim(_wpct_path)) then _wpct_kind = 3;
                  else do;
                    _wpct_fid = fopen(_wpct_mref, 'I');
                    if _wpct_fid > 0 then do;
                      _wpct_perm = left(finfo(_wpct_fid, 'Access Permission'));
                      if char(_wpct_perm, 1) = 'd' or char(_wpct_perm, 1) = 'l' then _wpct_kind = 4;
                      else _wpct_kind = 2;
                      _wpct_rc = fclose(_wpct_fid);
                    end;
                  end;
                  _wpct_rc = filename(_wpct_mref);
                end;
                if _wpct_kind = 1 then do;
                  if _wpct_qn >= 100000 then do;
                    put 'ERROR: (WPCT CAPTURE) The repository holds more than 100000 directories, more than the search covers.';
                    _wpct_ok = 0;
                  end;
                  else do;
                    _wpct_qn = _wpct_qn + 1;
                    _wpct_qk = _wpct_qn;
                    _wpct_qy = _wpct_path;
                    if _wpct_q.add() ne 0 then do;
                      put 'ERROR: (WPCT CAPTURE) The search cannot queue the repository directory ' _wpct_path;
                      _wpct_ok = 0;
                    end;
                  end;
                end;
                else if _wpct_kind = 4 then do;
                  put 'ERROR: (WPCT CAPTURE) Cannot list the repository directory ' _wpct_path +(-1) ', so the launcher cannot establish that no destination lies in it.';
                  _wpct_ok = 0;
                end;
                else if _wpct_kind = 0 then do;
                  put 'ERROR: (WPCT CAPTURE) Cannot search the repository entry ' _wpct_path +(-1) ': it can be neither listed as a directory nor opened as a file, so the launcher cannot establish that no destination lies below it.';
                  _wpct_ok = 0;
                end;
              end;
            end;
          end;
          _wpct_rc = dclose(_wpct_did);
          if _wpct_ok then do _wpct_k = 1 to _wpct_ne;
            if _wpct_py{_wpct_k} = ' ' and _wpct_nf{_wpct_k} = 0 and _wpct_ec{_wpct_k} = _wpct_ny then _wpct_py{_wpct_k} = _wpct_y;
          end;
        end;
        _wpct_rc = filename(_wpct_dref);
      end;

      /* The verdict for each destination: a repository directory with its listing refuses it. */
      do _wpct_i = 1 to 4;
        _wpct_p = _wpct_j{_wpct_i};
        if _wpct_p > 0 then do;
          if _wpct_py{_wpct_p} ne ' ' then do;
            _wpct_label = _wpct_s{_wpct_i};
            _wpct_target = _wpct_t{_wpct_i};
            _wpct_dir = _wpct_e{_wpct_i};
            _wpct_where = _wpct_py{_wpct_p};
            if _wpct_ec{_wpct_p} = 0 then
              put 'ERROR: (WPCT CAPTURE) ' _wpct_label _wpct_dir 'is an empty directory, as is ' _wpct_where 'in the repository, so the launcher cannot tell them apart. Put a file into ' _wpct_dir +(-1) ', or choose another directory, and run again.';
            else if _wpct_dir = _wpct_target then
              put 'ERROR: (WPCT CAPTURE) ' _wpct_label _wpct_target 'is the same physical directory as ' _wpct_where 'in the repository, through a link or another spelling.';
            else
              put 'ERROR: (WPCT CAPTURE) ' _wpct_label _wpct_target 'would be created in ' _wpct_dir +(-1) ', which is the same physical directory as ' _wpct_where 'in the repository, through a link or another spelling.';
            _wpct_ok = 0;
          end;
        end;
      end;

      /* Only when every check has passed, the empty capture, pdf and csv folders of an earlier
         run are removed; the directory step below creates them again. FDELETE removes the
         directory entry, a link included, and never the directory a link points to. */
      if _wpct_ok then do _wpct_i = 2 to 4;
        if _wpct_o{_wpct_i} then do;
          _wpct_dir = _wpct_t{_wpct_i};
          _wpct_rc = 1;
          if filename(_wpct_rref, trim(_wpct_dir)) = 0 then do;
            _wpct_rc = fdelete(_wpct_rref);
            _wpct_frc = filename(_wpct_rref);
          end;
          if _wpct_rc = 0 then put 'NOTE: (WPCT CAPTURE) Removed the empty directory ' _wpct_dir +(-1) ', left by an earlier run. It is created again.';
          else do;
            put 'ERROR: (WPCT CAPTURE) Cannot remove the empty directory ' _wpct_dir +(-1) '. Remove it by hand.';
            _wpct_ok = 0;
          end;
        end;
      end;
      if _wpct_ok then
        put 'NOTE: (WPCT CAPTURE) Physical containment: none of the ' _wpct_ne 'destination directories has the listing of any of the ' _wpct_qn 'directories searched below ' _wpct_repo +(-1) '.';
      call symputx('wpct_ok', _wpct_ok, 'G');
    run;
  %end;

  /* Directories: the work directory, its capture and pdf subfolders, and the CSV directory. */
  %if &wpct_ok = 1 %then %do;
    data _null_;
      length _wpct_dir _wpct_name _wpct_parent _wpct_made $1024;
      array _wpct_d {4} $1024 _wpct_d1-_wpct_d4;
      _wpct_d1 = symget('wpct_workdir');
      _wpct_d2 = cats(_wpct_d1, '/capture');
      _wpct_d3 = cats(_wpct_d1, '/pdf');
      _wpct_d4 = symget('wpct_csvdir');
      _wpct_ok = 1;
      do _wpct_i = 1 to 4;
        _wpct_dir = _wpct_d{_wpct_i};
        if not fileexist(_wpct_dir) then do;
          _wpct_name = scan(_wpct_dir, -1, '/');
          _wpct_plen = length(_wpct_dir) - length(_wpct_name) - 1;
          if _wpct_plen > 0 then _wpct_parent = substr(_wpct_dir, 1, _wpct_plen);
          else _wpct_parent = ' ';
          if _wpct_parent = ' ' or char(_wpct_parent, length(_wpct_parent)) = ':' then _wpct_parent = cats(_wpct_parent, '/');
          _wpct_made = dcreate(strip(_wpct_name), strip(_wpct_parent));
          if _wpct_made = ' ' then do;
            put 'ERROR: (WPCT CAPTURE) Cannot create the directory ' _wpct_dir +(-1) '. Its parent must exist and be writable.';
            _wpct_ok = 0;
          end;
          else put 'NOTE: (WPCT CAPTURE) Created the directory ' _wpct_made;
        end;
      end;
      call symputx('wpct_ok', _wpct_ok, 'G');
    run;
  %end;

  /* Output leaves (failure point 17). Before anything is written, every entry at one of the exact
     names the staged program and the launcher write is removed, and no other file. The launcher
     writes the staged copy and the capture log and listing in the work directory. The program
     keeps the strata DIABP at timepoint 815 and SYSBP at timepoint 816 (lines 90-93 of
     WPCT-F.07.02.sas), and line 339 writes the box plot of each stratum to outputs_folder, a
     backslash and WPCT-F.07.02_Box_plot_<PARAMCD>_Change_by_visit_for_timepoint_<ATPTN>.pdf.
     With outputs_folder set to <work>/pdf, Windows writes the two box plots into the pdf folder,
     and UNIX writes files named pdf\<name> into the work directory (failure point 12). The two
     bare names are removed from the pdf folder and the two pdf\ names from the work directory.
     Names are compared exactly on UNIX and ignoring case on Windows. Any other file, such as a
     box plot kept under another name, stays. A revision of the program that moves or changes an
     anchor line stops at the anchor check (failure point 6); one with other strata or another
     PDF name needs these names changed. FDELETE removes the directory entry, which may be a
     symbolic or a hard link, and never changes the file it points to, so no later FILE, PRINTTO
     or ODS PDF write goes through a link left at an output name. The directories are listed
     rather than tested name by name, so a link that points to no existing file is removed as
     well. The CSVs and the native datasets of an earlier run are removed before the program
     runs. */
  %if &wpct_ok = 1 %then %do;
    data _null_;
      length _wpct_work _wpct_dir _wpct_name _wpct_path $1024 _wpct_dref _wpct_lref $8;
      array _wpct_o {7} $72 _wpct_o1-_wpct_o7 (
        'WPCT-F.07.02_staged.sas' 'WPCT-F.07.02_capture.log' 'WPCT-F.07.02_capture.lst'
        'pdf\WPCT-F.07.02_Box_plot_DIABP_Change_by_visit_for_timepoint_815.pdf'
        'pdf\WPCT-F.07.02_Box_plot_SYSBP_Change_by_visit_for_timepoint_816.pdf'
        'WPCT-F.07.02_Box_plot_DIABP_Change_by_visit_for_timepoint_815.pdf'
        'WPCT-F.07.02_Box_plot_SYSBP_Change_by_visit_for_timepoint_816.pdf');
      declare hash _wpct_l();
      _wpct_l.defineKey('_wpct_lk');
      _wpct_l.defineData('_wpct_path');
      _wpct_l.defineDone();
      _wpct_ok = 1;
      _wpct_fold = ("&sysscp" = 'WIN');
      _wpct_work = symget('wpct_workdir');
      _wpct_dref = '_wpctdir';
      _wpct_lref = '_wpctlef';
      _wpct_ln = 0;
      do _wpct_k = 1 to 2;
        if _wpct_k = 1 then do;
          _wpct_dir = _wpct_work;
          _wpct_lo = 1;
          _wpct_hi = 5;
        end;
        else do;
          _wpct_dir = cats(_wpct_work, '/pdf');
          _wpct_lo = 6;
          _wpct_hi = 7;
        end;
        _wpct_did = 0;
        if filename(_wpct_dref, trim(_wpct_dir)) = 0 then _wpct_did = dopen(_wpct_dref);
        if _wpct_did = 0 then do;
          put 'ERROR: (WPCT CAPTURE) Cannot open the directory ' _wpct_dir +(-1) ' to remove the outputs of an earlier run.';
          _wpct_ok = 0;
        end;
        else do;
          do _wpct_i = 1 to dnum(_wpct_did);
            _wpct_name = dread(_wpct_did, _wpct_i);
            _wpct_hit = 0;
            do _wpct_x = _wpct_lo to _wpct_hi while (not _wpct_hit);
              if _wpct_fold then _wpct_hit = (upcase(_wpct_name) = upcase(_wpct_o{_wpct_x}));
              else _wpct_hit = (_wpct_name = _wpct_o{_wpct_x});
            end;
            if _wpct_hit then do;
              _wpct_ln = _wpct_ln + 1;
              _wpct_lk = _wpct_ln;
              _wpct_path = trim(_wpct_dir) || '/' || trim(_wpct_name);
              if _wpct_l.add() ne 0 then do;
                put 'ERROR: (WPCT CAPTURE) Cannot list ' _wpct_path 'for removal.';
                _wpct_ok = 0;
              end;
            end;
          end;
          _wpct_rc = dclose(_wpct_did);
        end;
        _wpct_rc = filename(_wpct_dref);
      end;
      do _wpct_lk = 1 to _wpct_ln;
        _wpct_rc = _wpct_l.find();
        _wpct_rc = 1;
        if filename(_wpct_lref, trim(_wpct_path)) = 0 then do;
          _wpct_rc = fdelete(_wpct_lref);
          _wpct_frc = filename(_wpct_lref);
        end;
        if _wpct_rc = 0 then put 'NOTE: (WPCT CAPTURE) Removed ' _wpct_path +(-1) ', left at an output name by an earlier run.';
        else do;
          put 'ERROR: (WPCT CAPTURE) Cannot remove ' _wpct_path +(-1) ', which stands at an output name. Remove it by hand.';
          _wpct_ok = 0;
        end;
      end;
      call symputx('wpct_ok', _wpct_ok, 'G');
    run;
  %end;

  /* Log redirect and the environment record. No manifest file is written. */
  %if &wpct_ok = 1 %then %do;
    %put NOTE: (WPCT CAPTURE) From here on the log goes to &wpct_workdir/WPCT-F.07.02_capture.log.;
    proc printto log="&wpct_workdir/WPCT-F.07.02_capture.log"
                 print="&wpct_workdir/WPCT-F.07.02_capture.lst" new;
    run;
    %let wpct_printto = 1;

    /* PROC PRINTTO moves only the log and the LISTING print file, not ODS output. Any other ODS
       destination the session opened (HTML, HTML5, RTF, SASREPORT, a TAGSETS destination or
       DOCUMENT) would still write the output of PROC GLM and PROC SGRENDER to its own path, and
       an ODS OUTPUT request the session made would still write its data set to its own library.
       Either can be in the checkout. Each open destination is listed in the capture log. ODS
       OUTPUT CLEAR then discards every ODS OUTPUT request the session made, persistent ones
       included, and ODS _ALL_ CLOSE closes every destination. Only LISTING is opened again: its
       print file is the redirected listing and its graphics path the work directory. The ODS
       OUTPUT statement of the program itself (line 274) then opens the OUTPUT destination for
       its ParameterEstimates table only, and its ODS PDF statement opens the only other
       destination. The closed destinations are not opened again after the run (failure
       point 15). */
    data _null_;
      set sashelp.vdest;
      put 'NOTE: (WPCT CAPTURE) ODS destination ' destination 'with style ' style 'was open. The launcher closes it for the run.';
    run;
    ods output clear;
    ods _all_ close;
    ods listing gpath="&wpct_workdir";
    %put NOTE: (WPCT CAPTURE) Every ODS destination listed above is closed and stays closed after the run. Only LISTING is open, with the print file &wpct_workdir/WPCT-F.07.02_capture.lst and the graphics path &wpct_workdir, besides the ODS OUTPUT and ODS PDF destinations the program opens itself. Every earlier ODS OUTPUT request, persistent ones included, is cleared.;

    %put NOTE: (WPCT CAPTURE) UNVERIFIED IN SAS until run in a licensed session.;
    %put NOTE: (WPCT CAPTURE) WPCT_REPO=%superq(wpct_repo);
    %put NOTE: (WPCT CAPTURE) WPCT_WORKDIR=%superq(wpct_workdir);
    %put NOTE: (WPCT CAPTURE) WPCT_CSVDIR=%superq(wpct_csvdir);
    %put NOTE: (WPCT CAPTURE) WPCT_ENCODING=%superq(wpct_encoding) WPCT_NUMFMT=%superq(wpct_numfmt);
    %put NOTE: (WPCT CAPTURE) SYSVLONG=&sysvlong SYSSCP=&sysscp SYSSCPL=&sysscpl SYSENCODING=&sysencoding;
    %put NOTE: (WPCT CAPTURE) ENCODING=%sysfunc(getoption(encoding)) LOCALE=%sysfunc(getoption(locale));
    %put NOTE: (WPCT CAPTURE) SASAUTOS before the launcher: %qsysfunc(getoption(sasautos));
    proc product_status;
    run;
    proc options;
    run;

    /* The SAS-supplied autocall macros the repository macros call are compiled now, from the
       autocall path the session started with. */
    %let wpct_t = %qtrim(%str(a  ))_%qleft(%str(  a))_%datatyp(1);
    %if %superq(wpct_t) ne a_a_NUMERIC %then %do;
      %put ERROR: (WPCT CAPTURE) The SAS-supplied autocall macros QTRIM, QLEFT and DATATYP did not resolve, which the repository macros need. Result: %superq(wpct_t);
      %let wpct_ok = 0;
    %end;
  %end;

  /* SASAUTOS becomes one flat fileref, WPCTAUTO: the repository utilities first, then every entry
     of the SASAUTOS option the session started with, in order (failure point 7). A quoted entry
     is carried as the path it names. A fileref entry is expanded to its directories in the row
     order of SASHELP.VEXTFL, as util_resolve_sasautos reads them, or from PATHNAME when the view
     has no row for it; the members of a concatenation are paths, so the tokens of a parenthesised
     PATHNAME value are all carried as paths. Each directory is written single-quoted into
     WPCT_SASDIRS, so no macro trigger in an inherited path is resolved by the FILENAME
     statement. WPCT_OK is set back to 1 only by a step that completes with the whole list. */
  %if &wpct_ok = 1 %then %do;
    %let wpct_ok = 0;
    data _null_;
      length _wpct_opt _wpct_src _wpct_inner _wpct_raw _wpct_entry _wpct_tok _wpct_path _wpct_item
             _wpct_dirs _wpct_desc $32767 _wpct_q _wpct_sq _wpct_dq $1 _wpct_ref $8 _wpct_how $32;
      _wpct_sq = byte(39);
      _wpct_dq = byte(34);
      _wpct_dirs = ' ';
      _wpct_over = 0;
      _wpct_k = 0;
      _wpct_opt = getoption('sasautos');
      _wpct_olen = lengthn(_wpct_opt);
      _wpct_opos = 1;
      if _wpct_olen >= 32767 then do;
        put 'ERROR: (WPCT CAPTURE) The SASAUTOS option holds 32767 characters or more, so it cannot be read whole. SASAUTOS is left unchanged.';
        _wpct_over = 1;
      end;
      else if _wpct_olen = 0 then
        put 'NOTE: (WPCT CAPTURE) The SASAUTOS option the session started with is blank, so WPCTAUTO holds only the repository utilities.';

      do while (_wpct_opos <= _wpct_olen and not _wpct_over);
        _wpct_src = _wpct_opt;
        _wpct_slen = _wpct_olen;
        _wpct_pos = _wpct_opos;
        link _wpct_token;
        _wpct_opos = _wpct_pos;
        if _wpct_got then do;
          _wpct_k = _wpct_k + 1;
          _wpct_entry = _wpct_raw;
          _wpct_desc = ' ';
          _wpct_cnt = 0;
          if _wpct_tq then do;
            if not _wpct_closed then
              put 'WARNING: (WPCT CAPTURE) SASAUTOS entry ' _wpct_k 'has no closing quotation mark and is not carried into WPCTAUTO: ' _wpct_entry;
            else if lengthn(_wpct_tok) = 0 then
              put 'NOTE: (WPCT CAPTURE) SASAUTOS entry ' _wpct_k _wpct_entry 'is an empty path and is skipped.';
            else do;
              _wpct_path = _wpct_tok;
              _wpct_how = 'the quoted path';
              link _wpct_carry;
            end;
          end;
          else if not prxmatch('/^[A-Za-z_][A-Za-z0-9_]{0,7}$/', trim(_wpct_tok)) then
            put 'WARNING: (WPCT CAPTURE) SASAUTOS entry ' _wpct_k _wpct_entry 'is neither a quoted path nor a fileref name and is not carried into WPCTAUTO.';
          else do;
            _wpct_ref = upcase(_wpct_tok);
            _wpct_how = 'SASHELP.VEXTFL';
            _wpct_dsid = open('sashelp.vextfl');
            if _wpct_dsid > 0 then do;
              _wpct_fnum = varnum(_wpct_dsid, 'fileref');
              _wpct_xnum = varnum(_wpct_dsid, 'xpath');
              if _wpct_fnum > 0 and _wpct_xnum > 0 then do while (fetch(_wpct_dsid) = 0);
                if upcase(getvarc(_wpct_dsid, _wpct_fnum)) = _wpct_ref then do;
                  _wpct_path = getvarc(_wpct_dsid, _wpct_xnum);
                  link _wpct_carry;
                end;
                if _wpct_over then leave;
              end;
              _wpct_rc = close(_wpct_dsid);
            end;
            else put 'WARNING: (WPCT CAPTURE) SASHELP.VEXTFL cannot be opened, so the fileref ' _wpct_ref 'is resolved through PATHNAME only.';

            if _wpct_cnt = 0 and not _wpct_over then do;
              _wpct_how = 'PATHNAME';
              _wpct_inner = pathname(_wpct_ref, 'F');
              _wpct_inner = left(_wpct_inner);
              if char(_wpct_inner, 1) = '(' then do;
                _wpct_src = _wpct_inner;
                _wpct_slen = lengthn(_wpct_inner);
                _wpct_pos = 1;
                do while (_wpct_pos <= _wpct_slen and not _wpct_over);
                  link _wpct_token;
                  if _wpct_got and (_wpct_closed or not _wpct_tq) then do;
                    _wpct_path = _wpct_tok;
                    link _wpct_carry;
                  end;
                end;
              end;
              else do;
                _wpct_path = strip(_wpct_inner);
                link _wpct_carry;
              end;
            end;
            if _wpct_cnt = 0 and not _wpct_over then
              put 'WARNING: (WPCT CAPTURE) SASAUTOS entry ' _wpct_k 'is the fileref ' _wpct_ref +(-1) ', which names no directory in SASHELP.VEXTFL or through PATHNAME, so it is not carried into WPCTAUTO.';
          end;
          if _wpct_cnt > 0 and not _wpct_over then
            put 'NOTE: (WPCT CAPTURE) SASAUTOS entry ' _wpct_k _wpct_entry 'is carried into WPCTAUTO from ' _wpct_how +(-1) ': ' _wpct_desc;
        end;
      end;

      if not _wpct_over then call symputx('wpct_sasdirs', _wpct_dirs, 'G');
      call symputx('wpct_ok', 1 - _wpct_over, 'G');
      return;

    /* The next token of _WPCT_SRC from position _WPCT_POS: outside quotation marks, blanks,
       commas and parentheses separate tokens; a token that starts with a quotation mark runs to
       the matching closing one, a doubled mark inside it standing for one mark. */
    _wpct_token:
      _wpct_got = 0;
      _wpct_tq = 0;
      _wpct_closed = 0;
      _wpct_tok = ' ';
      _wpct_raw = ' ';
      do while (_wpct_pos <= _wpct_slen);
        if indexc(char(_wpct_src, _wpct_pos), ' ,()') = 0 then leave;
        _wpct_pos = _wpct_pos + 1;
      end;
      if _wpct_pos <= _wpct_slen then do;
        _wpct_got = 1;
        _wpct_beg = _wpct_pos;
        _wpct_q = char(_wpct_src, _wpct_pos);
        if _wpct_q = _wpct_sq or _wpct_q = _wpct_dq then do;
          _wpct_tq = 1;
          _wpct_pos = _wpct_pos + 1;
          do while (_wpct_pos <= _wpct_slen and not _wpct_closed);
            if char(_wpct_src, _wpct_pos) ne _wpct_q then _wpct_pos = _wpct_pos + 1;
            else if _wpct_pos < _wpct_slen and char(_wpct_src, _wpct_pos + 1) = _wpct_q then _wpct_pos = _wpct_pos + 2;
            else do;
              _wpct_closed = 1;
              _wpct_pos = _wpct_pos + 1;
            end;
          end;
          _wpct_raw = substr(_wpct_src, _wpct_beg, _wpct_pos - _wpct_beg);
          if _wpct_closed then _wpct_tok = dequote(_wpct_raw);
        end;
        else do;
          do while (_wpct_pos <= _wpct_slen);
            if indexc(char(_wpct_src, _wpct_pos), ' ,()', _wpct_sq, _wpct_dq) then leave;
            _wpct_pos = _wpct_pos + 1;
          end;
          _wpct_raw = substr(_wpct_src, _wpct_beg, _wpct_pos - _wpct_beg);
          _wpct_tok = _wpct_raw;
        end;
      end;
      return;

    /* Adds _WPCT_PATH, single-quoted, to the list; a list beyond 32767 characters is an error,
       never a truncated list. */
    _wpct_carry:
      if lengthn(_wpct_path) > 0 then do;
        _wpct_ilen = lengthn(_wpct_path) + countc(_wpct_path, _wpct_sq) + 2;
        if lengthn(_wpct_dirs) + 1 + _wpct_ilen > 32767 then do;
          put 'ERROR: (WPCT CAPTURE) The directories of the SASAUTOS option exceed 32767 characters, so WPCTAUTO cannot hold them all. SASAUTOS is left unchanged.';
          _wpct_over = 1;
        end;
        else do;
          _wpct_item = quote(trim(_wpct_path), _wpct_sq);
          _wpct_dirs = catx(' ', _wpct_dirs, _wpct_item);
          _wpct_desc = catx(' ', _wpct_desc, _wpct_item);
          _wpct_cnt = _wpct_cnt + 1;
        end;
      end;
      return;
    run;

    %if &wpct_ok = 1 %then %do;
      filename wpctauto ("&wpct_repo/whitepapers/utilities" &wpct_sasdirs);
      %if &sysfilrc ne 0 %then %do;
        %put ERROR: (WPCT CAPTURE) The FILENAME statement for WPCTAUTO ended with SYSFILRC=&sysfilrc, so SASAUTOS is left unchanged.;
        %let wpct_ok = 0;
      %end;
    %end;
    %else %put ERROR: (WPCT CAPTURE) WPCTAUTO was not set up, so SASAUTOS is left unchanged. See the lines above.;
  %end;

  %if &wpct_ok = 1 %then %do;
    options mautosource sasautos=wpctauto;
    %put NOTE: (WPCT CAPTURE) SASAUTOS=%qsysfunc(getoption(sasautos)) with WPCTAUTO=%qsysfunc(pathname(wpctauto));

    libname wpctcap "&wpct_workdir/capture";
    %if &syslibrc ne 0 %then %do;
      %put ERROR: (WPCT CAPTURE) Cannot assign the capture library &wpct_workdir/capture.;
      %let wpct_ok = 0;
    %end;
  %end;

  /* A rerun starts clean: earlier captures and CSVs are removed before the program runs. Only
     the captures that exist are deleted, and a capture that remains stops the launcher. */
  %if &wpct_ok = 1 %then %do;
    %let wpct_old = ;
    %if %sysfunc(exist(wpctcap.css_anadata)) %then %let wpct_old = &wpct_old css_anadata;
    %if %sysfunc(exist(wpctcap.css_stats)) %then %let wpct_old = &wpct_old css_stats;
    %if %sysfunc(exist(wpctcap.parameter_estimates)) %then %let wpct_old = &wpct_old parameter_estimates;
    %if %length(&wpct_old) > 0 %then %do;
      proc datasets library=wpctcap memtype=data nolist nodetails;
        delete &wpct_old;
      quit;
    %end;
    %if %sysfunc(exist(wpctcap.css_anadata)) or %sysfunc(exist(wpctcap.css_stats)) or %sysfunc(exist(wpctcap.parameter_estimates)) %then %do;
      %put ERROR: (WPCT CAPTURE) Cannot remove the captures of an earlier run from &wpct_workdir/capture, so the program does not run.;
      %let wpct_ok = 0;
    %end;
    %else %do;
      %let wpct_action = clear;
      %wpct_exports
    %end;
  %end;

  /* The staged working copy. Statement keywords that start with a percent sign are built with
     BYTE(37), so no macro statement appears inside this macro as quoted text. */
  %if &wpct_ok = 1 %then %do;
    data _null_;
      length _wpct_line _wpct_text _wpct_new _wpct_out $32767 _wpct_repo _wpct_work $1024;
      retain _wpct_found 0 _wpct_bad 0;
      _wpct_repo = symget('wpct_repo');
      _wpct_work = symget('wpct_workdir');
      infile "&wpct_repo/whitepapers/WPCT/WPCT-F.07.02.sas" lrecl=32767 length=_wpct_reclen end=_wpct_eof;
      file "&wpct_workdir/WPCT-F.07.02_staged.sas" lrecl=32767;
      input;
      _wpct_line = _infile_;
      _wpct_len = _wpct_reclen;
      if _wpct_len > 0 then do;
        if char(_wpct_line, _wpct_len) = '0D'x then _wpct_len = _wpct_len - 1;
      end;
      if _wpct_len > 0 then _wpct_text = strip(substr(_wpct_line, 1, _wpct_len));
      else _wpct_text = ' ';
      _wpct_ind = verify(_wpct_line, ' ') - 1;

      _wpct_hit = 0;
      if _n_ in (85, 126, 280, 308) then do;
        _wpct_found + 1;
        if _n_ = 85 then _wpct_hit = (index(_wpct_text, trim(byte(37) || 'util_access_test_data(advs, local=)')) = 1);
        else if _n_ = 126 then _wpct_hit = (index(_wpct_text, trim(byte(37) || 'let outputs_folder = ;')) = 1);
        else if _n_ = 280 then _wpct_hit = (_wpct_text = 'run; quit;');
        else _wpct_hit = (_wpct_text = (byte(37) || 'end;'));
        if not _wpct_hit then do;
          _wpct_bad + 1;
          putlog 'ERROR: (WPCT CAPTURE) Line ' _n_ 'of WPCT-F.07.02.sas does not hold the expected anchor text. It reads: ' _wpct_text;
        end;
      end;

      if _n_ = 85 and _wpct_hit then do;
        _wpct_new = byte(37) || 'util_access_test_data(advs, local=' || strip(_wpct_repo) || '/data/adam/cdisc/) ;';
        link _wpct_put_new;
      end;
      else if _n_ = 126 and _wpct_hit then do;
        _wpct_new = byte(37) || 'let outputs_folder = ' || strip(_wpct_work) || '/pdf;';
        link _wpct_put_new;
      end;
      else do;
        put _wpct_line $varying32767. _wpct_len;
        if _n_ = 280 and _wpct_hit then do;
          put '              data work._wpct_pe;';
          put '                length PARAMCD $8 ATPTN 8;';
          put '                PARAMCD = "&&paramcd_val&pdx";';
          put '                ATPTN = &&atptn_val&tdx;';
          put '                set &css_pval_ds;';
          put '              run;';
          _wpct_new = byte(37) || 'wpct_append(base=wpctcap.parameter_estimates, data=work._wpct_pe, rc=&syserr)';
          link _wpct_put_new;
        end;
        else if _n_ = 308 and _wpct_hit then do;
          put '            data work._wpct_stats;';
          put '              length PARAMCD $8 ATPTN 8;';
          put '              PARAMCD = "&&paramcd_val&pdx";';
          put '              ATPTN = &&atptn_val&tdx;';
          put '              set css_stats;';
          put '            run;';
          _wpct_new = byte(37) || 'wpct_append(base=wpctcap.css_stats, data=work._wpct_stats, rc=&syserr)';
          link _wpct_put_new;
        end;
      end;

      if _wpct_eof then do;
        if _wpct_found = 4 and _wpct_bad = 0 then do;
          call symputx('wpct_staged', '1', 'G');
          putlog 'NOTE: (WPCT CAPTURE) Wrote the staged copy from ' _n_ 'lines of WPCT-F.07.02.sas, with 4 anchors matched.';
        end;
        else putlog 'ERROR: (WPCT CAPTURE) Staged copy rejected: ' _wpct_found 'of 4 anchors read, ' _wpct_bad 'mismatched.';
      end;
      return;

    _wpct_put_new:
      if _wpct_ind > 0 then _wpct_out = substr(_wpct_line, 1, _wpct_ind) || _wpct_new;
      else _wpct_out = _wpct_new;
      _wpct_olen = lengthn(_wpct_out);
      put _wpct_out $varying32767. _wpct_olen;
      return;
    run;
    %let wpct_rc = &syserr;
    %if &wpct_rc > 4 %then %do;
      %put ERROR: (WPCT CAPTURE) The step that writes the staged copy &wpct_workdir/WPCT-F.07.02_staged.sas ended with SYSERR=&wpct_rc..;
      %let wpct_staged = 0;
    %end;
    %else %if &wpct_staged = 1 and not %sysfunc(fileexist(&wpct_workdir/WPCT-F.07.02_staged.sas)) %then %do;
      %put ERROR: (WPCT CAPTURE) The staged copy &wpct_workdir/WPCT-F.07.02_staged.sas was not written.;
      %let wpct_staged = 0;
    %end;

    %if &wpct_staged = 1 %then %do;
      filename wpctrun "&wpct_workdir/WPCT-F.07.02_staged.sas";
      options nosyntaxcheck;
      ods path (prepend) work.templat(update);
      %let wpct_syscc = &syscc;
      %put NOTE: (WPCT CAPTURE) Running the staged copy. ERROR lines from lines 76 and 82 of the original are expected (failure point 8).;
    %end;
    %else %do;
      %let wpct_ok = 0;
      data _null_;
        length _wpct_fref $8;
        _wpct_fref = '_wpctstg';
        if filename(_wpct_fref, "&wpct_workdir/WPCT-F.07.02_staged.sas") = 0 then do;
          if fexist(_wpct_fref) then _wpct_rc = fdelete(_wpct_fref);
          _wpct_rc = filename(_wpct_fref);
        end;
      run;
    %end;
  %end;
%mend wpct_prepare;


/*** After the staged copy: halt detection, the css_anadata copy, the row-count and read-back
  self-checks, and the CSV exports. ***/
%macro wpct_finish;
  %local wpct_rc wpct_halt;
  %let wpct_rc = &syserr;
  %let wpct_halt = 0;

  %if &wpct_ok = 1 %then %do;
    options &wpct_syntaxcheck;
    ods path (remove) work.templat;

    %if "%sysfunc(getoption(obs))" = "0" %then %do;
      %put ERROR: (WPCT CAPTURE) The program left SAS in syntax-check mode (OBS=0).;
      %let wpct_halt = 1;
    %end;
    %if not %symexist(continue) %then %do;
      %put ERROR: (WPCT CAPTURE) The program did not set its halting symbol CONTINUE.;
      %let wpct_halt = 1;
    %end;
    %else %if %superq(continue) ne 1 %then %do;
      %put ERROR: (WPCT CAPTURE) The program halted: CONTINUE=%superq(continue).;
      %let wpct_halt = 1;
    %end;
    %if &wpct_capfail ne 0 %then %do;
      %put ERROR: (WPCT CAPTURE) A capture step of the staged copy failed. See the ERROR lines of WPCT_APPEND above.;
      %let wpct_halt = 1;
    %end;
    %if &wpct_rc > 4 %then %do;
      %put ERROR: (WPCT CAPTURE) The last step of the program ended with SYSERR=&wpct_rc..;
      %let wpct_halt = 1;
    %end;
    %if not %sysfunc(exist(work.css_anadata)) %then %do;
      %put ERROR: (WPCT CAPTURE) WORK.CSS_ANADATA does not exist.;
      %let wpct_halt = 1;
    %end;
    %if not %sysfunc(exist(wpctcap.css_stats)) %then %do;
      %put ERROR: (WPCT CAPTURE) No css_stats was captured.;
      %let wpct_halt = 1;
    %end;
    %if not %sysfunc(exist(wpctcap.parameter_estimates)) %then %do;
      %put ERROR: (WPCT CAPTURE) No ParameterEstimates table was captured.;
      %let wpct_halt = 1;
    %end;

    %if &wpct_halt = 0 %then %do;
      %put NOTE: (WPCT CAPTURE) The capture checks passed. The staged program ended with SYSCC=&syscc, which includes the expected ERRORs of lines 76 and 82 (failure point 8). SYSCC is set back to &wpct_syscc, its value before the program ran.;
      %let syscc = &wpct_syscc;
      proc copy in=work out=wpctcap memtype=data;
        select css_anadata;
      run;
      %if &syserr > 4 or not %sysfunc(exist(wpctcap.css_anadata)) %then %do;
        %put ERROR: (WPCT CAPTURE) Copying WORK.CSS_ANADATA to the capture library failed.;
        %let wpct_halt = 1;
      %end;
    %end;

    %if &wpct_halt = 0 %then %do;
      options missing='.';
      %let wpct_action = write;
      %wpct_exports
    %end;
    %else %do;
      %put ERROR: (WPCT CAPTURE) No CSV is written.;
      %let wpct_ok = 0;
    %end;
    %if &wpct_ok = 1 and &syscc > 4 %then %do;
      %put ERROR: (WPCT CAPTURE) A SAS step of the launcher itself ended with an ERROR (SYSCC=&syscc), so the capture is not complete. See the ERROR lines above.;
      %let wpct_ok = 0;
    %end;
  %end;

  %if &wpct_printto = 1 %then %do;
    proc printto;
    run;
  %end;
  /* Failure status: every refused or failed run ends with SYSCC 8 or more (batch exit status 2),
     without ending the session. */
  %if &wpct_ok ne 1 %then %do;
    %if &syscc <= 4 %then %let syscc = 8;
  %end;
  %if &wpct_ok = 1 %then
    %put NOTE: (WPCT CAPTURE) Capture finished with SYSCC=&syscc: native copies in &wpct_workdir/capture, CSVs in &wpct_csvdir, log in &wpct_workdir/WPCT-F.07.02_capture.log. Review every WARNING there.;
  %else %if &wpct_printto = 1 %then
    %put ERROR: (WPCT CAPTURE) Capture failed with SYSCC=&syscc. See the ERROR lines in &wpct_workdir/WPCT-F.07.02_capture.log.;
  %else
    %put ERROR: (WPCT CAPTURE) Capture failed with SYSCC=&syscc. See the ERROR lines above.;
%mend wpct_finish;


%wpct_prepare
%include wpctrun / source2;
%wpct_finish
