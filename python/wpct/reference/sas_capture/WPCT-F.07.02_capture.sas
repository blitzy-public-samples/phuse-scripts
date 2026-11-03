/*** WPCT-F.07.02 SAS capture launcher: full-precision reference exports for the Python port

  UNVERIFIED IN SAS until run in a licensed session.

  This program was written without a SAS engine. Nobody has run it, and it is not claimed to work.
  It makes no claim of SAS equivalence. It only captures the SAS results that the Python port
  compares against, in the form python/wpct/reference/sas_exports/README.md and wpct/schemas.py
  specify. See RUNBOOK.md in this folder for the run steps and the comparison commands.

  REQUIREMENTS
    Licensed SAS 9.4 with Base SAS and SAS/STAT, run once by a person in a fresh session.
    Match the historical references where possible: SAS 9.4 TS1M2 with SAS/STAT 13.2.
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
    program returns. The repository files are only read, never written.

  FILES PRODUCED (all below the settings-block directories, never in the repository)
    <work>/WPCT-F.07.02_staged.sas                 the staged working copy
    <work>/WPCT-F.07.02_capture.log and .lst       log and listing, from the redirect onwards
    <work>/capture/css_anadata.sas7bdat            native copies: the authoritative capture
    <work>/capture/css_stats.sas7bdat
    <work>/capture/parameter_estimates.sas7bdat
    <work>/pdf/*.pdf on Windows, <work>/pdf\*.pdf on UNIX    box plots of the program (point 12)
    <csv>/WPCT-F.07.02_css_anadata.csv             the contract CSVs
    <csv>/WPCT-F.07.02_css_stats.csv
    <csv>/WPCT-F.07.02_parameter_estimates.csv

  LIKELY FAILURE POINTS, for the first licensed run
    1. ODS table name and columns. The capture relies on the ODS table ParameterEstimates of
       PROC GLM and its variables Dependent Parameter Estimate Biased StdErr tValue Probt. If this
       SAS/STAT release names them differently, the parameter_estimates export stops with an
       ERROR naming the missing variable.
    2. Macro variable scoping. The inserted DATA steps run inside macro boxplot_each_param_tp and
       resolve its local pdx, tdx and css_pval_ds, and the global paramcd_val<n> and atptn_val<n>
       that util_labels_from_var creates. Check the MPRINT lines of DATA _WPCT_PE and
       DATA _WPCT_STATS: PARAMCD must read DIABP or SYSBP and ATPTN 815 or 816.
    3. Format stripping. The CSV step removes every format (FORMAT _ALL_) and writes each numeric
       with the explicit WPCT_NUMFMT format, so no estimate is rounded and no p-value becomes
       text such as <.0001. Character values are written as stored, without trailing blanks.
    4. E32.17. The SAS 9.4 documentation lists the E format as Ew. only, so whether E32.17 is
       accepted is unverified, and SAS documents formatted values beyond about 15 significant
       digits as possibly inaccurate. A pre-flight check writes test values with WPCT_NUMFMT and
       stops if SAS cannot read them back: set WPCT_NUMFMT to BEST32. then. After the export, a
       read-back check reports per column whether each CSV value reads back exactly as the
       native value. The sas7bdat copies stay exact either way.
    5. CRLF line endings. WPCT-F.07.02.sas has CRLF line endings. On UNIX each record then ends
       with a CR, which the staged-copy step removes.
    6. Anchor mismatch. Lines 85, 126, 280 and 308 are checked. A different revision of the
       program stops the launcher with an ERROR line naming the line.
    7. SASAUTOS. util_resolve_sasautos, used by assert_macro_exist, parses one flat list of
       filerefs and double-quoted paths, and line 76 wraps the current SASAUTOS value in another
       pair of parentheses with an empty "" entry. The launcher therefore sets SASAUTOS to the
       single fileref WPCTAUTO, which concatenates the double-quoted "<repo>/whitepapers/utilities"
       and the directories of the SASAUTOS fileref, so line 76 yields (WPCTAUTO ""). If
       assert_depend reports a missing macro, compare the WPCTAUTO rows of SASHELP.VEXTFL.
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
   16. PROC APPEND runs without FORCE. If the second stratum has a variable of another length,
       the append stops with an ERROR instead of truncating values.
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
  %local path dsid nobs rbobs n i var vnum types putlist header missing rc;
  %let path = &wpct_csvdir/&file;

  %if &wpct_action = clear %then %do;
    data _null_;
      length _wpct_fref $8;
      _wpct_fref = '_wpctcsv';
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
        _wpct_rc = filename(_wpct_fref);
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
          %let putlist = &putlist &var :&wpct_numfmt;
        %end;
        %else %do;
          %let types = &types C;
          %let putlist = &putlist &var;
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

      /* Formats are removed and every numeric is written with the explicit WPCT_NUMFMT format.
         Special missing values are written as the ordinary missing value. */
      data _null_;
        file "&path" dsd encoding="&wpct_encoding" lrecl=32767;
        if _n_ = 1 then put "&header";
        set &data;
        format _all_;
        %do i = 1 %to &n;
          %if %scan(&types, &i) = N %then %do;
            if missing(%scan(&vars, &i, %str( ))) then %scan(&vars, &i, %str( )) = .;
          %end;
        %end;
        put &putlist;
      run;
      %if &syserr > 4 %then %do;
        %put ERROR: (WPCT CAPTURE) Writing &path failed.;
        %let wpct_ok = 0;
      %end;
      %else %put NOTE: (WPCT CAPTURE) Wrote &path from &data..;

      /* Read-back check: SAS reads the CSV again and compares every value with the native copy. */
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


/*** Guards, environment record, autocall setup, capture library and staged copy. On success it
  points fileref WPCTRUN at the staged copy; otherwise WPCTRUN holds one blank line. ***/
%macro wpct_prepare;
  %global wpct_ok wpct_staged wpct_printto wpct_syntaxcheck wpct_sasdirs wpct_action;
  %local wpct_t;
  %let wpct_ok = 0;
  %let wpct_staged = 0;
  %let wpct_printto = 0;
  %let wpct_syntaxcheck = %sysfunc(getoption(syntaxcheck));
  %let wpct_sasdirs = ;
  %let wpct_action = ;

  filename wpctrun temp;
  data _null_;
    file wpctrun;
    put ' ';
  run;

  %if "%sysfunc(getoption(obs))" = "0" %then
    %put ERROR: (WPCT CAPTURE) This session is in syntax-check mode (OBS=0). Start a new SAS session and run the launcher again.;
  %else %do;
    /* Settings: present, usable in the macro calls of the program, absolute, and outside the
       protected parts of the checkout. Paths are written back with / separators and without a
       trailing separator. */
    data _null_;
      length _wpct_name $16 _wpct_value _wpct_clean $1024 _wpct_enc _wpct_fmt _wpct_probe $64
             _wpct_wp _wpct_dt $1024;
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
          _wpct_clean = prxchange('s#(?<=.)/{2,}#/#', -1, trim(_wpct_clean));
          do while (length(_wpct_clean) > 1 and char(_wpct_clean, length(_wpct_clean)) = '/');
            _wpct_clean = substr(_wpct_clean, 1, length(_wpct_clean) - 1);
          end;
          if not prxmatch('#^(/|[A-Za-z]:/)#', _wpct_clean) then do;
            put 'ERROR: (WPCT CAPTURE) Setting ' _wpct_name 'is not an absolute path: ' _wpct_value;
            _wpct_ok = 0;
          end;
          else if index(cats('/', _wpct_clean, '/'), '/./') or index(cats('/', _wpct_clean, '/'), '/../') then do;
            put 'ERROR: (WPCT CAPTURE) Setting ' _wpct_name 'holds a . or .. segment: ' _wpct_value;
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
        if _wpct_n2 = _wpct_n1 or index(_wpct_n2, cats(_wpct_n1, '/')) = 1 then do;
          put 'ERROR: (WPCT CAPTURE) WPCT_WORKDIR must lie outside the repository ' _wpct_c1;
          _wpct_ok = 0;
        end;
        if _wpct_n3 = _wpct_n1 then do;
          put 'ERROR: (WPCT CAPTURE) WPCT_CSVDIR must not be the repository root ' _wpct_c1;
          _wpct_ok = 0;
        end;
        _wpct_wp = cats(_wpct_c1, '/whitepapers');
        _wpct_dt = cats(_wpct_c1, '/data');
        if _wpct_fold then do;
          _wpct_wp = upcase(_wpct_wp);
          _wpct_dt = upcase(_wpct_dt);
        end;
        if _wpct_n2 = _wpct_wp or index(_wpct_n2, cats(_wpct_wp, '/')) = 1 or _wpct_n3 = _wpct_wp or index(_wpct_n3, cats(_wpct_wp, '/')) = 1 then do;
          put 'ERROR: (WPCT CAPTURE) WPCT_WORKDIR and WPCT_CSVDIR must not lie under ' _wpct_wp;
          _wpct_ok = 0;
        end;
        if _wpct_n2 = _wpct_dt or index(_wpct_n2, cats(_wpct_dt, '/')) = 1 or _wpct_n3 = _wpct_dt or index(_wpct_n3, cats(_wpct_dt, '/')) = 1 then do;
          put 'ERROR: (WPCT CAPTURE) WPCT_WORKDIR and WPCT_CSVDIR must not lie under ' _wpct_dt;
          _wpct_ok = 0;
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

  /* Log redirect and the environment record. No manifest file is written. */
  %if &wpct_ok = 1 %then %do;
    %put NOTE: (WPCT CAPTURE) From here on the log goes to &wpct_workdir/WPCT-F.07.02_capture.log.;
    proc printto log="&wpct_workdir/WPCT-F.07.02_capture.log"
                 print="&wpct_workdir/WPCT-F.07.02_capture.lst" new;
    run;
    %let wpct_printto = 1;
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

  /* SASAUTOS becomes one flat fileref: the repository utilities first, then the directories of
     the SASAUTOS fileref (failure point 7). */
  %if &wpct_ok = 1 %then %do;
    data _null_;
      length _wpct_dirs $32767;
      retain _wpct_dirs ' ';
      set sashelp.vextfl (where=(upcase(fileref) = 'SASAUTOS')) end=_wpct_last;
      _wpct_dirs = catx(' ', _wpct_dirs, quote(strip(xpath)));
      if _wpct_last then call symputx('wpct_sasdirs', _wpct_dirs, 'G');
    run;
    filename wpctauto ("&wpct_repo/whitepapers/utilities" &wpct_sasdirs);
    options mautosource sasautos=wpctauto;
    %put NOTE: (WPCT CAPTURE) SASAUTOS=%qsysfunc(getoption(sasautos)) with WPCTAUTO=%qsysfunc(pathname(wpctauto));

    libname wpctcap "&wpct_workdir/capture";
    %if &syslibrc ne 0 %then %do;
      %put ERROR: (WPCT CAPTURE) Cannot assign the capture library &wpct_workdir/capture.;
      %let wpct_ok = 0;
    %end;
  %end;

  /* A rerun starts clean: earlier captures and CSVs are removed before the program runs. */
  %if &wpct_ok = 1 %then %do;
    proc datasets library=wpctcap memtype=data nolist nodetails;
      delete css_anadata css_stats parameter_estimates;
    quit;
    %let wpct_action = clear;
    %wpct_exports
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
          put 'ERROR: (WPCT CAPTURE) Line ' _n_ 'of WPCT-F.07.02.sas does not hold the expected anchor text. It reads: ' _wpct_text;
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
          put '              data _wpct_pe;';
          put '                length PARAMCD $8 ATPTN 8;';
          put '                PARAMCD = "&&paramcd_val&pdx";';
          put '                ATPTN = &&atptn_val&tdx;';
          put '                set &css_pval_ds;';
          put '              run;';
          put '              proc append base=wpctcap.parameter_estimates data=_wpct_pe;';
          put '              run;';
          put '              proc datasets library=work memtype=data nolist nodetails;';
          put '                delete _wpct_pe;';
          put '              quit;';
        end;
        else if _n_ = 308 and _wpct_hit then do;
          put '            data _wpct_stats;';
          put '              length PARAMCD $8 ATPTN 8;';
          put '              PARAMCD = "&&paramcd_val&pdx";';
          put '              ATPTN = &&atptn_val&tdx;';
          put '              set css_stats;';
          put '            run;';
          put '            proc append base=wpctcap.css_stats data=_wpct_stats;';
          put '            run;';
          put '            proc datasets library=work memtype=data nolist nodetails;';
          put '              delete _wpct_stats;';
          put '            quit;';
        end;
      end;

      if _wpct_eof then do;
        if _wpct_found = 4 and _wpct_bad = 0 then do;
          call symputx('wpct_staged', '1', 'G');
          put 'NOTE: (WPCT CAPTURE) Wrote the staged copy from ' _n_ 'lines of WPCT-F.07.02.sas, with 4 anchors matched.';
        end;
        else put 'ERROR: (WPCT CAPTURE) Staged copy rejected: ' _wpct_found 'of 4 anchors read, ' _wpct_bad 'mismatched.';
      end;
      return;

    _wpct_put_new:
      if _wpct_ind > 0 then _wpct_out = substr(_wpct_line, 1, _wpct_ind) || _wpct_new;
      else _wpct_out = _wpct_new;
      _wpct_olen = lengthn(_wpct_out);
      put _wpct_out $varying32767. _wpct_olen;
      return;
    run;

    %if &wpct_staged = 1 %then %do;
      filename wpctrun "&wpct_workdir/WPCT-F.07.02_staged.sas";
      options nosyntaxcheck;
      ods path (prepend) work.templat(update);
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
  %end;

  %if &wpct_printto = 1 %then %do;
    proc printto;
    run;
  %end;
  %if &wpct_ok = 1 %then
    %put NOTE: (WPCT CAPTURE) Capture finished: native copies in &wpct_workdir/capture, CSVs in &wpct_csvdir, log in &wpct_workdir/WPCT-F.07.02_capture.log. Review every WARNING there.;
  %else %if &wpct_printto = 1 %then
    %put ERROR: (WPCT CAPTURE) Capture failed. See the ERROR lines in &wpct_workdir/WPCT-F.07.02_capture.log.;
  %else
    %put ERROR: (WPCT CAPTURE) Capture failed. See the ERROR lines above.;
%mend wpct_finish;


%wpct_prepare
%include wpctrun / source2;
%wpct_finish
