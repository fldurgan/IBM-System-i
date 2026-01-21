       ctl-opt bnddir('U297268GBL/QUSRUTILS') alwnull(*USRCTL) ;
     **  //ctl-opt bnddir('QUSRUTILS/QUSRUTILS') alwnull(*USRCTL) ;
       //*=======================================================================*//
       //*(C) Copyright. . . . :Oshkosh Corporation 2023                      *//
       //*=======================================================================*//
       //*Source File. . . . . :<srclib>/<srcfile>                            *//
       //*Member Name. . . . . :<srcmbr>                                      *//
       //*=======================================================================*//
       //*Format Name. . . . . :<srcmbr>                                      *//
       //*Description. . . . . :<srcmbrd>                                     *//
       //*=======================================================================*//
       //*Created by . . . . . :U297268 (Fredrick Durgan)                     *//
       //*Company. . . . . . . :JLG Industries, Inc.                          *//
       //*Date . . . . . . . . :09/05/2023                                   *//
       //*Project ID . . . . . :Infor ERP IDF Client Extension               *//
       //*Purpose. . . . . . . :Trigger Program for Infor ERP IDF Interface.  *//
       //*                     :Client maintenance interface trigger
       //*                     :native maintenance field updating             *//
       //*=======================================================================*//
       //*JLG Infor Customization Interface                                    *//
       //*VERSION: XA                                                          *//
       //*RELEASE: 03 MOD 10                                                   *//
       //*DATE:    09/05/2023                                                  *//
       //*=======================================================================*//
      /eject
       //*=======================================================================*//
       //*Program Modification Log                                             *//
       //*-----------------------------------------------------------------------*//
       //*SNOW#| DATE   | TECH   |MODIFICATION                           *//
       //*-----------------------------------------------------------------------*//
       //*                                                                       *//
       //*=======================================================================*//
       //*Notes:                                                               *//
       //*11/02/2012 fdurgan  This program will auto generate trigger shell    *//
       //*source for files
       //*to process.  Most processing is performed through the Infor XA ERP   *//
       //*client.  Originally a file was used to input the source lines.       *//
       //*However
       //*program that looks at an array
       //*source.  The template is set up to used replacable variables so you  *//
       //*can place the source where ever you want.                            *//
       //*                                                                     *//
       //*01/28/2011 fdurgan  After testing and evaluation
       //*for V6
       //*format - i.e. date is 7.0 numeric
       //*numeric
       //*be converted to *ISO formats in order to manipulate the fields a     *//
       //*little easier.                                                       *//
       //*                                                                     *//
       //*12/15/2020 fdurgan  Marker identification.                           *//
       //*  %%  Specification Breaks                                           *//
       //*  %I  Infor Required Code                                            *//
       //*  %D  Maintenance Field Code                                         *//
       //*  %K  Transaction Key Code                                           *//
       //*  %T  Table Creation                                                 *//
       //*The Maintenance Field and Transaction Key code can be used without   *//
       //*having an Infor based system.  Not all applications use maintenance  *//
       //*fields, and not all fields are used consistantly.                    *//
       //*=======================================================================*//
      /eject
       //*=======================================================================*//
       //*Files used (native RPG)                                              *//
       //*Files used (native RPG)                                              *//
       
       //*=======================================================================*//
       dcl-f QSRCOUT disk(*EXT) usage(*OUTPUT) rename(QSRCOUT:RSRCOUT)
                                               prefix(o) usropn ;

       //*=======================================================================*//
       dcl-s endfiles char(30)   Inz('------------------------------') ;
       //*=======================================================================*//

       //*=======================================================================*//
       //*Program Procedure Definition                                         *//
       //*=======================================================================*//
       dcl-pr Main  EXTPGM('CRTUSRPGMR')  ;
         Program$     char(10) ;
         SFile$Lib$   char(20) ;
         SSystem      char(6)  ;
         SClass$      char(10) ;
         SIncludeDt   char(4)  ;
         SDateFmt$    char(5)  ;
         SMntPreFx$   char(1)  ;
       end-pr ;
       dcl-pi Main ;
         Program$     char(10) ;
         SFile$Lib$   char(20) ;
         SSystem      char(6)  ;
         SClass$      char(10) ;
         SIncludeDt   char(4)  ;
         SDateFmt$    char(5)  ;
         SMntPreFx$   char(1)  ;
       end-pi ;

       dcl-s xFileLib  char(21) ;
       dcl-s xMbr      char(10) ;

       dcl-ds dsFile$Lib$   len(20) ;
         SFile$     char(10) pos(1) ;
         SLibrary$  char(10) pos(11);
       end-ds ;
       //*=======================================================================*//
       //*Program data structure                                               *//
       //*=======================================================================*//
      /Copy U297268GBL/Q.copybook,QUPSDSFREE
       dcl-c endpgmsts '------------------------------'  ;
     D*
      /eject
       //*=======================================================================*//
       //*Source Array Program Template (see bottom of program for layout)
       //*=======================================================================*//
     D* srcdta           S             96A   CTDATA DIM(1316)
       dcl-s srcdta   char(96) dim(*CTDATA) ;
       dcl-s condta   char(96) dim(*CTDATA) ;
       // dcl-c srcdtaEcnt  %elem(srcdta) ;
       //*=======================================================================*//
       //* Variable table definition loaded when the program is called.          *//
       //* this could be set up in a code file
       //* have a single self contained program that would have all the          *//
       //* necessary components without loading 35 other sub components.         *//
       //* However
       //* copy statements.  I was learning.                                     *//
       //*=======================================================================*//
       dcl-ds vfldref  len(96)   qualified DIM(36) ;
         vfldname      char(20)   pos(1)  ;
         vflddef       char(15)   pos(21) ;
         vfldval       char(20)   pos(36) ;
         vfldlen       zoned(2:0) pos(56) ;
         vfldlal       zoned(2:0) pos(58) ;
         vfldnml       zoned(2:0) pos(60) ;
       end-ds ;
       //*=======================================================================*//
       //* Variables used to move the above table entries to for further process *//
       //*=======================================================================*//
       dcl-s replvarn  char(20) ;                   //* Varible Name
       dcl-s replvarv  char(64) ;                   //* Varible Value
       dcl-s replvarl  int(10:0);                   //* Varible Allowed Length
       dcl-s replvara  int(10:0);                   //* Varible Value Length
       dcl-s replvari  int(10:0);                   //* Varible SRCDTA Length
       dcl-s replvarm  int(10:0);                   //* Varible SRCDTA Length
      /eject
       //*========================================================================*
       //*IBM Command Processor
       //*========================================================================*
       //*------------------------------------------------------------------*
       dcl-pr $command   ExtPgm('QCMDEXC') ;
         *n       char(3000)  Const options(*varsize) ;
         *n       packed(15:5) const ;
       end-pr ;
       dcl-s qcmd     char(3000) ;
     D*------------------------------------------------------------------*
       //*========================================================================*
       //*Get IBM OS Version
       //*========================================================================*
       //*------------------------------------------------------------------*
      */Copy qusrutils/Q.copybook
       //*------------------------------------------------------------------*
       //dcl-pr GenUtl_OSrelease   ;
       //           OSrelease  char(6) ;
       //end-pr ;
       dcl-s OSrelease  char(6) ;
       //*------------------------------------------------------------------*
     D*------------------------------------------------------------------*
     D*========================================================================*
     D*Work Variables
     D*========================================================================*
       dcl-s $Qte      char(1)  inz('''')  ;
       dcl-s Smkey     char(4) ;
       dcl-s WriteLine char(1) ;
     D replstr         s             15s 0
     D replend         s             15s 0
     D replstr@        s             15s 0
     D replend@        s             15s 0
     D varfldi         s             10I 0
     d $spaces         s             40a   inz(*blank)
     D #spc            s             10I 0
     D w#1dta          s             96a
     D w#2dta          s             96a
     D i               s             15s 0
     D srclbfl$        s             21A
     D company$        s             35A   inz('JLG Industries, Inc')
     D copyright$      s             40A   inz('Oshkosh Corporation')
     D system$         s              8A
     D mntpfx$         s              5A
     D projectID$      s             16A
     D tblname$        s              8A
     D srclinect       s             15s 0
     D ExtPgm$         s             17A
     D year$           s              4A
     D ryear$          s              6A
     D tech$           s             10A
     D date$           s             10A
     D rtech$          s             10A
     D class$          s              7A
     D TimeFmt$        s             32A
     D DateFmt$        s             32A
     D PgmNmFmt$       s             11A
     D UsrNmFmt$       s             11A
     D DSpecType$      s             32A
     D CSpecType$      s             32A
     D ExtFileNm$      s             15A
     D sQuote          s              1A   inz('''')
     D semiColon       s              1A   inz(';')
     D #i              s             15s 0
     D #ai             s             15s 0
     D #p1             s             15s 0
     D #p2             s             15s 0
     D #p3             s             15s 0
     D LoopCnt         s             15s 0
     D NullCnt         s             15s 0
     D NullFieldCnt    s             15s 0
     D NullFieldCnt$   s             15A
     D NullCapable     s               n
     D Ok2write        s               n
     D @CrtLib         s               n
     D @CrtSrc         s               n
     D @CrtMbr         s               n
     D LoopCnt$        s             15A
     D ESubrNm$        s              6A
     D CrtFldStuff     s              6A
     D W#EVTM$         s              7A
     D W#FDDT          s              1A
     D W#COMP$         s              8A
     D W#FLDI$         s             24A
     D WaFLDI$         s             24A
     D WbFLDI$         s             24A
     D W#FTYP$         s              6A
     D DblDash         s             66A   inz(*all'=')
      /eject
       //==================================================================*
       //*            Start Main Calc Processing                         *
       //==================================================================*
         exsr pgmdef ;
         exsr pgmdcl ;
         exsr pgminz ;
         exsr pgmopn ;
         exsr pgmprc ;
         exsr pgmcls ;
         exsr pgmend ;

         //*---------------------------------------------------------------*
         //* Put any program definitions here                              *
         //*---------------------------------------------------------------*
         begsr pgmdef ;
         //* get the IBM OS version
         //callp qrtvosver(osrelease) ;
         // osrelease = GenUtl_RtvOSrelease() ;
         //* Non-Executable code (if needed)
         if *InLR = *On and
            *InLR = *Off ;
         endif ;
         endsr ;

         //*---------------------------------------------------------------*
         //* Put any program declarations here                             *
         //*---------------------------------------------------------------*
         begsr pgmdcl ;
         //* Declare default values (could be put in initialize too but
         //*                         needed this here so initialize could
         //*                         reset as needed)
         //* Put the CTDATA to work array because I couldn't get the
         //* CTDATA to work like I thought it should
         osrcseq = 1  ;
         SrcLineCt = 1 ;
         i = SrcLineCt ;
         endsr ;

         //*---------------------------------------------------------------*
         //* Put any program initialization considerations here.           *
         //*---------------------------------------------------------------*
         begsr pgminz ;
         dsfile$lib$ = sfile$lib$ ;
         exsr $CrtWorkF ;
         exsr $CrtSrcF  ;
         //* temporary code
         if ssystem = '*INFOR' ;
           smkey = '*YES' ;
         else ;
           smkey = '*NO ' ;
         endif ;
         exsr $LoadVarFldArr ;
         endsr ;

         //*---------------------------------------------------------------*
         //*---------------------------------------------------------------*
         begsr pgmopn ;
         exsr OpnSrcOut  ;
         endsr ;
         //*---------------------------------------------------------------*
         //*---------------------------------------------------------------*
         begsr pgmprc ;
         Exsr MainProcess ;
         endsr ;
         //*---------------------------------------------------------------*
         //*---------------------------------------------------------------*
         begsr pgmcls ;
         exsr ClsSrcOut    ;
         endsr ;
         //*---------------------------------------------------------------*
         //*---------------------------------------------------------------*
         begsr pgmend ;
         exsr $CopySrc ;
         *InLR = *On ;
         endsr ;
         //*---------------------------------------------------------------*
       /eject
       //*=========================================================
       //*=========================================================
       BegSr MainProcess ;

       DoW SrcLineCt < %elem(srcdta)  ; // 1317 ;
         ok2write = *On ;
         // Even though If/Else would work using Select
         // Statement because if at some point in the future changes
         // are made to template to include comments or something
         // all you have to do is add another When...
           //* Check for Specification line
         if %subst(srcdta(i):1:2) = '%%' ;
           // Process the logic marker - ** is control code to
           // specify that the different spec types
           Select ;
             //* Set program to process H Spec
             When %scan('HSPEC' : srcdta(i))     > 0  ;
               WriteLine = 'H' ;
             //* Set program to process F Spec
             When %scan('FSPEC' : srcdta(i))     > 0  ;
               WriteLine = 'F' ;
             //* Set program to process E Spec
             When %scan('ESPEC' : srcdta(i))     > 0  ;
               WriteLine = 'E' ;
             //* Set program to process D Spec
             When %scan('DSPEC' : srcdta(i))     > 0  ;
               WriteLine = 'D' ;
               Select ;
                   when %scan('PDS' : srcdta(i))                  > 0  ;
                     DSpecType$ = 'PDS'                             ;
                   when %scan('PROGDEF' : srcdta(i))              > 0  ;
                     DSpecType$ = 'PROGDEF'                         ;
                   when %scan('WORK VARs' : srcdta(i))            > 0  ;
                     DSpecType$ = 'WORK VARs'                       ;
                   when %scan('TRANSACTION KEY LIST': srcdta(i))  > 0  ;
                     DSpecType$ = 'TRANSACTION KEY LIST'            ;
                   when %scan('CALLING PGM PROC': srcdta(i))      > 0  ;
                     DSpecType$ = 'CALLING PGM PROC'                ;
                   when %scan('INFOR CALLED PGMs': srcdta(i))     > 0  ;
                     DSpecType$ = 'INFOR CALLED PGMs'               ;
                   when %scan('IBM COMMAND PROCESSOR': srcdta(i)) > 0  ;
                     DSpecType$ = 'IBM COMMAND PROCESSOR'           ;
                   when %scan('OTHER FILES USED': srcdta(i))      > 0  ;
                     DSpecType$ = 'OTHER FILES USED'                ;
                   when %scan('COMMUNICATION AREA': srcdta(i))    > 0  ;
                     DSpecType$ = 'COMMUNICATION AREA'              ;
                   other ;
                     DSpecType$ = *Blank                            ;
                 endsl ;
               //* Set program to process C Spec
               When %scan('CSPEC' : srcdta(i))     > 0  ;
                 WriteLine = 'C' ;
                 Select ;
                   when %scan('MAIN' : srcdta(i))             > 0  ;
                     CSpecType$ = 'MAIN'                            ;
                   when %scan('COMMENTS' : srcdta(i))         > 0  ;
                     CSpecType$ = 'COMMENTS'                        ;
                   when %scan('SUBR PGMOPN' : srcdta(i))      > 0  ;
                     CSpecType$ = 'PGMOPN'                          ;
                   when %scan('SUBR PGMDEF' : srcdta(i))      > 0  ;
                     CSpecType$ = 'PGMDEF'                          ;
                   when %scan('SUBR PGMDCL' : srcdta(i))      > 0  ;
                     CSpecType$ = 'PGMDCL'                          ;
                   when %scan('SUBR PGMINZ' : srcdta(i))      > 0  ;
                     CSpecType$ = 'PGMINZ'                          ;
                   when %scan('SUBR GETCALLER' : srcdta(i))   > 0  ;
                     CSpecType$ = 'GETCALLER'                       ;
                   when %scan('SUBR PGMPRC' : srcdta(i))      > 0  ;
                     CSpecType$ = 'PGMPRC'                          ;
                   when %scan('SUBR PGMCLS': srcdta(i))        > 0  ;
                     CSpecType$ = 'PGMCLS'                          ;
                   when %scan('SUBR PGMEMD': srcdta(i))        > 0  ;
                     CSpecType$ = 'PGMEND'                          ;
                   when %scan('SUBR TBLXXX': srcdta(i))        > 0  ;
                     CSpecType$ = 'TBLXXX'                          ;
                   when %scan('SUBR CLOFIL': srcdta(i))           > 0  ;
                     CSpecType$ = 'CLOFIL'                          ;
                   when %scan('SUBR CLSTS1': srcdta(i))           > 0  ;
                     CSpecType$ = 'CLSTS1'                          ;
                   when %scan('SUBR CLOPGM': srcdta(i))           > 0  ;
                     CSpecType$ = 'CLOPGM'                          ;
                   when %scan('SUBR PGMABT': srcdta(i))           > 0  ;
                     CSpecType$ = 'PGMABT'                          ;
                   when %scan('SUBR UPDSTG': srcdta(i))           > 0  ;
                     CSpecType$ = 'UPDSTG'                          ;
                   when %scan('SUBR SXMSG0': srcdta(i))           > 0  ;
                     CSpecType$ = 'SXMSG0'                          ;
                   when %scan('SUBR OPNTS1': srcdta(i))           > 0  ;
                     CSpecType$ = 'OPNTS1'                          ;
                   when %scan('SUBR SVRMH0': srcdta(i))           > 0  ;
                     CSpecType$ = 'SVRMH0'                          ;
                   when %scan('SUBR SVMHG0': srcdta(i))           > 0  ;
                     CSpecType$ = 'SVMHG0'                          ;
                   when %scan('SUBR SXEMP1': srcdta(i))           > 0  ;
                     CSpecType$ = 'SXEMP1'                          ;
                   when %scan('SUBR XVRMH0': srcdta(i))           > 0  ;
                     CSpecType$ = 'XVRMH0'                          ;
                   when %scan('SUBR RLYMSG': srcdta(i))           > 0  ;
                     CSpecType$ = 'RLYMSG'                          ;
                   when %scan('SUBR PGMERR': srcdta(i))           > 0  ;
                     CSpecType$ = 'PGMERR'                          ;
                   when %scan('SUBR XXEMP1': srcdta(i))           > 0  ;
                     CSpecType$ = 'XXEMP1'                          ;
                   when %scan('SUBR XVMHG0': srcdta(i))           > 0  ;
                     CSpecType$ = 'XVMHG0'                          ;
                   when %scan('SUBR SNDESC': srcdta(i))           > 0  ;
                     CSpecType$ = 'SNDESC'                          ;
                   when %scan('SUBR XXMSG0': srcdta(i))           > 0  ;
                     CSpecType$ = 'XXMSG0'                          ;
                   when %scan('SUBR RTVTS1': srcdta(i))           > 0  ;
                     CSpecType$ = 'RTVTS1'                          ;
                   when %scan('SUBR CRTKEY': srcdta(i))           > 0  ;
                     CSpecType$ = 'CRTKEY'                          ;
                   when %scan('SUBR CUSTOM': srcdta(i))           > 0  ;
                     CSpecType$ = 'CUSTOM'                          ;
                   other ;
                     CSpecType$ = *Blank                            ;
                 endsl ;
               //* Set program to process Specs that are undefined
               Other     ;
             endsl ;
           //* Normal source line occurred
         else     ;
           //* Check if we are going to write the line.  If not
           //* set the write indicator off so no additional processing
           //* will occur.
           if (%subst(srcdta(i):1:2) = '%I' and Ssystem =  '*STD  ') or
              (%subst(srcdta(i):1:2) = '%I' and Ssystem =  '*DDL  ') or
              (%subst(srcdta(i):1:2) = '%S' and Ssystem =  '*INFOR') or
              (%subst(srcdta(i):1:2) = '%T' and Ssystem <> '*DDL  ') or
              (%subst(srcdta(i):1:2) = '%D' and SIncludeDt = '*NO ') or
              (%subst(srcdta(i):1:2) = '%K' and Smkey = '*NO ')      or
               %scan('TAGB' : srcdta(i)) > 0                          or
               %scan('TAGE' : srcdta(i)) > 0  ;
             ok2write = *Off ;
           endif ;
           //* if it is okay to write this line
           if ok2write = *On ;
             //* Check if this is Free indicator
             if %scan('@@Free' : srcdta(i)) > 0  ;
               //* replstr should always start at 1
               replstr  = %scan('@@Free' : srcdta(i))  ;
               srcdta(i) = %REPLACE('**Free':srcdta(i):replstr:6) ;
             else ;
               replstr = 1 ;
               replend  = 1 ;
               //* Replace any variables in the source line
               //* Must use a character that does not allow
               //* a math function - i.e. - < or > cannot
               //* be used because it can be used for
               //* math operations.
               dow (%scan('{' : srcdta(i):replstr) > 0  and
                    %scan('}' : srcdta(i):replstr) > 0)  ;
                 //* Find the start point of variable
                 replstr = %scan('{' : srcdta(i))  ;
                 replstr@ = replstr - 1  ;
                 //* Find the end point of variable
                 replend  = %scan('}' : srcdta(i))  ;
                 replend@ = replend + 1  ;
                 replvari = (replend - replstr)+1 ;
                 replvarn = %subst(srcdta(i):replstr:replvari) ;
                 replvarv = *blank ;
                 if %len(%trim(replvarn)) > 0 ;
                   //* We found a variable so process it
                   exsr SetVarVal ;
                   replstr = replend ;
                 endif ;
               enddo ;
             endif ;
           endif ;
           //* Set up output to write the formatted line
           //osrcdat = 0  ;
           osrcdta = srcdta(i) ;
           if %subst(osrcdta:1:2) <> '**' ;
             %subst(osrcdta:1:2) = '  ' ;
           endif ;
           if ok2write = *On ;
             write rsrcout ;
           endif ;
           osrcseq = osrcseq + 1   ;
         endif ;
         SrcLineCt = SrcLineCt + 1 ;
         i = SrcLineCt ;
         //if srclinect > 1545 ;
         //dsply srclinect ;
         //dsply %subst(srcdta(i):1:32) ;
         //endif ;
       Enddo ;

       Endsr ;
       /eject
       //*=========================================================
       //*=========================================================
       BegSr OpnFlds ;
       Endsr ;
       /eject
       //*=========================================================
       //*=========================================================
       BegSr ClsFlds ;
       Endsr ;
       /eject
       //*=========================================================
       //*=========================================================
       BegSr OpnSrcOut  ;
       Open qsrcout  ;
       Endsr ;
       /eject
       //*=========================================================
       //*=========================================================
       BegSr ClsSrcOut   ;
       Close qsrcout ;
       Endsr ;
       /eject
       //*=========================================================
       //*=========================================================
       BegSr SetVarVal  ;
       //* Retrieve the variable information
       #ai = %lookup(%trim(replvarn):vfldref(*).vfldname) ;
       if #ai > 0 ;
         //* found replvarn
         replvarl = vfldref(#ai).vfldlen ;           //* Variable Allowed Length (*info only*)
         replvarv = %trim(vfldref(#ai).vfldval) ;    //* Variable Value
         replvara = vfldref(#ai).vfldlen ;           //* Variable Value Actual Length
         replvari = %len(%trim(replvarn)) ;          //* Variable SRCDTA Length
       else ;
         dsply replvarn ;
       endif ;
       //* Fix the line to include the actual variable value
       select ;
         when replvara < replvari ;
           w#1dta = %subst(srcdta(i):1:replstr-1) ;             //* Get the first part that is good
           w#2dta = %subst(srcdta(i):replend@
                :%len(srcdta(i))-replend@)     ;
           %subst(w#1dta:replstr:replvari) = %trim(replvarv) ;  //* Add the current variable value
           #spc = replvara-replvari ;
           #p1 = %len(%trimr(w#1dta))+1 ;
           #p2 = %len(w#1dta)-%len(%trim(w#1dta));
           #p3 = %len(srcdta(i))-replend ;
           %subst(w#1dta:%len(%trimr(w#1dta))+1                  //* Get the next part
                :%len(w#1dta)-%len(%trimr(w#1dta))) =
           %subst(srcdta(i):replend@:%len(srcdta(i))-replend@) ;
           srcdta(i) = w#1dta ;
           if %scan('*//' : srcdta(i)) > 0  ;
             #p1 = %scan('*//' : srcdta(i)) ;
             w#1dta = %subst(srcdta(i):1:#p1-2) ;             //* Get the first part that is good
             w#2dta = %subst(srcdta(i):#p1-1
                  :%len(srcdta(i))-#p1-2)     ;
             srcdta(i) =
               %subst(w#1dta:1:#p1-2)              //* Get the first part that is good
              +%subst($spaces:1:#spc*-1)+
               %trimr(w#2dta) ;
           endif ;
         when replvara > replvari ;
           // This is actually an easy step
           w#1dta = %subst(srcdta(i):1:replstr-1) ;             //* Get the first part that is good
           w#2dta = %subst(srcdta(i):replend@
                :%len(srcdta(i))-replend@)     ;
           %subst(w#1dta:replstr:replvara) = %trim(replvarv) ;  //* Add the current variable value
           #p1 = %len(%trimr(w#1dta))+1 ;
           #p2 = %len(w#1dta)-%len(%trim(w#1dta));
           #p3 = %len(srcdta(i))-replend ;
           #spc = replvara-replvari ;
           %subst(w#1dta:%len(%trimr(w#1dta))+1                 //* Get the next part
                :%len(w#1dta)-%len(%trimr(w#1dta))) =
             %subst(srcdta(i):replend@:%len(srcdta(i))-replend) ;
           srcdta(i) = w#1dta ;
           if %scan('*//' : srcdta(i)) > 0  ;
             #p1 = %scan('*//' : srcdta(i)) ;
             w#1dta = %subst(srcdta(i):1:#p1-2) ;             //* Get the first part that is good
             w#2dta = %subst(srcdta(i):#p1-1
                  :%len(srcdta(i))-(#p1-2))     ;
             #p3 = %len(srcdta(i))-(#p1-2)      ;
             srcdta(i) =  %subst(w#1dta:1:#p1-2) ;            //* Get the first part that is good
             %subst(srcdta(i):(#p1-1)-#spc) =  %trimr(w#2dta) ;
           endif ;
           //w#srcl(1) = %subst(srcdta(i):1:replstr-1) ;
           //w#srcl(replstr) = %trim(replvarn) ;
           //#spc = replvarl-replvari ;
           //srcdta(i) =
           //  %REPLACE(%trim(replvarv)+%subst($spaces:1:#spc):
           //                       srcdta(i):replstr:replvarl) ;
         other ;
           // aahhhhh
           replvari = %len(%trim(replvarv)) ;
           srcdta(i) = %REPLACE(%trim(replvarv):srcdta(i):replstr:replvari) ;
       endsl ;
       Endsr ;
       /eject
       //*=============================================================*//
       //* Create the temporary work source file
       //*=============================================================*//
       BegSr $CrtWorkF ;
       //*-------------------------------------------------------------*//
       //* Check if source work file exists and create if it does not  *//
       @CrtSrc = *Off ;
       qcmd = 'CHKOBJ OBJ(QTEMP/QSRCOUT) OBJTYPE(*FILE)'  ;
       monitor;
       $command(%trim(qcmd):%size(%trim(qCmd)));
         on-error;
           select ;
             //* Source File not found
             when $pexty = 'CPF' and $pexno = '9801' ;
               //* set flag to add new member
               @CrtSrc = *On ;
             other ;
               //* not sure what they could be yet
               SrcLineCt = 99999 ;
             endsl ;
         // endif ;
         endmon;
       //*-------------------------------------------------------------*//
       if @CrtSrc ;
         qcmd = 'CRTSRCPF FILE(QTEMP/QSRCOUT) RCDLEN(132)' + ' ' +
                'TEXT(' + $Qte + 'User Program Source' + $Qte + ')' ;
         monitor;
         //$command(qcmd:%size(qCmd));
           $command(%trim(qcmd):%size(%trim(qCmd)));
           on-error;
             SrcLineCt = 99999 ;
         endmon;
       endif ;
       //*-------------------------------------------------------------*//
       //*-------------------------------------------------------------*//
       //* check if program source member already exists
       qcmd = 'CHKOBJ OBJ(QTEMP/QSRCOUT) OBJTYPE(*FILE)' + ' ' +
              'MBR(' + %trim(Program$) + ')' ;
       @CrtMbr = *Off ;
       monitor;
         $command(qcmd:%size(qCmd));
         on-error;
           select ;
            //* Member not found
            when $pexty = 'CPF' and $pexno = '9815' ;
              //* set flag to add new member
              @CrtMbr = *On ;
            other ;
            //* not sure what they could be yet
             SrcLineCt = 99999 ;
           endsl ;
       endmon;
       //*-------------------------------------------------------------*//
       if @CrtMbr ;
         qcmd = 'ADDPFM FILE(QTEMP/QSRCOUT)' + ' ' +
                'MBR(' + %trim(Program$) + ')' ;
         monitor;
           $command(%trim(qcmd):%size(%trim(qCmd)));
         //  $command(qcmd:%size(qCmd));
           on-error;
             SrcLineCt = 99999 ;
         endmon;
       else ;
         //qcmd = CLRPFM FILE(QTEMP/QSRCOUT) MBR($MEMBER)
         qcmd = 'CLRPFM  FILE(QTEMP/QSRCOUT)' + ' ' +
                'MBR(' + %trim(Program$) + ')' ;
         monitor;
           $command(%trim(qcmd):%size(%trim(qCmd)));
        // $command(qcmd:%size(qCmd));
           on-error;
             SrcLineCt = 99999 ;
         endmon;
       endif ;
       //*-------------------------------------------------------------*//
       //*-------------------------------------------------------------*//
       //* Set the source file                                         *//
       // qcmd = OVRDBF FILE(QSRCOUT) TOFILE($LIBRARY/$FILE) MBR($MEMBER)
       qcmd = 'OVRDBF FILE(QSRCOUT) TOFILE(QTEMP/QSRCOUT)' + ' ' +
              'MBR(' + %trim(Program$) + ')' ;
       //* Create the override for the input file
       monitor;
       //$command(qcmd:%size(qCmd));
         $command(%trim(qcmd):%size(%trim(qCmd)));
         on-error;
           SrcLineCt = 99999 ;
       endmon;
       //*-------------------------------------------------------------*//
       Endsr ;
       //*=============================================================*//
       //*=============================================================*//
       BegSr $CrtSrcF ;
       //*-------------------------------------------------------------*//
       //* Check if source library exists and create if it does not    *//
       // qcmd = CHKOBJ OBJ(QSYS/&SFILELB) OBJTYPE(*LIB)
       qcmd = 'CHKOBJ OBJ(QSYS/' +
               %trim(Slibrary$) + ') ' +
              'OBJTYPE(*LIB)'             ;
       @CrtLib = *Off ;
       monitor;
       //$command(qcmd:%size(qCmd));
         $command(%trim(qcmd):%size(%trim(qCmd)));
         on-error;
           select ;
             //* Library Not found
             when $pexty = 'CPF' and $pexno = '9801' ;
               @CrtLib = *On ;
               //* set flag to add new member
             other ;
               //* not sure what they could be yet
               SrcLineCt = 99999 ;
             endsl ;
       endmon;
       //*-------------------------------------------------------------*//
       if @CrtLib ;
         // qcmd = CRTLIB LIB(&SFILELB) TEXT('User Program Library')
         qcmd = 'CRTLIB LIB(' + %trim(Slibrary$) + ')'  + ' ' +
                'TEXT(' + $Qte + 'User Program Library' + $Qte + ')' ;
         monitor;
         //  $command(qcmd:%size(qCmd));
           $command(%trim(qcmd):%size(%trim(qCmd)));
           on-error;
             SrcLineCt = 99999 ;
         endmon;
       endif ;
       //*-------------------------------------------------------------*//
       //* Check if source file exists and create if it does not       *//
       //qcmd = CHKOBJ OBJ($LIBRARY/$FILE) OBJTYPE(*FILE)
       qcmd = 'CHKOBJ  OBJ(' +
               %trim(Slibrary$) + '/' + %trim(Sfile$) + ')' + ' ' +
              'OBJTYPE(*FILE)' ;
       @CrtSrc = *Off ;
       monitor;
         $command(qcmd:%size(qCmd));
         on-error;
           select ;
             //* Source File not found
             when $pexty = 'CPF' and $pexno = '9801' ;
               //* set flag to add new member
                 @CrtSrc = *On ;
             other ;
               //* not sure what they could be yet
               SrcLineCt = 99999 ;
           endsl ;
       endmon;
       //*-------------------------------------------------------------*//
       if @CrtSrc ;
         //qcmd = CRTSRCPF FILE($LIBRARY/$FILE) RCDLEN(132) TEXT('User Program Source')
         qcmd = 'CRTSRCPF  (' +
                 %trim(Slibrary$) + '/' + %trim(Sfile$) + ')' + ' ' +
                'RCDLEN(132)' + ' ' +
                'TEXT(' + $Qte + 'User Program Source' + $Qte + ')' ;
         monitor;
           $command(qcmd:%size(qCmd));
           on-error;
             SrcLineCt = 99999 ;
         endmon;
       endif ;
       //*-------------------------------------------------------------*//
       //* check if program source member already exists               *//
       //qcmd = CHKOBJ OBJ($CRTPGM/TSRCOUT) OBJTYPE(*FILE) MBR(MYMEMBER)
       qcmd = 'CHKOBJ OBJ(' +
               %trim(Slibrary$) + '/' + %trim(Sfile$) + ')' + ' ' +
              'OBJTYPE(*FILE)' + ' ' + 'MBR(' + %trim(Program$) + ')' ;
       @CrtMbr = *Off ;
       monitor;
         $command(qcmd:%size(qCmd));
         on-error;
           select ;
             //* Member not found
             when $pexty = 'CPF' and $pexno = '9815' ;
               //* set flag to add new member
               @CrtMbr = *On ;
             other ;
               //* not sure what they could be yet
               SrcLineCt = 99999 ;
           endsl ;
       endmon;
       //*-------------------------------------------------------------*//
       if @CrtMbr ;
         //qcmd = ADDPFM FILE($CRTPGM/TSRCOUT) MBR(MYMEMBER)
         qcmd = 'ADDPFM  FILE(' +
                 %trim(Slibrary$) + '/' + %trim(Sfile$) + ')' + ' ' +
                'MBR(' + %trim(Program$) + ')' ;
         monitor;
           $command(qcmd:%size(qCmd));
           on-error;
             SrcLineCt = 99999 ;
         endmon;
       else ;
         //qcmd = CLRPFM FILE($LIBRARY/$FILE) MBR($MEMBER)
         qcmd = 'CLRPFM  FILE(' +
                 %trim(Slibrary$) + '/' + %trim(Sfile$) + ')' + ' ' +
                'MBR(' + %trim(Program$) + ')' ;
         monitor;
           $command(qcmd:%size(qCmd));
           on-error;
             SrcLineCt = 99999 ;
         endmon;
       endif ;
       //*-------------------------------------------------------------*//
       Endsr ;
       //*=============================================================*//
       //*=============================================================*//
       BegSr $CopySrc ;
       //* CHGPFM FILE(&SFILELB/&SFILENM) MBR(&MBR) +
       //*    SRCTYPE(SQLRPGLE) TEXT('User program - ' || &MBR)
       qcmd = 'CPYF FROMFILE(QTEMP/QSRCOUT) TOFILE(' +
              %trim(Slibrary$) + '/' + %trim(Sfile$) + ')' + ' ' +
              'FROMMBR(' + %trim(Program$) + ')' + ' ' +
              'TOMBR(' + %trim(Program$) + ')'   + ' ' +
              'MBROPT(*REPLACE) FMTOPT(*NOCHK)' ;
       monitor;
         $command(qcmd:%size(qCmd));
         on-error;
       endmon;
       QCMD = 'CHGPFM FILE(' +
              %trim(Slibrary$) + '/' + %trim(Sfile$) + ')' + ' ' +
              'MBR(' + %trim(Program$) + ')' + ' ' +
              'SRCTYPE(SQLRPGLE)' + ' ' +
              'TEXT(' + $Qte + 'User Program - ' +
               %trim(Program$) +  $Qte + ')' ;
       monitor;
         $command(qcmd:%size(qCmd));
         on-error;
       endmon;
       // CHGPFM FILE(TEST) MBR(TEST) SRCTYPE(RPGLE)
       Endsr ;
       /eject
       //*=========================================================
       //*=========================================================
       BegSr $LoadVarFldArr ;
       //* Set up the array information
       //*------------------------------------------------
       varfldi = 1                                       ;
       vfldref(varfldi).vfldname = '{orel}'              ;
       vfldref(varfldi).vfldlal  = 06                    ;
       vfldref(varfldi).vflddef  = 'OSrelease'           ;
       vfldref(varfldi).vfldval  = OSrelease             ;
       vfldref(varfldi).vfldlen  =
                    %len(%trim(vfldref(varfldi).vfldval));
       //*------------------------------------------------
       varfldi = 2                                       ;
       vfldref(varfldi).vfldname = '{mmddyyyy}'          ;
       vfldref(varfldi).vfldlal  = 10                    ;
       vfldref(varfldi).vflddef  = '%char(%date():*usa/)';
       vfldref(varfldi).vfldval  = %char(%date():*usa/)  ;
       vfldref(varfldi).vfldlen  =
                    %len(%trim(vfldref(varfldi).vfldval));
       //*------------------------------------------------
       varfldi = 3                                       ;
       vfldref(varfldi).vfldname = '{copyright_name}'    ;
       vfldref(varfldi).vfldlal  = 40                    ;
       vfldref(varfldi).vflddef  = 'Copyright$'          ;
       vfldref(varfldi).vfldval  = copyright$            ;
       vfldref(varfldi).vfldlen  =
                    %len(%trim(vfldref(varfldi).vfldval));
       //*------------------------------------------------
       varfldi = 4                                       ;
       vfldref(varfldi).vfldname = '{year}'              ;
       vfldref(varfldi).vfldlal  = 04                    ;
       vfldref(varfldi).vflddef  =
                        '%char(%subdt(%date(): *years ))';
       vfldref(varfldi).vfldval  =
                          %char(%subdt(%date(): *years ));
       vfldref(varfldi).vfldlen  =
                    %len(%trim(vfldref(varfldi).vfldval));
       //*------------------------------------------------
       varfldi = 5                                       ;
       vfldref(varfldi).vfldname = '{srclib}'            ;
       vfldref(varfldi).vfldlal  = 10                    ;
       vfldref(varfldi).vflddef  = 'Slibrary$'           ;
       vfldref(varfldi).vfldval  = %trim(Slibrary$)      ;
       vfldref(varfldi).vfldlen  =
                    %len(%trim(vfldref(varfldi).vfldval));
       //*------------------------------------------------
       varfldi = 6                                       ;
       vfldref(varfldi).vfldname = '{srcfile}'           ;
       vfldref(varfldi).vfldlal  = 10                    ;
       vfldref(varfldi).vflddef  = 'Sfile$'              ;
       vfldref(varfldi).vfldval  = %trim(Sfile$)         ;
       vfldref(varfldi).vfldlen  =
                    %len(%trim(vfldref(varfldi).vfldval));
       //*------------------------------------------------
       varfldi = 7                                       ;
       vfldref(varfldi).vfldname = '{srcmbr}'            ;
       vfldref(varfldi).vfldlal  = 10                    ;
       vfldref(varfldi).vflddef  = 'Program$'            ;
       vfldref(varfldi).vfldval  = %trim(Program$)       ;
       vfldref(varfldi).vfldlen  =
                    %len(%trim(vfldref(varfldi).vfldval));
       //*------------------------------------------------
       varfldi = 8                                       ;
       vfldref(varfldi).vfldname = '{srcmbrd}'           ;
       vfldref(varfldi).vfldlal  = 40                    ;
       vfldref(varfldi).vflddef  = 'Text w/Program$'     ;
       vfldref(varfldi).vfldval  =
                      'User Program - ' + %trim(Program$);
       vfldref(varfldi).vfldlen  =
                    %len(%trim(vfldref(varfldi).vfldval));
       //*------------------------------------------------
       varfldi = 9                                       ;
       vfldref(varfldi).vfldname = '{tech}'              ;
       vfldref(varfldi).vfldlal  = 10                    ;
       vfldref(varfldi).vflddef  = '$pcuid'              ;
       vfldref(varfldi).vfldval  = %trim($pcuid)         ;
       vfldref(varfldi).vfldlen  =
                    %len(%trim(vfldref(varfldi).vfldval));
       //*------------------------------------------------
       varfldi = 10                                      ;
       vfldref(varfldi).vfldname = '{company_name}'      ;
       vfldref(varfldi).vfldlal  = 30                    ;
       vfldref(varfldi).vflddef  = 'company$'            ;
       vfldref(varfldi).vfldval  = %trim(company$)       ;
       vfldref(varfldi).vfldlen  =
                    %len(%trim(vfldref(varfldi).vfldval));
       //*------------------------------------------------
       varfldi = 11                                      ;
       vfldref(varfldi).vfldname = '{date}'              ;
       vfldref(varfldi).vfldlal  = 10                    ;
       vfldref(varfldi).vflddef  = '%char(%date():*usa/)';
       vfldref(varfldi).vfldval  =
                              %trim(%char(%date():*usa/));
       vfldref(varfldi).vfldlen  =
                    %len(%trim(vfldref(varfldi).vfldval));
       //*------------------------------------------------
       varfldi = 12                                      ;
       vfldref(varfldi).vfldname = '{userpgm}'           ;
       vfldref(varfldi).vfldlal  = 10                    ;
       vfldref(varfldi).vflddef  = 'Program$'            ;
       vfldref(varfldi).vfldval  = %trim(Program$)       ;
       vfldref(varfldi).vfldlen  =
                    %len(%trim(vfldref(varfldi).vfldval));
       //*------------------------------------------------
       varfldi = 13                                      ;
       if projectID$ = *blank ;
         projectID$ = '** Project ID **' ;
       endif ;
       vfldref(varfldi).vfldname = '{project_id}'        ;
       vfldref(varfldi).vfldlal  = 32                    ;
       vfldref(varfldi).vflddef  = 'projectID$'          ;
       vfldref(varfldi).vfldval  = %trim(projectID$)     ;
       vfldref(varfldi).vfldlen  =
                    %len(%trim(vfldref(varfldi).vfldval));
       //*------------------------------------------------
       varfldi = 14                                      ;
       vfldref(varfldi).vfldname = '{extpgm}'            ;
       vfldref(varfldi).vfldlal  = 10                    ;
       vfldref(varfldi).vflddef  = 'Program$'            ;
       vfldref(varfldi).vfldval  = %trim(Program$)       ;
       vfldref(varfldi).vfldlen  =
                    %len(%trim(vfldref(varfldi).vfldval));
       //*------------------------------------------------
       varfldi = 15                                      ;
       vfldref(varfldi).vfldname = '{system}'            ;
       vfldref(varfldi).vfldlal  = 10                    ;
       vfldref(varfldi).vflddef  = 'Ssystem'             ;
       vfldref(varfldi).vfldval  = %trim(Ssystem)        ;
       vfldref(varfldi).vfldlen  =
                    %len(%trim(vfldref(varfldi).vfldval));
       //*------------------------------------------------
       varfldi = 16                                      ;
       vfldref(varfldi).vfldname = '{class}'             ;
       vfldref(varfldi).vfldlal  = 10                    ;
       vfldref(varfldi).vflddef  = 'sclass$'             ;
       if sclass$ = *blank                               ;
       vfldref(varfldi).vfldval  = '*None'               ;
       else                                              ;
       vfldref(varfldi).vfldval  = sclass$               ;
       endif                                             ;
       vfldref(varfldi).vfldlen  =
                    %len(%trim(vfldref(varfldi).vfldval));
       //*------------------------------------------------
       varfldi = 17                                      ;
       vfldref(varfldi).vfldname = '{mt}'                ;
       vfldref(varfldi).vfldlal  = 04                    ;
       vfldref(varfldi).vflddef  = 'sincludedt'          ;
       vfldref(varfldi).vfldval  = %trim(sincludedt)     ;
       vfldref(varfldi).vfldlen  =
                    %len(%trim(vfldref(varfldi).vfldval));
       //*------------------------------------------------
       varfldi = 18                                      ;
       vfldref(varfldi).vfldname = '{fmt}'               ;
       vfldref(varfldi).vfldlal  = 05                    ;
       vfldref(varfldi).vflddef  = 'sdatefmt$'           ;
       vfldref(varfldi).vfldval  = %trim(sdatefmt$)      ;
       vfldref(varfldi).vfldlen  =
                    %len(%trim(vfldref(varfldi).vfldval));
       //*------------------------------------------------
       varfldi = 19                                      ;
       vfldref(varfldi).vfldname = '{pfx}'               ;
       vfldref(varfldi).vfldlal  = 01                    ;
       vfldref(varfldi).vflddef  = 'SMntPreFx$'          ;
       vfldref(varfldi).vfldval  = %trim(SMntPreFx$)     ;
       vfldref(varfldi).vfldlen  =
                    %len(%trim(vfldref(varfldi).vfldval));
       //*------------------------------------------------
       varfldi = 20                                      ;
       vfldref(varfldi).vfldname = '{*}'                 ;
       vfldref(varfldi).vfldlal  = 01                    ;
       vfldref(varfldi).vflddef  = 'SMntPreFx$'          ;
       vfldref(varfldi).vfldval  = %trim(SMntPreFx$)     ;
       vfldref(varfldi).vfldlen  =
                    %len(%trim(vfldref(varfldi).vfldval));
       //*------------------------------------------------
       varfldi = 21                                      ;
       vfldref(varfldi).vfldname = '{tblview}'           ;
       vfldref(varfldi).vfldlal  = 10                    ;
       vfldref(varfldi).vflddef  = 'Program$+V1'         ;
       vfldref(varfldi).vfldval  = %trim(Program$)+'V1'  ;
       vfldref(varfldi).vfldlen  =
                    %len(%trim(vfldref(varfldi).vfldval));
       //*------------------------------------------------
       varfldi = 22                                      ;
       vfldref(varfldi).vfldname = '{tblname}'           ;
       vfldref(varfldi).vfldlal  = 10                    ;
       vfldref(varfldi).vflddef  = 'Program$'            ;
       vfldref(varfldi).vfldval  = %trim(Program$)       ;
       vfldref(varfldi).vfldlen  =
                    %len(%trim(vfldref(varfldi).vfldval));
       //*------------------------------------------------
       varfldi = 23                                      ;
       vfldref(varfldi).vfldname = '{fmtname}'           ;
       vfldref(varfldi).vfldlal  = 10                    ;
       vfldref(varfldi).vflddef  = 'R + Program$'        ;
       vfldref(varfldi).vfldval  = 'R'+ %trim(Program$)  ;
       vfldref(varfldi).vfldlen  =
                    %len(%trim(vfldref(varfldi).vfldval));
       //*------------------------------------------------
       Endsr ;
**CTDATA srcdta  (Formatted Trigger Program Template)
%% HSPEC
@@Free
       //*=======================================================================*//
       //*Autogenerated source program                                        *//
       //*OS Release           :{orel}                                        *//
       //*Date                 :{mmddyyyy}                                   *//
       //*=======================================================================*//
       //*(C) Copyright. . . . :{copyright_name}  {year}                      *//
       //*=======================================================================*//
       //*Source File. . . . . :{srclib}/{srcfile}                            *//
       //*Member Name. . . . . :{srcmbr}                                      *//
       //*=======================================================================*//
       //*Format Name. . . . . :{srcmbr}                                      *//
       //*Description. . . . . :{srcmbrd}                                     *//
       //*=======================================================================*//
       //*Created by . . . . . :{tech}                                        *//
       //*Company. . . . . . . :{company_name}                                *//
       //*Date . . . . . . . . :{date}                                        *//
       //*Project ID . . . . . :{project_id}                                  *//
       //*Purpose. . . . . . . :Autogen Custom Program                        *//
       //*=======================================================================*//
       //*System:     {system}                                                *//
       //*Class:      {class}                                                 *//
       //*Maint Flds: {mt}                                                     *//
       //*Date Format:{fmt}                                                    *//
       //*Prefix:     {pfx}                                                    *//
       //*=======================================================================*//
      /eject
       //*=======================================================================*//
       //*Program Modification Log                                             *//
       //*-----------------------------------------------------------------------*//
       //*ZSDP#| DATE   | TECH   |MODIFICATION                           *//
       //*-----------------------------------------------------------------------*//
       //* RRRRR  mm/dd/yyyy technician xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx *//
       //*                                                                       *//
       //*=======================================================================*//
       //*START USER PROTECTED SECTION  - HSPEC 0001.000 / *USR                *//
       //* Any user modification history to be defined here                      *//
       //*FINISH USER PROTECTED SECTION - HSPEC 0001.000                       *//
       //*=======================================================================*//
       //*Notes:                                                               *//
       //*                                                                     *//
%D     //*For Maintenance fields
%D     //*insert and change info. Makes programming a little easier.  When     *//
%D     //*creating maintenance fields
%D     //*                                                                     *//
%D     //*For Y2K file Fields:                                                 *//
%D     //* *INSUS  Create User     10A      *UPDUS  Change User     10A        *//
%D     //* *INSPG  Create Program  10A      *UPDPG  Change Program  10A        *//
%D     //* *INSDT  Create Date      7S0     *UPDDT  Change Date      7S0       *//
%D     //* *INSTM  Create Time      6S0     *UPDTM  Change Time      6S0       *//
%D     //*                                                                     *//
%D     //*For ISO file Fields:                                                 *//
%D     //* *INSUS  Create User     10A      *UPDUS  Change User     10A        *//
%D     //* *INSPG  Create Program  10A      *UPDPG  Change Program  10A        *//
%D     //* *INSDT  Create Date       D      *UPDDT  Change Date       D        *//
%D     //* *INSTM  Create Time       T      *UPDTM  Change Time       T        *//
%D     //*                                                                     *//
%D     //* NOTE - The '*' will be replaced by a single letter.  However
%D     //*        default will be the letter 'M'.  We ran into an issue using  *//
%D     //*        multiple files having the same field name.                   *//
       //*=======================================================================*//
      /eject
%% FSPEC
       //*=======================================================================*//
       //*Files used (native RPG)                                              *//
       //*=======================================================================*//
%I     dcl-f TRNSTSL1 disk(*ext) usage(*input) keyed USROPN ;
%I     //*=======================================================================*//
%S     //*NONE
%S     //*=======================================================================*//
       //*START USER PROTECTED SECTION  - FSPEC 0001.000 / *USR                *//
       //*++TAGB0001                                                           *//
       //* Any native files to be defined here                                   *//
       //*++TAGE0001                                                           *//
       //*FINISH USER PROTECTED SECTION - FSPEC 0001.000                       *//
       //*=======================================================================*//
      /eject
%% DSPEC PROGDEF
       //*=======================================================================*//
       //*Program Procedure Definition
       //*=======================================================================*//
       dcl-pr Main           EXTPGM('{extpgm}')  ;
         //*=====================================================================*//
         //*START USER PROTECTED SECTION  - DSPEC 0002.000 / *USR              *//
         //*++TAGB0002                                                         *//
         //* Any passed parameters go here                                       *//
         //*++TAGE0002                                                         *//
         //*FINISH USER PROTECTED SECTION - DSPEC 0002.000                     *//
         //*=====================================================================*//
       end-pr ;
       dcl-pi Main                                      ;
         //*=====================================================================*//
         //*START USER PROTECTED SECTION  - DSPEC 0003.000 / *USR              *//
         //*++TAGB0003                                                         *//
         //* Any passed parameters go here (must match the PR definition)        *//
         //*++TAGE0003                                                         *//
         //*FINISH USER PROTECTED SECTION - DSPEC 0003.000                     *//
         //*=====================================================================*//
       end-pi ;
%% DSPEC PDS
      /Copy qusrutils/Q.copybook
%% DSPEC WORK VARs
       //*=======================================================================*//
       //*Program work variables
       //*=======================================================================*//
       dcl-s $Qte      char(1)  inz('''')  ;
%I     dcl-s #I        zoned(5:0) ;
%I     dcl-s W#ERED    char(1)    ;
       dcl-s W#SBTG    char(10)   ;
%I     dcl-s W#WA10    char(10)   ;
%I     dcl-s W#WNED    char(1)    ;
%I     dcl-s W#OTS1    char(1)    ;
       dcl-s W#PGOP    char(  1)   ;     //  Program Open Flag
%I     dcl-s W#CPGM    char( 10)   ;
%I     dcl-s W#TYAC    char(  8)   ;
%I     dcl-s W#MDRD    char(  8)   ;
%I     dcl-s W#TSTK    char( 10)   ;
%I     dcl-s W#PFPR    char(  8)   ;
%I     dcl-s W#SHDN    char(  8)   ;
%I     dcl-s W#TRID    char( 10)   ;
%I     dcl-s W#MSID    char(  7)   ;
%I     dcl-s W#MSDT    char(256)   ;
%I     dcl-s W#RCFD    char(  8)   ;
%I     dcl-s W#BLRQ    char(  8)   ;
%I     dcl-s W#KYGN    char(  8)   ;
%I     dcl-s W#MNTY    char( 10)   ;
%I     dcl-s W#OBCL    char( 10)   ;
%I     dcl-s W#KYDT    char(  1)   ;
%I     dcl-s W#KYST    char(256)   ;
%I     dcl-s W#KYNU    zoned(30:9) ;
%I     dcl-s W#FDNM    char(  6)   ;
%I     dcl-s W#FDST    char(256)   ;
%I     dcl-s W#FDSL    zoned(5:0)  ;
%I     dcl-s W#FDSM    char(  8)   ;
%I     dcl-s W#FDNU    zoned(30:9) ;
%I     dcl-s W#FDDT    char(  1)   ;
%I     dcl-s W#FDFA    char(256)   ;
%I     dcl-s W#FDFB    zoned( 3:0) ;
%I     dcl-s W#FDRA    char(256)   ;
%I     dcl-s W#FDOP    char(  1)   ;
%I     dcl-s W#FDOV    zoned(30:9) ;
%I     dcl-s W#RSCD    char( 10)   ;
%I     dcl-s W1MSID    char(  7)   ;
%I     dcl-s W#MSFN    char( 10)   ;
%I     dcl-s W#MSCL    char(  8)   ;
%I     dcl-s W#MSFL    char( 65)   ;
%I     dcl-s W#ERROR   char(  1)   ;
%I     dcl-s W#STATUS  zoned(30:9) ;
%I     dcl-s W#CLIENT  char(  1)   ;
%I     dcl-s libl char(11) dim(250) ;
%I     dcl-s liblidx int(10:0)     ;
%I     dcl-s liblcnt int(10:0)     ;
%T     dcl-s dsqlerr   char( 24)   ;
%T     dcl-s dlogerr   char( 64)   ;
       //*=======================================================================*//
       //*START USER PROTECTED SECTION  - DSPEC 0004.000 / *USR                *//
       //*++TAGB0004                                                           *//
       //* Any user defined fields to be defined here                            *//
       //*++TAGE0004                                                           *//
       //*FINISH USER PROTECTED SECTION - DSPEC 0004.000                       *//
       //*=======================================================================*//
%K     //* Transaction Key Generation  ---------------- from
%K     dcl-ds w#tsds  len(32)    ;
%K       W#TRKEY       char(32)                        ;
%K       w#TUKEY       char(32)    overlay(W#TRKEY)    ;
%K       w#TRXID       char( 6)    overlay(w#trkey)    ; // Trx ID Code
%K       w#tsRKEYN     zoned(20:0) overlay(w#trkey:7)  ;
%K       w#TRXTS       char(20)    overlay(w#trkey:7)  ; // Trx Timestamp
%K       w#DTKEY       zoned( 8:0) overlay(w#trkey:7)  ; // Trx Date
%K       w#TMKEY       zoned(12:0) overlay(w#trkey:14) ; // Trx Time
%K       W#JBKEY       zoned( 6:0) overlay(w#tRKEY:27) ; // Trx Job
%K     //w#TRXCT       zoned(10:0) overlay(w#trkey:23) ; // Trx Counter
%K     end-ds ;
%K
%K     dcl-s w#SQLTS timestamp ;                           // SQL Timestamp
%K     //* Transaction Key Generation  ---------------- thru
%% DSPEC TRANSACTION KEY LIST
%I     //*=======================================================================*//
%I     //*Define Transaction Key List
%I     //*=======================================================================*//
%I     dcl-ds K$1TS1   likerec(TRNSTSR1 : *key) ;
%I     dcl-s  K#TSTK   char(10)                 ;
%I
%I     /copy qusrutils/q.copybook
%I
%% DSPEC CALLING PGM PROC
%I     //*=======================================================================*//
%I     //*Get the Calling program information
%I     //*=======================================================================*//
%I     dcl-pr GetCaller Extpgm('QWVRCSTK') ;
%I        *n   char(2000)       ;
%I        *n   int(10:0)        ;
%I        *n   char(8)    CONST ;
%I        *n   char(56)         ;
%I        *n   char(8)    CONST ;
%I        *n   char(15)         ;
%I     end-pr ;
%I
%I     dcl-ds Var   len(2000) ;
%I        BytAvl    int(10:0) ;
%I        BytRtn    int(10:0) ;
%I        Entries   int(10:0) ;
%I        Offset    int(10:0) ;
%I     end-ds ;
%I
%I     dcl-s VarLen    int(10:0) Inz(%size(Var)) ;
%I     dcl-s ApiErr    char(15) ;
%I     dcl-s Doffset   int(10:0) ;
%I
%I     dcl-ds JobIdInf ;
%I      JIDQName       char(26)  Inz('*')     ;
%I      JIDIntID       char(16)               ;
%I      JIDRes3        char(2)   Inz(*loval)  ;
%I      JIDThreadInd   int(10:0) Inz(1)       ;
%I      JIDThread      char(8)   Inz(*loval)  ;
%I     end-ds ;
%I
%I     dcl-ds Entry   len(256)  ;
%I         EntryLen   int(10:0) ;
%I         PgmNam     char(10)  pos(25) ;
%I         PgmLib     char(10)  pos(35) ;
%I     end-ds ;
%I
%I    /eject
%% DSPEC INFOR CALLED PGMs
%I     //*=======================================================================*//
%I     //*Infor called programs
%I     //*=======================================================================*//
%I     dcl-pr PSVRMH0R   ExtPgm('PSVRMH0R') ;
%I       *n    char(8) ;
%I       *n    char(8) ;
%I       *n    char(10);
%I       *n    char(8) ;
%I       *n    char(7) ;
%I     end-pr ;
%I
%I     //* Passed Parms to PSVRMH0R ------
%I     dcl-s  P$PFPR   char(8) ;
%I     dcl-s  P$SHDN   char(8) ;
%I     dcl-s  P$TRID   char(10);
%I     dcl-s  P$MTHS   char(8) ;
%I     dcl-s  P$MSID   char(7) ;
%I     //*-----------------------------------------------------------------------*//
%I     //*-----------------------------------------------------------------------*//
%I     dcl-pr PSXRTM1C ExtPgm('PSXRTM1C') ;
%I      *n        char(7)              ;
%I      *n        char(10)             ;
%I      *n        char(10)             ;
%I      *n        char(256)            ;
%I      *n        char(512)            ;
%I      *n        char(3000)           ;
%I     end-pr ;
%I
%I     //* Passed Parms to PSXRTM1C ------
%I     dcl-s RTMSID     char(7)              ;     // Message ID
%I     dcl-s RTMFNM     char(10)             ;     // Message File
%I     dcl-s RTMFLB     char(10)             ;     // Message File Library
%I     dcl-s RTMSDT     char(256)            ;     // Message Data
%I     //* For retrieving messages - PSXRTM1C
%I     dcl-ds RTMSG     len(512)   end-ds;
%I     dcl-ds RTSCLV    len(3000)  end-ds;
%I     //*-----------------------------------------------------------------------*//
%I     //*-----------------------------------------------------------------------*//
%I     dcl-pr PSXSPM1C  ExtPgm('PSXSPM1C') ;
%I        *n     char(7)               ;           // Message ID
%I        *n     char(10)              ;           // Message File
%I        *n     char(256)             ;           // Message data
%I        *n     char(5)               ;           // PGMQ relation
%I        *n     char(10)              ;           // PGMQ name
%I     end-pr ;
%I     //* Passed Parms to PSXSPM1C ------
%I     dcl-s  SMMSID    char(7)               ;    // Message ID
%I     dcl-s  SMMFNM    char(10)              ;    // Message file
%I     dcl-s  SMMSDT    char(256)             ;    // Message data
%I     dcl-s  SMPQRL    char(5)               ;    // PGMQ relation
%I     dcl-s  SMPQNM    char(10)              ;    // PGMQ name
%I     //*-----------------------------------------------------------------------*//
%I     //*-----------------------------------------------------------------------*//
%I     dcl-pr PSXEMP1R   ExtPgm('PSXEMP1R') ;
%I       *n      char(8)    ;
%I       *n      char(8)    ;
%I       *n      char(10)   ;
%I       *n      char(7)    ;
%I       *n      char(256)  ;
%I       *n      char(10)   ;
%I       *n      char(8)    ;
%I       *n      char(65)   ;
%I       *n      char(7)    ;
%I     end-pr ;
%I     //* Passed Parms to PSXSPM1C ------
%I     //*dcl-s P$PFPR         char(8)    ;
%I     //*dcl-s P$SHDN         char(8)    ;
%I     dcl-s P$TSTK         char(10)   ;
%I     dcl-s P1MSID         char(7)    ;
%I     dcl-s P$MSDT         char(256)  ;
%I     dcl-s P$MSFN         char(10)   ;
%I     dcl-s P$MSCL         char(8)    ;
%I     dcl-s P$MSFL         char(65)   ;
%I     //*dcl-s P$MSID         char(7)    ;
%I     //*-----------------------------------------------------------------------*//
%I     //*-----------------------------------------------------------------------*//
%I     dcl-pr PSVMHG0R ExtPgm('PSVMHG0R') ;
%I       *n    char(8)                    ;
%I       *n    char(8)                    ;
%I       *n    char(10)                   ;
%I       *n    char(7)                    ;
%I     end-pr ;
%I     //* Passed Parms to PSXSPM1C ------
%I     //* dcl-s P$PFPR      char(8)      ;
%I     //* dcl-s P$SHDN      char(8)      ;
%I     //* dcl-s P$TSTK      char(10)     ;
%I     //* dcl-s P$MSID      char(7)      ;
%I     //*-----------------------------------------------------------------------*//
%I     //*-----------------------------------------------------------------------*//
%I     dcl-pr PSXSPM4C ExtPgm('PSXSPM4C') ;
%I       *n    char(7)                    ;        // Message ID
%I       *n    char(10)                   ;        // Message file
%I       *n    char(7)                    ;        // Message type
%I       *n    char(256)                  ;        // Message date
%I       *n    char(5)                    ;        // Relationship
%I       *n    char(10)                   ;        // Prog queue
%I     end-pr ;
%I     //* Passed Parms to PSXSPM1C ------
%I     //*dcl-s P$MSID   char(7)            ;        // Message ID
%I     //*dcl-s P$MSFN   char(10)           ;        // Message file
%I     dcl-s P$MSTY    char(7)            ;        // Message type
%I     //*dcl-s P$MSDT   char(256)          ;        // Message date
%I     dcl-s P$PGRL    char(5)            ;        // Relationship
%I     dcl-s P$PGQM    char(10)           ;        // Prog queue
%I     //*-----------------------------------------------------------------------*//
%I
%I     //*-----------------------------------------------------------------------*//
%I     dcl-pr PSXMSG0R   ExtPgm('PSXMSG0R') ;
%I         *n char(8)                       ;
%I         *n char(8)                       ;
%I         *n char(8)                       ;
%I         *n char(8)                       ;
%I         *n char(10)                      ;
%I         *n char(10)                      ;
%I         *n char(1)                       ;
%I         *n char(256)                     ;
%I         *n zoned(30:9)                   ;
%I         *n char(6)                       ;
%I         *n char(256)                     ;
%I         *n zoned(5:0)                    ;
%I         *n char(8)                       ;
%I         *n zoned(30:9)                   ;
%I         *n char(1)                       ;
%I         *n char(256)                     ;
%I         *n zoned(3:0)                    ;
%I         *n char(256)                     ;
%I         *n char(1)                       ;
%I         *n zoned(30:9)                   ;
%I         *n char(10)                      ;
%I         *n char(7)                       ;
%I     end-pr ;
%I     //* Passed Parms to PSXSPM1C ------
%I     //dcl-s P$PFPR    char(8)     ;
%I     //dcl-s P$SHDN    char(8)   ;
%I     dcl-s P$BLRQ    char(8)     ;
%I     dcl-s P$KYGN    char(8)     ;
%I     dcl-s P$MNTY    char(10)    ;
%I     dcl-s P$OBCL    char(10)    ;
%I     dcl-s P$KYDT    char(1)     ;
%I     dcl-s P$KYST    char(256)   ;
%I     dcl-s P$KYNU    zoned(30:9) ;
%I     dcl-s P$FDNM    char(6)     ;
%I     dcl-s P$FDST    char(256)   ;
%I     dcl-s P$FDSL    zoned(5:0)  ;
%I     dcl-s P$FDSM    char(8)     ;
%I     dcl-s P$FDNU    zoned(30:9) ;
%I     dcl-s P$FDDT    char(1)     ;
%I     dcl-s P$FDFA    char(256)   ;
%I     dcl-s P$FDFB    zoned(3:0)  ;
%I     dcl-s P$FDRA    char(256)   ;
%I     dcl-s P$FDOP    char(1)     ;
%I     dcl-s P$FDOV    zoned(30:9) ;
%I     dcl-s P$RSCD    char(10)    ;
%I     //dcl-s P$MSID    char(7)     ;
%I     //*-----------------------------------------------------------------------*//
%I    /Eject
%% DSPEC IBM COMMAND PROCESSOR
       //*=======================================================================*//
       //*IBM Command Processor
       //*=======================================================================*//
       //*-----------------------------------------------------------------------*//
       dcl-pr QCmdExc    ExtPgm('QCMDEXC') ;
             *n   char(3000)   Options(*Varsize) Const  ;
             *n   packed(15:5) Const                    ;
       end-pr ;
       dcl-s Cmd  char(3000)   ;
       //*-----------------------------------------------------------------------*//
%% DSPEC OTHER FILES USED
       //*=======================================================================*//
       //*other files used (usually for SQL definitions)
       //*=======================================================================*//
       //dcl-ds dsfile1   extname(filename) prefix(f1) ;
%% DSPEC COMMUNICATION AREA
%I     //*=======================================================================*//
%I     //*Outside calling program communication areas (data area)
%I     //*=======================================================================*//
%I     DCL-DS p#cmda DTAARA(*AUTO : *USRCTL : 'ZTRIGCOMM') ;
%I        P#CLID char(10)     ;
%I        P#TSTK char(10)     ;
%I        P#Fill char(1004)   ;
%I     end-ds ;
%I     dcl-ds W1SLCL len(3000) end-ds;
%I
%% CSPEC MAIN
      /eject
       //*=======================================================================*//
       //*Mainline Comments                                                    *//
       //*=======================================================================*//
%I     //* Valid Processing communications:
%I     //*   P#CLID = '*COMMON '          //* Server Generated
%I     //*   P#CLID = '*CLTPRC '          //* Client Generated
%I     //*   P#CLID = '*USRPRC '          //* Other User Generated
%I
       //*=======================================================================*//
       //*Main Program Processing Logic                                        *//
       //*=======================================================================*//
       //* Perform program declaration considerations.
       Exsr PgmDef ;
       //* Perform program declaration considerations.
       Exsr PgmDcl ;
       //* Perform program initialization.
       Exsr PgmInz ;
       //* Perform program open considerations.
       Exsr PgmOpn ;
       //* Perform the process.
       Exsr PGMPRC ;
       //* Perform program close considerations.
       Exsr PGMCLS ;
       //* Perform program end considerations.
       Exsr PGMEND ;
      /eject
%% CSPEC COMMENTS
       //*=======================================================================*//
       //*Subroutine usage summary.                                            *//
       //*=======================================================================*//
       //*                                                                       *//
       //* PGMDEF - Program definitions.                                         *//
       //* PGMDCL - Program declarations.                                        *//
       //* PGMINZ - Program initialization.                                      *//
       //* PGMOPN - Program open considerations.                                 *//
%I     //*   GetCallr - Get Program Caller Name                                  *//
       //* PGMPRC - Program processing logic.                                    *//
       //* PGMCLS - Program close considerations.                                *//
       //* PGMEND - Program end considerations.                                  *//
%I     //* PGMABT - Program abort logic.                                         *//
%I     //* PGMERR - Send error message to current programs message queue         *//
       //*                                                                       *//
       //* CLOFIL - Close open files.                                            *//
       //* CLOPGM - Close open programs.                                         *//
       //*                                                                       *//
       //* CLSxxn - Close open files subroutines.                                *//
%I     //*   CLSTS1 - Close the TRNSTSL1 file.                                   *//
%I     //*                                                                       *//
%K     //* CRTKEY - Create a unique key for transactions.                        *//
%K     //*                                                                       *//
       //* OPNxxn - Open files subroutines.                                      *//
%I     //*   OPNTS1 - Open the TRNSTSL1 file.                                    *//
       //*                                                                       *//
%I     //* RLYMSG - Relay program messages                                       *//
%I     //* RTVTS1 - Retrieve unique TRNSTSL1 file record.                        *//
%I     //* SNDESC - Send escape message to caller.                               *//
%I     //*                                                                       *//
%I     //* Safffn - Shut down open programs.                                     *//
%I     //*   SVMHG0 - Shutdown program - PSVMHG0R                                *//
%I     //*   SVRMH0 - Shutdown program - PSVRMH0R                                *//
%I     //*   SXEMP1 - Shutdown program - PSXEMP1R                                *//
%I     //*   SXMSG0 - Shutdown program - PSXMSG0R                                *//
%I     //*                                                                       *//
%I     //* Xafffn - Execute programs.                                            *//
%I     //*   XVMHG0 - Execute program - PSVMHG0R                                 *//
%I     //*   XVRMH0 - Execute program - PSVRMH0C                                 *//
%I     //*   XXEMP1 - Execute program - PSXEMP1R                                 *//
%I     //*   XXMSG0 - Execute program - PSXMSG0R                                 *//
%I     //*                                                                       *//
       //*=======================================================================*//
      /eject
%% CSPEC SUBR PGMDEF
       //*=======================================================================*//
       //*PgmDef - Program Definition considerations.                          *//
       //*=======================================================================*//
       BegSR PgmDef ;
       W#SBTG = 'PGMDEF01' ;
       EndSr ;
      /eject
%% CSPEC SUBR PGMDCL
       //==================================================================*
       //*PgmDef - Program Declarations considerations.                  *
       //==================================================================*
       BegSR PgmDcl ;
       W#SBTG = 'PGMDCL01' ;
       EndSr ;
      /eject
%% CSPEC SUBR PGMINZ
       //==================================================================*
       //*PgmInz - Program Initialize Considerations.                    *
       //==================================================================*
       BegSr PgmInz ;
       W#SBTG = 'PGMINZ01';
       *InLR = *Off ;
%I     W#ERED = *Off ;
%I     W#WA10 = *Blanks ;
%I     W#WNED = *Off ;
%I     If W#OTS1 = *Blank ;
%I       W#OTS1 = *Off ;
%I     EndIf;
%I
%I     libl = getlibl(llusrlibl) ;
%I     liblidx = %lookup('AMALIBB':libl) ;
%I     if liblidx > 0 ;
%I       //* Do nothing
%I     else ;
%I       liblcnt = addlible('AMALIBB':'*FIRST') ;
%I     endif ;
%I     libl = getlibl(llusrlibl) ;
%I     liblidx = %lookup('AMFLIB3':libl) ;
%I     if liblidx > 0 ;
%I       //* Do nothing
%I     else ;
%I       liblcnt = addlible('AMFLIB3':'*FIRST') ;
%I     endif ;
%I     libl = getlibl(llusrlibl) ;
%I     liblidx = %lookup('QGPL':libl) ;
%I     if liblidx > 0 ;
%I       //* Do nothing
%I     else ;
%I       liblcnt = addlible('QGPL':'*FIRST') ;
%I     endif ;
%I
       EndSr ;
      /eject
%% CSPEC SUBR PGMOPN
       //==================================================================*
       //*PgmOpn - Program open considerations.                          *
       //==================================================================*
       BegSR PgmOpn ;
       W#SBTG = 'PGMOPN01' ;
       //* Set the program open flag
       W#PGOP = *On ;
       EndSr ;
      /eject
%% CSPEC SUBR GETCALLER
%I     //==================================================================*
%I     //*GetCaller - Get Program Name that fired this trigger           *
%I     //==================================================================*
%I     BegSR GetCallr ;
%I     W#SBTG = 'GETCALLER' ;
%I
%I     //* Let's get the calling program that called this trigger
%I     W#CLIENT = *Off ;
%I     if P#CLID <> '*CLTPRC '   ;                 //* Not Client Generated
%I       GetCaller(Var:VarLen:'CSTK0100':JobIdInf:'JIDF0100':ApiErr);
%I       For #i = 1 TO Entries ;
%I         Entry = %subst(Var:Offset + 1) ;
%I         if  pgmnam <> $PPGNM and
%I             pgmlib <> 'QSYS'           ;
%I           leave                        ;
%I         endif                          ;
%I         Offset = Offset + EntryLen     ;
%I       Endfor                           ;
%I       //* Check if we should convert name to generic
%I       Select ;
%I         // WRKDBF was used
%I         when %trim(PgmNam) = 'DBCADDRC' or
%I              %trim(PgmNam) = 'DBRWRKF2'   ;
%I           PgmNam = 'WRKDBF'               ;
%I         // CPYF was used
%I         when %trim(PgmNam) = 'QUOMAIN' or
%I              %trim(PgmNam) = 'QUOCPP'     ;
%I           PgmNam = 'CPYF'                 ;
%I         // DFU was used
%I         when %trim(%subst(PgmNam:1:5)) = 'QDZTD'    ;
%I           PgmNam = 'STRDFU#5'             ;
%I         // Client Code file maintenance
%I         // needed if a file is a code file
%I         // Interactive SQL was used
%I         when %trim(PgmNam) = 'QSQISE'     ;
%I           PgmNam = 'STRSQL'               ;
%I         when %trim(PgmNam) = 'QZDASOINIT'    ;
%I           PgmNam = '{class}   ' ; //*Change to the correct EXT
%I           W#CLIENT = *On ;
%I       EndSl ;
%I       if PgmNam = *Blank ;
%I         pgmnam = $PPGNM ;
%I       endif ;
%I       W#CPGM = PgmNam ;
%I     else ;
%I       //* Client program was used ;
%I       Select ;
%I         //* Change the following to fit client name
%I         When TrgBuffer.Event = Insert ;
%I           W#CPGM = '{class}CRT' ;  //*<-- check this
%I         When TrgBuffer.Event = Delete ;
%I           W#CPGM = '{class}DLT' ;  //*<-- check this
%I         When TrgBuffer.Event = Update ;
%I           W#CPGM = '{class}CHG' ;  //*<-- check this
%I         Other ;
%I           //* Read
%I           W#CPGM = 'Read Only ' ;
%I       endsl ;
%I     Endif ;
%I
%I     EndSr ;
%I    /eject
%% CSPEC SUBR PGMPRC
       //==================================================================*
       //*PgmPrc - Program processing logic.                             *
       //==================================================================*
       BegSr PgmPrc ;
       W#SBTG = 'PGMPRC01' ;
%T     exsr TblDlt ;       //* Delete Old Table (make sure you rename old table)
%T     exsr TblCrt ;       //* Create the New Table
%T     exsr TblTxt ;       //* Create the Field Texts
%T     exsr TblLbl ;       //* Create the Field Labels
%T     exsr TblVwx ;       //* Create the Table Views and Indexes
%T
%I     //* Get the default user message file.
%I     W#MSFN = 'BAUSRMSG' ;              //* Set the Message file
%I
%I     IN(E) P#CMDA ;                     //* Get the communication area
%I     If %error() ;                      //* If communication area does
%I       P#CLID = '*COMMON ' ;            //* not exist set for common
%I     EndIf ;                            //* processing.
%I     Exsr GetCallr ;
%I
%I     //* Initialize work variables.
%I     W#ERED = *Off ;
%I     W#WNED = *Off ;
%I
       //*START USER PROTECTED SECTION  - CSPEC 0005.000 / *USR
       //*++TAGB0005
       //* Put user logic here
       //*++TAGE0005
       //*FINISH USER PROTECTED SECTION - CSPEC 0005.000 / *USR
       EndSr ;
      /eject
%% CSPEC SUBR PGMCLS
       //==================================================================*
       //*PGMCLS - Program close.                                        *
       //==================================================================*
       BegSr PGMCLS ;
       W#SBTG = 'PGMCLS01' ;
       //* Set the program open flag to be off
       W#PGOP = *Off ;
       //* Close files.
       Exsr CLOFIL ;
       //* Close program.
       Exsr CLOPGM ;
       EndSr ;
      /eject
%% CSPEC SUBR CLOFIL
       //==================================================================*
       //*CLOFIL - Close open files.                                     *
       //==================================================================*
       BegSr CLOFIL ;
       W#SBTG = 'CLOFIL01' ;
%I     Exsr CLSTS1 ;
%I
       EndSr ;
      /eject
%% CSPEC SUBR PGMEND
       //==================================================================*
       //*PGMEND - Program end.                                          *
       //==================================================================*
       BegSr PGMEND ;
       W#SBTG = 'PGMEND01' ;
       *InLR = *ON ;
       EndSr ;
      /eject
%% CSPEC SUBR TBLXXX
%T     //*---------------------------------------------------------------------
%T     //*Perform Table Delete
%T     //*---------------------------------------------------------------------
%T     begsr TblDlt ;
       W#SBTG = 'TblDlt' ;
%T
%T     //Delete Old view if it exists
%T     //  (only used when creating/testing)
%T     exec sql
%T       DROP VIEW {tblview} ;            //* Use a view name
%T     dsqlerr = *blank ;
%T     If SQLcod <> *Zero and
%T        SQLcod <> -204  ;
%T       //* Do nothing - views should not exist
%T       //* dsqlerr = 'Delete View err ' + %char(sqlcod) ;
%T       dlogerr = 'Delete View err ' + %char(sqlcod) ;
%T       //*dsply dSQLerr ;
%T     else  ;
%T       //* Do nothing else
%T       dlogerr = 'Delete View {tblview} completed normally' ;
%T     endif ;
%T
%T     //Delete Old table if it exists
%T     //  (only used when creating/testing)
%T     exec sql
%T       DROP TABLE {tblname} ;
%T     dsqlerr = *blank ;
%T     If SQLcod <> *Zero and
%T        SQLcod <> -204  ;
%T      //dsqlerr = 'Delete Table err ' + %char(sqlcod) ;
%T      //dsply dSQLerr ;
%T      dlogerr = 'Delete Table {tblname} err ' + %char(sqlcod) ;
%T     else  ;
%T       //* Do nothing else
%T      dlogerr = 'Delete Table {tblname} completed normally' ;
%T     endif ;
%T
%T     endsr ;
%T    /eject
%T     //*---------------------------------------------------------------------
%T     //*Perform Table Create
%T     //*---------------------------------------------------------------------
%T     begsr TblCrt ;
       W#SBTG = 'TblCrt' ;
%T
%T     //*Create the table
%T     Exec SQL
%T       Create Table {tblname}(
%T     //*START USER PROTECTED SECTION  - CSPEC 0006.000 / *USR
%T     //*++TAGB0006
%T     //* Put field definitions here
%T     //*++TAGE0006
%T     //*FINISH USER PROTECTED SECTION - CSPEC 0006.000 / *USR
%T     //** Maintenance ---------------------------
%T     {*}TRRID INTEGER NOT NULL
%T                  GENERATED ALWAYS AS IDENTITY
%T        (START WITH 1
%T
%T
%T
%T
%T     {*}TRXTY char(1)       not null with default
%T     {*}TRXKY char(32)      not null with default
%T     {*}INSDT DATE          Not Null With Default
%T                                   current date
%T     {*}INSTM TIME          Not Null With Default
%T                                   current time
%T     {*}INSUS CHAR(10)      Not Null With Default
%T     {*}INSPG CHAR(10)      Not Null With Default
%T     {*}UPDDT DATE          Not Null With Default
%T                             date('0001-01-01')
%T     {*}UPDTM TIME          Not Null With Default
%T                               time('00.00.00')
%T     {*}UPDUS CHAR(10)      Not Null With Default
%T     {*}UPDPG CHAR(10)      Not Null With Default
%T     CONSTRAINT {tblname}  Primary key({*}TRRID))
%T         RCDFMT {fmtname} ;
%T
%T     dsqlerr = *blank ;
%T     If sqlcod <> *zero and
%T       sqlcod <> 7905  ;
%T       //dsqlerr = 'Create Table err ' + %char(sqlcod) ;
%T       //dsply dSQLerr ;
%T       dlogerr = 'Create Table {tblname} err ' + %char(sqlcod) ;
%T     else ;
%T       exec sql
%T         label on table {tblname} is
%T           '{tblname} (*put your description here*)' ;
%T       dlogerr = 'Create Table {tblname} completed normally' ;
%T     Endif ;
%T
%T     endsr ;
%T    /eject
%T     //*---------------------------------------------------------------------
%T     //*Perform Table Text
%T     //*---------------------------------------------------------------------
%T     begsr TblTxt ;
       W#SBTG = 'TblTxt' ;
%T
%T     //*Text the columns on the table
%T     Exec SQL
%T       LABEL ON COLUMN {tblname}(
%T     //*=======================================================================*//
%T     //*START USER PROTECTED SECTION  - DSPEC 0007.000 / *USR                *//
%T     //*++TAGB0007                                                           *//
%T     //* Put "Text is" Text Headings here                                      *//
%T     //*++TAGE0007                                                           *//
%T     //*FINISH USER PROTECTED SECTION - DSPEC 0007.000                       *//
%T     //*=======================================================================*//
%T        //** Maintenance ---------------------------
%T        {*}TRRID Text IS 'Row ID'
%T        {*}TRXTY TEXT IS 'Transaction Type'
%T        {*}TRXKY TEXT IS 'Transaction Key'
%T        {*}INSDT TEXT IS 'Row Insert Date'
%T        {*}INSTM TEXT IS 'Row Insert Time'
%T        {*}INSUS TEXT IS 'Row Insert User'
%T        {*}INSPG TEXT IS 'Row Insert Program'
%T        {*}UPDDT TEXT IS 'Row Update Date'
%T        {*}UPDTM TEXT IS 'Row Update Time'
%T        {*}UPDUS TEXT IS 'Row Update User'
%T        {*}UPDPG TEXT IS 'Row Update Program') ;
%T     dsqlerr = *blank ;
%T     If SQLcod <> *Zero ;
%T       //dsqlerr = 'Text Column err ' + %char(sqlcod) ;
%T       //dsply dSQLerr ;
%T       dlogerr = 'Text Column {tblname} err ' + %char(sqlcod) ;
%T     else ;
%T       //* Do nothing else
%T       dlogerr = 'Text Column {tblname} completed normally' ;
%T     endif ;
%T
%T     endsr ;
%T    /eject
%T     //*---------------------------------------------------------------------
%T     //*Perform Table Label
%T     //*---------------------------------------------------------------------
%T     begsr TblLbl ;
       W#SBTG = 'TblLbl' ;
%T
%T     //*Label the columns on the table
%T     Exec Sql
%T       LABEL ON COLUMN {tblname}(
%T     //*=======================================================================*//
%T     //*START USER PROTECTED SECTION  - DSPEC 0008.000 / *USR                *//
%T     //*++TAGB0008                                                           *//
%T     //* Put "is" Column Headings here                                         *//
%T     //*++TAGE0008                                                           *//
%T     //*FINISH USER PROTECTED SECTION - DSPEC 0008.000                       *//
%T     //*=======================================================================*//
%T         //** Maintenance ---------------------------
%T       {*}TRRID IS 'Row                 ID'
%T       {*}TRXTY IS 'Transaction         Type'
%T       {*}TRXKY IS 'Transaction         Key'
%T       {*}INSPG IS 'Row Insert          Program'
%T       {*}INSUS IS 'Row Insert          User'
%T       {*}INSDT IS 'Row Insert          Date'
%T       {*}INSTM IS 'Row Insert          Time'
%T       {*}UPDPG IS 'Row Update          Program'
%T       {*}UPDUS IS 'Row Update          User'
%T       {*}UPDDT IS 'Row Update          Date'
%T       {*}UPDTM IS 'Row Update          Time');
%T     dsqlerr = *blank ;
%T     If SQLcod <> *Zero ;
%T       //dsqlerr = 'Label Column err ' + %char(sqlcod) ;
%T       //dsply dSQLerr ;
%T       dlogerr = 'Label Column {tblname} err ' + %char(sqlcod) ;
%T     else ;
%T       //* Do nothing else
%T       dlogerr = 'Label Column {tblname} completed normally' ;
%T     endif ;
%T
%T     endsr ;
%T    /eject
%T     //*--------------------------------------------------------------------
%T     //*Perform Table View and Index Creations
%T     //*--------------------------------------------------------------------
%T     begsr TblVwx ;
       W#SBTG = 'TblVwx' ;
%T     dlogerr = 'Create Views for {tblview} Completed' ;
%T
%T     endsr ;
%T    /eject
%% CSPEC SUBR CLSTS1
%I     //==================================================================*
%I     //*CLSTS1 - Close the TRNSTSL1 file.                              *
%I     //==================================================================*
%I       BegSr CLSTS1 ;
%I
%I         //* If the file is open
%I         If W#OTS1 = *ON ;
%I           W#OTS1 = *OFF ;
%I           Close TRNSTSL1 ;
%I         EndIf ;
%I
%I       EndSr ;
%I
%I    /eject
%% CSPEC SUBR CLOPGM
       //==================================================================*
       //*CLOPGM - Close open programs.                                  *
       //==================================================================*
       BegSr CLOPGM ;
       W#SBTG = 'CLOPGM01' ;
%I     Exsr SXMSG0 ;
%I     Exsr SVMHG0 ;
%I     Exsr SVRMH0 ;
%I
%I     //* It is INTENTIONAL that program PSXEMP1R (SXEMP1) is not
%I     //* closed.
%I
       Endsr ;
      /eject
%% CSPEC SUBR PGMABT
%I     //==================================================================*
%I     //*PGMABT - Program abort logic.                                  *
%I     //==================================================================*
%I       BegSr PgmAbt ;
%I       W#SBTG = 'PGMABT01' ;
%I
%I
%I       //*START USER PROTECTED SECTION  - CSPEC 0009.000 / *USR
%I       //*++TAGB0009
%I       //* Put any special abort logic here
%I       //*++TAGE0009
%I       //*FINISH USER PROTECTED SECTION - CSPEC 0009.000 / *USR
%I
%I       //* If message needs to be sent
%I       If W#MSID > *BLANKS ;
%I         //W#MSID =  W#MSID  ;
%I         //W#MSDT =  W#MSDT  ;
%I         ExSr PGMERR       ;
%I       EndIf ;
%I
%I       //* Relay messages to caller.
%I       ExSr RLYMSG  ;
%I
%I       //* Send escape message to caller thereby preventing any
%I       //*  record action.
%I       ExSr SNDESC ;
%I
%I       EndSr ;
%I    /eject
%% CSPEC SUBR SXMSG0
%I     //==================================================================*
%I     //*SXMSG0 - Shutdown program - PSXMSG0R                           *
%I     //==================================================================*
%I       BegSr SXMSG0 ;
%I
%I       W#PFPR = '*NO     ' ;
%I       W#SHDN = '*YES    ' ;
%I       EXSR      XXMSG0    ;
%I
%I       EndSr ;
%I    /eject
%% CSPEC SUBR OPNTS1
%I    /eject
%I     //==================================================================*
%I     //*OPNTS1 - Open the TRNSTSL1 file.                               *
%I     //==================================================================*
%I       BegSr OPNTS1 ;
%I
%I       //* If the file is not already open
%I       If W#OTS1 = *Off ;
%I         W#OTS1 =  *ON ;
%I         OPEN TRNSTSL1 ;
%I       EndIf ;
%I
%I       // custRecKeys is a qualified data structure
%I       K$1TS1.TSTSTK = *Blank;
%I       K#TSTK = *Blank;
%I
%I       EndSr ;
%I    /eject
%% CSPEC SUBR SVRMH0
%I     //==================================================================*
%I     //*SVRMH0 - Shutdown program - PSVRMH0R                           *
%I     //==================================================================*
%I       BegSr SVRMH0 ;
%I
%I         W#PFPR = '*NO     ' ;
%I         W#SHDN = '*YES    ' ;
%I         ExSr XVRMH0 ;
%I
%I       EndSr ;
%I
%% CSPEC SUBR SVMHG0
%I     //==================================================================*
%I     //*SVMHG0 - Shutdown program - PSVMHG0R                           *
%I     //==================================================================*
%I       BegSr SVMHG0 ;
%I
%I         W#PFPR = '*NO     ' ;
%I         W#SHDN = '*YES    ' ;
%I         ExSr XVMHG0         ;
%I
%I       EndSr ;
%I
%% CSPEC SUBR SXEMP1
%I     //==================================================================*
%I     //*SXEMP1 - Shutdown program - PSXEMP1R                           *
%I     //==================================================================*
%I       BegSr SXEMP1 ;
%I
%I         W#PFPR = '*NO     ' ;
%I         W#SHDN = '*YES    ' ;
%I         ExSr XXEMP1 ;
%I
%I       EndSr ;
%I    /eject
%% CSPEC SUBR XVRMH0
%I     //==================================================================*
%I     //*XVRMH0 - Execute program - PSVRMH0C                            *
%I     //==================================================================*
%I       BegSr XVRMH0 ;
%I
%I         P$PFPR = W#PFPR ;
%I         P$SHDN = W#SHDN ;
%I         P$TRID = W#TRID ;
%I         //* P$MTHS           Value returned from call
%I         P$MSID = *Blanks ;
%I         Callp(E) PSVRMH0R(P$PFPR:P$SHDN:P$TRID:P$MTHS:P$MSID);
%I
%I         //* If there is an error on the call to this program
%I         //*  report it.
%I         W#ERROR = %error() ;
%I         W#STATUS = %status() ;
%I
%I         If (%error() and  W#PFPR = '*YES    ') or
%I            (P$MSID <>  *BLANKS  and  W#PFPR = '*YES    ')  ;
%I           W#MSID = 'PSX0001'  ;
%I           W#MSDT = *BLANKS    ;
%I           W#WA10 = 'PSVRMH0R' ;
%I           W#MSDT = W#SBTG + $PPGNM  + W#WA10 ;
%I           ExSr PGMABT ;
%I         Endif ;
%I
%I       Endsr ;
%I    /eject
%% CSPEC SUBR RLYMSG
%I     //==================================================================*
%I     //* RLYMSG - Relay program messages                               *
%I     //==================================================================*
%I       BegSr RLYMSG ;
%I
%I         //*---------------------------------------------------------------*
%I         //* Dependencies:
%I         //* -------------
%I         //* Requires program name out of the program status data structure.
%I         //*   PGMSTS    ESDSPGMSTS
%I         //* -------------
%I         //* Requires data structures to define length of 1st and 2nd level
%I         //* message text.
%I         //*   RTMSG       DS                            512
%I         //*   RTSCLV      DS                           3000
%I         //*---------------------------------------------------------------*
%I
%I         //*  Assemble command string - RLYRPGMSG PGMQ()
%I         RTMSID = 'CMD0002' ;
%I         RTMFNM = 'PSIMSGF' ;
%I         RTMFLB = *Blanks   ;
%I         RTMSDT = $PPGNM    ;
%I         RTMSG  = *Blanks   ;
%I         RTSCLV = *Blanks   ;
%I         Callp(E) PSXRTM1C(rtmsid:rtmfnm:rtmflb:rtmsdt:rtmsg:rtsclv) ;
%I         W#ERROR = %error() ;             // use for debugging
%I         W#STATUS = %status() ;           // use for debugging
%I
%I         //* Execute command
%I         //QCLENG = 512 ;
%I         //Callp(E) QCMDEXC(rtsclv:QCLENG) ;
%I         Callp(E) QCMDEXC(rtsclv:%size(rtsclv)) ;
%I         W#ERROR = %error() ;             // use for debugging
%I         W#STATUS = %status() ;           // use for debugging
%I
%I       EndSr ;
%I    /eject
%% CSPEC SUBR PGMERR
%I     //==================================================================*
%I     //* PGMERR - Send error message to current programs message queue *
%I     //==================================================================*
%I       BegSr PGMERR ;
%I
%I         //*-------------------------------------------------------------*
%I         //* Input:
%I         //* ------
%I         //* W#MSID - Message ID to be sent.
%I         //* W#MSDT - Message data.
%I         //*-------------------------------------------------------------*
%I
%I         SMMSID = W#MSID  ;               // Message ID
%I         SMMFNM = *Blanks ;               // Message File
%I         SMMSDT = W#MSDT  ;               // Message Data
%I         SMPQRL = *Blanks ;               // PgmQ Relation
%I         SMPQNM = *Blanks ;               // PgmQ Name
%I
%I         PSXSPM1C(smmsid:smmfnm:smmsdt:smpqrl:smpqnm) ; //  Send message
%I
%I         W#ERROR = %error() ;             // use for debugging
%I         W#STATUS = %status() ;           // use for debugging
%I
%I       Endsr ;
%I    /eject
%% CSPEC SUBR XXEMP1
%I     //==================================================================*
%I     //*XXEMP1 - Execute program - PSXEMP1R                            *
%I     //==================================================================*
%I       BegSr XXEMP1 ;
%I
%I         P$PFPR = W#PFPR ;
%I         P$SHDN = W#SHDN ;
%I         P$TSTK = W#TSTK ;
%I         P1MSID = W1MSID ;
%I         P$MSDT = W#MSDT ;
%I         P$MSFN = W#MSFN ;
%I         P$MSCL = W#MSCL ;
%I         P$MSFL = W#MSFL ;
%I         P$MSID = *Blanks ;
%I
%I         Callp(E) PSXEMP1R(P$PFPR:P$SHDN:P$TSTK:P1MSID:P$MSDT:
%I                           P$MSFN:P$MSCL:P$MSFL:P$MSID) ;
%I
%I         W#ERROR = %error()   ;           // use for debugging
%I         W#STATUS = %status() ;           // use for debugging
%I
%I      EndSr ;
%I    /eject
%% CSPEC SUBR XVMHG0
%I     //==================================================================*
%I     //*XVMHG0 - Execute program - PSVMHG0R                            *
%I     //==================================================================*
%I       BegSr XVMHG0 ;
%I
%I         p$pfpr = W#PFPR  ;
%I         p$shdn = W#SHDN  ;
%I         p$tstk = W#TSTK  ;
%I         p$msid = *BLANKS ;
%I
%I         Callp(E) PSVMHG0R(P$PFPR:P$SHDN:P$TSTK:P$MSID) ;
%I
%I         //* If there is an error on the call to this program
%I         //* report it.
%I         W#ERROR = %error()    ;          // use for debugging
%I         W#STATUS = %status()  ;          // use for debugging
%I         If %error() or  P$MSID <> *Blanks ;
%I           W#MSID = 'PSX0001'  ;
%I           W#MSDT = *BLANKS    ;
%I           W#WA10 = 'PSVMHG0R' ;
%I           W#MSDT = W#SBTG + $PPGNM + W#WA10 ;
%I           ExSr PGMABT ;
%I         Endif;
%I
%I       EndSr ;
%I    /eject
%% CSPEC SUBR SNDESC
%I     //==================================================================*
%I     //*SNDESC - Send escape message to caller.                        *
%I     //==================================================================*
%I       BegSr SNDESC ;
%I
%I         W#SBTG = 'SNDESC01' ;
%I
%I         //* This message is strictly to notify the calling program
%I         //* that the record operation encountered an error.
%I         P$MSID = 'CPF9898' ;             //* Message ID
%I         P$MSFN = 'QCPFMSG' ;             //* Message file
%I         P$MSTY = '*ESCAPE' ;             //* Message type
%I         P$MSDT = *Blanks   ;             //* Message Data
%I         P$PGRL = '*PRV '   ;             //* Relationship
%I         P$PGQM = $PPGNM    ;             //* Prog queue
%I         Callp(E) PSXSPM4C(P$MSID:P$MSFN:P$MSTY:
%I                           P$MSDT:P$PGRL:P$PGQM) ;
%I         W#ERROR = %error() ;             // use for debugging
%I         W#STATUS = %status() ;           // use for debugging
%I       EndSr ;
%I    /eject
%% CSPEC SUBR XXMSG0
%I     //==================================================================*
%I     //*XXMSG0 - Execute program - PSXMSG0R                            *
%I     //==================================================================*
%I       BegSr XXMSG0 ;
%I
%I         P$PFPR = W#PFPR  ;
%I         P$SHDN = W#SHDN  ;
%I         P$BLRQ = W#BLRQ  ;
%I         P$KYGN = W#KYGN  ;
%I         P$MNTY = W#MNTY  ;
%I         P$OBCL = W#OBCL  ;
%I         P$KYDT = W#KYDT  ;
%I         P$KYST = W#KYST  ;
%I         P$KYNU = W#KYNU  ;
%I         P$FDNM = W#FDNM  ;
%I         P$FDST = W#FDST  ;
%I         P$FDSL = W#FDSL  ;
%I         P$FDSM = W#FDSM  ;
%I         P$FDNU = W#FDNU  ;
%I         P$FDDT = W#FDDT  ;
%I         P$FDFA = W#FDFA  ;
%I         P$FDFB = W#FDFB  ;
%I         P$FDRA = W#FDRA  ;
%I         P$FDOP = W#FDOP  ;
%I         P$FDOV = W#FDOV  ;
%I         P$RSCD = W#RSCD  ;
%I         P$MSID = *BLANKS ;
%I         Callp(E) PSXMSG0R(P$PFPR:P$SHDN:P$BLRQ:P$KYGN:P$MNTY:
%I                           P$OBCL:P$KYDT:P$KYST:P$KYNU:P$FDNM:
%I                           P$FDST:P$FDSL:P$FDSM:P$FDNU:P$FDDT:
%I                           P$FDFA:P$FDFB:P$FDRA:P$FDOP:P$FDOV:
%I                           P$RSCD:P$MSID) ;
%I
%I         //* If there is an error on the call to this program
%I         //* report it.
%I         W#ERROR = %error() ;             // use for debugging
%I         W#STATUS = %status() ;           // use for debugging
%I         if (%error() and W#PFPR = '*YES    ')  or
%I            (P$MSID <>  *BLANKS  and  W#PFPR =  '*YES    ')  ;
%I           W#MSID = 'PSX0001'  ;
%I           W#MSDT = *BLANKS    ;
%I           W#WA10 = 'PSXMSG0R' ;
%I           W#MSDT = W#SBTG + $PPGNM + W#WA10 ;
%I           ExSr PGMABT ;
%I         EndIf ;
%I
%I         W#KYST = *Blanks ;
%I         W#FDST = *Blanks ;
%I
%I       EndSr ;
%I    /eject
%% CSPEC SUBR RTVTS1
%I     //==================================================================*
%I     //*RTVTS1 - Retrieve unique TRNSTSL1 file record.                 *
%I     //==================================================================*
%I       BegSr RTVTS1 ;
%I
%I         //* Retrieve the record in either input or update mode.
%I
%I         If W#TYAC = '*INPUT  ' ;
%I           chain K$1TS1 TRNSTSL1  ;
%I         else ;
%I           chain(E) K$1TS1 TRNSTSL1  ;
%I           DoW %error() ;
%I             // Record is locked
%I             // Hey
%I             // if W#TYAC is always '*INPUT'....
%I             if %status = 1218  ;
%I               chain(E) K$1TS1 TRNSTSL1  ;
%I             else ;
%I               exsr pgmabt ;
%I             endif   ;
%I           EndDo  ;
%I
%I           // part of this logic is to check for record lock
%I           // keep performing chain until it is not (endless loop until
%I           // record is available).  I disagree with this type of handling
%I           // however
%I           // K$1TS1        CHAIN     TRNSTSL1                           9091
%I           // *IN91         DOWEQ     *ON
%I           // K$1TS1        CHAIN     TRNSTSL1                           9091
%I           // END
%I         Endif ;
%I
%I         If %found() ;
%I           W#RCFD = '*YES    ' ;
%I          Else ;
%I           W#RCFD = '*NO     ' ;
%I         EndIf ;
%I
%I         //* If the record is not found
%I         //* report the error and end the program.
%I         If W#RCFD = '*NO     '  and
%I            W#MDRD = '*YES    '      ;
%I           W#MSID = 'PSX0003'  ;
%I           W#MSDT = *BLANKS    ;
%I           W#WA10 = 'TRNSTSL1' ;
%I           W#MSDT = K#TSTK + W#WA10 + $PPGNM + W#SBTG ;
%I           ExSr PGMABT ;
%I         EndIf ;
%I
%I       EndSr ;
%I
%I    /eject
%% CSPEC SUBR CRTKEY
%K     //==================================================================*
%K     //*CrtKey - Create a unique key                                   *
%K     //==================================================================*
%K       BegSr CrtKey ;
%K
%K       //* Create a master key for the file (just in case)
%K       exec sql
%K         SELECT current timestamp into :w#SQLTS FROM sysibm.sysdummy1 ;
%K       w#TRXID = 'USRPGM' ;
%K       w#TRXTS = %char(%dec(%char(w#SQLTS : *iso0) : 20 : 0));
%K       //w#TRXCT = w#TRXCT + 1;
%K       w#TRKEY = %trim(w#TRXID) + %trim(w#TRXTS) ; // + %editc(w#TRXCT:'X');
%K       w#JBKEY = $PJBNO ;
%K       //* uptrxky = %trim(w#TRKEY) ;
%K
%K       EndSr ;
%K
%% CSPEC CUSTOM           *area to allow user to put there own code
       //==================================================================*
       //*Additional User Subroutines can go here                        *
       //==================================================================*
       //*START USER PROTECTED SECTION  - CSPEC 0010.000 / *USR
       //*++TAGB0010
       //* Put any additional user subroutines or procedures here.
       //*++TAGE0010
       //*FINISH USER PROTECTED SECTION - CSPEC 0010.000 / *USR
**CTDATA condta Field constants
       dcl-ds vfldref  len(96)   qualified DIM(36) ;
         vfldname      char(20)   pos(1)  ;
         vflddef       char(15)   pos(21) ;
         vfldval       char(20)   pos(36) ;
         vfldlen       zoned(2:0) pos(56) ;
         vfldlal       zoned(2:0) pos(58) ;
         vfldnml       zoned(2:0) pos(60) ;
       end-ds ;
     vfldname      |  vflddef      |  vfldval         |
         1         2         3         4          5         6         7         8         9
123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456
{orel}              OSrelease                          000600
{mmddyyyy}          %char(%date():*usa/)               001000
{copyright_name}    Copyright$                         004000
{year}              Copyright$                         000400
       //*------------------------------------------------
       varfldi = 2                                       ;
       vfldref(varfldi).vfldname = '{mmddyyyy}'          ;
       vfldref(varfldi).vfldlal  = 10                    ;
       vfldref(varfldi).vflddef  = '%char(%date():*usa/)';
       vfldref(varfldi).vfldval  = %char(%date():*usa/)  ;
       vfldref(varfldi).vfldlen  =
                    %len(%trim(vfldref(varfldi).vfldval));
       //*------------------------------------------------
       varfldi = 3                                       ;
       vfldref(varfldi).vfldname = '{copyright_name}'    ;
       vfldref(varfldi).vfldlal  = 40                    ;
       vfldref(varfldi).vflddef  = 'Copyright$'          ;
       vfldref(varfldi).vfldval  = copyright$            ;
       vfldref(varfldi).vfldlen  =
                    %len(%trim(vfldref(varfldi).vfldval));
       //*------------------------------------------------
       varfldi = 4                                       ;
       vfldref(varfldi).vfldname = '{year}'              ;
       vfldref(varfldi).vfldlal  = 04                    ;
       vfldref(varfldi).vflddef  =
                        '%char(%subdt(%date(): *years ))';
       vfldref(varfldi).vfldval  =
                          %char(%subdt(%date(): *years ));
       vfldref(varfldi).vfldlen  =
                    %len(%trim(vfldref(varfldi).vfldval));
       //*------------------------------------------------
       varfldi = 5                                       ;
       vfldref(varfldi).vfldname = '{srclib}'            ;
       vfldref(varfldi).vfldlal  = 10                    ;
       vfldref(varfldi).vflddef  = 'Slibrary$'           ;
       vfldref(varfldi).vfldval  = %trim(Slibrary$)      ;
       vfldref(varfldi).vfldlen  =
                    %len(%trim(vfldref(varfldi).vfldval));
       //*------------------------------------------------
       varfldi = 6                                       ;
       vfldref(varfldi).vfldname = '{srcfile}'           ;
       vfldref(varfldi).vfldlal  = 10                    ;
       vfldref(varfldi).vflddef  = 'Sfile$'              ;
       vfldref(varfldi).vfldval  = %trim(Sfile$)         ;
       vfldref(varfldi).vfldlen  =
                    %len(%trim(vfldref(varfldi).vfldval));
       //*------------------------------------------------
       varfldi = 7                                       ;
       vfldref(varfldi).vfldname = '{srcmbr}'            ;
       vfldref(varfldi).vfldlal  = 10                    ;
       vfldref(varfldi).vflddef  = 'Program$'            ;
       vfldref(varfldi).vfldval  = %trim(Program$)       ;
       vfldref(varfldi).vfldlen  =
                    %len(%trim(vfldref(varfldi).vfldval));
       //*------------------------------------------------
       varfldi = 8                                       ;
       vfldref(varfldi).vfldname = '{srcmbrd}'           ;
       vfldref(varfldi).vfldlal  = 40                    ;
       vfldref(varfldi).vflddef  = 'Text w/Program$'     ;
       vfldref(varfldi).vfldval  =
                      'User Program - ' + %trim(Program$);
       vfldref(varfldi).vfldlen  =
                    %len(%trim(vfldref(varfldi).vfldval));
       //*------------------------------------------------
       varfldi = 9                                       ;
       vfldref(varfldi).vfldname = '{tech}'              ;
       vfldref(varfldi).vfldlal  = 10                    ;
       vfldref(varfldi).vflddef  = '$pcuid'              ;
       vfldref(varfldi).vfldval  = %trim($pcuid)         ;
       vfldref(varfldi).vfldlen  =
                    %len(%trim(vfldref(varfldi).vfldval));
       //*------------------------------------------------
       varfldi = 10                                      ;
       vfldref(varfldi).vfldname = '{company_name}'      ;
       vfldref(varfldi).vfldlal  = 30                    ;
       vfldref(varfldi).vflddef  = 'company$'            ;
       vfldref(varfldi).vfldval  = %trim(company$)       ;
       vfldref(varfldi).vfldlen  =
                    %len(%trim(vfldref(varfldi).vfldval));
       //*------------------------------------------------
       varfldi = 11                                      ;
       vfldref(varfldi).vfldname = '{date}'              ;
       vfldref(varfldi).vfldlal  = 10                    ;
       vfldref(varfldi).vflddef  = '%char(%date():*usa/)';
       vfldref(varfldi).vfldval  =
                              %trim(%char(%date():*usa/));
       vfldref(varfldi).vfldlen  =
                    %len(%trim(vfldref(varfldi).vfldval));
       //*------------------------------------------------
       varfldi = 12                                      ;
       vfldref(varfldi).vfldname = '{userpgm}'           ;
       vfldref(varfldi).vfldlal  = 10                    ;
       vfldref(varfldi).vflddef  = 'Program$'            ;
       vfldref(varfldi).vfldval  = %trim(Program$)       ;
       vfldref(varfldi).vfldlen  =
                    %len(%trim(vfldref(varfldi).vfldval));
       //*------------------------------------------------
       varfldi = 13                                      ;
       if projectID$ = *blank ;
         projectID$ = '** Project ID **' ;
       endif ;
       vfldref(varfldi).vfldname = '{project_id}'        ;
       vfldref(varfldi).vfldlal  = 32                    ;
       vfldref(varfldi).vflddef  = 'projectID$'          ;
       vfldref(varfldi).vfldval  = %trim(projectID$)     ;
       vfldref(varfldi).vfldlen  =
                    %len(%trim(vfldref(varfldi).vfldval));
       //*------------------------------------------------
       varfldi = 14                                      ;
       vfldref(varfldi).vfldname = '{extpgm}'            ;
       vfldref(varfldi).vfldlal  = 10                    ;
       vfldref(varfldi).vflddef  = 'Program$'            ;
       vfldref(varfldi).vfldval  = %trim(Program$)       ;
       vfldref(varfldi).vfldlen  =
                    %len(%trim(vfldref(varfldi).vfldval));
       //*------------------------------------------------
       varfldi = 15                                      ;
       vfldref(varfldi).vfldname = '{system}'            ;
       vfldref(varfldi).vfldlal  = 10                    ;
       vfldref(varfldi).vflddef  = 'Ssystem'             ;
       vfldref(varfldi).vfldval  = %trim(Ssystem)        ;
       vfldref(varfldi).vfldlen  =
                    %len(%trim(vfldref(varfldi).vfldval));
       //*------------------------------------------------
       varfldi = 16                                      ;
       vfldref(varfldi).vfldname = '{class}'             ;
       vfldref(varfldi).vfldlal  = 10                    ;
       vfldref(varfldi).vflddef  = 'sclass$'             ;
       if sclass$ = *blank                               ;
       vfldref(varfldi).vfldval  = '*None'               ;
       else                                              ;
       vfldref(varfldi).vfldval  = sclass$               ;
       endif                                             ;
       vfldref(varfldi).vfldlen  =
                    %len(%trim(vfldref(varfldi).vfldval));
       //*------------------------------------------------
       varfldi = 17                                      ;
       vfldref(varfldi).vfldname = '{mt}'                ;
       vfldref(varfldi).vfldlal  = 04                    ;
       vfldref(varfldi).vflddef  = 'sincludedt'          ;
       vfldref(varfldi).vfldval  = %trim(sincludedt)     ;
       vfldref(varfldi).vfldlen  =
                    %len(%trim(vfldref(varfldi).vfldval));
       //*------------------------------------------------
       varfldi = 18                                      ;
       vfldref(varfldi).vfldname = '{fmt}'               ;
       vfldref(varfldi).vfldlal  = 05                    ;
       vfldref(varfldi).vflddef  = 'sdatefmt$'           ;
       vfldref(varfldi).vfldval  = %trim(sdatefmt$)      ;
       vfldref(varfldi).vfldlen  =
                    %len(%trim(vfldref(varfldi).vfldval));
       //*------------------------------------------------
       varfldi = 19                                      ;
       vfldref(varfldi).vfldname = '{pfx}'               ;
       vfldref(varfldi).vfldlal  = 01                    ;
       vfldref(varfldi).vflddef  = 'SMntPreFx$'          ;
       vfldref(varfldi).vfldval  = %trim(SMntPreFx$)     ;
       vfldref(varfldi).vfldlen  =
                    %len(%trim(vfldref(varfldi).vfldval));
       //*------------------------------------------------
       varfldi = 20                                      ;
       vfldref(varfldi).vfldname = '{*}'                 ;
       vfldref(varfldi).vfldlal  = 01                    ;
       vfldref(varfldi).vflddef  = 'SMntPreFx$'          ;
       vfldref(varfldi).vfldval  = %trim(SMntPreFx$)     ;
       vfldref(varfldi).vfldlen  =
                    %len(%trim(vfldref(varfldi).vfldval));
       //*------------------------------------------------
       varfldi = 21                                      ;
       vfldref(varfldi).vfldname = '{tblview}'           ;
       vfldref(varfldi).vfldlal  = 10                    ;
       vfldref(varfldi).vflddef  = 'Program$+V1'         ;
       vfldref(varfldi).vfldval  = %trim(Program$)+'V1'  ;
       vfldref(varfldi).vfldlen  =
                    %len(%trim(vfldref(varfldi).vfldval));
       //*------------------------------------------------
       varfldi = 22                                      ;
       vfldref(varfldi).vfldname = '{tblname}'           ;
       vfldref(varfldi).vfldlal  = 10                    ;
       vfldref(varfldi).vflddef  = 'Program$'            ;
       vfldref(varfldi).vfldval  = %trim(Program$)       ;
       vfldref(varfldi).vfldlen  =
                    %len(%trim(vfldref(varfldi).vfldval));
       //*------------------------------------------------
       varfldi = 23                                      ;
       vfldref(varfldi).vfldname = '{fmtname}'           ;
       vfldref(varfldi).vfldlal  = 10                    ;
       vfldref(varfldi).vflddef  = 'R + Program$'        ;
       vfldref(varfldi).vfldval  = 'R'+ %trim(Program$)  ;
       vfldref(varfldi).vfldlen  =
                    %len(%trim(vfldref(varfldi).vfldval));
       //*------------------------------------------------
         vfldname      char(20)   pos(1)  ;
         vflddef       char(15)   pos(21) ;
         vfldval       char(20)   pos(36) ;
         vfldlen       zoned(2:0) pos(56) ;
         vfldlal       zoned(2:0) pos(58) ;
         vfldnml       zoned(2:0) pos(60) ;
       vfldref(varfldi).vfldname = '{orel}'              ;
       vfldref(varfldi).vfldlal  = 06                    ;
       vfldref(varfldi).vflddef  = 'OSrelease'           ;
       vfldref(varfldi).vfldval  = OSrelease             ;
       vfldref(varfldi).vfldlen  =
                    %len(%trim(vfldref(varfldi).vfldval));
