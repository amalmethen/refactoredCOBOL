      ***************************************************************** 00000010
      *                                                               * 00000020
      *      MEMBER NAME   SFCFFD01                                   * 00000030
      *                                                               * 00000040
      *                **  FILE MAINTENANCE SYSTEM  **                * 00000050
      *               **  ACCOUNTING EXTRACT RECORD  **               * 00000060
      *                        25  DEC.  2002                         * 00000070
      ***************************************************************** 00000070
      *   DB2 PROJECT : REUSE FILLER FOR THE PURPOSE OF TXN UNIQUNESS.* 00000080
      *                                  ADSANI MANAL  24/12/2002.    * 00000080
      ***************************************************************** 00000090
                                                                        00000100
           10  FMA-TRANSACTION-LEAD.                                    00000110
               15  FMA-KEY.
                   20  FMA-LEAD-RECORD-TYPE      PIC  X(02).            00000120
                   20  FMA-LEAD-KEY.                                    00000130
                       25  FMA-LEAD-SSN-KEY      PIC  X(11).            00000140
                       25  FMA-LEAD-COUNT        PIC  S9(07) COMP-3.    00000150
                       25  FMA-LEAD-TIME-MILISEC PIC  S9(08) COMP-3.    00000150
                       25  FILLER                PIC  X(20).            00000150
                   20  FMA-LEAD-TRAN-CODE        PIC  X(04).            00000160
                   20  FMA-LEAD-TRAN-DATE        PIC S9(08) COMP-3.     00000170
                   20  FMA-LEAD-TIME             PIC S9(07) COMP-3.     00000180
               15  FMA-LEAD-TERM-ID          PIC  X(04).                00000190
               15  FMA-LEAD-DEPT-CLERK.                                 00000200
                   20  FMA-LEAD-DEPT         PIC  X(03).                00000210
                   20  FMA-LEAD-CLERK        PIC  X(02).                00000220
               15  FMA-LEAD-ACTION-CODE      PIC  X(02).                00000230
               15  FMA-LEAD-SOURCE           PIC  X(02).                00000240
               15  FMA-LEAD-DESTINATION      PIC  X(02).                00000250
               15  FMA-LEAD-BYPASS-CODE      PIC  X(02).                00000260
               15  FMA-LEAD-FORMAT-TYPE      PIC  X(02).                00000270
               15  FMA-LEAD-TXN-DATA-LENGTH  PIC S9(04) COMP.           00000280
           SKIP3                                                        00000290
           10  FMA-ACCTING-FLDS.                                        00000300
OCPJ15*        15  FMA-AF-ACCT-CDE           PIC  X(06).                00000310
OCPJ15         15  FMA-AF-ACCT-CDE           PIC  9(11)       COMP-3.   00000310
               15  FMA-AF-EFFECTIVE-DATE     PIC  X(08).                00000320
               15  FMA-AF-AMOUNT             PIC S9(12)V999   COMP-3.   00000330
               15  FMA-AF-REF-NUM.                                      00000340
                   20 FMA-AF-REF-NUM-SEC     PIC X.                     00000350
                   20 FMA-AF-REF-NUM-MIN     PIC X(06).                 00000360
                   20 FMA-AF-REST            PIC X(03).                 00000370
               15  FMA-AF-CIVIL-ID           PIC  9(12).                00000370
               15  FMA-AF-SS-NUMBER          PIC  9(11).                00000370
