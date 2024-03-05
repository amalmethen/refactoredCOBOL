000010*---------------------------------------------------------------* 00000010
000020*         DEFINETION OF  CURRENT DATE AND BUSINESS DATE         * 00000020
000030*         COPY MEMBER   ( -INC OOC0030 )                        * 00000030
000040*---------------------------------------------------------------* 00000040
000050                                                                  00000050
000060   02  CWA-CURRENT-BUSINESS-DATE.                                 00000060
000070                                                                  00000070
000080       05  CWA-CURRENT-DATE.                                      00000080
000090                                                                  00000090
000100           10  CWA-CUR-DATE-06.                                   00000100
000110               15  CWA-CUR-DD06         PIC X(02).                00000110
000120               15  CWA-CUR-MM06         PIC X(02).                00000120
000130               15  CWA-CUR-YY06         PIC X(02).                00000130
000140           10  CWA-CUR-DAT-06   REDEFINES                         00000140
000150               CWA-CUR-DATE-06          PIC 9(06).                00000140
000160                                                                  00000150
000170           10  CWA-CUR-DATE-08.                                   00000160
000180               15  CWA-CUR-DD08         PIC X(02).                00000170
000190               15  CWA-CUR-MM08         PIC X(02).                00000180
000200               15  CWA-CUR-YY08         PIC X(04).                00000190
000210           10  CWA-CUR-DAT-08   REDEFINES                         00000200
000220               CWA-CUR-DATE-08          PIC 9(08).                00000200
000230                                                                  00000210
000240           10  CWA-CUR-PC-YYMMDD        PIC S9(7)  COMP-3.        00000220
000250                                                                  00000230
000260           10  CWA-CUR-PC-YYYYMMDD      PIC S9(9)  COMP-3.        00000240
000270                                                                  00000250
000280           10  CWA-CUR-DATE-DLMTR-08.                             00000260
000290               15  CWA-CUR-DLMTR-DD08   PIC X(02).                00000270
000300               15  FILLER               PIC X(01).                00000280
000310               15  CWA-CUR-DLMTR-MM08   PIC X(02).                00000290
000320               15  FILLER               PIC X(01).                00000300
000330               15  CWA-CUR-DLMTR-YY08   PIC X(04).                00000310
000340                                                                  00000320
000350       05  CWA-BUSINESS-DATE.                                     00000330
000360                                                                  00000340
000370           10  CWA-BUS-DATE-06.                                   00000350
000380               15  CWA-BUS-DD06         PIC X(02).                00000360
000390               15  CWA-BUS-MM06         PIC X(02).                00000370
000400               15  CWA-BUS-YY06         PIC X(02).                00000380
000410           10  CWA-BUS-DAT-06   REDEFINES                         00000390
000420               CWA-BUS-DATE-06          PIC 9(06).                00000390
000430                                                                  00000400
000440           10  CWA-BUS-DATE-08.                                   00000410
000450               15  CWA-BUS-DD08         PIC X(02).                00000420
000460               15  CWA-BUS-MM08         PIC X(02).                00000430
000470               15  CWA-BUS-YY08         PIC X(04).                00000440
000480           10  CWA-BUS-DAT-08   REDEFINES                         00000450
000490               CWA-BUS-DATE-08          PIC 9(08).                00000450
000500                                                                  00000460
000510           10  CWA-BUS-PC-YYMMDD        PIC S9(7)  COMP-3.        00000470
000520                                                                  00000480
000530           10  CWA-BUS-PC-YYYYMMDD      PIC S9(9)  COMP-3.        00000490
000540                                                                  00000500
000550           10  CWA-BUS-DATE-DLMTR-08.                             00000510
000560               15  CWA-BUS-DLMTR-DD08   PIC X(02).                00000520
000570               15  FILLER               PIC X(01).                00000530
000580               15  CWA-BUS-DLMTR-MM08   PIC X(02).                00000540
000590               15  FILLER               PIC X(01).                00000550
000600               15  CWA-BUS-DLMTR-YY08   PIC X(04).                00000560
000610                                                                  00000570
000620       05  CWA-APPLICATION-AREAS.                                 00000580
000630                                                                  00000590
000640           10  CWA-ALPHA-INDEX-AREA.                              00000600
000650                                                                  00000590
000660               15  CWA-AI-STAT-SWITCH      PIC X.                 00000610
000670                   88  CWA-AI-STAT-SW-ON       VALUE  HIGH-VALUES.00000620
000680                   88  CWA-AI-STAT-SW-OFF      VALUE  LOW-VALUES. 00000630
000690                                                                  00000640
000700           10  CWA-AI-FILLER       PIC X(15).                     00000650
