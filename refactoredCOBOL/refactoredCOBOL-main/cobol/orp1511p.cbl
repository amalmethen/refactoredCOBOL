       CBL TEST(NONE,SYM,SEPARATE)
000010 ID DIVISION.
000020*****************************************************************
000030* *      I D   D I V I S I O N                  PW-WHXR      *  *
000040*****************************************************************
000050 PROGRAM-ID. ORP1511P.
000060 AUTHOR. SALWA SAYED.
000070 INSTALLATION. PUBLIC INSTITUTE FOR SOCIAL SECURITY.
000080 DATE-WRITTEN. SEP 09, 1981.
000090 DATE-COMPILED.
000100*REMARKS.
000110
000120*****************************************************************
000130*                        ORP1511P                               *
000140*                                                               *
000150*        FUNCTION - THIS PROGRAM DISPLAYS THE BASIC DATA        *
000160*           SCREEN(ORM1511).  **BOTH EMPLOYER & INDIVIDUAL**    *
000170*                                                               *
000180*        INPUT PARMS - NONE                                     *
000190*                                                               *
000200*        INPUT  - DISPLAY BASIC DATA SCREEN  ORM1411            *
000210*                                                               *
000220*        OUTPUT - BASIC DATA SCREEN                             *
000230*               - ERROR SCREEN(ORM1511) IF NEEDED, WITH ERRORS  *
000240*                 IN HIGH INTENSITY                             *
000250*                                                               *
000260*        TABLES -                                               *
000270*               SFCTB101  MASTER RECORD CODE                    *
000280*               SFCTB201  NAME TYPE CODE TABLE                  *
000290*               SFCOCCUR  MASTER RECORD OCCURENCE TABLE         *
000300*                                                               *
000310*        SWITCHES - ERR-SW   LOW-VALUES INDICATES NO ERROR FOUND*
000320*                            HIGH-VALUES INDICATES ERROR FOUND  *
000330*                                                               *
000340*        EXITS                                                  *
000350*            NORMAL - LINK TO OOP9005P EXPAND NAME AND ADDRESS  *
000360*                    MODULE  WHICH EXPAND NAME AND ADDRESS      *
000370*                    TRAILERS ONLY.                             *
000380*                   - LINK TO OOP0009P TRAILER EXPAND MODULES   *
000390*                     WHICH EXPANDS THE LOCATED TRAILER TO ITS  *
000400*                     FULL SIZE BASED ON THE NUMBER OF ITS OCCUR*
000410*                   - LINK TO OO90001P TRAILER LOCATE MODULE    *
000420*                     WHICH LOCATES THE REQUESTED TRAILER.      *
000430*                   - LINK TO OOP9020P FLIP MODULE WHICH        *
000440*                     REFORMATS THE INPUT FIELD.IT IS USED IN   *
000450*                     REARRANGING ARABIC CHARACTERS FROM LEFT   *
000460*                     TO RIGHT TO RIGHT TO LEFT.                *
000470*                   - LINK TO EMPLOYEE NUMBER WHICH RETURNS     *
000480*                     THE EMPLOYER SOCIAL SECURITY NUMBER BEING *
000490*                     ASSIGNED TO THIS ENROLLMENT               *
000500*                   - RETURN TO CICS                            *
000510*                                                               *
000520*            ABNORMAL - REQUEST A CICS TANSACTION DUMP THEN     *
000530*                     XCTL TO ORP1500P.                         *
000540*                                                               *
000550*        MODIFICATION -                                         *
000560*           SMR9538   - FORM RECEIVED DATE USED FOR EMPLOYEE    *
000570*                       REGISTRATION.                           *
000580*                                       22/01/83   ARIKAT       *
000590*                                                               *
000600*           SMR9631   - ADD NUMBER OF CHILDREN TO THE BASIC DATA*
000610*                       TRLR.                                   *
000620*                                    24/04/1983  SALWA SAYED.   *
000630*                                                               *
000640*           SMR4360   - ADD DEDCTION NOT PAID TO THE BASIC DATA *
000650*                       TRLR.                                   *
000660*                                    30/05/1984  AHMED SAEED    *
000670*****************************************************************
000680* -INC BIGCOMPR                                                 *
000690*     BIG COMPILE PROJECT CHANGES                               *
000700*     ****************************                              *
000710*  UPDATE TO MASTER RECORD                                      *
000720*            OCCURANCE TABLE                                    *
000730*            ON-LINE TRLRS                                      *
000740*            BACH TRLRS                                         *
000750*            ACCOUNTING TABLE                                   *
000760*            DEDUCTION TABLE                                    *
000770*  DATE      01/10/1984                                         *
000780*                      AHMED SAEED  07/10/84                    *
000790*                                                               *
000800*       SMR40079 : - CHANGE HEADING OF SPECIAL ID ON ORM1511    * 00000830
000810*                        29/07/1987  ANZI.                      * 00000850
000820*                                                               *
000830*****************************************************************
000840*     ********** : - XBASIC CONVERSION PROJECT                  * 00000830
000850*                                           MARCH 1990          * 00000850
000860*                                           MASOUMA B.          *
000870*---------------------------------------------------------------*
000880* SMR# 40584   ENLARGE M-BAS-DATA-NAT-CERT-NUM TO 9 DIGITS.     *
000890*                                   MANAL S. ADSANI  DEC. 1994  *
000900*---------------------------------------------------------------*
000910* SMR# 41417   UPDATE THE PROGRAM ACCOURDING TO CHANGES IN      *
000920*              PROGRAM ORP1411P.                                *
000930*                                   ALHADDAD A.    24/12/1195   *
000940*----------------------------------------------------------------*00000920
000950*   PROJECT OF INCREASING NUMBER OF COMPLIMENTARY TRLR.          *SEHISTPR
000960*                                   KURIEN BENJAMIN  02/01/2000  *
000970*---------------------------------------------------------------* SMR42489
000980* SMR# 42647 :CHANGE THE NUMBERS OF DIGITS OF FIELD SPECNUM     *
000990*             FROM 14 TO 12.                                    *
001000*                                 SAMEERA ALQALLAF 02/03/2002.  *
001010*---------------------------------------------------------------*
001020* SMR# 91553 : AMEND THE PROGRAM TO DISPLAY THE FILING NUMBER   *
001030*              EXISTING ON THE BASIC TRAILER OF THE MASTER FILE.*
001040*                                 LUBNA AL-GHABRA  18/05/2003.  *
001050*---------------------------------------------------------------* SMR40327
001060*  SMR 43515   ADD A NEW FELIED FOR NUMBER OF CHILDREN AFTER    * SMR40327
001070*              RETIRE.                   A.SHARIF      MAY 2005 * SMR40327
001080*---------------------------------------------------------------*
001080*  SMR 44432   MODIFY THE PROGRAM TO TAKE THE NEW FEILED FOR    *
001080*              BLOCK CODE.                                      *
001080*                                 TAHANI AL-OTHMAN   14/07/2008 *
      *---------------------------------------------------------------*
      *  SMR 93306:  MODIFY THE PROGRAM TO DEAL WITH THE  ENLARGE OF  *
      *              THE BANK ACCOUNT NUMBER FROM 12 TO 14 DIGIT.     *
      *                                 ZAINAB AL-JURAIWI  22/12/2008.*
      *---------------------------------------------------------------*
      * SMR#44608     RECOMPILE PROGRAM TO TAKE NEW VERSION OF TABLE  *
      *               SSSLTBNK.                                       *
      *                                SAWSAN AL-MOAWED    19/02/2009.*
      *---------------------------------------------------------------*
      * SMR# 45342    AMEND PROGRAM TO DISPLAY DROP NATIONALITY DATE  *
      *                                                               *
      *                                SARAH ALMUDHAF      10-04-2011 *
      *---------------------------------------------------------------*
      * SMR# 41652    AMEND PROGRAM TO DISPLAY CIVIL-ID EXPIREY DATE  *
      *                                                               *
      *                                SARAH ALMUDHAF      20-04-2011 *
      *---------------------------------------------------------------*
      * SMR# 96847    AMEND PROGRAM TO DISPLAY DATE AND TIME          *
      *                                                               *
      *                                MOODY AL-HUDAIB        06-2013 *
      *---------------------------------------------------------------*
S97283* SMR#97283     AMEND PROGRAM TO INCREASE THE SIZE OF ACCT-NUM  *
S97283*                                                               *
S97283*                                MOODY AL-HUDAIB     19-06-2014 *
      *---------------------------------------------------------------*
      * SMR#9279  : RESTRICT XCTL TO PROGRAM ODS1500 AND ODS2350.     *
      *             BY CALL OPACEE PROGRAM ONLY FOR FINANCIAL DEPT.   *
      * ODS1500 : »—‰«„Ã «·«” ›”«— ⁄‰ «·‘Ìﬂ«                          *
      * ODS2350 : »—‰«„Ã «·«” ›”«— ⁄‰ «·„»«·€ «·„ÕÊ·Â                 *
      *                                ARWA AL-FARES       09-02-2015 *
      *****************************************************************
      *****************************************************************
001100*
001090     EJECT
001100*****************************************************************
001110*        E N V I R O N M E N T   D I V I S I O N                *
001120*****************************************************************
001130
001140 ENVIRONMENT DIVISION.
001150
001160 CONFIGURATION SECTION.
001170
001180 SOURCE-COMPUTER. IBM-370.
001190 OBJECT-COMPUTER. IBM-370.
001200     EJECT
001210*****************************************************************
001220*                D A T A   D I V I S I O N                      *
001230*****************************************************************
001240
001250 DATA DIVISION.
001260     SKIP3
001270*****************************************************************
001280*        W O R K I N G   S T O R A G E   S E C T I O N          *
001290*****************************************************************
001300*        THE WORKING STORAGE SECTION CONTAINS CONSTANTS         *
001310*    TABLES, CICS FIELD ATTRIBUTES, AND CICS ATTENTION          *
001320*    IDENTIFIERS.                                               *
001330*****************************************************************
001340
001350 WORKING-STORAGE SECTION.
001360

S9279  77  WS-OOPACEEE-PROGRAM PIC X(8) VALUE 'OOPACEEE'.
S9279  01  SYSTEM-PARAM.
S9279      05 WS-CICS-AREA    PIC X(01) VALUE SPACES.
S9279          88 PROD-CICS      VALUE 'Y'.
S9279          88 TEST-CICS      VALUE 'N'.
S9279  01  ACCESS-PARAM.
S9279      05 WS-ACCESS-AUTH   PIC X(01) VALUE SPACES.
S9279          88 READ-AUTH              VALUE 'R'.
S9279          88 UPDATE-AUTH            VALUE 'U'.
S9279  01  TWA-CHX-MISC.
S9279      05 TWA-CHX-MISC-DEBUG-FLAG         PIC X(01) VALUE SPACES.
S9279      05 TWA-CHX-MISC-LPAR-NAME          PIC X(08) VALUE SPACES.
S9279      05 TWA-CHX-MISC-DEFAULT-RACF-GRP   PIC X(08) VALUE SPACES.
S9279  01  SPECIAL-FUNCTION-01.
S9279 * FUNCT 01 :- SPECIAL AUTH. TO GROUP OF FIN XCTL ODS2350 ODS1500
S9279      05 WS-FUNC-01-AUTH  PIC X(01) VALUE SPACES.
S9279          88 FUNC-01-AUTH-OK        VALUE 'R'.
S9279          88 FUNC-01-NOT-AUTH       VALUE 'N'.
S9279
S9279  01  TWA-CHX-ACEE-PROGRAMS.
S9279      05 TWA-PROGRAM-RESOURCE-TYPE PIC X(12) VALUE 'PROGRAM     '.
S9279      05 TWA-PROGRAM-COUNT         PIC S9(4) COMP VALUE +3.
S9279      05 TWA-PROGRAM-RESOURCES.
S9279         10 TWA-PROGRAM-SECCHK          PIC X(08) VALUE 'SECCHK  '.
S9279         10 TWA-PROGRAM-SECCHK-ACCESS   PIC X(01).
S9279         10 TWA-PROGRAM-ORP1511P        PIC X(08) VALUE 'ORP1511P'.
S9279         10 TWA-PROGRAM-ORP1511P-ACCESS PIC X(01).
S9279         10 TWA-PROGRAM-FP151101        PIC X(08) VALUE 'FP151101'.REALTIME
S9279         10 TWA-PROGRAM-FP151101-ACCESS PIC X(01).                 REALTIME
S9279      05 TWA-PROGRAM-RESOURCER REDEFINES TWA-PROGRAM-RESOURCES
S9279         OCCURS 3 TIMES.
S9279         10 TWA-PROGRAM-RESOURCE-NAME   PIC X(08).
S9279         10 TWA-PROGRAM-RESOURCE-FLAG   PIC X(01).
001390
001370 01  FILLER                      PIC X(24)
001380         VALUE 'PROGRAM-ID ORP1511P'.
001390
001400*****************************************************************
001410*                    CONSTANTS                                  *
001420*****************************************************************
001430
001440 01  CONSTANTS.
001450
001460     05  C-SLASH                     PIC X  VALUE '/'.
001470     05  C-MAP-SIZE                  PIC S9(4) COMP SYNC
001480                                     VALUE +660.                  SMR90539
001490      SKIP3                                                       00001470
001500       05   W-BANK-ACC-NUM-ALL.                                   00001480
001510                                                                  00001490
001520            10   W-FILLER             PIC       XX.               00001500
001530            10   W-BANK-ACC-NUM       PIC      X(12).             00001510
001540                                                                  00001520
001550       05   W-BAN-ACC  REDEFINES   W-BANK-ACC-NUM-ALL  PIC X(14). 00001530
001560     EJECT

003340       05   W-ER-REG-NUM              PIC 9(07).                  SMR42647
003340       05   W-CIVIL-ID                PIC 9(12).                  SMR42647
001540                                                                  00001520
001540       05  W-MSG-BLOCK-SSN              PIC X(70) VALUE           00001520
001540           'Ì’Ê·« …ÌÊÂ ‰„ œﬂ√ ·« ÌÃ—Ì ÂÌ·⁄ —ÊÃÕ„ ..!'.            00001520

S9279        05  K-PF10-11.                                             SMR91223
S9279         10  FILLER                      PIC X(70)  VALUE          00000020
S9279            'Â·ÊÕ„·« €·«»„·« ‰⁄ —«”› ”·«/ 01›»  «ﬂÌ‘·« ‰⁄ —«”› ”·« 00000030
S9279 -           '›» ﬂÊ‰»··11 '.                                       00000040


S45342         05  TWA-DROP-NAT-DATE-IN1         PIC X(9).
S45342         05  TWA-DROP-NAT-DATE-IN.
S45342             10  TWA-DROP-NATYYYY-IN       PIC X(4).
S45342             10  TWA-DROP-NATMM-IN         PIC XX.
S45342             10  TWA-DROP-NATDD-IN         PIC XX.
S45165         05  TWA-CIV-EXP-DATE-IN1         PIC X(9).
S45165         05  TWA-CIV-EXP-DATE-IN.
S45165             10  TWA-CIV-EXPYYYY-IN       PIC X(4).
S45165             10  TWA-CIV-EXPMM-IN         PIC XX.
S45165             10  TWA-CIV-EXPDD-IN         PIC XX.

001540                                                                  00001520
001570*****************************************************************
001580*                    TABLES                                     *
001590*****************************************************************
001600
001610 01  SFCTB101.
000000*DB2-INC SFCTB101  MASTER RECORD CODE                             00001630
000000     COPY SFCTB101.                                               00001630
001640     EJECT
001660
001650 01  SFCTB201.
000000*DB2-INC SFCTB201  NAME TYPE CODE TABLE                           00001670
000000     COPY SFCTB201.                                               00001670
001680     EJECT
001660
001690*DB2 01  SFCOCCUR.
000000*DB2-INC SFCOCCUR  M/R OCCURENCE TABLE                            00001710
001720     EJECT
001730*****************************************************************
001740*                    CICS FIELD DEFINITIONS                     *
001750*****************************************************************
001760
001770****                 CICS FIELD ATTRIBUTES                   ****
001780     SKIP3
000000*DB2-INC OOC0020                                                  00001790
000000     COPY  OOC0020.                                               00001790
001820
001810****                 CICS ATTENTION IDENTIFIERS              ****
001820 COPY DFHBMSCA.
001830     COPY DFHAID.
001840     EJECT
001820
001850*01  MAP01I                      COPY ORM1511.
001860     COPY ORM1511.
001870     EJECT
001820
001880 01  FLIP-AREA.                                                   00002540
001890   02  FLIP-A.                                                    00002550
001900     03  TWA-FLIP-A-LENGTH     PIC S9(4) COMP SYNC.               00002560
001910     03  TWA-FLIP-A-IN.                                           00002570
001920         05  TWA-EMP-NUM-IN                 PIC X(11).            00002580
001930         05  TWA-ER-REG-NUM-IN              PIC X(7).             00002590
001940                                                                  00002600
001950         05  TWA-SYS-ENTRY-DATE-IN         PIC X(8).              00002610
001960         05  TWA-NEXT-ACT-DATE-IN          PIC X(8).              00002620
001970         05  TWA-LAST-ACCT-DATE-IN         PIC X(8).              00002630
001980         05  TWA-LAST-FM-DATE-IN           PIC X(8).              00002640
001990         05  TWA-ER-BUS-LIC-DATE-IN        PIC X(8).              00002650
002000         05  TWA-REG-EFF-DATE-IN           PIC X(8).              00002660
002010         05  TWA-PAY-DAY-IN                PIC X(2).              00002670
002020         05  TWA-BIRTH-DATE-IN             PIC X(8).              00002680
002030         05  TWA-DEATH-DATE-IN             PIC X(8).              00002690
002040         05  TWA-NAT-EFF-DATE-IN           PIC X(8).              00002700
002050         05  TWA-COLL-DAY-IN               PIC X(2).              00002710
002060         05  TWA-ER-BUS-TYPE-IN            PIC X(4).              00002720
002070         05  TWA-OCC-CODE-IN  REDEFINES  TWA-ER-BUS-TYPE-IN       00002730
002080                                           PIC X(4).              00002740
002090         05  TWA-FORM-REC-DATE-IN          PIC X(8).              00002750
002100         05  TWA-SOURCE-IN                 PIC X(2).              00002760
002110         05  TWA-CHILD-NO1-IN              PIC 9(2).              SMR90539
002120         05  TWA-CHILD-NO2-IN              PIC 9(2).              SMR90539
002130         05  TWA-20KD-1992-IN              PIC X(2).              00002780
002140                                                                  00002790
002150     03  TWA-FLIP-A-OUT.                                          00002800
002160         05  TWA-20KD-1992-OUT             PIC X(2).              00002810
002170         05  TWA-CHILD-NO2-OUT             PIC 9(2).              SMR90539
002180         05  TWA-CHILD-NO1-OUT             PIC 9(2).              SMR90539
002190         05  TWA-SOURCE-OUT                PIC X(2).              00002830
002200         05  TWA-FORM-REC-DATE-OUT.                               00002840
002210             10  TWA-FORM-REC-DD-OUT       PIC XX.                00002850
002220             10  TWA-FORM-REC-MM-OUT       PIC XX.                00002860
002230             10  TWA-FORM-REC-YYYY-OUT     PIC X(4).              00002870
002240         05  TWA-ER-BUS-TYPE-OUT           PIC X(4).              00002880
002250         05  TWA-OCC-CODE-OUT  REDEFINES  TWA-ER-BUS-TYPE-OUT     00002890
002260                                           PIC X(4).              00002900
002270         05  TWA-COLL-DAY-OUT              PIC XX.                00002910
002280                                                                  00002920
002290         05  TWA-NAT-EFF-DATE-OUT.                                00002930
002300             10  TWA-NAT-EFF-DD-OUT        PIC XX.                00002940
002310             10  TWA-NAT-EFF-MM-OUT        PIC XX.                00002950
002320             10  TWA-NAT-EFF-YYYY-OUT      PIC X(4).              00002960
002330                                                                  00002970
002340         05  TWA-DEATH-DATE-OUT.                                  00002980
002350             10  TWA-DEATH-DD-OUT          PIC XX.                00002990
002360             10  TWA-DEATH-MM-OUT          PIC XX.                00003000
002370             10  TWA-DEATH-YYYY-OUT        PIC X(4).              00003010
002380                                                                  00003020
002390         05  TWA-BIRTH-DATE-OUT.                                  00003030
002400             10  TWA-BIRTH-DD-OUT          PIC XX.                00003040
002410             10  TWA-BIRTH-MM-OUT          PIC XX.                00003050
002420             10  TWA-BIRTH-YYYY-OUT        PIC X(4).              00003060
002430                                                                  00003070
002440         05  TWA-PAY-DAY-OUT               PIC XX.                00003080
002450                                                                  00003090
002460         05  TWA-REG-EFF-DATE-OUT.                                00003100
002470             10  TWA-REG-EFF-DD-OUT        PIC XX.                00003110
002480             10  TWA-REG-EFF-MM-OUT        PIC XX.                00003120
002490             10  TWA-REG-EFF-YYYY-OUT      PIC X(4).              00003130
002500                                                                  00003140
002510         05  TWA-ER-BUS-LIC-DATE-OUT.                             00003150
002520             10  TWA-ER-BUS-LIC-DD-OUT     PIC XX.                00003160
002530             10  TWA-ER-BUS-LIC-MM-OUT     PIC XX.                00003170
002540             10  TWA-ER-BUS-LIC-YYYY-OUT   PIC X(4).              00003180
002550                                                                  00003190
002560         05  TWA-LAST-FM-DATE-OUT.                                00003200
002570             10  TWA-LAST-FM-DD-OUT        PIC XX.                00003210
002580             10  TWA-LAST-FM-MM-OUT        PIC XX.                00003220
002590             10  TWA-LAST-FM-YYYY-OUT      PIC X(4).              00003230
002600                                                                  00003240
002610         05  TWA-LAST-ACCT-DATE-OUT.                              00003250
002620             10  TWA-LAST-ACCT-DD-OUT      PIC XX.                00003260
002630             10  TWA-LAST-ACCT-MM-OUT      PIC XX.                00003270
002640             10  TWA-LAST-ACCT-YYYY-OUT    PIC X(4).              00003280
002650                                                                  00003290
002660         05  TWA-NEXT-ACT-DATE-OUT.                               00003300
002670             10  TWA-NEXT-ACT-DD-OUT       PIC XX.                00003310
002680             10  TWA-NEXT-ACT-MM-OUT       PIC XX.                00003320
002690             10  TWA-NEXT-ACT-YYYY-OUT     PIC X(4).              00003330
002700                                                                  00003340
002710         05  TWA-SYS-ENTRY-DATE-OUT.                              00003350
002720             10  TWA-SYS-ENTRY-DD-OUT      PIC XX.                00003360
002730             10  TWA-SYS-ENTRY-MM-OUT      PIC XX.                00003370
002740             10  TWA-SYS-ENTRY-YYYY-OUT    PIC X(4).              00003380
002750                                                                  00003390
002760         05  TWA-ER-REG-NUM-OUT            PIC X(7).              00003400
002770         05  TWA-EMP-NUM-OUT               PIC X(11).             00003410
002780                                                                  00003420
002790   02  FLIP-B.                                                    00003430
002800     03  TWA-FLIP-B-LENGTH            PIC S9(4) COMP SYNC.        00003440
002810     03  TWA-FLIP-B-IN.                                           00003450
002820         05  TWA-REC-TYPE-IN               PIC X(2).              00003460
002830         05  TWA-ACTV-CODE-IN              PIC X(2).              00003470
002840         05  TWA-BILL-TYPE-IN              PIC X(2).              00003480
002850         05  TWA-BILL-FREQ-IN              PIC X(2).              00003490
002860         05  TWA-MAR-STAT-IN               PIC X(2).              00003500
002870         05  TWA-ER-LEG-ENT-CODE-IN  REDEFINES  TWA-MAR-STAT-IN   00003510
002880                                           PIC X(2).              00003520
002890         05  TWA-TERM-ACT-DATE-IN          PIC X(8).              00003540
002900         05  TWA-NAT-CODE-IN               PIC X(3).              00003550
002910         05  TWA-SEX-CODE-IN               PIC X(1).              00003560
002920                                                                  00003570
002930         05  TWA-LOC-CODE-IN               PIC X(3).              00003580
002940         05  TWA-PAY-FREQ-IN               PIC X(2).              00003590
002950         05  TWA-NAT-CERT-NUM-IN           PIC X(9).              SMR40584
002960         05  TWA-SPEC-ID-NUM-IN            PIC X(14).             00003610
002970         05  TWA-SPEC-ID-NUM-IN-R REDEFINES TWA-SPEC-ID-NUM-IN.   SMR42647
002980             10  TWA-SPEC-ID-NUM-IN-2      PIC X(02).             SMR42647
002990             10  TWA-SPEC-ID-NUM-IN-12     PIC 9(12).             SMR42647
002990*DB2         10  TWA-SPEC-ID-NUM-IN-12     PIC X(12).             SMR42647
003000***ZAI   05  TWA-BANK-PAYEE-ACCT-NUM-IN    PIC X(12).             SMR93306
S97283*        05  TWA-BANK-PAYEE-ACCT-NUM-IN    PIC X(14).             SMR93306
S97283         05  TWA-BANK-PAYEE-ACCT-NUM-IN    PIC X(30).             SMR93306
003010         05  TWA-BANK-BR-AGY-NUM-IN        PIC X(4).              00003630
003020         05  TWA-BANK-PAY-METH-CODE-IN     PIC X(2).              00003640
003030         05  TWA-CHILD-NO3-IN              PIC 9(2).              SMR90359
003040         05  TWA-CHILD-NO4-IN              PIC 9(2).              SMR90539
003050         05  TWA-PIS-NUM-IN                PIC X(11).
003060         05  TWA-DED-NOT-PD-IN             PIC XX.
003070         05  TWA-50KD-1992-IN              PIC X(2).
003080         05  TWA-FIL-NUM-IN                PIC 9(11).             SMR91553
003090         05  TWA-FIL-NUM-IN-R     REDEFINES TWA-FIL-NUM-IN.       SMR91553
003100             10  TWA-FIL-NUM-IN-9          PIC 9(09).             SMR91553
003110             10  TWA-FIL-NUM-IN-2          PIC 9(02).             SMR91553
003120
003130         05  TWA-CHD-AFT-RET-IN            PIC X(02).             SMR43515
003140         05  TWA-BLOCK-CODE-IN             PIC X(01).             SMR44432
003140
003150
003160     03  TWA-FLIP-B-OUT.
003170
003140         05  TWA-BLOCK-CODE-OUT            PIC X(01).             SMR44432
003180         05  TWA-CHD-AFT-RET-OUT           PIC X(02).             SMR43515
003190
003200         05  TWA-FIL-NUM-OUT               PIC 9(11).             SMR91553
003210         05  TWA-FIL-NUM-OUT-R    REDEFINES TWA-FIL-NUM-OUT.      SMR91553
003220             10  TWA-FIL-NUM-OUT-2         PIC 9(02).             SMR91553
003230             10  TWA-FIL-NUM-OUT-9         PIC 9(09).             SMR91553
003240         05  TWA-50KD-1992-OUT             PIC X(2).              00003720
003250         05  TWA-DED-NOT-PD-OUT            PIC XX.                00003730
003260         05  TWA-PIS-NUM-OUT               PIC X(11).             00003740
003270         05  TWA-CHILD-NO4-OUT             PIC 9(2).              SMR90539
003280         05  TWA-CHILD-NO3-OUT             PIC 9(2).              SMR90539
003290         05  TWA-BANK-PAY-METH-CODE-OUT    PIC X(2).              00003760
003300         05  TWA-BANK-BR-AGY-NUM-OUT       PIC X(4).              00003770
003310***ZAI   05  TWA-BANK-PAYEE-ACCT-NUM-OUT   PIC X(12).             SMR93306
S97283*        05  TWA-BANK-PAYEE-ACCT-NUM-OUT   PIC X(14).             SMR93306
S97283         05  TWA-BANK-PAYEE-ACCT-NUM-OUT   PIC X(30).             SMR93306
003320         05  TWA-SPEC-ID-NUM-OUT           PIC X(14).             00003790
003330         05  TWA-SPEC-ID-NUM-OUT-R REDEFINES TWA-SPEC-ID-NUM-OUT. SMR42647
003340*DB2         10  TWA-SPEC-ID-NUM-OUT-12    PIC X(12).             SMR42647
003340             10  TWA-SPEC-ID-NUM-OUT-12    PIC 9(12).             SMR42647
003350             10  TWA-SPEC-ID-NUM-OUT-2     PIC X(02).             SMR42647
003360         05  TWA-NAT-CERT-NUM-OUT          PIC X(9).              SMR40584
003370         05  TWA-PAY-FREQ-OUT              PIC X(2).              00003810
003380         05  TWA-LOC-CODE-OUT              PIC X(3).              00003820
003390                                                                  00003830
003400         05  TWA-SEX-CODE-OUT              PIC X(1).              00003840
003410         05  TWA-NAT-CODE-OUT              PIC X(3).              00003850
003420                                                                  00003870
003430         05  TWA-TERM-ACT-DATE-OUT.                               00003880
003440             10  TWA-TRMAC-DD-OUT          PIC XX.                00003890
003450             10  TWA-TRMAC-MM-OUT          PIC XX.                00003900
003460             10  TWA-TRMAC-YYYY-OUT        PIC X(4).              00003910
003470                                                                  00003920
003480         05  TWA-MAR-STAT-OUT              PIC X(2).              00003930
003490         05  TWA-ER-LEG-ENT-CODE-OUT  REDEFINES  TWA-MAR-STAT-OUT 00003940
003500                                           PIC X(2).              00003950
003510         05  TWA-BILL-FREQ-OUT             PIC X(2).              00003960
003520         05  TWA-BILL-TYPE-OUT             PIC X(2).              00003970
003530         05  TWA-ACTV-CODE-OUT             PIC X(2).              00003980
003540         05  TWA-REC-TYPE-OUT              PIC X(2).              00003990
003550                                                                  00004000
003560     02  TWA-FLIP-SSN.                                            00004010
003570       03  TWA-FLIP-SSN-LEN              PIC S9(4) COMP SYNC.     00004020
003580       03  TWA-FLIP-SSN-IN               PIC X(11).               00004030
003590       03  TWA-FLIP-SSN-OUT              PIC X(11).               00004040
             03  TWA-FLIP-SSN-OUT-R REDEFINES TWA-FLIP-SSN-OUT.
                05 TWA-FLIP-SSN-O-4            PIC X(04).
                   88 TWA-EMPLOYER-REC         VALUE '0000'.
                05 TWA-FLIP-SSN-O-7            PIC X(07).

       01  W-MOD-COBADDR        PIC  X(08)  VALUE  'CCOBADDR'.
003600
003610 01  EXPAND-AREA.
003620     05  EXPAND-TYPE              PIC  X.
003630     05  EXPAND-LEN               PIC S9(4) COMP.
003640     05  EXPAND-IN.
003650         10  EXPAND-IN-AREA       PIC X(186).
003660         10  EXPAND-IN-NAME REDEFINES EXPAND-IN-AREA   PIC X(100).
003670     05  EXPAND-OUT-AREA          PIC X(180).
003680     05  EXPAND-OUT-ER-NAME REDEFINES EXPAND-OUT-AREA
003690                                  PIC X(64).
003700     05  EXPAND-OUT-EE-NAME REDEFINES EXPAND-OUT-AREA.
003710         10  EXPAND-OUT-EE-NAME1  PIC X(12).
003720         10  EXPAND-OUT-EE-NAME2  PIC X(12).
003730         10  EXPAND-OUT-EE-NAME3  PIC X(12).
003740         10  EXPAND-OUT-EE-NAME4  PIC X(12).
003750     05  EXPAND-OUT-K-PARM  REDEFINES EXPAND-OUT-AREA.
003760         10  EXPAND-OUT-TYPE      PIC X.
003770         10  EXPAND-OUT-LENGTH    PIC S9(4) COMP.
003780      SKIP3                                                       00001540
003790 01 TCA-FIELD.                                                    00002330
003800     05  TCANXTID                  PIC X(04) VALUE SPACES.
003810     05  TCAPCPI                   PIC X(08) VALUE SPACES.
003820                                                                  00002330
003830     05  FILLER                        PIC X(21) VALUE            00002330
003840             '****  THE DUMP-CODE ='.                             00002330
003850     05  TCADCDC.
003860        10  FILLER                    PIC X(04) VALUE '1511'.
003870        10  TCADCDC1                  PIC X(03) VALUE SPACES.
003880                                                                  00002330
003890     05  W-RESP                     PIC S9(04) COMP.
003900     05  W-HEX-VALUE               PIC X(01) VALUE SPACE.
003910                                                                  00006620
003920 01  FLIP-PTR                   PIC S9(08) COMP.
003930
003940 01  OCCR-TBL-PTR              USAGE POINTER.
003950 01  OCCR-TBL-PTR-R  REDEFINES OCCR-TBL-PTR  PIC S9(8) COMP.
003960
003970 01  NAME-PTR             USAGE POINTER.
003980 01  NAME-PTR-R   REDEFINES  NAME-PTR   PIC S9(08) COMP.
003990
004000 01  BANK-PTR             USAGE POINTER.
004010 01  BANK-PTR-R   REDEFINES  BANK-PTR   PIC S9(08) COMP.
004020
004030 01  NA-EXP-PTR           USAGE POINTER.
004040 01  NA-EXP-PTR-R   REDEFINES  NA-EXP-PTR   PIC S9(08) COMP.
004050
004060 01  BASIC-PTR           USAGE POINTER.
004070 01  BASIC-PTR-R   REDEFINES   BASIC-PTR   PIC S9(08) COMP.
004080
004090 01  FILLER                        PIC X(44) VALUE                00002330
004100       '*****  THE END OF THE WORKING STORAGE  *****'.            00002330
004110     EJECT
      *
       01  RESP-W                     PIC 9(07).
      *
       01  ABEND-CODE                 PIC X(04).
      *
       01  ERROR-CODE                 PIC X(12) VALUE SPACES.
      *
       01  ORSS0400-AREA.
      *
           02 ORSS0400-SQLCA          PIC X(136).
           02 F REDEFINES ORSS0400-SQLCA.
              03 F1                   PIC  X(12).
              03 ORSS0400-SQLCD       PIC S9(09)   COMP.
              03 F2                   PIC  X(120).
      *
           02 ORSS0400-SYSID          PIC  X(03).
      *
           02 ORSS0400-CLERK          PIC  X(05).
      *
           02 ORSS0400-TABNM          PIC  X(30).
      *
           02 ORSS0400-SSN            PIC  9(12).
           02 ORSS0400-SSN-X REDEFINES ORSS0400-SSN
                                      PIC  X(12).
      *
           02 ORSS0400-TRNID          PIC  X(04).
      *
       01  ABEND-ERR-MSG.
           02 ABEND-ERR-TEXT          PIC X(10).
           02 F                       PIC X(01) VALUE SPACES.
           02 ABEND-ERR-EIBDS         PIC X(30).
           02 F                       PIC X(01) VALUE SPACES.
           02 F                       PIC X(05) VALUE 'Â“„— '.
           02 F                       PIC X(01) VALUE SPACES.
           02 ABEND-ERR-CODE          PIC X(04).
      *
       01  W-SQLCD                      PIC Z(09)-.
       01  W-SQLCD-C REDEFINES W-SQLCD  PIC X(10).
      *
       01  W-EIBDS                     PIC X(30).
      *
           EXEC SQL INCLUDE SQLCA            END-EXEC.
      *
           EXEC SQL INCLUDE NEWLTEEB         END-EXEC.
      *
           EXEC SQL INCLUDE SSSLTNAM         END-EXEC.
      *
           EXEC SQL INCLUDE SSSLTBNK         END-EXEC.
      *
           EXEC SQL INCLUDE SSSLTBEN         END-EXEC.
      *
       01  W-DATE                       PIC X(10).
       01  W-DATEX REDEFINES W-DATE.
           02 W-YEAR                    PIC 9(04).
           02 F                         PIC X(01).
           02 W-MNTH                    PIC 9(02).
           02 F                         PIC X(01).
           02 W-DAY                     PIC 9(02).
      *
       01  W-DATE-9.
           02 W-YEAR9                   PIC 9(04).
           02 W-MNTH9                   PIC 9(02).
           02 W-DAY9                    PIC 9(02).
       01  W-DATE9 REDEFINES W-DATE-9   PIC 9(08).
      *
S96847 01  GREG-DATE                  PIC X(08).
S96847 01  W-CUR-DATE                 PIC X(02)/X(02)/X(04).
S96847 01  W-TIME                     PIC 99/99/99.
      *
        01  EMPLER-IND                PIC S9(04) COMP.
        01  REGNUM-IND                PIC S9(04) COMP.
        01  ENTDT-IND                 PIC S9(04) COMP.
        01  LACTDT-IND                PIC S9(04) COMP.
        01  LASFMDT-IND               PIC S9(04) COMP.
        01  REGFDT-IND                PIC S9(04) COMP.
        01  DETHDT-IND                PIC S9(04) COMP.
        01  NCRTF-IND                 PIC S9(04) COMP.
        01  NEFFDT-IND                PIC S9(04) COMP.
        01  RECFDT-IND                PIC S9(04) COMP.
        01  TRMDT-IND                 PIC S9(04) COMP.
        01  FUNDT-IND                 PIC S9(04) COMP.
        01  FRMDT-IND                 PIC S9(04) COMP.
        01  THRD-IND                  PIC S9(04) COMP.
S45342  01  NDRPDT-IND                PIC S9(04) COMP.
S45165  01  CEXPDT-IND                PIC S9(04) COMP.
      *
        01  COUNT-ROW                 PIC S9(04) COMP.
        01  COUNT-ROW2                PIC S9(04) COMP.

004120*****************************************************************
004130*                    LINKAGE SECTION                            *
004140*****************************************************************
004150
004160 LINKAGE SECTION.
004170
004180**********           TWA FIELDS                         *********
004190
004200 01 TWA-1511.
000000*DB2-INC TWA1511                                                  00004220
000000     COPY TWA1511.                                                00004220
004230     EJECT
004240
004250*DB2 01  MASTER-RECORD-BASIC-DATA.
000000*DB2-INC SFCMMT10      BASIC DATA TRLR                            00004270
004280*DB2 EJECT
004290*DB2
004300*DB2 01  MASTER-RECORD-NAME.
000000*DB2-INC SFCMMT20      NAME TRLR                                  00004320
004330*DB2 EJECT
004340*DB2
004350*DB2 01  MASTER-RECORD-BANK.
000000*DB2-INC SFCMMT80      BANK TRLR                                  00004370
004380*DB2 EJECT
004390*DB2                                                              00002380
000000*DB2 INC OOC0010                                                  00004400
000000     COPY  OOC0010.                                               00004400
004410*****************************************************************
004420*        P R O C E D U R E   D I V I S I O N                    *
004430*****************************************************************
004440
004450 PROCEDURE DIVISION.
004460
004470*****************************************************************
004480*                    A0000-MAINLINE                             *
004490*****************************************************************
004500
004510 A0000-MAINLINE  SECTION.
004520
004530     PERFORM  B0000-INIT.
004540
004550     PERFORM  C0000-PROCESS.
004560
004570     PERFORM  D0000-FINAL.
004580     GOBACK.
004590 A0000-MAINLINE-EXIT.  EXIT.
004600     EJECT
004610*****************************************************************
004620*                        B0000-INIT                             *
004630*****************************************************************
004640*         THIS SECTION:                                         *
004650*                   - ESTABLISHES  ADDRESSABILITY TO THE TCA,   *
004660*                     TCTTE, AND THE TIOA.                      *
004670*                   - MAPS THE BASIC DATA SCREEN.               *
004680*                   - SETS UP FOR ANY ABEND.                    *
004690*****************************************************************
004700
004710 B0000-INIT SECTION.
004720
004730                                                                  00006860
004740     EXEC CICS  ADDRESS                                           00006240
004750                TWA (ADDRESS OF TWA-1511)                         00006240
004760                TCTUA (ADDRESS OF TCTTE-USER-AREA)                00006240
004770     END-EXEC.                                                    00006240
004780                                                                  00006280
S9279      MOVE SPACES  TO  WS-CICS-AREA
S9279                       WS-ACCESS-AUTH
S9279                       WS-FUNC-01-AUTH.
S9279      CALL WS-OOPACEEE-PROGRAM  USING
S9279           DFHEIBLK DFHCOMMAREA
S9279           TWA-CHX-MISC TWA-CHX-ACEE-PROGRAMS.
S9279
S9279      IF TWA-PROGRAM-SECCHK-ACCESS EQUAL 'N'
S9279            MOVE  'Y'    TO  WS-CICS-AREA
S9279         ELSE
S9279            MOVE  'N'    TO  WS-CICS-AREA
S9279      END-IF.
S9279      MOVE  TWA-PROGRAM-ORP1511P-ACCESS  TO  WS-ACCESS-AUTH.       REALTIME
S9279      MOVE  TWA-PROGRAM-FP151101-ACCESS  TO  WS-FUNC-01-AUTH.      REALTIME
S9279
S9279  B0000-INIT-EXIT.  EXIT.
00481      EJECT
00482 *****************************************************************
004830*                    C0000-PROCESS                              *
004840*****************************************************************
004850*        THIS SECTION  PERFORMS THE INQUIRY FUNCTION WHICH IS   *
004860*    TO BUILD THE BASIC DATA SCREEN.                            *
004870*****************************************************************
004880
004890 C0000-PROCESS  SECTION.
004900
004910     MOVE LOW-VALUES TO TWAERROR.
004920     MOVE ZEROS      TO TWAKEY.
004930

S96847     PERFORM C0750-DATE-TIME.

           IF  TCTTE-CURNT-TRAN-ID EQUAL TO '1511'      AND
               TCTTE-ENTRY-COUNT   EQUAL TO LOW-VALUES  AND
               TCTTE-SSN           NUMERIC              AND
               TCTTE-SSN           GREATER THAN ZERO
               MOVE TCTTE-SSN TO TWANUM
               PERFORM C0015-FLIP-SSN
                   IF TWA-EMPLOYER-REC
                      MOVE '1510'  TO  TCTTE-CURNT-TRAN-ID
                      PERFORM C0012-XCTL-1510
                   ELSE
                      NEXT SENTENCE
             ELSE
               NEXT SENTENCE.
S9279
S9279      IF  EIBAID EQUAL  TO  DFHPF10  AND
S9279          TCTTE-ENTRY-COUNT NOT EQUAL TO LOW-VALUES
S9279              MOVE  '0'         TO  TCTTE-ENTRY-SW
S9279              PERFORM C0011-CHECK-AUTHORITY.
S9279
S9279      IF  EIBAID EQUAL  TO  DFHPF11  AND
S9279          TCTTE-ENTRY-COUNT NOT EQUAL TO LOW-VALUES
S9279              MOVE  '0'         TO  TCTTE-ENTRY-SW
S9279              PERFORM C0012-CHECK-AUTHORITY .
S9279

004940     IF  EIBAID EQUAL TO DFHPF12
004950
005000         PERFORM C0200-XCTL-RETURN-MENU
004970     ELSE
004980         IF  EIBAID EQUAL TO DFHCLEAR
004990
005000             PERFORM C0200-XCTL-RETURN-MENU
005010         ELSE
005020             IF  EIBAID  EQUAL TO  DFHENTER  OR
005030                                   DFHPA1    OR
005040                                   DFHPA2    OR
005050                                   DFHPA3
005060                 PERFORM C0200-XCTL-RETURN-MENU.
005070
005080     IF  TCTTE-ENTRY-COUNT EQUAL TO LOW-VALUES
005090
005100         MOVE  LOW-VALUE  TO MAP01O
005110
005120         IF  TCTTE-SSN NUMERIC
005130
005140             IF  TCTTE-SSN GREATER THAN ZERO
005150
005160                 MOVE TCTTE-SSN TO TWANUM
005170
005180                 PERFORM C0015-FLIP-SSN
005190
005200                 MOVE TWANUM    TO TWA-EMP-NUM-IN
005210
005220                 PERFORM C0300-INQUIRY
005230
005230                 PERFORM F0000-CHECK-BLOCK-CODE
005230
005240                 GO TO C0000-PROCESS-EXIT
005250             ELSE
005260                 PERFORM C0200-XCTL-RETURN-MENU
005270         ELSE
005280             PERFORM C0200-XCTL-RETURN-MENU
005290     ELSE
005300         PERFORM C0200-XCTL-RETURN-MENU.
005310

005320 C0000-PROCESS-EXIT.  EXIT.
005330     EJECT
S9279  C0011-CHECK-AUTHORITY SECTION.                                   000516
S9279                                                                   000517
S9279         IF ( ( PROD-CICS   AND  FUNC-01-AUTH-OK ) OR              REALTIME
S9279                TEST-CICS )                                        REALTIME
S9279             PERFORM C0011-XCTL-ODS1500                            000519
S9279          ELSE                                                     000521
S9279             MOVE 'ÂÌ·«„·« Â—«œ·«» …’«Œ …‘«‘·« ·ÊŒœ·« Õ„”Ì ·'      000523
S9279                          TO ERRMSG1O .
S9279  C0011-CHECK-AUTHORITY-EXIT.
S9279       EXIT.
           EJECT                                                        000528
S9279  C0012-CHECK-AUTHORITY SECTION.                                   000516
S9279                                                                   000517
S9279         IF ( ( PROD-CICS   AND  FUNC-01-AUTH-OK ) OR              REALTIME
S9279                TEST-CICS )                                        REALTIME
S9279             PERFORM C0012-XCTL-ODS2350                            000519
S9279          ELSE                                                     000521
S9279             MOVE 'ÂÌ·«„·« Â—«œ·«» …’«Œ …‘«‘·« ·ÊŒœ·« Õ„”Ì ·'      000523
S9279                          TO ERRMSG1O .
S9279                                                                   000517
S9279  C0012-CHECK-AUTHORITY-EXIT.
S9279       EXIT.
           EJECT                                                        000528
      ***************************************************************** 00005770
      *                     C0012-XCTL-ODS1500   SECTION              * 00005780
      ***************************************************************** 00005790
      *        THIS SECTION XCTL TO PROGRAM (ODS1500 ) WHICH          * 00005800
      *     RELATED TO THE EMPLOYER INQUIRY AND CHANGING.             * 00005810
      ***************************************************************** 00005820
S9279  C0011-XCTL-ODS1500 SECTION.
S9279                                                                   00005500
S9279      MOVE LOW-VALUES  TO  TCTTE-ENTRY-SW.                         00005500
S9279      MOVE '1500'      TO  TCTTE-CURNT-TRAN-ID.                    00005500
S9279      MOVE LOW-VALUES  TO  TCTTE-ENTRY-COUNT.                      00005500
S9279                                                                   00005500
S9279      EXEC CICS XCTL                                               00014070
S9279           PROGRAM ('ODS1500')                                     00014080
S9279      END-EXEC.                                                    00014070
S9279                                                                   00005500
       C0011-XCTL-ODS1500-EXIT.
              EXIT.
      ***************************************************************** 00005770
      *                     C0012-XCTL-ODS2350   SECTION              * 00005780
      ***************************************************************** 00005790
      *        THIS SECTION XCTL TO PROGRAM (ODS2350 ) WHICH          * 00005800
      *     RELATED TO THE EMPLOYER INQUIRY AND CHANGING.             * 00005810
      ***************************************************************** 00005820
       C0012-XCTL-ODS2350 SECTION.
S9279                                                                   00005500
S9279      MOVE LOW-VALUES  TO  TCTTE-ENTRY-SW.                         00005500
S9279      MOVE '2350'      TO  TCTTE-CURNT-TRAN-ID.                    00005500
S9279      MOVE LOW-VALUES  TO  TCTTE-ENTRY-COUNT.                      00005500
S9279                                                                   00005500
S9279      EXEC CICS XCTL                                               00014070
S9279           PROGRAM ('ODS2350')                                     00014080
S9279      END-EXEC.                                                    00014070
S9279                                                                   00005500
S9279  C0012-XCTL-ODS2350-EXIT.
              EXIT.
      *****************************************************************
      *                     C0012-XCTL-1428  SECTION                  *
      *****************************************************************
      *        THIS SECTION XCTL TO PROGRAM (ORP1428P) WHICH          *
      *     RELATED TO THE EMPLOYER INQUIRY AND CHANGING.             *
      *****************************************************************
       C0012-XCTL-1510    SECTION.

           MOVE LOW-VALUES  TO  TCTTE-ENTRY-SW.
           MOVE '1510'      TO  TCTTE-CURNT-TRAN-ID.
           MOVE LOW-VALUES  TO  TCTTE-ENTRY-COUNT.

           EXEC CICS XCTL
                PROGRAM ('ORP1510P')
           END-EXEC.

       C0012-XCTL-1510-EXIT.
              EXIT.
005340*****************************************************************
005350*                     C0015-FLIP-SSN                            *
005360*****************************************************************
005370*        THIS SECTION FLIPS THE ERMPLOYER REGESTRATION NUMBER   *
005380*     OR THE EMPLOYEE SOCIAL SECURITY NUMBER WHEN ENTERED.      *
005390*****************************************************************
005400
005410 C0015-FLIP-SSN SECTION.
005420
005430     MOVE TWANUM    TO TWA-FLIP-SSN-IN.
005440
005450*******************
005460*    GET ADDRESS FOR SSN FLIP
005470*******************
005480
005490     CALL 'CCOBADDR'  USING  TWA-FLIP-SSN  FLIP-PTR.
005500
005510     MOVE  FLIP-PTR          TO  TCTTE-ADDR4.
005520     MOVE  FLIP-PTR          TO  TCTTE-ADDR4.
005530     ADD   FLIP-PTR   2  GIVING  TCTTE-ADDR2.
005540     ADD   FLIP-PTR  13  GIVING  TCTTE-ADDR3.
005550
005560     MOVE  11  TO  TWA-FLIP-SSN-LEN.
005570
005580     MOVE 'OOP9020P' TO TCAPCPI.
005590
005600     EXEC  CICS  LINK  PROGRAM (TCAPCPI)
005610                         RESP  (W-RESP)
005620     END-EXEC.
005630
005640     IF  W-RESP = DFHRESP (NORMAL)
005650
005660         IF  RETURN-ZERO
005670
005680             NEXT SENTENCE
005690         ELSE
005700             MOVE '016' TO TCADCDC1
005710
005720             PERFORM S0400-DUMP-XCTL
005730     ELSE
005740         MOVE '017' TO TCADCDC1
005750
005760         PERFORM S0400-DUMP-XCTL.
005770
005780     MOVE  TWA-FLIP-SSN-OUT  TO  TWANUM.
005790
005800 C0015-FLIP-SSN-EXIT.  EXIT.
005810     EJECT
005820*****************************************************************
005830*                    C0200-XCTL-RETURN-MENU                     *
005840*****************************************************************
005850*   THIS SECTION:                                               *
005860*         RETURN CONTROL TO HIGHER LEVEL MENU ON ENTRY OF PF12  *
005870*****************************************************************
005880
005890 C0200-XCTL-RETURN-MENU SECTION.
005900
005910     MOVE LOW-VALUES TO TCTTE-ENTRY-COUNT.
005920     MOVE LOW-VALUES TO TCTTE-ENTRY-SW.
005930     MOVE '1500'     TO TCTTE-CURNT-TRAN-ID.
005940
005950********************
005960*    RETURN CONTROL TO INQ MENU                                  *
005970********************
005980
005990     MOVE 'ORP1500P' TO TCAPCPI.
006000                                                                  00018700
006010     EXEC CICS XCTL PROGRAM (TCAPCPI)                             00000010
006020     END-EXEC.                                                    00000010
006030                                                                  00018730
006040
006050 C0200-XCTL-RETURN-MENU-EXIT.  EXIT.
006060     EJECT
006070*****************************************************************
006080*                     C0300-INQUIRY                             *
006090*****************************************************************
006100*        THIS SECTION BUILDS INQUIRY MAP.                       *
006110*****************************************************************
006130 C0300-INQUIRY  SECTION.
006140
006160     MOVE +0         TO TWAOCCA.
006170     MOVE LOW-VALUES TO TWARDSW.
006180
006180     MOVE 'ST_MST_EMPLOYF COPYIC'  TO W-EIBDS.
006180     MOVE TWANUM                   TO T-M-BAS-EE-SSN.
006180
006180     EXEC SQL
006180          SELECT M_BAS_EE_EMPLOYER_NUM     ,
006180                 M_BAS_EE_SEC_REG_NUM      ,
006180                 M_BAS_EE_MAST_REC_CODE    ,
006180                 M_BAS_EE_LOC_CODE         ,
006180                 M_BAS_EE_ACT_CODE         ,
006180                 M_BAS_EE_SYS_ENTRY_DATE   ,
006180                 M_BAS_EE_SYS_ENTRY_SOURCE ,
006180                 M_BAS_EE_NEXT_ACT_DATE    ,
006180                 M_BAS_EE_LAST_ACCT_DATE   ,
006180                 M_BAS_EE_LAST_FM_DATE     ,
006180                 M_BAS_EE_BILL_FREQ_CODE   ,
006180                 M_BAS_EE_BILL_TYPE_CODE   ,
006180                 M_BAS_EE_BILL_DAY         ,
006180                 M_BAS_EE_PAY_FREQ_CODE    ,
006180                 M_BAS_EE_PAY_DAY          ,
006180                 M_BAS_EE_REG_EFF_DATE     ,
006180                 M_BAS_EE_BIRTH_DATE       ,
006180                 M_BAS_EE_DEATH_DATE       ,
006180                 M_BAS_EE_OCC_CODE         ,
006180                 M_BAS_EE_SEX_CODE         ,
006180                 M_BAS_EE_MAR_STAT_CODE    ,
006180                 M_BAS_EE_PIS_NUM          ,
006180                 M_BAS_EE_CIVIL_ID         ,
006180                 M_BAS_EE_NAT_CODE         ,
006180                 M_BAS_EE_NAT_CERT_NUM     ,
006180                 M_BAS_EE_NAT_EFF_DATE     ,
006180                 M_BAS_EE_DED_DED_NT_PD    ,
006180                 M_BAS_EE_TERM_ACT_DATE    ,
006180                 M_BAS_EE_20KD_1992        ,
006180                 M_BAS_EE_50KD_1992        ,
006180                 M_BAS_EE_CHILD_5KD_89     ,
006180                 M_BAS_EE_CHILD_10KD_89    ,
006180                 M_BAS_EE_CHILD_30KD_A89   ,
006180                 M_BAS_EE_CHILD_30KD_B89   ,
006180                 M_BAS_EE_FILING_NUMBER    ,
006180                 M_BAS_EE_CHLD_RETIRE    ,
006180                 M_BAS_EE_OVR_MAX_941231 ,
006180                 M_BAS_EE_BLOCK_CODE     ,
006180                 M_BAS_EE_FORM_REC_DATE ,
S45342                 M_BAS_EE_DROP_NAT_DATE,
S45165                 M_BAS_EE_CV_EXP_DATE
006180          INTO  :T-M-BAS-EE-EMPLOYER-NUM:EMPLER-IND   ,
006180                :T-M-BAS-EE-SEC-REG-NUM:REGNUM-IND    ,
006180                :T-M-BAS-EE-MAST-REC-CODE      ,
006180                :T-M-BAS-EE-LOC-CODE           ,
006180                :T-M-BAS-EE-ACT-CODE           ,
006180                :T-M-BAS-EE-SYS-ENTRY-DATE:ENTDT-IND ,
006180                :T-M-BAS-EE-SYS-ENTRY-SOURCE          ,
006180                :T-M-BAS-EE-NEXT-ACT-DATE             ,
006180                :T-M-BAS-EE-LAST-ACCT-DATE:LACTDT-IND ,
006180                :T-M-BAS-EE-LAST-FM-DATE:LASFMDT-IND  ,
006180                :T-M-BAS-EE-BILL-FREQ-CODE            ,
006180                :T-M-BAS-EE-BILL-TYPE-CODE            ,
006180                :T-M-BAS-EE-BILL-DAY                  ,
006180                :T-M-BAS-EE-PAY-FREQ-CODE             ,
006180                :T-M-BAS-EE-PAY-DAY                   ,
006180                :T-M-BAS-EE-REG-EFF-DATE:REGFDT-IND ,
006180                :T-M-BAS-EE-BIRTH-DATE              ,
006180                :T-M-BAS-EE-DEATH-DATE:DETHDT-IND   ,
006180                :T-M-BAS-EE-OCC-CODE                ,
006180                :T-M-BAS-EE-SEX-CODE                ,
006180                :T-M-BAS-EE-MAR-STAT-CODE           ,
006180                :T-M-BAS-EE-PIS-NUM                 ,
006180                :T-M-BAS-EE-CIVIL-ID                ,
006180                :T-M-BAS-EE-NAT-CODE                ,
006180                :T-M-BAS-EE-NAT-CERT-NUM:NCRTF-IND  ,
006180                :T-M-BAS-EE-NAT-EFF-DATE:NEFFDT-IND ,
006180                :T-M-BAS-EE-DED-DED-NT-PD           ,
006180                :T-M-BAS-EE-TERM-ACT-DATE:TRMDT-IND ,
006180                :T-M-BAS-EE-20KD-1992               ,
006180                :T-M-BAS-EE-50KD-1992               ,
006180                :T-M-BAS-EE-CHILD-5KD-89            ,
006180                :T-M-BAS-EE-CHILD-10KD-89           ,
006180                :T-M-BAS-EE-CHILD-30KD-A89          ,
006180                :T-M-BAS-EE-CHILD-30KD-B89          ,
006180                :T-M-BAS-EE-FILING-NUMBER           ,
006180                :T-M-BAS-EE-CHLD-RETIRE             ,
006180                :T-M-BAS-EE-OVR-MAX-941231          ,
006180                :T-M-BAS-EE-BLOCK-CODE              ,
006180                :T-M-BAS-EE-FORM-REC-DATE:FRMDT-IND ,
S45342                :T-M-BAS-EE-DROP-NAT-DATE:NDRPDT-IND,
S45165                :T-M-BAS-EE-CV-EXP-DATE:CEXPDT-IND
006180          FROM ST_MST_EMPLOYEE_BASIC
006180          WHERE     M_BAS_EE_SSN = :T-M-BAS-EE-SSN
006180           ORDER BY M_BAS_EE_SSN   ASC
006180                    FETCH FIRST 1 ROW ONLY
006180          END-EXEC
006180
006180          PERFORM DB2-ERROR.
006180          IF SQLCODE  =  +0
006180             NEXT SENTENCE
C06180          ELSE
006180          IF SQLCODE  = +100
006380             MOVE '›·„·«Ï·⁄œÊÃÊ„—Ì€·Ã”·«' TO ERRMSG1O
006390             MOVE HIGH-VALUES             TO TWAERROR
006400             GO TO C0300-INQUIRY-EXIT
C06180          ELSE
006380             MOVE 'ÂÌ”«”√  «‰«Ì» ›·„ ⁄„ ·„«⁄ ·«» √ÿŒ' TO ERRMSG1O
006390             MOVE HIGH-VALUES                         TO TWAERROR
006400             GO TO C0300-INQUIRY-EXIT
006180          END-IF.
006180
006190*DB2 MOVE  10  TO  TWATRLNO.
006200*DB2 MOVE  01  TO  TWARELNO.
006210
006220*DB2 CALL 'CCOBADDR'        USING  SFCOCCUR
006230*DB2                               OCCR-TBL-PTR.
006240*DB2 MOVE OCCR-TBL-PTR-R  TO  TWAOCCA.
006250
006260*DB2 PERFORM S0100-LOCATE-TRAILER.
006270
006280**   MOVE  TWATLADR     TO   BASIC-PTR-R.
006290*DB2 MOVE  TWAGETAD     TO   BASIC-PTR-R.
006300*DB2 MOVE  BASIC-PTR-R  TO  TWA-BASIC-DATA-BAR.                   00008100
006310*DB2
006320*DB2 SET   ADDRESS  OF  MASTER-RECORD-BASIC-DATA  TO  BASIC-PTR.
006330
006340*DB2 IF  TWARTCD  EQUAL TO ZERO
006350*DB2
006360*DB2     NEXT SENTENCE
006370*DB2 ELSE
006380*DB2     MOVE '›·„·«Ï·⁄œÊÃÊ„—Ì€·Ã”·«' TO ERRMSG1O
006390*DB2     MOVE HIGH-VALUES TO TWAERROR
006400*DB2     GO TO C0300-INQUIRY-EXIT.
006410
006420     PERFORM C0301-SET-REC-TYPE.
006430
006440     IF  EE-RECORD
006460       PERFORM C0550-BUILD-BANK-INFO
006470     ELSE
006480       NEXT SENTENCE.
006490
006500     PERFORM  C0600-FLIP-AND-BUILD-SCREEN.
006510
006520     PERFORM  C0400-BUILD-NAME.
006530
006540     PERFORM  C0310-FORMAT-SCREEN-BASIC-DATA.
006550
006560 C0300-INQUIRY-EXIT.  EXIT.
006570     EJECT
006580*****************************************************************
006590*                   C0301-SET-REC-TYPE                          *
006600*                                                               *
006610*****************************************************************
006630 C0301-SET-REC-TYPE  SECTION.
006640                                                                  00007750
006650     MOVE  LOW-VALUES  TO  TWA-SSN-TYPE                           00007760
006660                           TWA-BENEF-SW.                          00007770
006670                                                                  00007780
006680     MOVE T-M-BAS-EE-MAST-REC-CODE  TO TWA-MAST-CODE.             00007790
006680*DB2 MOVE T-M-BAS-EE-MAST-REC-CODE  (1)  TO TWA-MAST-CODE.        00007790
006690                                                                  00007780
006700     IF  TWA-MAST-CODE   EQUAL  TB101-MAST-ER                     00007800
006710                                                                  00007780
006720         MOVE  'R'  TO  TWA-SSN-TYPE                              00007810
006730     ELSE                                                         00007820
006740         IF  TWA-MAST-CODE  EQUAL TB101-MAST-EE             OR    00007830
006750             TWA-MAST-CODE  EQUAL TB101-MAST-EE-PENR        OR    00007840
006760             TWA-MAST-CODE  EQUAL TB101-MAST-EE-BENEF       OR    00007850
006770             TWA-MAST-CODE  EQUAL TB101-MAST-EE-VOL-INS     OR    00007860
006780             TWA-MAST-CODE  EQUAL TB101-MAST-BENEF-SELF-EMP OR    SMR40051
006790             TWA-MAST-CODE  EQUAL TB101-MAST-SELF-EMP       OR    SMR40051
006800             TWA-MAST-CODE  EQUAL  TB101-LEAV-IND-EE              SMR40265
006810             MOVE  'E'  TO  TWA-SSN-TYPE                          00007870
006820         ELSE                                                     SMR90539
006830             NEXT  SENTENCE.                                      00007890
006840                                                                  00007780
006850     IF  TWA-MAST-CODE EQUAL TB101-MAST-EE-PENR    OR             SMR90539
006860         TWA-MAST-CODE EQUAL TB101-MAST-PENR       OR             SMR90539
006870         TWA-MAST-CODE EQUAL TB101-MAST-PENR-BENEF                SMR90539
006880                                                                  00007780
006890         MOVE 'P' TO TWA-SSN-TYPE1                                SMR90539
006900     ELSE                                                         SMR90539
006910         NEXT SENTENCE.                                           SMR90539
006920                                                                  00007780
006930     IF  TWA-MAST-CODE  EQUAL  TB101-MAST-EE-BENEF        OR      00007900
006940         TWA-MAST-CODE  EQUAL  TB101-MAST-PENR-BENEF      OR      00007910
006950         TWA-MAST-CODE  EQUAL  TB101-MAST-BENEF           OR      00007920
006960         TWA-MAST-CODE  EQUAL  TB101-MAST-BENEF-SELF-EMP  OR      00007930
006970         TWA-MAST-CODE  EQUAL  TB101-MAST-BENEF-VOL-INS           00007940
006980                                                                  00007780
006990         MOVE 'B' TO TWA-BENEF-SW                                 00007950
007000     ELSE                                                         00007960
007010         NEXT SENTENCE.                                           00007970
007020                                                                  00007980
007030 C0301-SET-REC-TYPE-EXIT.  EXIT.
007040     EJECT
007050***************************************************************** 00008010
007060*                   C0310-FORMAT-SCREEN-BASIC-DATA              * 00008020
007070***************************************************************** 00008030
007080*    THIS SECTION:                                              * 00008040
007090*           FORMATS SCREEN FIELDS FOR BASIC DATA                * 00008050
007100***************************************************************** 00008060
007120 C0310-FORMAT-SCREEN-BASIC-DATA  SECTION.                         00008080
007130                                                                  00008090
007140**   MOVE TWATLADR TO M-BAS-DATA-BAR.                             00008100
007150**   MOVE TWATLADR TO TWA-BASIC-DATA-BAR.                         00008110
007160*    MOVE BASIC-PTR-R TO                                          00008100
007170*                        TWA-BASIC-DATA-BAR.                      00008110
007180                                                                  00008120
007190     MOVE TWA-EMP-NUM-OUT        TO SSNO.                         00008130
007200                                                                  00008140
007210     MOVE TWA-SYS-ENTRY-YYYY-OUT TO SENTRYYO.                     00008150
007220     MOVE TWA-SYS-ENTRY-MM-OUT   TO SENTRMMO.                     00008160
007230     MOVE TWA-SYS-ENTRY-DD-OUT   TO SENTRDDO.                     00008170
007240                                                                  00008180
007250     MOVE TWA-NEXT-ACT-YYYY-OUT  TO NXACTYYO.                     00008190
007260     MOVE TWA-NEXT-ACT-MM-OUT    TO NXACTMMO.                     00008200
007270     MOVE TWA-NEXT-ACT-DD-OUT    TO NXACTDDO.                     00008210
007280                                                                  00008220
007290     MOVE TWA-LAST-ACCT-YYYY-OUT TO LACCTYYO.                     00008230
007300     MOVE TWA-LAST-ACCT-MM-OUT   TO LACCTMMO.                     00008240
007310     MOVE TWA-LAST-ACCT-DD-OUT   TO LACCTDDO.                     00008250
007320                                                                  00008260
007330     MOVE TWA-LAST-FM-YYYY-OUT   TO LFMYYO.                       00008270
007340     MOVE TWA-LAST-FM-MM-OUT     TO LFMMMO.                       00008280
007350     MOVE TWA-LAST-FM-DD-OUT     TO LFMDDO.                       00008290
007360                                                                  00008300
007370     MOVE TWA-REG-EFF-YYYY-OUT   TO REGYYO.                       00008310
007380     MOVE TWA-REG-EFF-MM-OUT     TO REGMMO.                       00008320
007390     MOVE TWA-REG-EFF-DD-OUT     TO REGDDO.                       00008330
007400                                                                  00008340
007410     MOVE TWA-PAY-DAY-OUT        TO DTPAYMTO.                     00008350
007420                                                                  00008370
007430     MOVE TWA-BIRTH-YYYY-OUT     TO DOBYYO.                       00008380
007440     MOVE TWA-BIRTH-MM-OUT       TO DOBMMO.                       00008390
007450     MOVE TWA-BIRTH-DD-OUT       TO DOBDDO.                       00008400
007460                                                                  00008410
007470     MOVE TWA-DEATH-YYYY-OUT     TO DEATHYYO.                     00008420
007480     MOVE TWA-DEATH-MM-OUT       TO DEATHMMO.                     00008430
007490     MOVE TWA-DEATH-DD-OUT       TO DEATHDDO.                     00008440
007500                                                                  00008450

S45342     MOVE FUNCTION REVERSE(TWA-DROP-NATYYYY-IN) TO NTDRPYYO.
S45342     MOVE FUNCTION REVERSE(TWA-DROP-NATMM-IN) TO NTDRPMMO.
S45342     MOVE FUNCTION REVERSE(TWA-DROP-NATDD-IN) TO NTDRPDDO.


S45165     MOVE FUNCTION REVERSE(TWA-CIV-EXPYYYY-IN) TO CIVEXPYO.
S45165     MOVE FUNCTION REVERSE(TWA-CIV-EXPMM-IN) TO   CIVEXPMO.
S45165     MOVE FUNCTION REVERSE(TWA-CIV-EXPDD-IN) TO   CIVEXPDO.

007510     MOVE TWA-NAT-EFF-YYYY-OUT   TO NTISSYYO.                     00008460
007520     MOVE TWA-NAT-EFF-MM-OUT     TO NTISSMMO.                     00008470
007530     MOVE TWA-NAT-EFF-DD-OUT     TO NTISSDDO.                     00008480
007540                                                                  00008490
007550     MOVE TWA-SOURCE-OUT         TO SOURCETO.                     00008500
007560     MOVE TWA-FORM-REC-YYYY-OUT  TO FORMRYYO.                     00008510
007570     MOVE TWA-FORM-REC-MM-OUT    TO FORMRMMO.                     00008520
007580     MOVE TWA-FORM-REC-DD-OUT    TO FORMRDDO.                     00008530
007590     MOVE TWA-CHILD-NO1-OUT      TO CHLD1NOO.                     SMR90539
007600     MOVE TWA-CHILD-NO2-OUT      TO CHLD2NOO.                     SMR90539
007610     MOVE TWA-CHILD-NO3-OUT      TO CHLD3NOO.                     SMR90539
007620     MOVE TWA-CHILD-NO4-OUT      TO CHLD4NOO.                     SMR90539
007630
007640     MOVE TWA-COLL-DAY-OUT       TO DATCOLLO.
007650     MOVE TWA-REC-TYPE-OUT       TO TYPRECO.
007660     MOVE TWA-ACTV-CODE-OUT      TO ACTCODEO.
007670     MOVE TWA-BILL-TYPE-OUT      TO TYPCOLLO.
007680     MOVE TWA-BILL-FREQ-OUT      TO FRQCOLLO.
007690     MOVE TWA-MAR-STAT-OUT       TO STATUSO.
007700     MOVE TWA-NAT-CODE-OUT       TO NATNLTYO.
007710     MOVE TWA-SEX-CODE-OUT       TO SEXO.
007720
007730     MOVE TWA-LOC-CODE-OUT       TO LOCO.
007740     MOVE TWA-OCC-CODE-OUT       TO BUSCODEO.
007750     MOVE TWA-PAY-FREQ-OUT       TO PAYFREQO.
007760     MOVE TWA-NAT-CERT-NUM-OUT   TO NTLCERTO.
007770     MOVE TWA-20KD-1992-OUT      TO KID20KDO.
007780     MOVE TWA-50KD-1992-OUT      TO KID50KDO.
007790
007800     MOVE TWA-FIL-NUM-OUT-9      TO FILING1O.                     SMR91553
007810     IF   TWA-FIL-NUM-IN-2  IS GREATER THAN ZERO                  SMR91553
007820          MOVE TWA-FIL-NUM-OUT-2 TO FILING2O                      SMR91553
007830          MOVE '/'               TO FILINGSO.                     SMR91553
007840
007850
007860     MOVE TWA-CHD-AFT-RET-OUT    TO CHLDRTOO.                     SMR43515
007850
007860     MOVE TWA-BLOCK-CODE-OUT     TO BLOCKCDO.                     SMR44432
007870
007880     IF  ER-RECORD
007890         NEXT SENTENCE
007900     ELSE
007910****     MOVE TWA-SPEC-ID-NUM-OUT    TO SPECNUMO.                 SMR42647
007920         MOVE TWA-SPEC-ID-NUM-OUT-12 TO SPECNUMO.                 SMR42647
007930         MOVE TWA-TRMAC-YYYY-OUT     TO TRMACYYO.
007940         MOVE TWA-TRMAC-MM-OUT       TO TRMACMMO.
007950         MOVE TWA-TRMAC-DD-OUT       TO TRMACDDO.
007960     SKIP3
007970     IF  EE-RECORD
007980         IF  TWA-BANKSW EQUAL LOW-VALUES                          00008820
007990             MOVE TWA-BANK-PAYEE-ACCT-NUM-OUT   TO ACCTNUMO       00008830
008000             MOVE TWA-BANK-BR-AGY-NUM-OUT       TO BANKNUMO       00008840
008010             MOVE TWA-BANK-PAY-METH-CODE-OUT    TO TYPPYMTO       00008850
008020         ELSE                                                     00008860
008030             NEXT SENTENCE                                        00008870
008040     ELSE                                                         00008880
008050         NEXT SENTENCE.                                           00008890
008060                                                                  00008900
008070     MOVE TWA-ER-REG-NUM-OUT   TO REGNUMO.                        00008910
008080     MOVE TWA-PIS-NUM-OUT      TO PISNUMO.                        00008950
008090     MOVE TWA-DED-NOT-PD-OUT   TO DEDNTPDO.                       00008960
008100                                                                  00008970
008110 C0310-FORMAT-EXIT.  EXIT.                                        00008980
008120     EJECT                                                        00008990
008130*****************************************************************
008140*                      C0400-BUILD-NAME                         *
008150*****************************************************************
008160*       THIS SECTION:                                           *
008170*            -BUILDS THE NAME                                   *
008180*            -LINKS  TO TRAILER EXPAND                          *
008190*            -CHECKS THE RETURN CODE                            *
008200*****************************************************************
008220 C0400-BUILD-NAME  SECTION.
008230
008480       PERFORM C0410-EXPAND-EE-NAME.
008230
008240*DB2 MOVE 20          TO TWATRLNO.
008250*DB2 MOVE HIGH-VALUES TO TWARDSW.
008260
008270*DB2 CALL 'CCOBADDR'        USING  SFCOCCUR
008280*DB2                               OCCR-TBL-PTR.
008290*DB2 MOVE OCCR-TBL-PTR-R TO  TWAOCCA.
008300
008310***************
008320* LINK TO TRLR EXPAND
008330***************
008340
008350*DB2 MOVE 'OOP0009P' TO TCAPCPI.
008360*DB2
008370*DB2 EXEC  CICS  LINK  PROGRAM (TCAPCPI)
008380*DB2                     RESP  (W-RESP)
008390*DB2 END-EXEC.
008400*DB2
008410*DB2 IF  W-RESP = DFHRESP (NORMAL)
008420*DB2     IF  TWARTCD EQUAL ZERO
008430*DB2         MOVE TWAGETAD TO  NAME-PTR-R
008440*DB2         SET  ADDRESS OF MASTER-RECORD-NAME TO  NAME-PTR
008450*DB2         IF  ER-RECORD
008460*DB2             PERFORM C0411-EXPAND-ER-NAME
008470*DB2         ELSE
008480*DB2             PERFORM C0410-EXPAND-EE-NAME
008490*DB2     ELSE
008500*DB2         IF  TWARTCD EQUAL +4
008510*DB2             MOVE '401' TO TCADCDC1
008520*DB2             PERFORM S0400-DUMP-XCTL
008530*DB2         ELSE
008540*DB2             IF  TWARTCD EQUAL +8
008550*DB2                 MOVE 'œÊÃÊ„—Ì€„”··ﬁÕ·„·«·Ã”·«' TO NAMEO
008560*DB2             ELSE
008570*DB2                 MOVE '402' TO TCADCDC1
008580*DB2                 PERFORM S0400-DUMP-XCTL
008590*DB2 ELSE
008600*DB2     MOVE '403' TO TCADCDC1
008610*DB2     PERFORM S0400-DUMP-XCTL.
008630
008640
008650 C0400-NAME-EXIT.  EXIT.
008660     EJECT
008670*****************************************************************
008680*                   C0410-EXPAND-EE-NAME                        *
008690*****************************************************************
008700*   THIS SECTION:                                               *
008710*      -BUILDS THE PARMLIST FOR EXPAND NAME MODULE TO EXPAND    *
008720*       INDIVIDUAL NAME                                         *
008730*      -LINKS TO THE MODULE                                     *
008740*      -CHECKS RETURN CODE - IF RC NOT ZERO, MESSAGE IS SENT TO *
008750*       THE SCREEN AND CONTROL RETURNS TO CICS                  *
008760*****************************************************************
008780 C0410-EXPAND-EE-NAME  SECTION.
008790
008800     MOVE 'ST_MST_NAME'  TO W-EIBDS.
008800     MOVE TWANUM         TO T-M-NAME-SSN.
008790
008800     EXEC SQL
008800          SELECT M_NAME_TRLR_SEQ_NUM       ,
008800                 M_NAME_TYPE_CODE          ,
008800                 M_NAME_FIRST              ,
008800                 M_NAME_MIDDLE             ,
008800                 M_NAME_THIRD              ,
008800                 M_NAME_FAMILY
008800          INTO  :T-M-NAME-TRLR-SEQ-NUM     ,
008800                :T-M-NAME-TYPE-CODE        ,
008800                :T-M-NAME-FIRST            ,
008800                :T-M-NAME-MIDDLE           ,
008800                :T-M-NAME-THIRD:THRD-IND   ,
008800                :T-M-NAME-FAMILY
008800          FROM ST_MST_NAME
008800          WHERE     M_NAME_SSN = :T-M-NAME-SSN
008800           AND      M_NAME_TYPE_CODE = '20'
008800           ORDER BY M_NAME_TRLR_SEQ_NUM ASC
008800                    FETCH FIRST 1 ROW ONLY
008800          END-EXEC
008800
008800          PERFORM DB2-ERROR
008800          IF SQLCODE = +0
008800             MOVE FUNCTION REVERSE
008800                            (T-M-NAME-FAMILY) TO W-NAME4
008800             IF THRD-IND = +0
008800                MOVE FUNCTION REVERSE
008800                            (T-M-NAME-THIRD)  TO W-NAME3
008800             END-IF
008800             MOVE FUNCTION REVERSE
008800                            (T-M-NAME-MIDDLE) TO W-NAME2
008800             MOVE FUNCTION REVERSE
008800                            (T-M-NAME-FIRST)  TO W-NAME1
008800             MOVE W-NAME  TO NAMEO
008800          ELSE
008800          IF SQLCODE = +100
008800             MOVE 'ST_MST_NAME' TO  W-EIBDS
008800             MOVE TWANUM        TO  T-M-NAME-SSN
008790
008800             EXEC SQL
008800               SELECT M_NAME_TRLR_SEQ_NUM       ,
008800                      M_NAME_TYPE_CODE          ,
008800                      M_NAME_FIRST              ,
008800                      M_NAME_MIDDLE             ,
008800                      M_NAME_THIRD              ,
008800                      M_NAME_FAMILY
008800               INTO  :T-M-NAME-TRLR-SEQ-NUM     ,
008800                     :T-M-NAME-TYPE-CODE        ,
008800                     :T-M-NAME-FIRST            ,
008800                     :T-M-NAME-MIDDLE           ,
008800                     :T-M-NAME-THIRD:THRD-IND   ,
008800                     :T-M-NAME-FAMILY
008800               FROM ST_MST_NAME
008800               WHERE     M_NAME_SSN = :T-M-NAME-SSN
008800               ORDER BY M_NAME_TRLR_SEQ_NUM ASC
008800                    FETCH FIRST 1 ROW ONLY
008800             END-EXEC
008800
008800             PERFORM DB2-ERROR
008800             IF SQLCODE = +0
008800                MOVE FUNCTION REVERSE
008800                            (T-M-NAME-FAMILY) TO W-NAME4
008800                IF THRD-IND = +0
008800                   MOVE FUNCTION REVERSE
008800                            (T-M-NAME-THIRD)  TO W-NAME3
008800                END-IF
008800                MOVE FUNCTION REVERSE
008800                            (T-M-NAME-MIDDLE) TO W-NAME2
008800                MOVE FUNCTION REVERSE
008800                            (T-M-NAME-FIRST)  TO W-NAME1
008800                MOVE W-NAME  TO NAMEO
008800             ELSE
008800             IF SQLCODE = +100
008800                MOVE  HIGH-VALUES               TO TWAERROR
008870                MOVE 'œÊÃÊ„ —Ì€ „”·«'           TO ERRMSG1O
008800                PERFORM D0000-FINAL
008800             ELSE
008800                MOVE  HIGH-VALUES               TO TWAERROR
008870                MOVE '„”·« ›·„ ⁄„ ·„«⁄ ·«» √ÿŒ' TO ERRMSG1O
008800                PERFORM D0000-FINAL
008800          ELSE
008800             MOVE  HIGH-VALUES               TO TWAERROR
008870             MOVE '„”·« ›·„ ⁄„ ·„«⁄ ·«» √ÿŒ' TO ERRMSG1O
008800             PERFORM D0000-FINAL
008800          END-IF.
008800
008810*DB2 CALL 'CCOBADDR'  USING   EXPAND-AREA   NA-EXP-PTR-R.         00025040
008820*DB2 MOVE   NA-EXP-PTR-R  TO  TWAMSTA.
008830*DB2                                                              00025060
008840*DB2 SET I-NAME-NDX TO +1.
008850*DB2 SEARCH MAST-NAME-TRLR
008860*DB2   AT END
008870*DB2      MOVE 'œÊÃÊ„—Ì€„”·«' TO NAMEO
008880*DB2      GO TO C0410-EXPAND-EE-NAME-EXIT
008890*DB2   WHEN M-NAME-TYPE-CODE (I-NAME-NDX) EQUAL
008900*DB2                                   TB201-NAME-IND-REC-HOLD
008910*DB2      NEXT SENTENCE.
008920*DB2
008930*DB2 MOVE 'N' TO EXPAND-TYPE.
008940*DB2 MOVE M-NAME-LTH (I-NAME-NDX) TO EXPAND-LEN.
008950*DB2 MOVE M-NAME-DATA-ELEMENTS (I-NAME-NDX) TO EXPAND-IN-NAME.
008960*DB2
008970*DB2 MOVE 'OOP9005P' TO TCAPCPI.
008980*DB2
008990*DB2 EXEC  CICS  LINK  PROGRAM (TCAPCPI)
009000*DB2                     RESP  (W-RESP)
009010*DB2 END-EXEC.
009020*DB2
009030*DB2 IF W-RESP = DFHRESP (NORMAL)
009040*DB2     IF  TWA-NA-RET-CODE EQUAL ZERO
009050*DB2         MOVE EXPAND-OUT-EE-NAME1 TO W-NAME4
009060*DB2         MOVE EXPAND-OUT-EE-NAME2 TO W-NAME3
009070*DB2         MOVE EXPAND-OUT-EE-NAME3 TO W-NAME2
009080*DB2         MOVE EXPAND-OUT-EE-NAME4 TO W-NAME1
009090*DB2         MOVE W-NAME              TO NAMEO
009100*DB2     ELSE
009110*DB2         IF  TWA-NA-RET-CODE EQUAL +4
009120*DB2             MOVE '411' TO TCADCDC1
009130*DB2             PERFORM S0400-DUMP-XCTL
009140*DB2         ELSE
009150*DB2             IF  TWA-NA-RET-CODE EQUAL +8
009160*DB2                 MOVE '412' TO ERRMSG2O
009170*DB2             ELSE
009180*DB2                 MOVE '413' TO TCADCDC1
009190*DB2                 PERFORM S0400-DUMP-XCTL
009200*DB2 ELSE
009210*DB2     MOVE '414' TO TCADCDC1
009220*DB2     PERFORM S0400-DUMP-XCTL.
009230*DB2
009240*DB2C0410-EXPAND-EE-NAME-EXIT.  EXIT.
009250*DB2 EJECT
009260*****************************************************************
009270*                  C0411-EXPAND-ER-NAME                         *
009280*****************************************************************
009290*   THIS SECTION:                                               *
009300*      -BUILDS THE PARMLIST FOR EXPAND NAME MODULE TO EXPAND    *
009310*       EMPLOYER NAME                                           *
009320*      -LINKS TO THE MODULE                                     *
009330*      -CHECKS RETURN CODE - IF RC NOT ZERO, MESSAGE IS SENT TO *
009340*       THE SCREEN AND CONTROL RETURNS TO ORP1500P.             *
009350*****************************************************************
009360
009370*DB2 C0411-EXPAND-ER-NAME  SECTION.
009380*DB2
009390*DB2
009400*DB2 CALL 'CCOBADDR'  USING   EXPAND-AREA   NA-EXP-PTR-R.         00025040
009410*DB2 MOVE   NA-EXP-PTR-R  TO  TWAMSTA.
009420*DB2
009430*DB2 SEARCH MAST-NAME-TRLR
009440*DB2   AT END
009450*DB2      MOVE 'œÊÃÊ„ —Ì€ „”·«' TO NAMEO
009460*DB2      GO TO C0411-EXPAND-ER-NAME-EXIT
009470*DB2   WHEN M-NAME-TYPE-CODE (I-NAME-NDX) EQUAL
009480*DB2                                      TB201-NAME-ER-NAME
009490*DB2      NEXT SENTENCE.
009500*DB2
009510*DB2 MOVE 'K' TO EXPAND-TYPE.
009520*DB2 MOVE M-NAME-LTH (I-NAME-NDX) TO EXPAND-LEN.
009530*DB2 MOVE M-NAME-DATA-ELEMENTS (I-NAME-NDX) TO EXPAND-IN-NAME.
009540*DB2 MOVE 'P' TO EXPAND-OUT-TYPE.
009550*DB2 MOVE +64 TO EXPAND-OUT-LENGTH.
009560*DB2
009570*DB2 MOVE 'OOP9005P' TO TCAPCPI.
009580*DB2
009590*DB2 EXEC  CICS  LINK  PROGRAM (TCAPCPI)
009600*DB2                     RESP  (W-RESP)
009610*DB2 END-EXEC.
009620*DB2
009630*DB2 IF W-RESP = DFHRESP (NORMAL)
009640*DB2     IF  TWA-NA-RET-CODE EQUAL ZERO
009650*DB2         MOVE EXPAND-OUT-ER-NAME TO NAMEO
009660*DB2     ELSE
009670*DB2         IF  TWA-NA-RET-CODE EQUAL +4
009680*DB2             MOVE '415' TO TCADCDC1
009690*DB2             PERFORM S0400-DUMP-XCTL
009700*DB2         ELSE
009710*DB2             IF  TWA-NA-RET-CODE EQUAL +8
009720*DB2                 MOVE '416' TO ERRMSG2O
009730*DB2             ELSE
009740*DB2                 MOVE '417' TO TCADCDC1
009750*DB2                 PERFORM S0400-DUMP-XCTL
009760*DB2 ELSE
009770*DB2     MOVE '418' TO TCADCDC1
009780*DB2     PERFORM S0400-DUMP-XCTL.
009790*DB2
009800*DB2C0411-EXPAND-ER-NAME-EXIT.  EXIT.
009810*DB2 EJECT
009820***************************************************************** 00010800
009830*                    C0550-BUILD-BANK-INFO                      * 00010810
009840***************************************************************** 00010820
009850*   THIS SECTION:                                               * 00010830
009860*       -BUILDS THE INFORMATION OF THE BANK TRAILER.            * 00010840
009870*       -LINKS TO MODULE                                        * 00010850
009880*       -CHECKS RETURN CODE                                     * 00010860
009890***************************************************************** 00010870
009910 C0550-BUILD-BANK-INFO SECTION.                                   00010890
009920                                                                  00010900
009920     MOVE LOW-VALUES TO TWARDSW.                                  00010900
009920                                                                  00010900
009920     MOVE 'ST_MST_BANK_PAY'  TO  W-EIBDS.                         00010900
009920     MOVE TWANUM             TO  T-M-BANK-PAY-SSN.                00010900
009920                                                                  00010900
009920     EXEC SQL                                                     00010900
009920          SELECT  M_BANK_PAY_METHOD_CODE      ,                   00010900
009920                  M_BANK_PAY_PAYEE_ACCT_NUM   ,                   00010900
009920                  M_BANK_PAY_BANK_CODE        ,                   00010900
009920                  M_BANK_PAY_BANK_BR_AGY_CODE                     00010900
009920          INTO  :T-M-BANK-PAY-METHOD-CODE     ,                   00010900
009920                :T-M-BANK-PAY-PAYEE-ACCT-NUM  ,                   00010900
009920                :T-M-BANK-PAY-BANK-CODE       ,                   00010900
009920                :T-M-BANK-PAY-BANK-BR-AGY-CODE                    00010900
009920          FROM ST_MST_BANK_PAY                                    00010900
009920          WHERE                                                   00010900
009920              M_BANK_PAY_SSN   =  :T-M-BANK-PAY-SSN               00010900
009920                              AND                                 00010900
009920              M_BANK_PAY_END_DATE IS NULL                         00010900
009920          ORDER BY M_BANK_PAY_SSN ,                               00010900
009920                   M_BANK_PAY_TRLR_SEQ_NUM DESC                   00010900
009920           FETCH FIRST 1 ROW ONLY                                 00010900
009920                                                                  00010900
009920      END-EXEC.                                                   00010900
009920                                                                  00010900
009920          PERFORM DB2-ERROR.                                      00010900
009920          IF SQLCODE  =  +0                                       00010900
009920             NEXT SENTENCE                                        00010900
009920          ELSE                                                    00010900
009920          IF SQLCODE  =  +100                                     00010900
009920             MOVE 'œÊÃÊ„ —Ì€ ﬂ‰»·« ·Ã” '      TO ERRMSG1O         00010900
009920          ELSE                                                    00010900
008800             MOVE  HIGH-VALUES                TO TWAERROR
008870             MOVE 'ﬂ‰»·« ›·„ ⁄„ ·„«⁄ ·«» √ÿŒ' TO ERRMSG1O
008800             PERFORM D0000-FINAL
009920          END-IF.                                                 00010900
009920                                                                  00010900
009930*DB2 MOVE HIGH-VALUES TO TWA-BANKSW                               00010910
009940*DB2 MOVE 80 TO TWATRLNO.                                         00010920
009950
009960*DB2 CALL 'CCOBADDR'        USING  SFCOCCUR
009970*DB2                               OCCR-TBL-PTR.
009980*DB2 MOVE OCCR-TBL-PTR-R TO  TWAOCCA.
009990
010000                                                                  00010980
010010*DB2 MOVE 'OOP0009P' TO TCAPCPI.
010020*DB2
010030*DB2 EXEC  CICS  LINK  PROGRAM (TCAPCPI)
010040*DB2                     RESP  (W-RESP)
010050*DB2 END-EXEC.
010060*DB2
010070*DB2 IF W-RESP = DFHRESP (NORMAL)
010080*DB2     IF  TWARTCD EQUAL ZERO                                   00011000
010090*DB2         MOVE TWAGETAD TO  BANK-PTR-R
010100*DB2         SET  ADDRESS OF MASTER-RECORD-BANK  TO  BANK-PTR
010110*DB2         MOVE LOW-VALUES TO TWA-BANKSW                        00011020
010120*DB2     ELSE                                                     00011030
010130*DB2         IF  TWARTCD EQUAL +4                                 00011040
010140*DB2             MOVE '551' TO TCADCDC1                           011050
010150*DB2             PERFORM S0400-DUMP-XCTL                          00011060
010160*DB2         ELSE                                                 00011070
010170*DB2             IF  TWARTCD EQUAL +8                             00011080
010180*DB2                 MOVE 'œÊÃÊ„—Ì€ﬂ‰»··ﬁÕ·„·«·Ã”·«' TO ERRMSG3O  00011090
010190*DB2             ELSE                                             00011100
010200*DB2                 MOVE '552' TO TCADCDC1                       011110
010210*DB2                 PERFORM S0400-DUMP-XCTL                      00011120
010220*DB2 ELSE                                                         00011130
010230*DB2     MOVE '553' TO TCADCDC1                                   011140
010240*DB2     PERFORM S0400-DUMP-XCTL.                                 00011150
010250*DB2
010270                                                                  00011160
010280 C0550-BUILD-BANK-INFO-EXIT.                                      00011170
010280     EXIT.                                                        00011170
010290     EJECT                                                        00011180
010300***************************************************************** 00011190
010310*                    C0600-FLIP-AND-BUILD-SCREEN                * 00011200
010320***************************************************************** 00011210
010330*    THIS SECTION:                                              * 00011220
010340*       -FLIPS DATA TO BE PLACED ON SCREEN                      * 00011230
010350*       -PLACES FLIPED DATA IN OUTPUT MAP                       * 00011240
010360***************************************************************** 00011250
010380 C0600-FLIP-AND-BUILD-SCREEN  SECTION.                            00011270
010390                                                                  00025010
010400     CALL 'CCOBADDR'  USING  TWA-FLIP-A-LENGTH  FLIP-PTR.         00025040
010410     MOVE  FLIP-PTR   TO  TCTTE-ADDR4.                            00025040
010420                                                                  00025010
010430     MOVE FLIP-PTR    TO TCTTE-ADDR4.                             00011320
010440     ADD FLIP-PTR 2   GIVING TCTTE-ADDR2.                         00011330
010450     ADD FLIP-PTR 116 GIVING TCTTE-ADDR3.                         SMR90539
010460                                                                  00011350
010470     MOVE 114  TO  TWA-FLIP-A-LENGTH.                             SMR90539
010480                                                                  00011370
010490     IF ENTDT-IND = +0                                            00011380
010490       MOVE T-M-BAS-EE-SYS-ENTRY-DATE  TO W-DATE                  00011380
010540       PERFORM C9000-VALDT                                        00011430
010490       MOVE W-DATE9                    TO TWA-SYS-ENTRY-DATE-IN.  00011380
010490*DB2 MOVE M-BAS-DATA-SYS-ENTRY-DATE (1) TO TWA-SYS-ENTRY-DATE-IN. 00011380
010490                                                                  00011380
010500     MOVE T-M-BAS-EE-SYS-ENTRY-SOURCE  TO TWA-SOURCE-IN.          00011390
010500*DB2 MOVE M-BAS-DATA-SYS-ENTRY-SOURCE (1) TO TWA-SOURCE-IN.       00011390
010490                                                                  00011380
010510     MOVE T-M-BAS-EE-NEXT-ACT-DATE     TO W-DATE.                 00011400
010540     PERFORM C9000-VALDT.                                         00011430
010510     MOVE W-DATE9                      TO TWA-NEXT-ACT-DATE-IN.   00011400
010510*DB2 MOVE M-BAS-DATA-NEXT-ACT-DATE  (1) TO TWA-NEXT-ACT-DATE-IN.  00011400
010490                                                                  00011380
010520     IF LACTDT-IND = +0                                           00011410
010520       MOVE T-M-BAS-EE-LAST-ACCT-DATE TO W-DATE                   00011410
010540       PERFORM C9000-VALDT                                        00011430
010520       MOVE W-DATE9                   TO TWA-LAST-ACCT-DATE-IN.   00011410
010520*DB2 MOVE M-BAS-DATA-LAST-ACCT-DATE (1) TO TWA-LAST-ACCT-DATE-IN. 00011410
010490                                                                  00011380
010530     IF LASFMDT-IND = +0                                          00011420
010530       MOVE T-M-BAS-EE-LAST-FM-DATE  TO W-DATE                    00011420
010540       PERFORM C9000-VALDT                                        00011430
010530       MOVE W-DATE9                  TO TWA-LAST-FM-DATE-IN.      00011420
010530*DB2 MOVE M-BAS-DATA-LAST-FM-DATE   (1) TO TWA-LAST-FM-DATE-IN.   00011420
010490                                                                  00011380
010540     IF REGFDT-IND = +0                                           00011430
010540       MOVE T-M-BAS-EE-REG-EFF-DATE TO W-DATE                     00011430
010540       PERFORM C9000-VALDT                                        00011430
010540       MOVE W-DATE9                 TO TWA-REG-EFF-DATE-IN.       00011430
010540*DB2 MOVE M-BAS-DATA-REG-EFF-DATE   (1) TO TWA-REG-EFF-DATE-IN.   00011430
010490                                                                  00011380
010550     MOVE T-M-BAS-EE-LOC-CODE       TO TWA-LOC-CODE-IN.           00011440
010550*DB2 MOVE M-BAS-DATA-LOC-CODE       (1) TO TWA-LOC-CODE-IN.       00011440
010490                                                                  00011380
010560     MOVE T-M-BAS-EE-OCC-CODE       TO TWA-OCC-CODE-IN.           00011450
010560*DB2 MOVE M-BAS-DATA-OCC-CODE       (1) TO TWA-OCC-CODE-IN.       00011450
010490                                                                  00011380
010570     MOVE T-M-BAS-EE-PAY-FREQ-CODE  TO TWA-PAY-FREQ-IN.           00011460
010570*DB2 MOVE M-BAS-DATA-PAY-FREQ-CODE  (1) TO TWA-PAY-FREQ-IN.       00011460
010490                                                                  00011380
010580     MOVE T-M-BAS-EE-CIVIL-ID       TO W-CIVIL-ID.                00011470
010580     MOVE W-CIVIL-ID                TO TWA-SPEC-ID-NUM-IN-12.     00011470
010580*    MOVE T-M-BAS-EE-CIVIL-ID       TO TWA-SPEC-ID-NUM-IN.        00011470
010580*DB2 MOVE M-BAS-DATA-SPEC-ID-NUM    (1) TO TWA-SPEC-ID-NUM-IN.    00011470
010490                                                                  00011380
010590     IF NCRTF-IND = 0                                             00011480
010590       MOVE T-M-BAS-EE-NAT-CERT-NUM  TO TWA-NAT-CERT-NUM-IN.      00011480
010590*DB2 MOVE M-BAS-DATA-NAT-CERT-NUM   (1) TO TWA-NAT-CERT-NUM-IN.   00011480
010490                                                                  00011380
010610     IF DETHDT-IND = +0                                           00011500
010600       MOVE T-M-BAS-EE-DEATH-DATE TO W-DATE                       00011490
010540       PERFORM C9000-VALDT                                        00011430
010600       MOVE W-DATE9               TO TWA-DEATH-DATE-IN.           00011490
010600*DB2 MOVE M-BAS-DATA-DEATH-DATE     (1) TO TWA-DEATH-DATE-IN.     00011490
010490                                                                  00011380
010610     MOVE T-M-BAS-EE-BIRTH-DATE   TO W-DATE                       00011500
010540     PERFORM C9000-VALDT                                          00011430
010610     MOVE W-DATE9                 TO TWA-BIRTH-DATE-IN.           00011500
010610*DB2 MOVE M-BAS-DATA-BIRTH-DATE     (1) TO TWA-BIRTH-DATE-IN.     00011500
010490                                                                  00011380
010620     MOVE T-M-BAS-EE-PAY-DAY      TO TWA-PAY-DAY-IN.              00011510
010620*DB2 MOVE M-BAS-DATA-PAY-DAY        (1) TO TWA-PAY-DAY-IN.        00011510
010630
010640     IF  T-M-BAS-EE-CHLD-RETIRE  IS NUMERIC                       SMR43515
010650
010660         MOVE  T-M-BAS-EE-CHLD-RETIRE  TO  TWA-CHD-AFT-RET-IN     SMR43515
010670     ELSE                                                         SMR43515
010680         MOVE  ZEROES  TO  TWA-CHD-AFT-RET-IN.                    SMR43515
010650
010650
010650     IF  T-M-BAS-EE-BLOCK-CODE  IS NUMERIC                        SMR44432
010650                                                                  SMR44432
010650         MOVE  T-M-BAS-EE-BLOCK-CODE   TO  TWA-BLOCK-CODE-IN      SMR44432
010650     ELSE                                                         SMR44432
010650         MOVE  ZEROES  TO  TWA-BLOCK-CODE-IN.                     SMR44432
010650
010650
010640*DB2 IF  M-BAS-DATA-CHLD-RETIRE (1) IS NUMERIC                    SMR43515
010660*DB2     MOVE  M-BAS-DATA-CHLD-RETIRE (1)  TO  TWA-CHD-AFT-RET-IN SMR43515
010670*DB2 ELSE                                                         SMR43515
010680*DB2     MOVE  ZEROES  TO  TWA-CHD-AFT-RET-IN.                    SMR43515
010690
010700     IF   T-M-BAS-EE-FILING-NUMBER   IS NOT NUMERIC               SMR91553
010710          MOVE ZEROES  TO TWA-FIL-NUM-IN                          SMR91553
010720     ELSE                                                         SMR91553
010730          MOVE T-M-BAS-EE-FILING-NUMBER  TO TWA-FIL-NUM-IN.       SMR91553
010740
010700*DB2 IF   M-BAS-DATA-FILING-NUMBER  (1) IS NOT NUMERIC            SMR91553
010710*DB2      MOVE ZEROES  TO TWA-FIL-NUM-IN                          SMR91553
010720*DB2 ELSE                                                         SMR91553
010730*DB2      MOVE M-BAS-DATA-FILING-NUMBER  (1) TO TWA-FIL-NUM-IN.   SMR91553
010740
010750
010760     IF   T-M-BAS-EE-20KD-1992       IS GREATER THAN ZERO         00011520
010770        MOVE T-M-BAS-EE-20KD-1992      TO TWA-20KD-1992-IN        00011530
010780     ELSE                                                         00011540
010790        MOVE ZEROS                     TO TWA-20KD-1992-IN.       00011550
010800
010760*DB2 IF   M-BAS-DATA-20KD-1992      (1) IS NUMERIC                00011520
010770*DB2    MOVE M-BAS-DATA-20KD-1992      (1) TO TWA-20KD-1992-IN    00011530
010780*DB2 ELSE                                                         00011540
010790*DB2    MOVE ZEROS                         TO TWA-20KD-1992-IN.   00011550
010800
010810     IF   T-M-BAS-EE-50KD-1992       IS GREATER THAN ZERO         00011560
010820         MOVE T-M-BAS-EE-50KD-1992      TO TWA-50KD-1992-IN       00011570
010830     ELSE                                                         00011580
010840         MOVE ZEROS                     TO TWA-50KD-1992-IN.      00011590
010850
010810*DB2 IF   M-BAS-DATA-50KD-1992      (1) IS NUMERIC                00011560
010820*DB2     MOVE M-BAS-DATA-50KD-1992   (1)    TO TWA-50KD-1992-IN   00011570
010830*DB2 ELSE                                                         00011580
010840*DB2     MOVE ZEROS                         TO TWA-50KD-1992-IN.  00011590
010850
010860
010870     IF  RECFDT-IND = +0                                          00011600
010890        MOVE  T-M-BAS-EE-FORM-REC-DATE   TO   W-DATE              00011620
010540        PERFORM C9000-VALDT                                       00011430
010890        MOVE  W-DATE9                    TO TWA-FORM-REC-DATE-IN. 00011620
010870*DB2 IF  M-BAS-DATA-FORM-REC-DATE (1)  IS NUMERIC  AND            00011600
010880*DB2     M-BAS-DATA-FORM-REC-DATE (1)  GREATER THAN ZERO          00011610
010890*DB2     MOVE M-BAS-DATA-FORM-REC-DATE (1)                        00011620
010900*DB2                                    TO TWA-FORM-REC-DATE-IN   00011630
010910*DB2 ELSE                                                         00011640
010920*DB2     MOVE ZEROS TO TWA-FORM-REC-DATE-IN.                      00011650
010930                                                                  00011660
010940     IF  T-M-BAS-EE-CHILD-5KD-89  IS GREATER THAN ZERO            SMR90539
010950     THEN                                                         SMR90539
010960         MOVE T-M-BAS-EE-CHILD-5KD-89  TO TWA-CHILD-NO1-IN        SMR90539
010970     ELSE                                                         SMR90539
010980         MOVE ZEROES  TO  TWA-CHILD-NO1-IN.                       SMR90539
010990                                                                  SMR90539
010940*DB2 IF  M-BAS-DATA-CHILD-5KD-89 (1) IS NUMERIC                   SMR90539
010950*DB2 THEN                                                         SMR90539
010960*DB2     MOVE M-BAS-DATA-CHILD-5KD-89 (1) TO TWA-CHILD-NO1-IN     SMR90539
010970*DB2 ELSE                                                         SMR90539
010980*DB2     MOVE ZEROES  TO  TWA-CHILD-NO1-IN.                       SMR90539
010990                                                                  SMR90539
011000     IF  T-M-BAS-EE-CHILD-10KD-89  IS GREATER THAN ZERO           SMR90539
011010     THEN                                                         SMR90539
011020         MOVE T-M-BAS-EE-CHILD-10KD-89  TO TWA-CHILD-NO2-IN       SMR90539
011030     ELSE                                                         SMR90539
011040         MOVE ZEROES  TO  TWA-CHILD-NO2-IN.                       SMR90539
011050                                                                  SMR90539
011000*DB2 IF  M-BAS-DATA-CHILD-10KD-89 (1) IS NUMERIC                  SMR90539
011010*DB2 THEN                                                         SMR90539
011020*DB2     MOVE M-BAS-DATA-CHILD-10KD-89 (1) TO TWA-CHILD-NO2-IN    SMR90539
011030*DB2 ELSE                                                         SMR90539
011040*DB2     MOVE ZEROES  TO  TWA-CHILD-NO2-IN.                       SMR90539
011050                                                                  SMR90539
011060     IF  T-M-BAS-EE-CHILD-30KD-A89  IS GREATER THAN ZERO          SMR90539
011070     THEN                                                         SMR90539
011080         MOVE T-M-BAS-EE-CHILD-30KD-A89  TO TWA-CHILD-NO3-IN      SMR90539
011090     ELSE                                                         SMR90539
011100         MOVE ZEROES  TO  TWA-CHILD-NO3-IN.                       SMR90539
011050                                                                  SMR90539
011060*DB2 IF  M-BAS-DATA-CHILD-30KD-A89 (1) IS NUMERIC                 SMR90539
011070*DB2 THEN                                                         SMR90539
011080*DB2     MOVE M-BAS-DATA-CHILD-30KD-A89 (1) TO TWA-CHILD-NO3-IN   SMR90539
011090*DB2 ELSE                                                         SMR90539
011100*DB2     MOVE ZEROES  TO  TWA-CHILD-NO3-IN.                       SMR90539
011110                                                                   0011720
011120     IF  T-M-BAS-EE-CHILD-30KD-B89  IS GREATER THAN ZERO          SMR90539
011130     THEN                                                         SMR90539
011140         MOVE T-M-BAS-EE-CHILD-30KD-B89  TO TWA-CHILD-NO4-IN      SMR90539
011150     ELSE                                                         SMR90539
011160         MOVE ZEROES  TO  TWA-CHILD-NO4-IN.                       SMR90539
011170                                                                   0011720
011120*DB2 IF  M-BAS-DATA-CHILD-30KD-B89 (1) IS NUMERIC                 SMR90539
011130*DB2 THEN                                                         SMR90539
011140*DB2     MOVE M-BAS-DATA-CHILD-30KD-B89 (1) TO TWA-CHILD-NO4-IN   SMR90539
011150*DB2 ELSE                                                         SMR90539
011160*DB2     MOVE ZEROES  TO  TWA-CHILD-NO4-IN.                       SMR90539
011170                                                                   0011720
011180     IF LACTDT-IND = +0                                           00011730
011180       MOVE T-M-BAS-EE-LAST-ACCT-DATE TO W-DATE                   00011730
010540       PERFORM C9000-VALDT                                        00011430
011180       MOVE W-DATE9                   TO TWA-LAST-ACCT-DATE-IN.   00011730
011180*DB2 MOVE M-BAS-DATA-LAST-ACCT-DATE (1) TO TWA-LAST-ACCT-DATE-IN. 00011730
011190                                                                  00011740
011200     IF  TWA-BANKSW EQUAL LOW-VALUES                              00011750
011210                                                                  00011760
011220*DB2     MOVE M-BANK-PAY-BANK-BR-AGY-CODE (1)                     00011770
011220         MOVE T-M-BANK-PAY-BANK-BR-AGY-CODE                       00011770
011230                                TO  TWA-BANK-BR-AGY-NUM-IN        00011780
011240                                                                  00011790
011250*DB2     MOVE M-BANK-PAY-PAYEE-ACCT-NUM (1)                       00011820
011250         MOVE T-M-BANK-PAY-PAYEE-ACCT-NUM                         00011820
011260***ZAI                          TO  W-BANK-ACC-NUM-ALL            SMR93306
011270***ZAI   MOVE W-BANK-ACC-NUM                                      SMR93306
011280                                TO  TWA-BANK-PAYEE-ACCT-NUM-IN    00011850
011290                                                                  00011860
011300*DB2     MOVE M-BANK-PAY-METHOD-CODE (1)                          00011870
011300         MOVE T-M-BANK-PAY-METHOD-CODE                            00011870
011310                                TO  TWA-BANK-PAY-METH-CODE-IN     00011880
011320     ELSE                                                         00011890
011330         NEXT SENTENCE.                                           00011900
011340                                                                  00011910
011350     IF T-M-BAS-EE-DED-DED-NT-PD  IS NUMERIC                      00011920
011360        MOVE T-M-BAS-EE-DED-DED-NT-PD  TO TWA-DED-NOT-PD-IN       00011930
011370     ELSE                                                         00011940
011380        MOVE ZEROS TO TWA-DED-NOT-PD-IN.                          00011950
011390                                                                  00011960
011350*DB2 IF M-BAS-DATA-DED-DED-NT-PD (1) IS NUMERIC                   00011920
011360*DB2    MOVE M-BAS-DATA-DED-DED-NT-PD (1) TO TWA-DED-NOT-PD-IN    00011930
011370*DB2 ELSE                                                         00011940
011380*DB2    MOVE ZEROS TO TWA-DED-NOT-PD-IN.                          00011950
011390                                                                  00011960
011400     MOVE T-M-BAS-EE-MAST-REC-CODE     TO TWA-REC-TYPE-IN.        00011970
011400*DB2 MOVE M-BAS-DATA-MAST-REC-CODE     (1) TO TWA-REC-TYPE-IN.    00011970
010490                                                                  00011380
011410     MOVE T-M-BAS-EE-ACT-CODE          TO TWA-ACTV-CODE-IN.       00011980
011410*DB2 MOVE M-BAS-DATA-ACT-CODE          (1) TO TWA-ACTV-CODE-IN.   00011980
010490                                                                  00011380
011420     MOVE T-M-BAS-EE-BILL-TYPE-CODE    TO TWA-BILL-TYPE-IN.       00011990
011420*DB2 MOVE M-BAS-DATA-BILL-TYPE-CODE    (1) TO TWA-BILL-TYPE-IN.   00011990
010490                                                                  00011380
011430     MOVE T-M-BAS-EE-BILL-FREQ-CODE    TO TWA-BILL-FREQ-IN.       00012000
011430*DB2 MOVE M-BAS-DATA-BILL-FREQ-CODE    (1) TO TWA-BILL-FREQ-IN.   00012000
010490                                                                  00011380
011440     MOVE T-M-BAS-EE-MAR-STAT-CODE     TO TWA-MAR-STAT-IN.        00012010
011440*DB2 MOVE M-BAS-DATA-MAR-STAT-CODE     (1) TO TWA-MAR-STAT-IN.    00012010
010490                                                                  00011380
011450     SKIP3                                                        00012020
011460     IF  TRMDT-IND = +0                                           00012040
011460       IF  T-M-BAS-EE-TERM-ACT-DATE   IS NUMERIC  AND             00012040
011470           T-M-BAS-EE-TERM-ACT-DATE   GREATER THAN ZERO           00012050
011480             MOVE T-M-BAS-EE-TERM-ACT-DATE  TO  W-DATE            00012060
010540             PERFORM C9000-VALDT                                  00011430
011490             MOVE W-DATE9                 TO TWA-TERM-ACT-DATE-IN 00012070
011500       ELSE                                                       00012080
011510           MOVE ZEROS TO TWA-TERM-ACT-DATE-IN.                    00012090
011520                                                                  00012100
011460*DB2 IF  M-BAS-DATA-TERM-ACT-DATE (1)  IS NUMERIC  AND            00012040
011470*DB2     M-BAS-DATA-TERM-ACT-DATE (1)  GREATER THAN ZERO          00012050
011480*DB2     MOVE M-BAS-DATA-TERM-ACT-DATE (1)                        00012060
011490*DB2                                    TO TWA-TERM-ACT-DATE-IN   00012070
011500*DB2 ELSE                                                         00012080
011510*DB2     MOVE ZEROS TO TWA-TERM-ACT-DATE-IN.                      00012090
011520      SKIP3                                                       00012100
011520                                                                  00012100
011530     MOVE T-M-BAS-EE-NAT-CODE          TO TWA-NAT-CODE-IN.        00012110
011530*DB2 MOVE M-BAS-DATA-NAT-CODE          (1) TO TWA-NAT-CODE-IN.    00012110
010490                                                                  00011380
011540     MOVE T-M-BAS-EE-SEX-CODE          TO TWA-SEX-CODE-IN.        00012120
011540*DB2 MOVE M-BAS-DATA-SEX-CODE          (1) TO TWA-SEX-CODE-IN.    00012120
010490                                                                  00011380
010490     IF ( EMPLER-IND = +0 )          AND                          00011380
010490        ( T-M-BAS-EE-EMPLOYER-NUM )   >   0                       00011380
010490        MOVE T-M-BAS-EE-EMPLOYER-NUM  TO W-ER-REG-NUM             00011380
010490        MOVE W-ER-REG-NUM             TO TWA-ER-REG-NUM-IN        00011380
010490     ELSE                                                         00011380
010490     IF ( REGNUM-IND = +0 )          AND                          00011380
010490        ( T-M-BAS-EE-SEC-REG-NUM )    >   0                       00011380
010490        MOVE T-M-BAS-EE-SEC-REG-NUM   TO W-ER-REG-NUM             00011380
010490        MOVE W-ER-REG-NUM             TO TWA-ER-REG-NUM-IN        00011380
010490     ELSE                                                         00011380
010490        MOVE ZEROES                   TO W-ER-REG-NUM             00011380
010490        MOVE W-ER-REG-NUM             TO TWA-ER-REG-NUM-IN.       00011380
010490                                                                  00011380
011550*       MOVE T-M-BAS-EE-EMPLOYER-NUM  TO TWA-ER-REG-NUM-IN.       00012130
011550*DB2 MOVE M-BAS-DATA-ER-REG-NUM        (1) TO TWA-ER-REG-NUM-IN.  00012130
010490                                                                  00011380
011560     MOVE T-M-BAS-EE-BILL-DAY          TO TWA-COLL-DAY-IN.        00012140
011560*DB2 MOVE M-BAS-DATA-BILL-DAY          (1) TO TWA-COLL-DAY-IN.    00012140
010490                                                                  00011380
011570     MOVE T-M-BAS-EE-PIS-NUM           TO TWA-PIS-NUM-IN.         00012150
011570*DB2 MOVE M-BAS-DATA-PIS-NUM           (1) TO TWA-PIS-NUM-IN.     00012150
010490                                                                  00011380
011580     IF NEFFDT-IND = +0                                           00012170
011580       MOVE T-M-BAS-EE-NAT-EFF-DATE   TO W-DATE                   00012170
010540       PERFORM C9000-VALDT                                        00011430
011580       MOVE W-DATE9                   TO TWA-NAT-EFF-DATE-IN.     00012170
011580*DB2 MOVE M-BAS-DATA-NAT-EFF-DATE      (1) TO TWA-NAT-EFF-DATE-IN.00012170

S45342     IF NDRPDT-IND = +0
S45342       MOVE T-M-BAS-EE-DROP-NAT-DATE  TO W-DATE
S45342       PERFORM C9000-VALDT
S45342       MOVE W-DATE9                   TO TWA-DROP-NAT-DATE-IN1
S45342       MOVE TWA-DROP-NAT-DATE-IN1     TO TWA-DROP-NAT-DATE-IN
S45342       MOVE  DFHRED                   TO NTDRPYYC
S45342                                         NTDRPMMC
S45342                                         NTDRPDDC
S45342                                         NATNLTYC
S45342     ELSE
S45342       MOVE ZEROS                     TO TWA-DROP-NAT-DATE-IN.


S45165     IF CEXPDT-IND = +0
S45165       MOVE T-M-BAS-EE-CV-EXP-DATE  TO W-DATE
S45165       PERFORM C9000-VALDT
S45165       MOVE W-DATE9                   TO TWA-CIV-EXP-DATE-IN1
S45165       MOVE TWA-CIV-EXP-DATE-IN1     TO TWA-CIV-EXP-DATE-IN
S45165       MOVE  DFHRED                   TO CIVEXPYC
S45165                                         CIVEXPMC
S45165                                         CIVEXPDC
S45165     ELSE
S45165       MOVE ZEROS                     TO TWA-CIV-EXP-DATE-IN.

011590                                                                  00012200
011600     MOVE 'OOP9020P' TO TCAPCPI.                                  00012220
011610
011620     EXEC  CICS  LINK  PROGRAM (TCAPCPI)
011630                         RESP  (W-RESP)
011640     END-EXEC.
011650
011660     IF  W-RESP = DFHRESP (NORMAL)
011670         IF  RETURN-ZERO                                          00012260
011680             NEXT SENTENCE                                        00012270
011690         ELSE                                                     00012280
011700             MOVE '601' TO TCADCDC1                               012290
011710             PERFORM S0400-DUMP-XCTL                              00012300
011720     ELSE                                                         00012310
011730         MOVE '602' TO TCADCDC1                                   012320
011740         PERFORM S0400-DUMP-XCTL.                                 00012330
011750                                                                  00012340
011760                                                                  00025010
011770     CALL 'CCOBADDR'  USING  TWA-FLIP-B-LENGTH  FLIP-PTR.         00025040
011780     MOVE  FLIP-PTR   TO  TCTTE-ADDR4.                            00025040
011790                                                                  00025010
011800     MOVE  FLIP-PTR            TO  TCTTE-ADDR4.                   00012380
011810     ADD   FLIP-PTR  2     GIVING  TCTTE-ADDR2.                   00012390
011820**   ADD   FLIP-PTR  89    GIVING  TCTTE-ADDR3.                   SMR91553
011830**   ADD   FLIP-PTR  2  98 GIVING  TCTTE-ADDR3.                   SMR91553
011840
011850*TAH ADD   FLIP-PTR  2 100 GIVING  TCTTE-ADDR3.                   SMR43515
011850*ZAI ADD   FLIP-PTR  2 101 GIVING  TCTTE-ADDR3.                   SMR43515
S97283*    ADD   FLIP-PTR  2 103 GIVING  TCTTE-ADDR3.                   SMR93306
S97283     ADD   FLIP-PTR  2 119 GIVING  TCTTE-ADDR3.                   SMR93306
011860
011870*TAH MOVE  100  TO  TWA-FLIP-B-LENGTH.                            SMR43515
011870*ZAI MOVE  101  TO  TWA-FLIP-B-LENGTH.                            SMR43515
S97283*    MOVE  103  TO  TWA-FLIP-B-LENGTH.                            SMR93306
S97283     MOVE  119  TO  TWA-FLIP-B-LENGTH.                            SMR93306
011880
011890**   MOVE  87   TO  TWA-FLIP-B-LENGTH.                            SMR91553
011900**   MOVE  98   TO  TWA-FLIP-B-LENGTH.                            SMR91553
011910                                                                  00012430
011920     MOVE 'OOP9020P' TO TCAPCPI.                                  00012220
011930
011940     EXEC  CICS  LINK  PROGRAM (TCAPCPI)
011950                         RESP  (W-RESP)
011960     END-EXEC.
011970
011980     IF  W-RESP = DFHRESP (NORMAL)
011990         IF  RETURN-ZERO                                          00012490
012000             NEXT SENTENCE                                        00012500
012010         ELSE                                                     00012510
012020             MOVE '605' TO TCADCDC1                               012520
012030             PERFORM S0400-DUMP-XCTL                              00012530
012040     ELSE                                                         00012540
012050         MOVE '606' TO TCADCDC1                                   012550
012060         PERFORM S0400-DUMP-XCTL.                                 00012560
012070                                                                  00012570
012080 C0600-FLIP-EXIT.  EXIT.                                          00012580
012090     EJECT                                                        00012590

      *****************************************************************
      *                     C0750-DATE-TIME  SECTION                  *
      *****************************************************************
      *        THIS SECTION MOVE CURRENT DATE AND TIME                *
      *             TO THE MAP'S FIELDS                               *
      *****************************************************************
S96847 C0750-DATE-TIME SECTION.

           MOVE FUNCTION CURRENT-DATE TO GREG-DATE
           MOVE FUNCTION REVERSE(GREG-DATE) TO W-CUR-DATE
           MOVE W-CUR-DATE TO CDATEO

           EXEC CICS
                ASKTIME
           END-EXEC.
           MOVE EIBTIME  TO W-TIME
           MOVE FUNCTION REVERSE(W-TIME) TO CTIMEO
           INSPECT CTIMEO CONVERTING '/' TO ':'.

       C0750-DATE-TIME-EXIT.
           EXIT.

012100*****************************************************************
012110*                        D0000-FINAL                            *
012120*   THIS SECTION:                                               *
012130*        -RETURNS CONTROL TO CICS.                              *
012140*****************************************************************
012160 D0000-FINAL SECTION.
S9279      SKIP1                                                        SMR91223
S9279      MOVE  K-PF10-11                 TO ERRMSG1O.                 SMR91223
S9279      MOVE  DFHNEUTR                  TO ERRMSG1C.                 SMR94468
S9279      SKIP1                                                        SMR91223
012170
S96847     PERFORM C0750-DATE-TIME.

012180     EXEC CICS SEND MAP ('MAP01   ')                              00000010
012190                    MAPSET ('ORM1511')                            00000010
012200                    FROM  (MAP01O)                                00000010
012210                    ERASE  CURSOR  FREEKB                         00000010
012220     END-EXEC.                                                    00000010
010490                                                                  00011380
012250 D0000-FINAL-RETURN.
012260
012270     MOVE '1511' TO TCANXTID.
           MOVE '1'    TO TCTTE-ENTRY-COUNT.
012280     EXEC CICS RETURN                                             00000010
012290          TRANSID (TCANXTID)                                      00020170
012300     END-EXEC.                                                    00020170
012310                                                                  00020170
012320
012330 D0000-FINAL-EXIT.
012330     EXIT.
012340     EJECT
012320
012320 F0000-CHECK-BLOCK-CODE   SECTION.
012320
012320     IF  T-M-BAS-EE-BLOCK-CODE    EQUAL TO '1'
012320         MOVE W-MSG-BLOCK-SSN        TO ERRMSG1O
012320         MOVE ATT-UNPROT-HILITE-MDT  TO ERRMSG1A
012320     ELSE
012320         NEXT SENTENCE.
012320
012320 F0000-CHECK-BLOCK-CODE-EXIT.
012320            EXIT.
012350*****************************************************************
012360*                      S0100-LOCATE-TRAILER                     *
012370*   THIS SECTION:                                               *
012380*        -LOCATE DESIRED TRAILER                                *
012390*****************************************************************
012410 S0100-LOCATE-TRAILER  SECTION.
012420*DB2
012430*DB2 MOVE 'OOP0009P' TO TCAPCPI.
012440*DB2
012450*DB2 EXEC  CICS  LINK  PROGRAM (TCAPCPI)
012460*DB2                     RESP  (W-RESP)
012470*DB2 END-EXEC.
012480*DB2
012490*DB2 IF  W-RESP = DFHRESP (NORMAL)
012500*DB2     NEXT SENTENCE
012510*DB2 ELSE
012520*DB2     MOVE 'TLK' TO TCADCDC1
012530*DB2     PERFORM S0400-DUMP-XCTL.
012540*DB2
012550 S0100-LOCATE-EXIT.
012550     EXIT.
012560     EJECT
012570*****************************************************************
012580*                    S0400-DUMP-XCTL                            *
012600*****************************************************************
012620 S0400-DUMP-XCTL SECTION.
012630
012650     EXEC CICS DUMP                                               00024580
012660             DUMPCODE (TCADCDC)
012670     END-EXEC.                                                    00024580
012680
012690     MOVE HIGH-VALUES TO TCTTE-EXIT-SW.
012700     MOVE HIGH-VALUES TO TCTTE-ENTRY-SW.
012710     MOVE 'ORP1500P'  TO TCAPCPI.
012720     PERFORM S0500-XCTL.
012730
012740 S0400-DUMP-XCTL-EXIT.
012740     EXIT.
012750     SKIP3
012760*****************************************************************
012770*                    S0500-XCTL                                 *
012780*****************************************************************
012800 S0500-XCTL SECTION.
012820                                                                  00024580
012830     EXEC CICS XCTL PROGRAM (TCAPCPI)                             00024580
012840     END-EXEC.                                                    00024580
012850                                                                  00024580
012870 S0500-XCTL-EXIT.
012870      EXIT.
012860
012860
012860 C9000-VALDT     SECTION.
012860*-----------------------*
012860
012860     MOVE W-YEAR TO W-YEAR9
012860     MOVE W-MNTH TO W-MNTH9
012860     MOVE W-DAY  TO W-DAY9.
012860
012860 C9000-VALDT-EXIT.
012860     EXIT.
012860
012860
012860 DB2-ERROR         SECTION.
012860*-------------------------*
012860     IF NOT (SQLCODE = +0 OR +100)
012860        MOVE SQLCA TO ORSS0400-SQLCA
012860        EXEC CICS
012860             SYNCPOINT ROLLBACK
012860        END-EXEC
012860*
012860        MOVE 'SSS'               TO ORSS0400-SYSID
012860        MOVE TWANUM              TO ORSS0400-SSN
012860*
012860        MOVE TCTTE-CURNT-TRAN-ID TO ORSS0400-TRNID
012860        MOVE TCTTE-DEPT          TO ORSS0400-CLERK(1:2)
012860        MOVE TCTTE-CLERK         TO ORSS0400-CLERK(3:3)
012860        MOVE W-EIBDS             TO ORSS0400-TABNM
012860        EXEC CICS
012860             LINK
012860             PROGRAM ('ORSS0400')
012860             COMMAREA(ORSS0400-AREA)
012860             LENGTH  (LENGTH OF ORSS0400-AREA)
012860        END-EXEC
012860        MOVE ORSS0400-SQLCD                   TO W-SQLCD
012860        MOVE FUNCTION REVERSE(W-SQLCD-C(7:4)) TO ABEND-ERR-CODE
012860        MOVE FUNCTION REVERSE(W-EIBDS)        TO ABEND-ERR-EIBDS
012860        MOVE '›·„ ⁄„ √ÿŒ'                     TO ABEND-ERR-TEXT
012860*
012860        MOVE ATT-PROT-HILITE                  TO ERRMSG1A
012860        MOVE ABEND-ERR-MSG                    TO ERRMSG1O
012860        PERFORM D0000-FINAL
012860     END-IF.
012860
012860 DB2-ERROR-EXIT.
012860     EXIT.
012860
