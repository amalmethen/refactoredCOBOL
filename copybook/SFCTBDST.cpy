000010******************************************************************00000010
000020*                   *** SSSS TABLE ***                           *00000020
000030*      PROD VERSION                    COPY MEMBER - SFCTBDST    *00000030
000040*      LAST CHANGE BY TECH-SUPP 28 NOV 1985. (ADD EMBSYS)        *00000040
000050*      D E S T I N A T I O N   T Y P E S   T A B L E             *00000050
000060*                                                                *00000060
000070*  LAST MODIFICATION ===> SMR 90461  ADD DEST CODE 19 (GL ALW)   *00000060
000080*                                    YOUSEF ALBRAHIM 30/10/88    *00000060
000090*  MODIFICATION ===> SMR 90683  ADD DEST CODE 75 (ALGL)          *00000060
000100*                                    BASSAM ABU-ATWAN 26/10/92   *00000060
000110******************************************************************00000070
000120     SKIP2                                                        00000080
000130     05  K-OA-DEST               PIC X(2) VALUE '03'.             00000090
000140     05  K-EIF-EXT-DEST          PIC X(2) VALUE '04'.             00000100
000150     05  K-AUTO-DISB-DEST        PIC X(2) VALUE '06'.             00000110
000160     05  K-CCF-UPD-DEST          PIC X(2) VALUE '07'.             00000120
000170     05  K-BCH-COLL-DEST         PIC X(2) VALUE '08'.             00000130
000180     05  K-PI-DEST               PIC X(2) VALUE '09'.             00000140
000190     05  K-FM-GL-DEST            PIC X(2) VALUE '10'.             00000150
000200     05  K-FM-DEST               PIC X(2) VALUE '12'.             00000160
000210     05  K-ALPHNDX1-DEST         PIC X(2) VALUE '14'.             00000170
000220     05  K-ALPHNDX2-DEST         PIC X(2) VALUE '15'.             00000180
000230     05  K-BILL-EXT-DEST         PIC X(2) VALUE '16'.             00000190
000240     05  K-PERSNL-DEST           PIC X(2) VALUE '17'.             00000200
000250     05  K-EMBSYS-DEST           PIC X(2) VALUE '18'.             00000210
000260     05  K-GL-ALW-DEST           PIC X(2) VALUE '19'.             00000220
000270     05  K-GL-GL-DEST            PIC X(2) VALUE '20'.             00000220
000280     05  K-KK-TABLE-DEST         PIC X(2) VALUE '21'.             00000230
000290     05  K-GL-ALN-DEST           PIC X(2) VALUE '22'.             00000220
000300     05  K-GL-ADM-DEST           PIC X(2) VALUE '23'.             00000220
000310     05  K-GL-GLB-DEST           PIC X(2) VALUE '24'.             00000220
000320     05  K-INVEST-DEST           PIC X(2) VALUE '26'.             00000240
000330     05  K-FM-RPT-DEST           PIC X(2) VALUE '30'.             00000250
000340     05  K-TRANS-HIST-DEST       PIC X(2) VALUE '50'.             00000260
000350     05  K-SE-CONT-DEST          PIC X(2) VALUE '60'.             00000270
000360     05  K-BATCH-COLL-RPT-DEST   PIC X(2) VALUE '61'.             00000280
000370     05  K-AUTO-COLL-NDX-UPD     PIC X(2) VALUE '70'.             00000290
000380     05  K-ALGL-DEST             PIC X(2) VALUE '75'.             00000110
